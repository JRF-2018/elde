#!/usr/bin/perl
require 5.008;
our $VERSION = "0.03"; #Time-stamp: <2018-04-13T11:19:40Z>

##
## License:
##
##   The author is a Japanese.
##
##   I intended this program to be public-domain, but you can treat
##   this program under the (new) BSD-License or under the Artistic
##   License, if it is convenient for you.
##
##   Within three months after the release of this program, I
##   especially admit responsibility of efforts for rational requests
##   of correction to this program.
##
##   I often have bouts of schizophrenia, but I believe that my
##   intention is legitimately fulfilled.
##
## Author:
##
##   JRF
##   http://jrf.cocolog-nifty.com/software/
##   (The page is written in Japanese.)
##

use strict;
use warnings;
use utf8; # Japanese

BEGIN {
  my @mypackage = qw(
		      Main
		      Main::_Simple
		      Main::_ContextHasher
		      ELDE2::_Context
		      ELDE2::_Process
		      ELDE2::_Emulator
		      ELDE0::_Context
		      ELDE0::_Process
		      ELDE0::_ResultContextTreeNode
		      ELDE0::_Emulator
		   );
  for my $p (@mypackage) {
    eval <<"EOM"; # eval the string til "End Of Macro".
    {
      package $p;
      use POSIX;
      use Math::Trig; # for pi
      use Storable qw(dclone);
      use Data::Dumper;
      use Carp;
      use Encode;
    }
EOM
    die $@ if $@;
  }

  my @myfunction = qw(
		    );
  {
    no strict;
    foreach my $p (@mypackage) {
      for my $f (@myfunction) {
	my @f = split("::", $f);
	*{$p . "::" . $f[$#f]} = \&{$f};
      }
    }
  }
}


package Main;

our $DEBUG = 1;

{
  package Combinations;

  ## refer to:
  ## 《python - Where can I find source code for itertools.combinations() function - Stack Overflow》
  ## https://stackoverflow.com/questions/5731505/where-can-i-find-source-code-for-itertools-combinations-function

  sub new {
    my $class = shift;
    my $obj = {};
    $obj->{l} = shift;
    $obj->{r} = shift;
    if (scalar @{$obj->{l}} < $obj->{r}) {
      return undef;
    }
    bless $obj, $class;
    return $obj;
  }

  sub next {
    my $self = shift;
    my $n = scalar @{$self->{l}};
    my $r = $self->{r};
    if (! exists $self->{tmp}) {
      $self->{tmp} = [0 .. ($r - 1)];
    } else {
      my $i;
      for ($i = $r - 1; $i >= 0; $i--) {
	if ($self->{tmp}->[$i] != $i + $n - $r) {
	  last;
	}
      }
      if ($i == -1) {
	return undef;
      }
      $self->{tmp}->[$i]++;
      for (my $j = $i + 1; $j < $r; $j++) {
	$self->{tmp}->[$j] = $self->{tmp}->[$j - 1] + 1;
      }
    }
    my @r;
    foreach my $c (@{$self->{tmp}}) {
      push(@r, $self->{l}->[$c]);
    }
    return \@r;
  }
}

{
  package Main::_Simple;

  my %template = ("Main::_Simple" => {});

  sub extend_template {
    my $class = shift;
    my @hash = @_;
    if (exists $template{$class}) {
      $template{$class} = {%{$template{$class}}, @hash} if @hash;
    } else {
      $template{$class} = 
	{(map {
	        $_->extend_template() if ! exists $template{$_};
	        %{$template{$_}};
	      } (eval '@{' . $class . '::ISA}')),
	 @hash
	};
    }
    return $template{$class};
  }

  sub get_template {
    my $class = shift;
    return $class->extend_template();
  }

  sub new {
    my $class = shift;
    my $obj = dclone($class->get_template());
    bless $obj, $class;
    return $obj;
  }
}

{
  package Main::_ContextHasher;
  use base qw(Main::_Simple);

  __PACKAGE__->extend_template
    (
     _array => [],
     _hash_table => {},
    );

  sub _dump {
    my ($context) = @_;
    my $dumper = Data::Dumper->new([{_data => $context->{_data}}]);
    $dumper->Sortkeys(1);
    $dumper->Terse(1);
    $dumper->Indent(0);
    return $dumper->Dump();
  }

  sub _compute_hash {
    my ($s) = @_;
    my $h = 0;
    foreach my $i (unpack("C*", $s)) {
      $h = ($h + $i) & 0x3FF;
    }
    return $h;
  }

  sub get_or_create_id {
    my $self = shift;
    my ($context) = @_;
    my $d = _dump($context);
    my $h = _compute_hash($d);
    if (exists $self->{_hash_table}->{$h}) {
      foreach my $i (@{$self->{_hash_table}->{$h}}) {
	if ($self->{_array}->[$i] eq $d) {
	  return $i;
	}
      }
    }
    if (! exists $self->{_hash_table}->{$h}) {
      $self->{_hash_table}->{$h} = [];
    }

    push(@{$self->{_array}}, $d);

    push(@{$self->{_hash_table}->{$h}}, $#{$self->{_array}});
    return $#{$self->{_array}};
  }

  sub id_to_data {
    my $self = shift;
    my ($id) = @_;
    return undef if $id > $#{$self->{_array}};
    return eval($self->{_array}->[$id]);
  }
}

##
## ELDE2
##

{
  package ELDE2::_Context;
  use base qw(Main::_Simple);

  __PACKAGE__->extend_template
    (
     _state => "U", # U: Unlocked, L: Locked.

     _process => undef,

     _data => {}, # hash of name => value,

     _parent_ds => "",

     _debug_symbol => "",
    );

  sub lock {
    my $self = shift;
    my (%opt) = @_;

    my $ds = sprintf("P%dL%d:lock", $self->{_process}->{_id}, $self->{_process}->{_lock_count});

    die "$ds: Reach the limit of lock count."
      if $self->{_process}->{_lock_count}
	>= $self->{_process}->{_lock_limit};
    die "$ds: Locked already."
      if $self->{_state} eq "L";

    $self->{_process}->synthesize_me(%opt);
    $self->{_state} = "L";
  }

  sub unlock {
    my $self = shift;
    my (%opt) = @_;

    my $ds = sprintf("P%dL%d:lock", $self->{_process}->{_id}, $self->{_process}->{_lock_count});

    die "$ds: Unlocked already."
      if $self->{_state} eq "U";

    $self->{_process}->synthesize_other(%opt);
    $self->{_state} = "U";
    $self->{_process}->{_lock_count}++;
  }

  sub get {
    my $self = shift;
    my ($name) = @_;

    my $ds = sprintf("P%dL%d:get", $self->{_process}->{_id}, $self->{_process}->{_lock_count});

    die "$ds: 'get' called before lock."
      if $self->{_state} ne "L";

    return $self->{_data}->{$name};
  }

  sub set {
    my $self = shift;
    my ($name, $value) = @_;

    my $ds = sprintf("P%dL%d:set", $self->{_process}->{_id}, $self->{_process}->{_lock_count});

    die "$ds: 'set' called before lock."
      if $self->{_state} ne "L";

    $self->{_data}->{$name} = $value;
  }

  sub cut {
    my $self = shift;

    my $ds = sprintf("P%dL%d:set", $self->{_process}->{_id}, $self->{_process}->{_lock_count});
    die "NORMAL: $ds: cut down by user.";
  }
}

{
  package ELDE2::_Process;
  use base qw(Main::_Simple);

  __PACKAGE__->extend_template
    (
     _id => undef,

     _emulator => undef,

     _program => undef,

     _context => undef,

     _result_context => [],

     _follow_mode => 0,

     _lock_spec => [],

     _cur_spec => [],

     _lock_limit => undef, # currently need this value.

     _lock_count => 0, # <= _lock_limit
    );

  sub synthesize_me {
    my $self = shift;
    my %opt = @_;
    my @spec = @{$self->{_lock_spec}};
    my $q = $self->{_id};
    my $m = $self->{_lock_count};
    my $lid = "P${q}L${m}";
    my $ds = sprintf("P%dL%d:synthesize_me", $q, $m);

    return if $self->{_follow_mode};

    my $i;
    for ($i = 0; $i < @spec; $i++) {
      last if $lid eq $spec[$i];
    }
    die "$ds: \"$lid\" no in lock spec" if $i == @spec;

    my $spec = "";
    $spec = join(",", @spec[0 .. ($i - 1)]) if $i > 0;
    if (exists $self->{_emulator}->{_result_context}->{$spec}) {
      my $rc = $self->{_emulator}->{_result_context}->{$spec};
      my $cont = $self->{_emulator}->{_context_hasher}->id_to_data($rc->{_context});
      $self->{_context}->{_data} = $cont->{_data};
      $self->{_cur_spec} = [];
      $self->{_cur_spec} = [@spec[0 .. ($i - 1)]] if $i > 0;
      return;
    } else {
      $spec = join(",", $spec, $lid);
      if (exists $self->{_emulator}->{_result_context}->{$spec}) {
	die "NORMAL: $ds: Run already.";
      }
      $self->{_follow_mode} = 1;
      return;
    }
  }

  sub synthesize_other {
    my $self = shift;
    my %opt = @_;
    my $q = $self->{_id};
    my $m = $self->{_lock_count};
    my $lid = "P${q}L${m}";
    my $cid = $self->{_emulator}->{_context_hasher}->get_or_create_id($self->{_context});
    push(@{$self->{_cur_spec}}, $lid);
    my $spec = join(",", @{$self->{_cur_spec}});
    $self->{_emulator}->{_result_context}->{$spec}
      = {_context => $cid, _terminate => 0};
  }

  sub run_by_lock_spec {
    my $self = shift;
    my ($spec) = @_;
    # $spec にある lock ナンバーよりも短いときどうしよう？prev_spec と同じにするか、cut するか。undef を返して cut してもらうのがよさそう…。
    $self->{_lock_spec} = [grep {$_ ne ""} (split(/,/, $spec))];
    my $cont = ELDE2::_Context->new();
    if (defined $self->{_emulator}->{_initial_context_id}) {
      my $id = $self->{_emulator}->{_initial_context_id};
      my $dat = $self->{_emulator}->{_context_hasher}->id_to_data($id);
      $cont->{_data} = $dat->{_data};
    }
    $cont->{_process} = $self;
    $self->{_context} = $cont;
    $self->{_cur_spec} = [];
    $self->{_follow_mode} = 0;
    $self->{_lock_count} = 0;
    my @arg;
    #ここで arg_spec に基づいて @arg を作る。
    eval { &{$self->{_program}}($cont, @arg) };
    if ($@) {
      if ($@ =~ /^NORMAL:/) {
	my $cspec = join(",", @{$self->{_cur_spec}});
#	print "cut!\n" if $DEBUG;
	return $cspec;
      } else {
	die $@;
      }
    } else {
      my $p = $self->{_id};
      my $n = $self->{_lock_count};
      my $lid = "P${p}L{$n}";
      if ($self->{_context}->{_state} eq 'L') {
	die "$lid: Teminated before unlocking.";
      }
      my $cspec = join(",", @{$self->{_cur_spec}});
      $self->{_emulator}->{_result_context}->{$cspec}->{_terminate} = 1;
      return $cspec;
    }
  }
}


{
  package ELDE2::_Emulator;
  use base qw(Main::_Simple);

  __PACKAGE__->extend_template
    (
     _process => [],

     _result_context => {},

     _terminated_context => {},

     _context_hasher => undef,

     _initial_context_id => undef,

    );

  sub new_process {
    my $self = shift;
    my ($prog, %opt) = @_;
    my $proc = ELDE2::_Process->new();
    foreach my $need ("lock_limit") {
      if (! exists $opt{$need}) {
	die "NEW_PROCESS: Not enough argument.: $need";
      }
      $proc->{"_" . $need} = $opt{$need};
    }
    $proc->{_emulator} = $self;
    $proc->{_program} = $prog;

    push(@{$self->{_process}}, $proc);
    $proc->{_id} = $#{$self->{_process}};
  }

  sub set_initial_context {
    my $self = shift;
    my %data = @_;
    $self->{_initial_context_id}
      = $self->{_context_hasher}->get_or_create_id({_data => \%data});
  }

  sub run_by_lock_spec {
    my $self = shift;
    my ($lock_spec) = @_;
    my @spec = grep {$_ ne ""} (split(/,/, $lock_spec));

    print "run_by_lock_spec: $lock_spec\n" if $DEBUG;

    if (! exists $self->{_result_context}->{""}) {
      $self->{_result_context}->{""}
	= {_context => $self->{_initial_context_id}, _terminate => 0};
    }
    my %terminated = ();
    for (my $i = 0; $i < @{$self->{_process}}; $i++) {
      $terminated{$i} = 0;
    }
    for (my $cur = 0; $cur < @spec; $cur++) {
      my $cspec = "";
      my $lid = $spec[$cur];
      $lid =~ /^P([01-9]+)L([01-9]+)$/;
      my $p = $1;
      my $r;
      $cspec = join(",", @spec[0 .. $cur]);
      if (! exists $self->{_result_context}->{$cspec}) {
	if (! $terminated{$p}) {
	  $r = $self->{_process}->[$p]->run_by_lock_spec($lock_spec);
	} else {
	  print "cut! lock spec is too long.\n";
	  last;
	}
      }
      if (! exists $self->{_result_context}->{$cspec}) {
	die "$lid: Something wrong!: $r";
      }
      if ($self->{_result_context}->{$cspec}->{_terminate}) {
	$terminated{$p} = 1;
      }
      my $terminated = 1;
      for (my $i = 0; $i < @{$self->{_process}}; $i++) {
	if (! $terminated{$i}) {
	  $terminated = 0;
	  last;
	}
      }
      if ($terminated) {
	$self->{_terminated_context}->{$cspec} = $self->{_result_context}->{$cspec}->{_context};
	if ($cspec eq $lock_spec) {
	  print "normally terminated.\n";
	} else {
	  print "cut! terminated before lock_limit: $cspec.\n";
	}
	last;
      }
    }
  }

  sub run {
    my $self = shift;

    my @vec1;
    my @vec2;
    my $acc = 0;
    for (my $i = 0; $i < @{$self->{_process}}; $i++) {
      my $ll = $self->{_process}->[$i]->{_lock_limit};
      push(@vec1, $ll);
      $acc += $ll;
      push(@vec2, $acc);
    }
    my @vec3;
    my @vec4;
    for (my $i = 0; $i < @{$self->{_process}} - 1; $i++) {
      $vec3[$i] = Combinations->new([0 .. ($vec2[$i + 1] - 1)], $vec1[$i + 1]);
      $vec4[$i] = $vec3[$i]->next();
    }

    while (1) {
      my @cur = ();
      for (my $i = 0; $i < $vec1[0]; $i++) {
	push(@cur, "P0L${i}");
      }
      for (my $i = 0; $i < @vec4; $i++) {
	my $q = $i + 1;
	my $l = 0;
	my @l = @{$vec4[$i]};
	my @next;
	for (my $j = 0; $j < $vec2[$i + 1]; $j++) {
	  if (@l && $l[0] == $j) {
	    push(@next, "P${q}L${l}");
	    $l++;
	    shift(@l);
	  } else {
	    push(@next, shift(@cur));
	  }
	}
	@cur = @next;
      }

      $self->run_by_lock_spec(join(",", @cur));

      my $k;
      for ($k = @vec3 - 1; $k >= 0; $k--) {
	my $n = $vec3[$k]->next();
	if (defined $n) {
	  $vec4[$k] = $n;
	  for (my $j = $k + 1; $j < @vec4; $j++) {
	    $vec3[$j] = Combinations->new([0 .. ($vec2[$j + 1] - 1)], $vec1[$j + 1]);
	    $vec4[$j] = $vec3[$j]->next();
	  }
	  last;
	}
      }
      last if ($k < 0);
    }
  }

  sub new {
    my $class = shift;
    my %opt = @_;
    my $obj = $class->SUPER::new(@_);
    if (exists $opt{context_hasher}) {
      $obj->{_context_hasher} = $opt{context_hasher};
    } else {
      $obj->{_context_hasher} = Main::_ContextHasher->new();
    }
    my $init = {};
    if (exists $opt{initial_context}) {
      $init = $opt{initial_context};
    }
    my $cid = $obj->{_context_hasher}->get_or_create_id({_data => $init});
    $obj->{_initial_context_id} = $cid;

    return $obj;
  }
}


##
## ELDE0
##

{
  package ELDE0::_Context;
  use base qw(Main::_Simple);

  __PACKAGE__->extend_template
    (
     _state => "U", # U: Unlocked, L: Locked.

     _process => undef,

     _data => {}, # hash of name => value,

     _parent_ds => "",

     _debug_symbol => "",
    );

  sub lock {
    my $self = shift;
    my (%opt) = @_;

    $self->{_debug_symbol} = $self->{_parent_ds} . $opt{ds}
      if exists $opt{ds};
    $self->{_debug_symbol} = $self->{_parent_ds} . $opt{debug_symbol}
      if exists $opt{debug_symbol};

    die "$self->{_debug_symbol}: Reach the limit of lock count."
      if $self->{_process}->{_lock_count}
	>= $self->{_process}->{_lock_limit};
    die "$self->{_debug_symbol}: Locked already."
      if $self->{_state} eq "L";

    $self->{_process}->synthesize_me(%opt);
    $self->{_state} = "L";
  }

  sub unlock {
    my $self = shift;
    my (%opt) = @_;

    $self->{_debug_symbol} = $self->{_parent_ds} . $opt{ds}
      if exists $opt{ds};
    $self->{_debug_symbol} = $self->{_parent_ds} . $opt{debug_symbol}
      if exists $opt{debug_symbol};

    die "$self->{_debug_symbol}: Unlocked already."
      if $self->{_state} eq "U";

    $self->{_process}->synthesize_other(%opt);
    $self->{_state} = "U";
    $self->{_process}->{_lock_count}++;
  }

  sub get {
    my $self = shift;
    my ($name) = @_;

    die "$self->{_debug_symbol}: 'get' called before lock."
      if $self->{_state} ne "L";

    return $self->{_data}->{$name};
  }

  sub set {
    my $self = shift;
    my ($name, $value) = @_;

    die "$self->{_debug_symbol}: 'set' called before lock."
      if $self->{_state} ne "L";

    $self->{_data}->{$name} = $value;
  }

  sub cut {
    my $self = shift;
    die "NORMAL: cut down by user.";
  }
}

{
  package ELDE0::_Process;
  use base qw(Main::_Simple);

  __PACKAGE__->extend_template
    (
     _emulator => undef,

     _program => undef,

     _context => undef,

     _result_context => [],

     _lock_spec => "",

     _lock_limit => undef, # currently need this value.

     _lock_count => 0, # <= _lock_limit

     _process_num => undef,
    );

  sub synthesize_me {
    my $self = shift;
    my %opt = @_;
    my @spec = grep {$_ ne ""} (split(/,/, $self->{_lock_spec}));
    my $q = $self->{_process_num};
    my $m = $self->{_lock_count};

   for (my $i = 0; $i < @spec; $i++) {
      my $part = $spec[$i];
      if ($part =~ /^P([01-9]+)U([01-9]+)\-\>P${q}L${m}$/) {
	my $p = $1;
	my $n = $2;
	my $prev = "";
	$prev = join(",", @spec[0 .. ($i - 1)]) if $i > 0;
	my $rc = $self->{_emulator}->{_result_context_tree}->get($prev)->{_result_context};
	if ($n >= @{$rc->[$p]}) {
	  # unlock が足りないからこの先は実行できないけど、別に普通のことなので catch しないといけない。
	  die("NORMAL: lock spec is too long.");
	}

	my $cont = $self->{_emulator}->{_context_hasher}->id_to_data($rc->[$p]->[$n]);
	$self->{_context}->{_data} = $cont->{_data};
	return;
      }
    }
    return;
#    die "$self->{_debug_symbol}: No spec : $self->{_lock_spec}";
  }

  sub synthesize_other {
    my $self = shift;
    my %opt = @_;
    my $cid = $self->{_emulator}->{_context_hasher}->get_or_create_id($self->{_context});
    $self->{_result_context}->[$self->{_lock_count}] = $cid;
  }

  sub run_by_lock_spec {
    my $self = shift;
    my ($spec) = @_;
    # $spec にある lock ナンバーよりも短いときどうしよう？prev_spec と同じにするか、cut するか。undef を返して cut してもらうのがよさそう…。
    $self->{_lock_spec} = $spec;
    my $cont = ELDE0::_Context->new();
    if (defined $self->{_emulator}->{_initial_context_id}) {
      my $id = $self->{_emulator}->{_initial_context_id};
      my $dat = $self->{_emulator}->{_context_hasher}->id_to_data($id);
      $cont->{_data} = $dat->{_data};
    }
    $cont->{_process} = $self;
    $cont->{_parent_ds} = $self->{_debug_symbol}
      if exists $self->{_debug_symbol};
    $self->{_context} = $cont;
    $self->{_lock_count} = 0;
    $self->{_result_context} = [];
    my @arg;
    #ここで arg_spec に基づいて @arg を作る。
    eval { &{$self->{_program}}($cont, @arg) };
    if ($@) {
      if ($@ =~ /^NORMAL:/) {
	print "cut!\n" if $DEBUG;
	return undef;
      } else {
	die $@;
      }
    } else {
      my $p = $self->{_process_num};
      my $n = $self->{_lock_count};
      my @spec = grep {$_ ne ""} (split(/,/, $self->{_lock_spec}));
      foreach my $part (@spec) {
	if ($part =~ /^P([01-9]+)U([01-9]+)\-\>P([01-9]+)L([01-9]+)$/) {
	  if ($1 == $p && $2 >= $n
	      || $3 == $p && $4 >= $n) {
	    print "cut!\n" if $DEBUG;
	    return undef;
	  }
	}
      }
      return $self->{_result_context};
    }
  }
}

{
  package ELDE0::_ResultContextTreeNode;
  use base qw(Main::_Simple);

  __PACKAGE__->extend_template
    (
     _result_context => undef, # length == num of processes.
     _branch => {},
    );

  sub size {
    my $self = shift;
    my $r = 1;
    foreach my $k (keys %{$self->{_branch}}) {
      $r += $self->{_branch}->{$k}->size();
    }
    return $r;
  }

  sub depth {
    my $self = shift;
    my $max = 0;
    foreach my $k (keys %{$self->{_branch}}) {
      my $tmp = $self->{_branch}->{$k}->depth();
      $max = $tmp if $tmp > $max;
    }
    return $max + 1;
  }

  sub get {
    my $self = shift;
    my ($spec) = @_;
    my @spec = grep {$_ ne ""} (split(/,/, $spec));
    return $self if ! @spec;
    my $f = shift @spec;
    return undef if ! exists $self->{_branch}->{$f};
    return $self->{_branch}->{$f}->get(join(",", @spec));
  }

  sub dump {
    my $self = shift;
    my ($spec) = @_;
    my @spec = grep {$_ ne ""} (split(/,/, $spec));
    my $r .= "$spec\n";
    foreach my $k (sort keys %{$self->{_branch}}) {
      $r .= $self->{_branch}->{$k}->dump(join(",", @spec, $k));
    }
    return $r;
  }
}

{
  package ELDE0::_Emulator;
  use base qw(Main::_Simple);

  __PACKAGE__->extend_template
    (
     _process => [],

     _result_context_tree => undef, 

     _context_hasher => undef,

     _initial_context_id => undef,

    );

  sub new_process {
    my $self = shift;
    my ($prog, %opt) = @_;
    my $proc = ELDE0::_Process->new();
    foreach my $need ("lock_limit") {
      if (! exists $opt{$need}) {
	die "NEW_PROCESS: Not enough argument.: $need";
      }
      $proc->{"_" . $need} = $opt{$need};
    }
    $proc->{_emulator} = $self;
    $proc->{_program} = $prog;
    $proc->{_debug_symbol} = "P" . 

    push(@{$self->{_process}}, $proc);
    $proc->{_process_num} = $#{$self->{_process}};
    $proc->{_debug_symbol} = "P" . $proc->{_process_num};
  }

  sub set_initial_context {
    my $self = shift;
    my %data = @_;
    $self->{_initial_context_id}
      = $self->{_context_hasher}->get_or_create_id({_data => \%data});
  }

  sub _make_lock_spec_tree {
    my $self = shift;
    my ($spec) = @_;
    my @spec = grep {$_ ne ""} (split(/,/, $spec));
    my @max_u = (-1) x scalar @{$self->{_process}};
    my @max_l = (-1) x scalar @{$self->{_process}};
    foreach my $part (@spec) {
      if ($part !~ /^P([01-9]+)U([01-9]+)->P([01-9]+)L([01-9]+)$/) {
	die "MAKE_LOCK_SPEC_TREE: Parse error. ";
      }
      my $p = $1;
      my $n = $2;
      my $q = $3;
      my $m = $4;
      $max_u[$p] = $n + 1;
      $max_l[$p] = $n + 1;
      $max_u[$q] = $m;
      $max_l[$q] = $m + 1;
    }

    my $node = ELDE0::_ResultContextTreeNode->new();
    for (my $p = 0; $p <= $#{$self->{_process}}; $p++) {
      for (my $q = 0; $q <= $#{$self->{_process}}; $q++) {
	next if $p == $q;
	for (my $n = $max_u[$p];
	     $n < $self->{_process}->[$p]->{_lock_limit}; $n++) {
	  next if $n < 0;
	  for (my $m = $max_l[$q];
	       $m < $self->{_process}->[$q]->{_lock_limit}; $m++) {
	    next if $m < 0;
	    my $new_part = "P${p}U${n}->P${q}L${m}";
	    $node->{_branch}->{$new_part}
	      = $self->_make_lock_spec_tree(join(",", @spec, $new_part));
	  }
	}
      }
    }
    return $node;
  }

  sub run_by_lock_spec {
    my $self = shift;
    my ($lock_spec) = @_;
    my @spec = grep {$_ ne ""} (split(/,/, $lock_spec));

    print "run_by_lock_spec: $lock_spec\n" if $DEBUG;

    $self->{_result_context_tree} = $self->_make_lock_spec_tree("")
      if ! defined $self->{_result_context_tree};

    my $node = $self->{_result_context_tree}->get($lock_spec);
    return undef if ! defined $node;
    return $node->{_result_context} if defined $node->{_result_context};
    my $last;
    my $prev;
    my $prev_node;
    if (@spec) {
      $last = pop(@spec);
      $prev = join(",", @spec);
      $prev_node = $self->{_result_context_tree}->get($prev);
      if (! defined $prev_node->{_result_context}) {
	if (! defined $self->run_by_lock_spec($prev)) {
	  return undef;
	}
      }
    }
    my $r = [];
    foreach my $p (@{$self->{_process}}) {
      my $res = $p->run_by_lock_spec($lock_spec);
      if (defined $res) {
	push(@{$r}, $res);
      } else {
	# 一つでも $res が undefined ならば、全体として undefined のはず。
	if (defined $prev_node) {
	  delete $prev_node->{_branch}->{$last};
	}
	return undef;
      }
    }
    $node->{_result_context} = $r;
    return $r;
  }

  sub _run_tree_node {
    my $self = shift;
    my ($spec, $node) = @_;
    my @spec = grep {$_ ne ""} (split(/,/, $spec));

    if (! defined $self->run_by_lock_spec($spec)) {
      return undef;
    }
    foreach my $b (keys %{$node->{_branch}}) {
      $self->_run_tree_node(join(",", @spec, $b), $node->{_branch}->{$b});
    }
  }

  sub run {
    my $self = shift;

    $self->{_result_context_tree} = $self->_make_lock_spec_tree("")
      if ! defined $self->{_result_context_tree};
    $self->_run_tree_node("", $self->{_result_context_tree});
  }

  sub dump_tree {
    my $self = shift;

    $self->{_result_context_tree} = $self->_make_lock_spec_tree("")
      if ! defined $self->{_result_context_tree};
    print $self->{_result_context_tree}->dump("");
  }

  sub new {
    my $class = shift;
    my %opt = @_;
    my $obj = $class->SUPER::new(@_);
    if (exists $opt{context_hasher}) {
      $obj->{_context_hasher} = $opt{context_hasher};
    } else {
      $obj->{_context_hasher} = Main::_ContextHasher->new();
    }

    return $obj;
  }
}

MAIN:
{
  my $chasher = Main::_ContextHasher->new();

  my $prog1 = sub {
    my ($context) = @_;
    $context->lock();
    my $a = $context->get("a");
    print $a . " a00\n" if defined $a;
    print "undef a00\n" if ! defined $a;
    $a = "" if ! defined $a;
    $context->set("a", $a . "1");
    $context->unlock();

    $context->lock();
    my $a2 = $context->get("a");
    print $a2 . " a01\n" if defined $a2;
    print "undef a01\n" if ! defined $a2;
    $a2 = "" if ! defined $a2;
    $context->set("a", $a2 . "2");
    $context->unlock();

    $context->lock();
    my $a3 = $context->get("a");
    print $a3 . " a02\n" if defined $a3;
    print "undef a02\n" if ! defined $a3;
    $a3 = "" if ! defined $a3;
    $context->set("a", $a3 . "3");
    $context->unlock();
  };

  my $prog2 = sub {
    my ($context) = @_;
    $context->lock();
    my $a = $context->get("a");
    print $a . " a10\n" if defined $a;
    print "undef a10\n" if ! defined $a;
    $a = "" if ! defined $a;
    $context->set("a", $a . "4");
    $context->unlock();

    $context->lock();
    my $a2 = $context->get("a");
    print $a2 . " a11\n" if defined $a2;
    print "undef a11\n" if ! defined $a2;
    $a2 = "" if ! defined $a2;
    $context->set("a", $a2 . "5");
    $context->unlock();
  };

  my $prog3 = sub {
    my ($context) = @_;
    $context->lock();
    my $a = $context->get("a");
#    print $a . "\n" if defined $a;
#    print "undef\n" if ! defined $a;
    $context->set("a", 1);
    $context->unlock();

    $context->lock();
    my $a2 = $context->get("a");
#    print $a2 . "\n" if defined $a2;
#    print "undef\n" if ! defined $a2;
    $context->set("a", 2);
    $context->unlock();
  };

  my $prog4 = sub {
    my ($context) = @_;
    $context->lock();
    my $a = $context->get("a");
    print $a . "\n" if defined $a;
    print "undef\n" if ! defined $a;
#    $context->set("a", 3);
    $context->unlock();
  };

  my $emu2 = ELDE2::_Emulator->new(context_hasher => $chasher);
  #    $emu2->set_initial_context("a" => 9);
  $emu2->new_process($prog1, lock_limit => 3);
  $emu2->new_process($prog2, lock_limit => 2);
  print "\n\nELDE2 Result:\n";
  $emu2->run();

  my $emu0 = ELDE0::_Emulator->new(context_hasher => $chasher);
  #    $emu0->set_initial_context("a" => 9);
  $emu0->new_process($prog1, lock_limit => 3);
  $emu0->new_process($prog2, lock_limit => 2);
  print "\n\nELDE0 Result:\n";
  $emu0->run();

  print "\n\nCheck Result:\n";

  foreach my $spec2 (sort keys %{$emu2->{_terminated_context}}) {
    my $cid2 = $emu2->{_terminated_context}->{$spec2};
    print "Emu2: $spec2: cid = $cid2\n";
    my @spec2 = split(/,/, $spec2);
    my @spec0;
    my $prev;
    my $p;
    my $n;
    for (my $i = 0; $i < @spec2; $i++) {
      my $cur = $spec2[$i];
      $cur =~ /^P([01-9]+)L([01-9]+)$/;
      my $q = $1;
      my $m = $2;
      if (defined $prev) {
	if ($p != $q) {
	  push(@spec0, "P${p}U${n}->P${q}L${m}");
	}
      }
      $p = $q;
      $n = $m;
      $prev = $cur;
    }
    my $spec0 = join(",", @spec0);
    my $rc = $emu0->{_result_context_tree}->get($spec0);
    my $cid0 = "undef";
    $cid0 = $rc->{_result_context}->[$p]->[$n] if defined $rc;
    print "Emu0: $spec0: cid = $cid0\n";
    if ($cid0 eq $cid2) {
      print "  OK!\n";
    } else {
      print "  Something wrong!\n";
    }
  }
}

