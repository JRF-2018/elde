#!/usr/bin/perl
require 5.008;
our $VERSION = "0.03"; #Time-stamp: <2018-04-13T11:06:50Z>

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
		      Main::_Context
		      Main::_ContextHasher
		      Main::_Process
		      Main::_Emulator
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

{
  package Main::_Context;
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
  package Main::_Process;
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
    my $cont = Main::_Context->new();
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
  package Main::_Emulator;
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
    my $proc = Main::_Process->new();
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

MAIN:
{
  if (1) {
    my $emu = Main::_Emulator->new();
    $emu->new_process(sub {
			my ($context) = @_;
			$context->lock();
			my $a = $context->get("a");
#			print $a . "\n" if defined $a;
#			print "undef\n" if ! defined $a;
			$context->set("a", 1);
			$context->unlock();

			$context->lock();
			my $a2 = $context->get("a");
#			print $a2 . "\n" if defined $a2;
#			print "undef\n" if ! defined $a2;
			$context->set("a", 2);
			$context->unlock();
		      },
		      lock_limit => 3,
		     );
    $emu->new_process(sub {
			my ($context) = @_;
			$context->lock();
			my $a = $context->get("a");
			print $a . "\n" if defined $a;
			print "undef\n" if ! defined $a;
#			$context->set("a", 3);
			$context->unlock();
		      },
		      lock_limit => 2,
		     );
#    $emu->set_initial_context("a" => 9);

    print "\n\nResult:\n";
    $emu->run();
  }
}

