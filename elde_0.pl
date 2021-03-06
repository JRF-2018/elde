#!/usr/bin/perl
require 5.008;
our $VERSION = "0.03"; #Time-stamp: <2018-04-13T09:56:23Z>

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
		      Main::_ResultContextTreeNode
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
  package Main::_Process;
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
    my $cont = Main::_Context->new();
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
  package Main::_ResultContextTreeNode;
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
  package Main::_Emulator;
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
    my $proc = Main::_Process->new();
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

    my $node = Main::_ResultContextTreeNode->new();
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
    print "\n\nPlan:\n";
    $emu->dump_tree();

    print "\n\nResult:\n";
    $emu->run();
    print "\n\nResult Tree:\n";
    $emu->dump_tree();
  }
}


__END__

=pod

=encoding utf8

=head1	DESCRIPTION

lock_spec の構成。

lock_spec は "P2U1->P3L2,P3U2->P1L2,..." といった文字列になる。

P${p}U${n}->P${q}L${m} の意味は、プロセス番号 $p の $n 番目の Unlock 時の context が、プロセス番号 $q の $m 番目の lock 時の context になるという意味である。そして、その並びは、そういった「context switch」が次々に起こったことを表している。
P${p_0}U${n_0}->P${q_0}L${m_0},...,P${p_i}U${n_i}->P${q_i}L${m_i},... とする。

Forall j < i.
       p_i = p_j --> n_i > n_j
   and p_i = q_j --> n_i >= m_j
   and q_i = q_j --> m_i > m_j
   and q_i = p_j --> m_i > n_j

つまり、i 番目より前に p_i がすでに現れていれば、その n, m よりも n_i は大きい数字でなければならないし、q_i についても同様のことがいえる。

なお、split(,) した長さは、プロセス数 * プロセス lock_limit 数(の max)以下になる。

run_by_lock_spec は、この lock_spec の通りに run する。すべての lock_spec を総当たりにすることもあろうし、今回はやっていないが、ランダムにいくつかだけする場合もあろう。


「虚実行」について。

P0U1->P1L2 かつ P1U4->P0L3 とする。すると、間に P0L2 と P1L3 があるわけで、こららは必ずどちらかが前でどちらかが後になる。P0U2->P1L3 か P1U3->P0L2 でなければならない。これが仕様に含まれてない実行はニセモノ…「虚実行」というべきものになる。

もっと簡単に、プロセス群の中の一番最初のロック以外のプロセス q の最初のロックは P${p}L${n}->P${q}L0 なる仕様(p n は任意)を含んでいないと「虚実行」になるし、プロセス群の最後のロックを含むプロセス以外のプロセス q の最後のロック l は P${q}U${l}->P${p}L${n} なる仕様(p n は任意)を含んでいないと「虚実行」になる。

elde_0.pl は「虚実行」までもチェックしていることになる。

「虚実行」群の中に「実実行」というべきものがすべて入っているので、「虚実行」群全体でテストに合格したものは「実実行」においてもテストに合格すると言えるが、余計なものまで(大量に)チェックしていることは否めない。

ただ、この「虚実行」以外にも cut している実行があり、それらと「虚実行」を分けるものが何かと問われると難しく、「虚実行」にはそれほど意味がないのかもしれない。

=cut
