# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..1\n"; }
END {print "not ok 1\n" unless $loaded;}
use Text::FastTemplate;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

$A= { HERE => 1, A => [ { HERE => 1 }, { HERE => 2 }, { HERE => 3 } ] };

push @tests,
  undef, undef,
  sub { Text::FastTemplate->defaults( path => [ 'test_templates' ]) },
  sub { Text::FastTemplate->preload( [
    { key => 'simple1',	file => 'simple1.tpl'	},
    { key => 'simple2',	file => 'simple2.tpl'	},
    { key => 'simple3',	file => 'simple3.tpl'	},
    { key => 'simple4',	file => 'simple4.tpl'	},
    { key => 'simple5',	file => 'simple5.tpl'	},
    { key => 'if',	file => 'if.tpl'	},
    { key => 'elsif',	file => 'elsif.tpl'	},
    { key => 'else',	file => 'else.tpl'	},
    { key => 'for',	file => 'for.tpl'	},
    ]) },
  sub { Text::FastTemplate->new( key => 'simple1')->output( $A) eq "1\n" },
  sub { Text::FastTemplate->new( key => 'simple2')->output( $A) eq "come1\n" },
  sub { Text::FastTemplate->new( key => 'simple3')->output( $A) eq "1not there\n" },
  sub { Text::FastTemplate->new( key => 'simple4')->output( $A) eq "1HERE##\n" },
  sub { Text::FastTemplate->new( key => 'simple5')->output( $A) eq "11\n" },
  sub { Text::FastTemplate->new( key => 'if') },
  sub { Text::FastTemplate->new( key => 'include', file => 'include.tpl' ) },
  sub { Text::FastTemplate->new( key => 'include')->output( $A) eq "include\nincluded\n" },
  sub { Text::FastTemplate->new( key => 'if')->output( $A) eq "if\n" },
  sub { Text::FastTemplate->new( key => 'elsif')->output( $A) eq "elsif\n" },
  sub { Text::FastTemplate->new( key => 'else')->output( $A) eq "else\n" },
  sub { Text::FastTemplate->new( key => 'for')->output( $A) eq "A=1\nA=2\nA=3\n" },
  ;

map { printf "%s %u\n", ( $tests[$_]->() ? "ok" : "not ok"), $_; } ( 2..$#tests );

