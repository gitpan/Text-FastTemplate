package Text::FastTemplate;

use strict;
use integer;
use vars qw/ $VERSION %FILE_CACHE %PAGE_CACHE %CODE_CACHE @DEFAULTS /;

use Carp;
use File::Basename qw/ dirname /;

$VERSION = '0.91';

# template tokens / block types
use constant BASIC		=> 0;
use constant INCLUDE		=> 1;
use constant FOR		=> 2;
use constant ENDFOR		=> 3;
use constant IF			=> 4;
use constant ELSIF		=> 5;
use constant ELSE		=> 6;
use constant ENDIF		=> 7;
use constant DEFINE		=> 8;
use constant UNDEF		=> 9;

# object attributes
use constant TEMPLATE		=> 0;
use constant SUBNAME		=> 1;
use constant FILENAME		=> 2;
use constant KEY		=> 3;
use constant FILE		=> 4;
use constant PATH		=> 5;
use constant GROUP		=> 6;
use constant SOURCE		=> 7;

# block structure
#	BLOCK_TYPE	block type
#	MACROES		array ref of strings
#	CODE		array ref of scalar refs into the original template
use constant BLOCK_TYPE		=> 0;
use constant MACROES		=> 1;
use constant CODE		=> 2;

# block reference structure
#	BLOCK_INDEX	this block's index into the block table, @blocks
#	MACRO		macro assigned to this block, if any
use constant BLOCK_INDEX	=> 0;
use constant MACRO		=> 1;

@DEFAULTS[ PATH, GROUP ]= ( [ '.' ], '_default' );

#################################################################################################
#################################################################################################
#########										#########
#########				Public Methods					#########
#########										#########
#########										#########
#################################################################################################

sub defaults
{
	my $class= shift;

	while ( @_ ) {
		my ( $k, $v)= ( eval (uc shift), shift);
		$DEFAULTS[$k]= $v;
	};

	return 1;
}

sub preload
{
	my $class= shift;
	my $files= shift;

	for my $x ( @$files )
	{
		new $class (%$x), @_;
	}

	return 1;
}

sub new
{
	my $class= shift;

	my %a= @_;
	return $PAGE_CACHE{$a{key}} if $a{key} && $PAGE_CACHE{$a{key}};

	my @a;
	for my $x ( keys %a )
	{
		my ( $k, $v)= ( eval (uc $x), $a{$x});
		$a[$k]= $v;
	}

	if ( ! $a[FILE] )
	{
		carp "A file needs to specified when creating a new $class object" if ! $a[FILE];
		return 0;
	}

	my $self= bless [ @DEFAULTS ], $class;	# defaults
	for ( KEY, FILE, PATH, GROUP, SOURCE )
	{
		$self->[$_]= $a[$_] if defined $a[$_]
	};

	my $x= $self->_get_common or return undef;
	@$self[TEMPLATE,SUBNAME,FILENAME]= @$x[TEMPLATE,SUBNAME,FILENAME];
	$PAGE_CACHE{$self->[KEY]}= $self if defined $self->[KEY];

	return $self;
}

sub output
{
	my $self= shift;
	return $self->[TEMPLATE]->( ref $_[0] ? shift : { @_ } );
}

sub print
{
	my $self= shift;
	print $self->output( @_);
}

#################################################################################################
#################################################################################################
#########										#########
#########				Private Methods					#########
#########										#########
#########										#########
#################################################################################################

sub _get_subname
{
	my $self= shift;
	return $self->_get_common( @_)->[SUBNAME];
}

sub _get_common
{
	my $self= shift;
	my $file= ( @_ ? shift : $self->[FILE] );
	my $filename;
	my @file_contents;
	my $parsed_template;
	my $compiled_template;

	$filename= $self->_find_file( $file);

	if ( exists $FILE_CACHE{$filename} )
	{
		$parsed_template= $FILE_CACHE{$filename};
	}
	elsif ( $filename )
	{
		open( FH, "< $filename") or croak "cannot open $filename";
		@file_contents= <FH>;
		close FH;
		chomp @file_contents;
		$parsed_template= $self->_parse( \@file_contents);
		$compiled_template= $self->_compile( $filename, \@file_contents, $parsed_template);
		$FILE_CACHE{$filename}= $self;
	}
	else
	{
		croak "Cannot find the file specified:  $file\nCroaked";
	}

	return $compiled_template;
}

sub _find_file
{
	my $self= shift;
	my $file= shift;
	my $filename;

	if ( $file =~ m:^/: )
	{
		$filename= $file;
	}
	else
	{
		my @path;
		@path= dirname $self->[FILENAME] if $self->[FILENAME];
		push @path, @{$self->[PATH]};
		foreach my $d ( @path ) {
			my $F= "$d/$file";
			if ( $FILE_CACHE{$F} || ( -e $F && -r $F ))
			{
				$filename= $F;
				last;
			}
		}
	}

	return $filename;
}

sub _parse
{
	my $self= shift;
	my $template= shift;
	my $class= ref $self;

	# splice token-lines that are continued with a backslash and
	# delete extraneous white-space
	my @pointer_table;
	@pointer_table= ( 0..$#$template );
	while ( @pointer_table )
	{
		my $i= shift @pointer_table;

		my $x= $template->[$i];

		next if $x !~ /^\s*#\s*(?:include|for|endfor|if|elsif|else|endif)(?:\s+.*|\s*\\\s*)?$/ix;

		while ( $x =~ s/\s*\\\s*$// )
		{
			my $j= shift @pointer_table;
			my $y= $template->[$j];
			$x .= " $y";
			undef $template->[$j];
			;
		}

		$x =~ s/^\s+//;
		$x =~ s/\s+$//;
		$x =~ s/\s+/ /g;

		$template->[$i]= $x;
	}

	# clear lines from template that were removed by splicing continued lines
	my $n= -1;
	@pointer_table= ( 0..$#$template );
	while ( @pointer_table )
	{
		my $i= shift @pointer_table;

		my $x= $template->[$i];
		$template->[++$n]= $x if defined $x;
	}
	$#$template= $n;

	# the real parsing is here; parse the tokens and macroes construct the loop-blocks and if-blocks
	my ( @blocks, @block_stack );
	@blocks= ( [ BASIC, [], [] ] );
	push @block_stack, $#blocks;
	my ( $block, $code, $macroes, $block_type );
	$block= $blocks[0];
	( $block_type, $code, $macroes)= @$block[BLOCK_TYPE,CODE,MACROES];

	@pointer_table= ( 0..$#$template );
	while ( @pointer_table )
	{
		my $i= shift @pointer_table;

		my $x= $template->[$i];

		my $y= ( $x =~ /^
				\#\s?
				(?:
					(?:
						(include)			# $1
						\s
						(?:
							([^'"]+?)		# $2
							|
							"([^']+?)"		# $3
							|
							'(.+?)'			# $4
						)
					)
					|
					(?:
						(for)				# $5
						\s
						\#\#
						(\w+?)				# $6
						\#\#
					)
					|
					(?:
						( if | elsif )			# $7
						\s
						(.+)				# $8
					)
					|
					( endfor | else | endif )		# $9
				)
			$/igsx
			);

		my ( $cmd, $macro );
		( $cmd, $macro )= ( $1 || $5 || $7 || $9, $2 || $3 || $4 || $6 || $8 );

		if ( $y )
		{
			if ( lc( $cmd) eq 'include' )
			{
				push @blocks, [ INCLUDE, [], [] ];
				push @$code, [ $#blocks, $macro ];
			}
			elsif ( lc( $cmd) eq 'for' )
			{
				push @blocks, [ FOR, [], [] ];
				push @$code, [ $#blocks, $macro ];
				push @$macroes, $macro if ! grep { $_ eq $macro } @$macroes;
				push @block_stack, $#blocks;
				$block= $blocks[$block_stack[$#block_stack]];
				( $code, $macroes)= @$block[CODE,MACROES];
			}
			elsif ( lc( $cmd) eq 'endfor' )
			{
				pop @block_stack;
				$block= $blocks[$block_stack[$#block_stack]];
				( $code, $macroes)= @$block[CODE,MACROES];
			}
			elsif ( lc( $cmd) eq 'if' )
			{
				push @blocks, [ IF, [], [] ];
				push @$code, [ $#blocks, $macro ];
				while ( $macro =~ /##(\w+?)##/g )
				{
					my $macro= $1;
					push @$macroes, $macro if (( ! grep { $_ eq $macro } @$macroes ) && ( $macro !~ /^\w+_LOOP_ID$/ ));
				}

				push @block_stack, $#blocks;
				$block= $blocks[$block_stack[$#block_stack]];
				$code= $block->[CODE];
			}
			elsif ( lc( $cmd) eq 'elsif' )
			{
#				close the block first
				pop @block_stack;
				$block= $blocks[$block_stack[$#block_stack]];
				( $code, $macroes)= @$block[CODE,MACROES];

#				now open another one
				push @blocks, [  ELSIF, [], [] ];
				push @$code, [ $#blocks, $macro ];
				while ( $macro =~ /##(\w+?)##/g )
				{
					my $macro= $1;
					push @$macroes, $macro if (( ! grep { $_ eq $macro } @$macroes ) && ( $macro !~ /^\w+_LOOP_ID$/ ));
				}

				push @block_stack, $#blocks;
				$block= $blocks[$block_stack[$#block_stack]];
				$code= $block->[CODE];
			}
			elsif ( lc( $cmd) eq 'else' )
			{
#				close the block first
				pop @block_stack;
				$block= $blocks[$block_stack[$#block_stack]];
				$code= $block->[CODE];

#				now open another one
				push @blocks, [  ELSE, [], [] ];
				push @$code, [ $#blocks, ( $macro ? $macro : '' ) ];
				push @$macroes, $macro if $macro && ! grep { $_ eq $macro } @$macroes;

				push @block_stack, $#blocks;
				$block= $blocks[$block_stack[$#block_stack]];
				$code= $block->[CODE];
			}
			elsif ( lc( $cmd) eq 'endif' )
			{
				pop @block_stack;
				$block= $blocks[$block_stack[$#block_stack]];
				$code= $block->[CODE];
			}
			else
			{
				croak "Possible token mistype on line #$i of \$file:\n\t$$x\nCroaked";
			}
		}
		else
		{
			while ( $x =~ /##(\w+?)##/g )
			{
				my $macro= $1;
				push @$macroes, $macro if (( ! grep { $_ eq $macro } @$macroes ) && ( $macro !~ /^\w+_LOOP_ID$/ ));
			}

			push @$code, $i;
		}
	}

	return \@blocks;
}

sub _compile($)
{
	my $self= shift;
	my $filename= shift;
	my $template= shift;
	my $blocks= shift;
	my $class= ref $self;

# most critical step is creating the actual subroutine code that gets passed to an eval
# construct subroutine code from template lines and argument list and
# eval it for the magic template subroutine
# then we save it and pass it up the chain
	my $subname;
	( $subname= $filename ) =~ s<[^\w]+><_>g;
	$subname= "${class}::$self->[GROUP]::" . $subname;

	my @subroutine;
	push @subroutine,
		"sub $subname {\n",
		"#line 1 \"$subname\"\n",
		"\tlocal \$^W= 0;\n",
		"\tmy \$ABC= shift;\n",
		"\tmy \$formatted_text;\n",
		$self->_generate_block_code( 0, $blocks, $template),
		"\n",
		"\treturn \$formatted_text;\n",
		"};\n"
		;

# This is the moment of truth.  Will the subroutine compile correctly?
	eval "@subroutine";
	if ( $@ )
	{
		croak "Couldn't compile the template-subroutine: $subname\n$@";
	}
	else
	{
		$CODE_CACHE{$subname}= \&{$subname} if ! $@;
	}

# final step is to create the actual template array and return it
	my @parsed;
	@parsed[ SUBNAME, TEMPLATE, FILENAME]= ( $subname, \&{$subname}, $filename);
	return \@parsed;
#	TEMPLATE		code ref
#	SUBNAME			string
#	FILENAME		string
};

sub _generate_block_code
{
	my $self= shift;
	my $block_index= shift;
	my $blocks= shift;
	my $template= shift;
	my $class= ref $self;

	my $block;
	my $block_type;
	my $code;
	my $macroes;
	$block=		$blocks->[$block_index];
	$block_type=	$block->[BLOCK_TYPE];
	$code=		$block->[CODE];
	$macroes=	$block->[MACROES];

	my @pointer_table;
	my @block_code;

	for ( 0..$#$macroes )
	{
		my $macro= $macroes->[$_];
		push @block_code,
			"\tmy \$$macro=\t\$ABC->{$macro};\n"
			;
	};

	for ( my $i= 0; $i <= $#$code; $i++ )
	{
		my ( $a, $b, $y, $z );

		$a= $code->[$i];

		$b= $template->[$a];
		$y= ( $i ? $code->[$i-1] : 0 );
		$z= $code->[$i+1];

		push @block_code, "\t\$formatted_text .=\n" if ( ! ref $a && ( ! $i || ref $y ));

		if ( ref $a )
		{
			my $block_index= $a->[BLOCK_INDEX];
			my $macro= $a->[MACRO];

			if ( $blocks->[$block_index]->[BLOCK_TYPE] == INCLUDE )
			{
				push @block_code,
					sprintf( "\t\$formatted_text .= \$CODE_CACHE{'%s'}->( \$ABC);\n", $self->_get_subname( $macro))
					;
			}
			if ( $blocks->[$block_index]->[BLOCK_TYPE] == FOR )
			{
				push @block_code,
					sprintf( "\tfor ( my \$%s_LOOP_ID= 0; \$%s_LOOP_ID <= \$#\$%s; \$%s_LOOP_ID++ ) {\n", $macro, $macro, $macro, $macro),
					sprintf( "\tmy \$ABC= \$%s->[\$%s_LOOP_ID];\n", $macro, $macro),
					$self->_generate_block_code( $block_index, $blocks, $template),
					"\t}\n"
					;
			}
			elsif ( $blocks->[$block_index]->[BLOCK_TYPE] == IF )
			{
				( my $expression= $macro ) =~ s/##(\w+)##/\${$1}/g;
				push @block_code,
					sprintf( "\tif ( %s ) {\n", $expression),
					$self->_generate_block_code( $block_index, $blocks, $template),
					"\t}\n"
					;
			}
			elsif ( $blocks->[$block_index]->[BLOCK_TYPE] == ELSIF )
			{
				( my $expression= $macro ) =~ s/##(\w+)##/$1/g;
				push @block_code,
					sprintf( "\telsif ( \$%s ) {\n", $expression),
					$self->_generate_block_code( $block_index, $blocks, $template),
					"\t}\n"
					;
			}
			elsif ( $blocks->[$block_index]->[BLOCK_TYPE] == ELSE )
			{
				push @block_code,
					"\telse {\n",
					$self->_generate_block_code( $block_index, $blocks, $template),
					"\t};\n"
					;
			}
		}
		else
		{
			$b =~ s/([@\$"\\])/\\$1/g;
			$b =~ s/##(\w+?)##/\${$1}/g;
			push @block_code, "\t\t\"$b\\n\"" . ( ref $z || ( $i == $#$code ) ? ";\n" : ".\n" );
		}
	};

	return @block_code;
}

1;

#############################################################################################

__END__

