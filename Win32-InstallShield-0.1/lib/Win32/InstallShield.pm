package Win32::InstallShield;

use Carp;
use IO::File;
use XML::Parser;
use Data::Dumper;

use strict;
use warnings;

our $AUTOLOAD;
our $VERSION = 0.1;

=head1 NAME

Win32::InstallShield - InstallShield data file interface

=head1 SYNOPSIS

  use InstallShield;

  # Constructors
  $is = Win32::InstallShield->new();
  $is = Win32::InstallShield->new( $ism_file );

=head1 ABSTRACT

An OO interface for manipulating InstallShield XML .ism files

=head1 DESCRIPTION

This module provides an interface to add, remove and modifify rows
in an InstallShield .ism file. It only supports versions of
InstallShield that save their data as XML.

=head1 EXAMPLES

This example updates the product version.

  use Win32::InstallShield;

  $is = Win32::InstallShield->new( $ism_file );
  $is->UpdateProperty(
      { 
          Property => 'ProductVersion',
          Value    => '1.2.3.4',
      }
  );

  $is->save_file( $ism_file );

=head1 METHODS

=over 4

=item I<new>

  $is = Win32::InstallShield->new();
  $is = Win32::InstallShield->new( $installshield_filename );
  $is = Win32::InstallShield->new( $io_file_handle );

The constructor. Can optionally be called with the same 
arguments as I<loadfile>.

=cut
sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;

	my $self = {
		parser		=> XML::Parser->new(Style => 'Tree'),
		parsed		=> {}, # tables that the user has read or modified
		sections	=> {}, # the contents of the original file
		order		=> [], # the order in which sections appear in the file
	};

	bless $self, $class;
	
	if(@_) {
		$self->loadfile( shift );
	}

	return $self;
}

sub DESTROY {
}

sub AUTOLOAD {
	my $self = shift;
	my $name = $AUTOLOAD;
	$name =~ s/.*://;
	if($name =~ /^(gethash|getarray|add|del|update)_?(.*)$/i) {
		my ($op, $table) = (lc($1), lc($2));

		if($table eq 'row') {
			$table = lc(shift @_);
		}

		unless($self->{'sections'}{$table}) {
			croak("No such table: $2");
		}

		if($op eq 'gethash') {
			return $self->_get_row_hash($table, @_);
		} elsif($op eq 'getarray') {
			return $self->_get_row_array($table, @_);
		} elsif($op eq 'add') {
			return $self->_add_row($table, @_);
		} elsif($op eq 'del') {
			return $self->_del_row($table, @_);
		} elsif($op eq 'update') {
			return $self->_update_row($table, @_);
		}

	} else {
		croak("Invalid method $name");
	}
}

# internal function, opens a file and returns the
# IO::File filehandle if given a filename. if called
# with an IO::File filehandle, it just returns the filehandle.
sub _openfile {
	my ($file, $mode) = @_;

	if(ref($file)) {
		if(ref($file) eq 'IO::File') {
			return ($file, 0);
		} else {
			carp("Invalid file argument: " . ref($file) . "\n");
			return (undef, 0);
		}
	} else {
		my $fh = IO::File->new($file, $mode);
		unless(defined($fh)) {
			carp("Unable to read $file $!");
			return (undef, 0);
		}
		return ($fh, 1);
	}

}

=item I<loadfile>

  $is->loadfile( $filename );
  $is->loadfile( $io_file_handle );

Loads an InstallShield ism file. Can be called
with either a filename or an IO::File object that is
opened in read ("r") mode.
Returns 1 on success, 0 on failure.

=cut
sub loadfile {
	my ($self, $file) = @_;

	my ($fh, $i_opened_file) = _openfile($file, "r");

	unless(defined($fh)) {
		return 0;
	}

	my $return = $self->load( join('', <$fh>) );
	$fh->close() if($i_opened_file);
	return $return;
}

=item I<load>

  $is->load( $ism_text );

Loads the supplied text of an InstallShield ism file.
Returns 1 on success, 0 on failure.

=cut
sub load {
	my ($self, $data) = @_;

	# loading wipes out whatever was previously loaded
	$self->{'parsed'} = {};
	$self->{'sections'} = {};
	$self->{'order'} = [];

	my $section = 'header';
	my $lastsection = $section;

	push(@{$self->{'order'}}, $section);

	my @lines = split("\n", $data);

	foreach (@lines) {
		if(/^<msi/) {
			$section = 'msi';
		} elsif(/^\s*<summary>/) {
			$section = 'summary';
		} elsif(/^\s*<table name="([^"]+)"/) {
			$section = lc($1);
		} elsif(/^<\/msi>/) {
			$section = 'trailer';
		}
		
		if($section ne $lastsection) {
			$lastsection = $section;
			push(@{$self->{'order'}}, $section);
		}

		push(@{$self->{'sections'}{$section}}, $_);
	}

	return 1;
}

=item I<savefile>

  $is->savefile( $filename );
  $is->savefile( $io_file_handle );

Stores the ism data in a file. Can be called
with either a filename or an IO::File object that is
opened in write ("w") mode.
Returns 1 on success, 0 on failure.

=cut
sub savefile {
	my ($self, $file) = @_;
	
	my ($fh, $i_opened_file) = _openfile($file, "w");

	unless(defined($fh)) {
		return 0;
	}

	print $fh $self->save();
	$fh->close() if($i_opened_file);
	return 1;
}

=item I<save>

  $is->save();

Returns the ism data as a string.

=cut
sub save {
	my ($self) = @_;

	my $text = '';

	foreach my $table (@{$self->{'order'}}) {
		if($self->{'parsed'}{$table}) {
			$text .= $self->_save_table($table);
		} else {
			$text .= join("\n", @{$self->{'sections'}{$table}}) . "\n";
		}
	}

	return $text;
}

# internal function. formats the data in a table that has
# been modified back to the appropriate output format
sub _save_table {
	my ($self, $table) = @_;

	my $p = $self->_parsed($table);
	my $text = $p->{'header'};
	
	foreach my $key (sort keys %{$p->{'data'}}) {
		my $row = $p->{'data'}{$key};
		$text .= "\t\t<row>";
		foreach my $col (@$row) {
			if(defined($col) and length($col) > 0) {
				$text .= "<td>" . _xml_escape($col) . "</td>";
			} else {
				$text .= "<td/>";
			}
		}
		$text .= "</row>\n";
	}
	
	$text .= $p->{'trailer'};
	
	return $text;
}

# internal function. parses the text of an IS table
# so that it can be easily manipulated
sub _parse_table {
	my ($self, $table) = @_;
	
	$table = lc($table);
	return if($self->{'parsed'}{$table});

	my $text = $self->{'sections'}{$table};
	unless(defined($text)) {
		carp("No such table $table");
	}

	my $header = '';
	my $trailer = '';
	my @cols;
	my %data;

	foreach my $line (@$text) {
		if($line =~ /^\s+<table/) {
			$header .= $line . "\n";
		} elsif($line =~ /^\s+<col ([^>]+)>([^<]+)</) {
			my $column_name = $2;
			my $attrs = $1;
			my $is_key = ($attrs =~ /key="yes"/i);
			my ($type, $width) = ($attrs =~ /def="(\w)(\d+)"/i);
			push(@cols, {
				name	=> $column_name,
				is_key	=> $is_key,
				type	=> $type,
				width	=> $width,
			});
			$header .= $line . "\n";
		} elsif($line =~ /^\s+<row/) {
			# FIXME get rid of xml parser and do this locally
			my $columns = $self->{'parser'}->parse($line)->[1];
			my $lookup_key = '';
			my @row;
			foreach my $i (0..$#cols) {
				my $value = $columns->[ ($i+1)*2 ][2];
				$row[$i] = $value;
				$lookup_key .= sprintf("%-" . $cols[$i]{'width'} . "s", $value) if($cols[$i]{'is_key'});
			}
			$data{ $lookup_key } = \@row;

		} elsif($line =~ /^$/ or $line =~ /^\s+<\/table/) {
			$trailer .= $line . "\n";
		} else {
			print "Invalid line in file: $line\n";
		}
	}

	$self->{'parsed'}{$table} = {
		header		=> $header,
		trailer		=> $trailer,
		columns		=> \@cols,
		data		=> \%data,
	};
}

sub _parsed {
	my ($self, $table) = @_;
	$table = lc($table);
	unless(exists($self->{'parsed'}{$table})) {
		$self->_parse_table($table);
	}
	return $self->{'parsed'}{$table};
}

=item I<column_is_key>

  my $is_key = $is->column_is_key( $table, $column_name );

Returns true if the column is a key column, false
other wise. Returns undef if the column doesn't exist.

=cut
sub column_is_key { 
	my ($self, $table, $column) = @_;
	my $p = $self->_parsed($table);
	foreach my $col (@{$p->{'columns'}}) {
		if($col->{'name'} eq $column) {
			return $col->{'is_key'};
		}
	}
	return undef;
}

=item I<column_width>

  my $width = $is->column_width( $table, $column_name );

Returns the width of the named column. Returns undef if
the column doesn't exist.

=cut
sub column_width {
	my ($self, $table, $column) = @_;
	my $p = $self->_parsed($table);
	return $p->{'columns'}{$column}{'width'};
}

=item I<column_type>

  my $type = $is->column_type( $table, $column_name );

Returns the type of the named column. Returns undef if the 
column doesn't exist.

=cut
sub column_type {
	my ($self, $table, $column) = @_;
	my $p = $self->_parsed($table);
	return $p->{'columns'}{$column}{'type'};
}

=item I<columns>

  my $columns = $is->columns( $table );

Returns an array ref containing the names of the columns
in the given table.

=cut
sub columns {
	my ($self, $table) = @_;
	my $p = $self->_parsed($table);
	my @cols;
	foreach my $col (@{$p->{'columns'}}) {
		push(@cols, $col->{'name'});
	}
	return \@cols;
}

=item I<key_columns>

  my $key_columns = $is->key_columns( $table );

Returns an array ref containing the names of the
key columns in the given table.

=cut
sub key_columns {
	my ($self, $table) = @_;
	my $p = $self->_parsed($table);
	my @keys;
	foreach my $col (@{$p->{'columns'}}) {
		if($col->{'is_key'}) {
			push(@keys, $col->{'name'});
		}
	}
	return \@keys;
}

sub _find_row {
	my ($self, $table, $rowdata) = @_;
	my $p = $self->_parsed($table);

	my $lookup_key = $self->_build_key( $table, $rowdata );

	if(exists($p->{'data'}{$lookup_key})) {
		return $lookup_key;
	} else {
		return undef;
	}
}

sub _build_key {
	my ($self, $table, $values) = @_;

	my $p = $self->_parsed($table);
	my $lookup_key = '';

	# build the lookup key by concatenating the key columns
	foreach my $i (0..$#{$p->{'columns'}}) {
		if($p->{'columns'}[$i]{'is_key'}) {
			my $width = $p->{'columns'}[$i]{'width'};
			if(defined($values->[$i])) {
				$lookup_key .= sprintf("%-${width}s", $values->[$i]);
			} else {
				carp("Missing key column " . $p->{'columns'}[$i]{'name'});
				return undef;
			}
		}
	}

	return $lookup_key;
}

sub _reformat_args {
	my ($self, $table, @args) = @_;

	my $p = $self->_parsed($table);
	my $row = [];

	if(ref($args[0]) eq 'ARRAY') {
		$row = $args[0];
	} elsif(ref($args[0]) eq 'HASH') {
		my $h = $args[0];
		foreach my $col (@{$p->{'columns'}}) {
			push(@$row, $h->{ $col->{'name'} });
		}
	} else {
		$row = \@args;
	}

	return $row;
}

sub _check_args {
	my ($self, $table, @args) = @_;

	my $p = $self->_parsed($table);
	my $row = $self->_reformat_args($table, @args);

	unless( $#{$row} eq $#{$p->{'columns'}} ) {
		carp("Wrong number of columns supplied for table $table");
		return undef;
	}

	foreach my $i (0..$#{$row}) {
		next unless(defined($row->[$i]));
		my $type = $p->{'columns'}[$i]{'type'};
		if($type =~ /^i$/i) {
			if($row->[$i] =~ /[^\d-]/) {
				croak("Value in $p->{'columns'}[$i]{'name'} column must be numeric");
				return undef;
			}
		} else {
			my $width = $p->{'columns'}[$i]{'width'};
			if($width > 0 and length($row->[$i]) > $width) {
				croak("Value in $p->{'columns'}[$i]{'name'} column is too long");
				return undef;
			}
		}
	}

	return $row;
}

=back

=head1 ROW MANIPULATION METHODS

Row manipulation methods can be called in different ways.
First, they are all case insensitve, and the '_' is
optional, so for the 'Property' table, these are equivilent:

  $is->add_row( 'Property', $rowdata );
  $is->AddRow( 'Property', $rowdata );

Also, you can call each method using the table name in
place of the word 'row', so these are equivilent to the
two above:

  $is->add_property( $rowdata );
  $is->AddProperty( $rowdata );

All row manipulation methods are called with a set of data
describing a row. In the methods below, it is represented by
the variable $rowdata. It can be passed to the function in
one of three formats: a list, an array ref or a hash ref.

List

You can simply put the columns in an array in the correct
order (which you can get by looking at the ism or calling
the I<columns> method), and pass it to the method.

  my @rowdata = ( 'Column_1_Value', 'Column_2_value' );
  $success = update_row( $table, @rowdata );

Array ref

You can do the same as above, but pass it as a single
array reference.

  $success = update_row( $table, \@rowdata );

Hash ref

You can also pass a hash ref, using column names
as keys.

  my %rowdata = (
      Property   => 'ProductVersion',
      Value      => '1.2.3.4',
      ISComments => '',
  );
  $success = update_row( $table, \%rowdata );

=over 4

=item I<gethash_row>

  my $row = gethash_row( $table, $rowdata );

Returns a hash ref containing the data that matches the keys
supplied in $rowdata. Returns undef if the row is not found.

=cut
sub _get_row_hash {
	my ($self, $table, @args) = @_;
	my $args = $self->_reformat_args($table, @args);
	my $rowkey = $self->_find_row($table, $args);
	if(defined($rowkey)) {
		my %rowdata;
		my $p = $self->_parsed($table);
		
		foreach my $i (0..$#{$p->{'columns'}}) {
			$rowdata{ $p->{'columns'}[$i]{'name'} } = $p->{'data'}{$rowkey}[$i];
		}
		return \%rowdata;
	} else {
		return undef;
	}
}

=item I<getarray_row>

  my $row = getarray_row( $table, $rowdata );

Returns an array ref containing the data that matches the keys
supplied in $rowdata. Returns undef if the row is not found.

=cut
sub _get_row_array {
	my ($self, $table, @args) = @_;
	my $args = $self->_reformat_args($table, @args);
	my $rowkey = $self->_find_row($table, $args);
	if(defined($rowkey)) {
		my $p = $self->_parsed($table);
		return $p->{'data'}{$rowkey};
	} else {
		return undef;
	}
}

=item I<update_row>

  my $success = update_row( $table, $rowdata );

Updates the row that matches the keys supplied in
$rowdata. Any columns for which an undef is supplied
will remain unchanged. An empty string will force
the column to be empty. Returns 1 on success, 0 on
failure.

=cut
sub _update_row {
	my ($self, $table, @args) = @_;
	my $rowdata = $self->_check_args($table, @args);
	unless(defined($rowdata)) {
		return 0;
	}
	my $rowkey = $self->_find_row($table, $rowdata);
	unless(defined($rowkey)) {
		carp("Failed to locate row in $table for update");
		return 0;
	}
	my $p = $self->_parsed($table);
	foreach my $i (0..$#{$rowdata}) {
		if(defined($rowdata->[$i])) {
			$p->{'data'}{$rowkey}[$i] = $rowdata->[$i];
		}
	}
	return 1;
}

=item I<add_row>

  my $success = add_row( $table, $rowdata );

Adds a row containing the data in $rowdata. Returns
1 on success, 0 on failure.

=cut
sub _add_row {
	my ($self, $table, @args) = @_;
	my $rowdata = $self->_check_args($table, @args);
	unless(defined($rowdata)) {
		return 0;
	}
	my $rowkey = $self->_find_row($table, $rowdata);
	if(defined($rowkey)) {
		carp("Row to add in '$table' table already exists");
		return 0;
	}
	$rowkey = $self->_build_key($table, $rowdata);
	unless(defined($rowkey)) {
		return 0;
	}
	my $p = $self->_parsed($table);
	$p->{'data'}{$rowkey} = $rowdata;
	return 1;
}

=item I<del_row>

  my $success = del_row( $table, $rowdata );

Deletes the row that matches the keys supplied in
$rowdata. Returns 1 on success, 0 on failure.

=cut
sub _del_row {
	my ($self, $table, @args) = @_;
	my $args = $self->_reformat_args($table, @args);
	my $rowkey = $self->_find_row($table, $args);
	unless(defined($rowkey)) {
		carp("Failed to locate row in $table for delete");
		return 0;
	}
	my $p = $self->_parsed($table);
	delete($p->{'data'}{$rowkey});
	return 1;
}

# this is (almost) a copy of the xml_escape function in XML::Parser::Expat.
# The version there doesn't seem to work properly on data that was read
# in via XML::Parser, because a call to study causes subsequent matches to
# fail
sub _xml_escape {
	my $text = shift @_;

	$text =~ s/\&/\&amp;/g;
	$text =~ s/</\&lt;/g;
	$text =~ s/>/\&gt;/g;

	foreach (@_) {
		die "xml_escape: '$_' isn't a single character" if length($_) > 1;

		if ($_ eq '"') {
			$text =~ s/\"/\&quot;/;
		}
		elsif ($_ eq "'") {
			$text =~ s/\'/\&apos;/;
		}
		else {
			my $rep = '&#' . sprintf('x%X', ord($_)) . ';';
			if (/\W/) {
				my $ptrn = "\\$_";
				$text =~ s/$ptrn/$rep/g;
			}
			else {
				$text =~ s/$_/$rep/g;
			}
		}
	}
	$text;
}

=back

=head1 AUTHOR

Kirk Baucom, E<lt>kbaucom@schizoid.comE<gt>

=cut

1;
