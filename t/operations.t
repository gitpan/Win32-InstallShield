use Test::More;
use Win32::InstallShield;

my $is = Win32::InstallShield->new();

my @tests = (
	\&no_change,
	\&array_add, 
	\&hash_add, 
	\&add_and_del, 
	\&del_and_readd,
	\&hash_update,
	\&array_update,
	\&hash_del,
	\&array_del,
	\&add_or_update_add,
	\&add_or_update_update,
	\&hash_search,
	\&array_search,
	\&foreign_key_remove,
	\&get_property,
	\&set_property,
	\&tables,
	\&has_table,
	\&hasnt_table,
	\&feature_components,
);

main();

sub main {
	plan(tests => scalar(@tests));
	# for each test routine, reload the original ISM file
	foreach my $sub (@tests) {
		$is->loadfile( 't/original.ism' );
		&$sub();
	}
}

sub no_change {
	ok( compare_file( 't/original.ism' ), "No change" );
}

sub hash_add {
	$is->add_property( { Property=>"TestProperty", Value=>"TestValue", ISComments=>"TestDesc" } );
	ok( compare_file( 't/add.ism' ), "Hash add" );
}

sub array_add {
	$is->add_property("TestProperty", "TestValue", "TestDesc");
	my $state = $is->save();
	ok( compare_file( 't/add.ism' ), "Array add" );
}

sub add_and_del {
	$is->add_property("TestProperty", "TestValue", "TestDesc");
	$is->del_property("TestProperty");
	ok( compare_file( 't/original.ism' ), "Add and delete" );
}

sub del_and_readd {
	$is->del_property("ProductName");
	$is->add_property("ProductName", "Test", undef);
	ok( compare_file( 't/original.ism' ), "Delete and re-add" );
}

sub hash_update {
	$is->update_property( { Property=>"ProductName", Value=>"Updated" } );
	ok( compare_file( 't/update.ism' ), "Hash update" );
}

sub array_update {
	$is->update_property("ProductName", "Updated", undef);
	ok( compare_file( 't/update.ism' ), "Array update" );
}

sub hash_del {
	$is->del_property( { Property=>"ProductName" });
	ok( compare_file( 't/del.ism' ), "Hash delete" );
}

sub array_del {
	$is->del_property("ProductName");
	ok( compare_file( 't/del.ism' ), "Array delete" );
}

sub add_or_update_add {
	$is->addOrUpdate_property("TestProperty", "TestValue", "TestDesc");
	ok( compare_file( 't/add.ism' ), "AddOrUpdate Add" );
}

sub add_or_update_update {
	$is->addOrUpdate_property("ProductName", "Updated", undef);
	ok( compare_file( 't/update.ism' ), "AddOrUpdate Update" );
}

sub hash_search {
	my $expected_result = [
		{
			Property	=> 'ProductVersion',
			Value		=> '1.2.3',
			ISComments	=> undef,
		}
	];
	my $result = $is->searchHash_property( { Property=>qr/Version/ } );
	is_deeply( $result, $expected_result, "Hash Search" );
}

sub array_search {
	my $expected_result = [
		[
			'ProductVersion',
			'1.2.3',
			undef,
		]
	];
	my $result = $is->searchArray_property( qr/Version/ );
	is_deeply( $result, $expected_result, "Array Search" );
}

sub foreign_key_remove {
	$is->purge_Component( 'ComponentA.dll' );
	ok( compare_file( 't/purge.ism' ), "Purge Key" );
}

# tests for the 'property' convenience function
sub get_property {
	my $product_name = $is->property('ProductName');
	is( $product_name->{'Value'}, 'Test', 'Get Property' );
}

sub set_property {
	$is->property( 'ProductName', 'Updated' );
	ok( compare_file( 't/update.ism' ), 'Set Property' );
}

sub tables {
	my $expected_result = [
		'Component',
		'Empty',
		'FeatureComponents',
		'File',
		'Property',
	];
	my $tables = $is->tables();
	is_deeply( $tables, $expected_result, "Get Tables" );
}

sub has_table {
	ok( $is->has_table( 'Property' ) );
}

sub hasnt_table {
	ok( !$is->has_table( 'NonExistantTable' ) );
}

sub feature_components {
	my $expected_result = [
		'ComponentA.dll',
		'ComponentB.dll',
	];
	my $components = $is->featureComponents( 'Feature1' );
	is_deeply( $components, $expected_result, "Feature Components" );
}

# compares the contents of a file on disk to the current
# value of the ISM in memory
sub compare_file {
	my ($file) = @_;
	open(F, "<$file");
	my $contents = join('', <F>);
	close(F);
	my $state = $is->save();
	return ($state eq $contents);
}
