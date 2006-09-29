use Test::More tests => 9;
use Win32::InstallShield;

my $is = Win32::InstallShield->new();

main();

sub main {
	foreach my $sub ( 
			\&no_change,
			\&array_add, 
			\&hash_add, 
			\&add_and_del, 
			\&del_and_readd,
			\&hash_update,
			\&array_update,
			\&hash_del,
			\&array_del,
		) {
		$is->loadfile( 't/original.ism' );
		&$sub();
	}
}

sub no_change {
	ok( compare_file( 't/original.ism' ) );
}

sub hash_add {
	$is->add_property( { Property=>"TestProperty", Value=>"TestValue", ISComments=>"TestDesc" } );
	ok( compare_file( 't/add.ism' ) );
}

sub array_add {
	$is->add_property("TestProperty", "TestValue", "TestDesc");
	my $state = $is->save();
	ok( compare_file( 't/add.ism' ) );
}

sub add_and_del {
	$is->add_property("TestProperty", "TestValue", "TestDesc");
	$is->del_property("TestProperty");
	my $state = $is->save();
	ok( compare_file( 't/original.ism' ) );
}

sub del_and_readd {
	$is->del_property("ProductName");
	$is->add_property("ProductName", "Test", undef);
	ok( compare_file( 't/original.ism' ) );
}

sub hash_update {
	$is->update_property( { Property=>"ProductName", Value=>"Updated" } );
	ok( compare_file( 't/update.ism' ) );
}

sub array_update {
	$is->update_property("ProductName", "Updated", undef);
	ok( compare_file( 't/update.ism' ) );
}

sub hash_del {
	$is->del_property( { Property=>"ProductName" });
	ok( compare_file( 't/del.ism' ) );
}

sub array_del {
	$is->del_property("ProductName");
	ok( compare_file( 't/del.ism' ) );
}

sub compare_file {
	my ($file) = @_;
	open(F, "<$file");
	my $contents = join('', <F>);
	close(F);
	my $state = $is->save();
	return ($state eq $contents);
}
