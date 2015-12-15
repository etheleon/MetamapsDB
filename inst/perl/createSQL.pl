#!/usr/bin/env perl

use DBD::SQLite;

#my $file = "~/Desktop/sql_tax_nodes";
#my $dbfile = "~/Desktop/aSQL";

die "usage: $0 <input file> <output.sql> \n" unless $#ARGV == 1;

do {print "removing\n"; unlink $dbfile} if -e $dbfile;

my $dbh = DBI->connect(
    "dbi:SQLite:dbname=$dbfile","","",
    {RaiseError => 1, AutoCommit => 0}  #Some attributes: PrintError, RaiseError, AutoCommit
);

$dbh -> do(<<'SQL'
CREATE TABLE taxon(
    taxid integer PRIMARY KEY,
    name varchar(20),
    rank varchar(20)
)
SQL
);

open my $in, "<", $file ||
    die "$in not found: $!\n";

while(<$in>){
    chomp;
    my ($id, $name, $rank)  = split /\t/;
#    say join "\t",$id, $name, $rank;
    my $sth = $dbh->prepare('INSERT INTO taxon VALUES (?,?,?)');
    $sth->execute($id, $name, $rank);
}

$dbh->commit;
