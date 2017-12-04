#!/usr/bin/env perl

use DBD::SQLite;
use Net::FTP;
use File::Tempdir;
use Archive::Tar;
use v5.20;

die "usage: $0 <output> \n" unless $#ARGV == 0;

my $dbfile = $ARGV[0];
do {print "removing\n"; unlink $dbfile} if -e $dbfile;

my $ftp = Net::FTP->new("ftp.ncbi.nlm.nih.gov", Debug => 1) or die "Cannot connect to ftp://ftp.ncbi.nlm.nih.gov/: $@";
$ftp->login("anonymous",'-anonymous@')
    or die "Cannot login ", $ftp->message;
$ftp->cwd("/pub/taxonomy")
    or die "Cannot change working directory ", $ftp->message;

#my $dir = File::Tempdir->new("/tmp/sqltemp");

#my $dirname = $dir->name;
$ftp->get("taxdump.tar.gz","/tmp/sqldump/taxdump.tar.gz")
    or die "get failed ", $ftp->message;


my $tar=Archive::Tar->new();
$tar->read("/tmp/sqldump/taxdump.tar.gz");
say "read";
$tar->extract();
#$tar->extract([["names.dmp", "nodes.dmp"]]);

# my $dbh = DBI->connect(
#     "dbi:SQLite:dbname=$dbfile","","",
#     {
#         RaiseError => 1,
#         AutoCommit => 0
#     }  #Some attributes: PrintError, RaiseError, AutoCommit
# );
#
# $dbh -> do(<<'SQL'
# CREATE TABLE taxon(
#     taxid integer PRIMARY KEY,
#     name varchar(20),
#     rank varchar(20)
# )
# SQL
# );
#
#
# my %hash;
#
# open my $file, "<", "/nodes.dmp" || die "$dirname/nodes.dmp not found: $!\n";
# while(<$file>){
#     chomp;
#     my ($id, $rank) = (split/\t\|\t/)[1,2];
#     #print join "\t", $id, $rank;
#     $hash{$id} = {
#     'rank' => $rank
#     };
# }
# close($file);
#
# open my $file2, "<", "$dirname/names.dmp" || die "$dirname/names.dmp not found: $!\n";
# while(<$file2>){
#     chomp;
#     if(m/scientific name/)
#     {
#         my ($id, $name) = (split/\t\|\t/)[0,1];
#         $name =~ s/|//g;
#         #print join "\t", $id, $name;
#         $hash{$id}->{name} = $name;
#     }
# }
# close($file2);
#
# # printing the table out
# # open my $file3, ">", $dbfile || die "$dbfile not found: $!\n";
# # foreach (keys %hash){
# #     print $file3 join("|", $_, $hash{$_}->{name}, $hash{$_}->{rank})."\n";
# #     }
# # close($file3);
#
#
# foreach my $key (keys %hash){
#    my $sth = $dbh->prepare('INSERT INTO taxon VALUES (?,?,?)');
#    $sth->execute($key, $hash{$key}->{name}, $hash{$key}->{rank});
# }
# $dbh->commit;
#
#     #open my $in, "<", $file ||
#     #    die "$in not found: $!\n";
#
#     # while(<$in>){
#     #     chomp;
#     #     my ($id, $name, $rank)  = split /\t/;
#     # #    say join "\t",$id, $name, $rank;
#     #     my $sth = $dbh->prepare('INSERT INTO taxon VALUES (?,?,?)');
#     #     $sth->execute($id, $name, $rank);
#     # }
