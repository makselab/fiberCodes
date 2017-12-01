#!/usr/bin/perl

open(FID, "regulonDBdata.txt")
  or die "Failed to open input.txt: $!\n";

# creating map of all gene names 
#while($line = <FID>) {
my %map = ();
$line = <FID>;
print($line);
$line =~ s/(\w+)\s+(\w+)\s+//;
$map{0} = $1;
$map{1} = $2;
print($map{0});
#}

close(FID);
