#!/usr/bin/perl
require "regulonTransformer.pl";

# TODO: there is a gene with name CBR which has transcription factor cbr, check how many of genes like this are out there
print("Running algorithm to transform input data\n");
my ($map_ref, $adjacency_ref) = transformAdajcency();
my @map = @$map_ref;
my @adjacency = @$adjacency_ref;

# adjacency.txt structure
# size in cells
# directed/undirected
# weighted/not weighted
# number of weights
# adjacency matrix
open (adjacencyFile, '>adjacency.txt');
print adjacencyFile scalar @map;
print adjacencyFile "\n1\n";
print adjacencyFile "1\n";
print adjacencyFile "3\n";
for($i = 0; $i < $linenumber; $i++) {
        for($j = 0; $j < 3; $j++) {
                print adjacencyFile "$adjacency[$i][$j]\t";
        }
        print adjacencyFile "\n";
}
close (MYFILE); 

# here we compile and run groupoid finding code
print("Compiling code\n");
system("g++ -std=c++11 ./grcode.cpp -o grcode");
# TODO: check if there are no "   " instead of '\t' symbol, cause it will cause problems
print("Running code\n");
my $groupoidOutput = qx("./grcode");

# now we want to put output in a more readable way
print("Creating output\n");
my @output = ();
my @data = split /\n/, $groupoidOutput;
my $i = 0;
$numberOfGroupoids = 0;
foreach(@data) {
	my @tmp = split /\t/, $_;
	$output[$i][0] = $tmp[0];
	$output[$i++][1] = $tmp[1];
	if($tmp[1] > $numberOfGroupoids) {$numberOfGroupoids = $tmp[1];}
}
my %groupoids;
for($i = 0; $i < scalar @output; $i++) {
	push(@{$groupoids{$output[$i][1]}}, $map[$output[$i][0]]); 
}

for($i = 0; $i < $numberOfGroupoids; $i++) {
	print("$i: ");
	for($j = 0; $j < @{$groupoids{$i}}; $j++) {
		print("$groupoids{$i}[$j], ");
	}
	print("\n");
}
