#!/usr/bin/perl
require "regulonTransformer.pl";

# TODO: there is a gene with name CBR which has transcription factor cbr, check how many of genes like this are out there
print("Running algorithm to transform input data\n");
my ($map_ref, $adjacency_ref) = transformAdajcency();
my @map = @$map_ref;
my @adjacency = @$adjacency_ref;

# adjacency.txt structure
# 1: size in cells
# 2: directed/undirected
# 3: weighted/not weighted
# 4: number of weights
# 5..inf: adjacency matrix
open (adjacencyFile, '>adjacency.txt');
print adjacencyFile scalar @map;
print adjacencyFile "\n0\n";
print adjacencyFile "0\n";
print adjacencyFile "1\n";
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
my $start = time;
my $groupoidOutput = qx("./grcode");
my $duration = time - $start;
print "Execution time: $duration s\n";

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
# we add 1, cause number of groupoids is bigger then maximum index of groupoid
$numberOfGroupoids++;

my %groupoids;

for($i = 0; $i < scalar @output; $i++) {
	push(@{$groupoids{$output[$i][1]}}, $map[$output[$i][0] - 1]);
}

for($i = 0; $i < $numberOfGroupoids; $i++) {
	print("$i:\t");
	for($j = 0; $j < scalar @{$groupoids{$i}}; $j++) {
		print("$groupoids{$i}[$j]\t");
	}
	print("\n");
}
