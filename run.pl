#!/usr/bin/perl
use strict;
use warnings;

my $help = 0;
my $directed = 0;
my $weighted = 0;
my $inputFile = "";
my $gephi = 0;
my $outputFile = "";
my @globalInput;

main();

sub main {
	readCLInput();
	readInputFile();
	my @map = createMap();
	my @adjacency = createAdjacency(@map);
	createConfigurationFile(scalar @map, @adjacency);
	my $codeOutput = runCode();
	my %groupoids = parseCodeOutput($codeOutput, @map);
	createOutput(%groupoids);
}

sub readCLInput {
	while (my $arg = shift @ARGV) {
		if($arg eq "-help") {
			$help = 1;
			last;
		}
		if($arg eq "-directed") {
			$directed = 1;
			next;
		}
		if($arg eq "-weighted") {
			$weighted = 1;
			next;
		}
		if($arg eq "-gephi") {
			$gephi = 1;
			next;
		}
		if($arg eq "-inputFrom") {
			$inputFile = shift @ARGV;
			next;
		}
		if($arg eq "-outputTo") {
			$outputFile = shift @ARGV;
			next;
		}
		$help = 1;
	}

	if ($help == 1) {
		print("Usage:\n");
		print("./run.pl -\$keys\n");
		print("Possible keys:\n");
		print("-help\t\t\t- Prints this help message with usage\n");
		print("-directed\t\t- Initially we think that graph is undirected, sets it to directed\n");
		print("-weighted\t\t- Initially we think that graph is unweighted, sets it to weighted\n");
		print("-gephi\t\t\t- Create output in a gephi format\n");
		print("-inputFrom filename\t- Path to file with connections to setup adjacency\n");
		print("-outputTo filename\t- Path to output file\n");
	}
	print("help\t\t= $help\n");
	print("directed\t= $directed\n");
	print("weighted\t= $weighted\n");
	print("gephi\t\t= $gephi\n");
	print("inputFile\t= $inputFile\n");
	print("outputFile\t= $outputFile\n");
}

sub readInputFile {
	if($inputFile eq "") {
		print("No input file path\n");
		return;
	}
	local $/=undef;
	open(FID, $inputFile)
		or die "Failed to open $inputFile: $!\n";
	my $input = <FID>;
	close(FID);
	@globalInput = split(/\n/, $input);

	#print input
	foreach my $line (@globalInput) {
		print("$line\n");
	}

	return;
}

sub createMap {
	my @tmpInput = @globalInput;
	my @map = ();
	my $numberofnames = 0;
	foreach my $line (@tmpInput) {
		$line =~ s/(\w+)\s+(\w+)\s+//;
		my $tmp1 = $1;
		my $tmp2 = $2;

		if(!grep{$_ =~ /$tmp1/} @map) {
			@map[$numberofnames++] = $tmp1;
		}
		if(!grep{$_ =~ /$tmp2/} @map) {
			@map[$numberofnames++] = $tmp2;
		}
	}
	@map = sort values @map;

	#print map
	my $i = 0;
	foreach my $entry (@map) {
		print("$i\t- $entry\n");
		$i++;
	}
	return @map;
}

sub createAdjacency {
	my @map = @_;
	my @tmpInput = @globalInput;
	my @adjacency = ();

	my $lineNumber = 0;
	foreach my $line (@tmpInput) {
		my $source;
		my $destination;
		my $weight;
		if($weighted == 1) {
			$line =~ s/(\w+)\s+(\w+)\s+(\S+)\s+//;
			$source = $1;
			$destination = $2;
			$weight = $3;
		} else {
			$line =~ s/(\w+)\s+(\w+)\s+//;
			$source = $1;
			$destination = $2;
		}

		my $pos;
		($pos) = grep{ $map[$_] =~ /$source/} 0 .. $#map;
		$adjacency[$lineNumber][0] = $pos;
		($pos) = grep{ $map[$_] =~ /$destination/} 0 .. $#map;
		$adjacency[$lineNumber][1] = $pos;
		if($weighted == 1) {
		#	$adjacency[$lineNumber][2] = $weightMap{$weight};
		}
		$lineNumber++;
	}

	for(my $i = 0; $i < $lineNumber; $i++) {
		print("$adjacency[$i][0]\t$adjacency[$i][1]\n");
	}

	return @adjacency;
}

sub createConfigurationFile {
	my ($size, @adjacency) = @_;
	my $numberOfConnections = scalar @adjacency;
	my $secondSize = @{$adjacency[0]};
	# adjacency.txt structure
	# 1: size in cells
	# 2: directed/undirected
	# 3: weighted/not weighted
	# 4: number of weights
	# 5..inf: adjacency matrix
	open (adjacencyFile, '>', "adjacency.txt");
	print adjacencyFile $size;
	print adjacencyFile "\n$directed\n";
	print adjacencyFile "$weighted\n";
	print adjacencyFile "1\n";
	for(my $i = 0; $i < $numberOfConnections; $i++) {
			for(my $j = 0; $j < $secondSize; $j++) {
					print adjacencyFile "$adjacency[$i][$j]\t";
			}
			print adjacencyFile "\n";
	}
	close (adjacencyFile); 
}

sub runCode {
	print("Compiling code\n");
	system("g++ -std=c++11 ./grcode.cpp -o grcode");
	# TODO: check if there are no "   " instead of '\t' symbol, cause it will cause problems
	print("Running code\n");
	my $start = time;
	my $codeOutput = qx("./grcode");
	my $duration = time - $start;
	print "Execution time: $duration s\n";
	return $codeOutput;
}

sub parseCodeOutput {
	my ($codeOutput, @map) = @_;
	print("Creating output\n");
	my @output = ();
	my @data = split /\n/, $codeOutput;
	my $i = 0;
	foreach(@data) {
		my @tmp = split /\t/, $_;
		$output[$i][0] = $tmp[0];
		$output[$i++][1] = $tmp[1];
	}

	my %groupoids;

	for($i = 0; $i < scalar @output; $i++) {
		push(@{$groupoids{$output[$i][1]}}, $map[$output[$i][0]]);
	}
	return %groupoids;
}

sub createOutput {
	my %groupoids = @_;
	my $numberOfGroupoids = keys %groupoids;
	for(my $i = 0; $i < $numberOfGroupoids; $i++) {
		print("$i:\t");
		for(my $j = 0; $j < scalar @{$groupoids{$i}}; $j++) {
			print("$groupoids{$i}[$j]\t");
		}
		print("\n");
	}
	if($outputFile ne "") {
		open (outputFile, '>', $outputFile);
		for(my $i = 0; $i < $numberOfGroupoids; $i++) {
			print(outputFile "$i:\t");
			for(my $j = 0; $j < scalar @{$groupoids{$i}}; $j++) {
				print(outputFile "$groupoids{$i}[$j]\t");
			}
			print(outputFile "\n");
		}
		close(outputFile)
	}
}
