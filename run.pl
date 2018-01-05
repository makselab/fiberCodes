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
	my ($map_ref, $weightMap_ref) = createMaps();
	my @adjacency = createAdjacency($map_ref, $weightMap_ref);
	createConfigurationFile($map_ref, $weightMap_ref, @adjacency);
	my $codeOutput = runCode();
	my %groupoids = parseCodeOutput($codeOutput, $map_ref);
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
		exit;
	}

	print("Printing insterted parameters for script to run...\n");
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
		print("Try using \"-help\" key for usage\n");
		exit;
	}
	local $/=undef;
	open(FID, $inputFile)
		or die "Failed to open $inputFile: $!\n";
	my $input = <FID>;
	close(FID);
	@globalInput = split(/\n/, $input);

	@globalInput = grep { $_ ne "" } @globalInput;

	#print input
	print("Printing input file.\n");
	foreach my $line (@globalInput) {
		print("$line\n");
	}

	return;
}

sub createMaps {
	my @tmpInput = @globalInput;
	my @map;
	my %weightMap;
	foreach my $line (@tmpInput) {
		my $tmp1;
		my $tmp2;
		my $tmp3;
		if($weighted == 0) {
			$line =~ s/(\S+)\s+(\S+)\s*//;
			$tmp1 = $1;
			$tmp2 = $2;
		} else {
			$line =~ s/(\S+)\s+(\S+)\s+(\S+)\s*//;
			$tmp1 = $1;
			$tmp2 = $2;
			$tmp3 = $3;
		}

		if(!grep{$_ =~ /^$tmp1$/} @map) {
			push @map, $tmp1;
		}
		if(!grep{$_ =~ /^$tmp2$/} @map) {
			push @map, $tmp2;
		}
		if($weighted == 1 and !exists $weightMap{$tmp3}) {
			$weightMap{$tmp3} = keys %weightMap;
		}
	}
	@map = sort values @map;

	#print maps
	print("Printing map of all possible nodes...\n");
	my $i = 0;
	foreach my $entry (@map) {
		print("$i\t- \"$entry\"\n");
		$i++;
	}
	print("Printing map of all possible weights...\n");
	foreach my $entry (keys %weightMap) {
		print("$weightMap{$entry}\t - $entry\n");
	}
	return (\@map, \%weightMap);
}

sub createAdjacency {
	my ($map_ref, $weightMap_ref) = @_;
	my @map = @$map_ref;
	my %weightMap = %$weightMap_ref;
	my @tmpInput = @globalInput;
	my @adjacency = ();

	my $lineNumber = 0;
	foreach my $line (@tmpInput) {
		my $source;
		my $destination;
		my $weight;
		if($weighted == 1) {
			$line =~ s/(\S+)\s+(\S+)\s+(\S+)\s*//;
			$source = $1;
			$destination = $2;
			$weight = $3;
		} else {
			$line =~ s/(\S+)\s+(\S+)\s*//;
			$source = $1;
			$destination = $2;
		}

		my $pos;
		($pos) = grep{ $map[$_] =~ m/^$source$/} 0 .. $#map;
		$adjacency[$lineNumber][0] = $pos;
		($pos) = grep{ $map[$_] =~ m/^$destination$/} 0 .. $#map;
		$adjacency[$lineNumber][1] = $pos;
		if($weighted == 1) {
			$adjacency[$lineNumber][2] = $weightMap{$weight};
		}
		$lineNumber++;
	}


	print("Printing adjacency matrix...\n");
	if($weighted == 0) {
		for(my $i = 0; $i < $lineNumber; $i++) {
			print("$adjacency[$i][0]\t$adjacency[$i][1]\n");
		}
	} else {
		for(my $i = 0; $i < $lineNumber; $i++) {
			print("$adjacency[$i][0]\t$adjacency[$i][1]\t$adjacency[$i][2]\n");
		}
	}

	return @adjacency;
}

sub createConfigurationFile {
	my ($map_ref, $weightMap_ref, @adjacency) = @_;
	my $numberOfNodes = scalar @$map_ref;
	my $numberOfWeights = scalar %$weightMap_ref;
	my $numberOfConnections = scalar @adjacency;
	my $secondSize = @{$adjacency[0]};
	# adjacency.txt structure
	# 1: size in cells
	# 2: directed/undirected
	# 3: weighted/not weighted
	# 4: number of weights
	# 5..inf: adjacency matrix
	open (adjacencyFile, '>', "adjacency.txt");
	print adjacencyFile $numberOfNodes;
	print adjacencyFile "\n$directed\n";
	print adjacencyFile "$weighted\n";
	print adjacencyFile "$numberOfWeights\n";
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
	print("Parsing code output...\n");
	my ($codeOutput, $map_ref) = @_;
	my @map = @$map_ref;
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
	print("Printing parsed code output...\n");
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
