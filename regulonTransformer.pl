#!/usr/bin/perl

sub transformAdajcency {
	# during first time we read file we create the list of all genes that were there as source and as destination
	open(FID, "regulonDBdata.txt")
	  or die "Failed to open input.txt: $!\n";

	my @map = ();
	my $numberOfNames = 0;
	while($line = <FID>) {
		$line =~ s/(\w+)\s+(\w+)\s+//;
		$tmp1 = $1;
		$tmp2 = $2;
		# here we change first letter from big to small, because we know the structure of input of regulon
		$tmp1 = lcfirst($tmp1);

		# now let's add genes to gene map and weights to weight map
		if(!grep{$_ =~ /$tmp1/} @map) {
			@map[$numberOfNames++] = $tmp1;
		}
		if(!grep{$_ =~ /$tmp2/} @map) {
			@map[$numberOfNames++] = $tmp2;
		}
	}
	close(FID);

	@map = sort values @map;

	# during second run we create an array which we will represents adjacency matrix
	my @adjacency = ();
	open(FID, "regulonDBdata.txt")
	  or die "Failed to open input.txt: $!\n";

	my %weightMap = ("+" => 0, "-" => 1, "+-" => 2);
	$lineNumber = 0;
	while($line = <FID>) {
		$line =~ s/(\w+)\s+(\w+)\s+(\S+)\s+//;
		$tmp1 = $1;
		$tmp2 = $2;
		$tmp3 = $3;
		# here we change first letter from big to small, because we know the structure of input of regulon
		$tmp1 = lcfirst($tmp1);

		# we add 1 here to enumerate all cells from 1 to n
		my ($pos) = grep{ $map[$_] =~ /$tmp1/} 0 .. $#map;
		$adjacency[$linenumber][0] = $pos + 1;
		my ($pos) = grep{ $map[$_] =~ /$tmp2/} 0 .. $#map;
		$adjacency[$linenumber][1] = $pos + 1;
		$adjacency[$linenumber++][2] = $weightMap{$tmp3};
	}

	close(FID);
	return (\@map, \@adjacency);
};
1;
