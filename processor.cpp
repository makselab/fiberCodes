#include "processor.h"
#include <iostream>
#include <fstream>
#include <string>

Processor* Processor::p_Processor = 0;

Processor* Processor::getProcessor() {
	if(!p_Processor) {
		p_Processor = new Processor();
	}
	return p_Processor;
}

Processor::Processor() {
	readConfigFile();

	connections.resize(numberOfConnections);
	for(int i = 0; i < numberOfConnections; i++)
		connections[i].resize(weighted?3:2);

	for(int i = 0; i < numberOfNodes; i++)
		nodes.push_back(Node(i, 0));

	readConnectionsFile();
}

void Processor::run() {
	// different colors here just mean different type of nodes
	vector<int> nodeColors(numberOfNodes, 0);
	findGroupoids(numberOfNodes, connections, nodeColors);

	for(int i = 0; i < numberOfNodes; i++) {
		cout << i << "\t" << nodeColors[i] << endl;
	}
	
	for(int i = 0; i < numberOfNodes; i++) {
		nodes[i].setNodeColor(nodeColors[i]);
	}
	/*for(int i = 0; i < numberOfNodes; i++) {
		nodes[i].printNode();
	}*/
}

void Processor::addConnection(int source, int destination) {
	if(source < 0 || source >= numberOfNodes || destination < 0 || destination >= numberOfNodes) {
		cout << "Error: Trying to add connection out of bound. Number of nodes = " << numberOfNodes << ", source = " << source << ", destination = " << destination << endl;
	}
	nodes[source]     .addOutput(&nodes[destination]);
	nodes[destination].addInput (&nodes[source]);
}

void Processor::readConfigFile() {
// First line is number of nodes
// Second is if graph is directed(true or false)
// Third is weighted(true or false)
// Forth is number of different weights
/* Here the small remark included. We assume that we know exactly the amount of different possible
n weights and they are from 0..n. From the perspective of the algorithm there is no difference if
the weights are numbers or names, but the person who creates input has to take care of it being exactly n in form 0..n. */
// then connections follow up
	string line;
	ifstream config;

	config.open(inputFile, ifstream::in);

	getline(config, line, '\n');
	numberOfNodes = stoi(line);

	getline(config, line, '\n');
	directed = stoi(line);

	getline(config, line, '\n');
	weighted = stoi(line);

	getline(config, line, '\n');
	if(weighted) {
		numberOfWeights = stoi(line);
	} else {
		numberOfWeights = 0;
	}

	numberOfConnections = 0;
	while(1) {
		if(!std::getline(config, line, '\n')) {break;}
		numberOfConnections++;
	}
	config.close();
}

void Processor::readConnectionsFile() {
	string line;
	ifstream config;

	config.open(inputFile, ifstream::in);

	// skip 3 lines with config data
	getline(config, line, '\n');
	getline(config, line, '\n');
	getline(config, line, '\n');
	getline(config, line, '\n');

	int i = 0;
	while(1) {
		if(!getline(config, line, '\t')) {break;}
		connections[i][0] = stoi(line);
		getline(config, line, weighted?'\t':'\n');
		connections[i][1] = stoi(line);
		if(weighted) {
			getline(config, line, '\n');
			connections[i][2] = stoi(line);
		}
		addConnection(connections[i][0], connections[i][1]);
		i++;
	}
	config.close();
	// print connections
	/*for(int i = 0; i < numberOfConnections; i++) {
		cout << "Connection " << i << ": " << connections[i][0] + 1 << " -> " << connections[i][1] + 1;
		if(weighted) {cout << ". Weight = " << connections[i][2];}
		cout << endl;
	}*/
}

void Processor::calculateVectors(vector<int> nodeColors, vector< vector<int> > &vectors) {
	for(int i = 0; i < connections.size(); i++) {
		if(directed == false) {
			int pos = 0;
			if(numberOfWeights == 0) {
				pos = nodeColors[connections[i][1]];
			} else {
				pos = nodeColors[connections[i][1]] * numberOfWeights + connections[i][2];
			}
			vectors[connections[i][0]][pos]++;
		}
		int pos = 0;
		if(numberOfWeights == 0) {
			pos = nodeColors[connections[i][0]];
		} else {
			pos = nodeColors[connections[i][0]] * numberOfWeights + connections[i][2];
		}
		vectors[connections[i][1]][pos]++;
	}

	// output vector value
	/*for(int i = 0; i < vectors.size(); i++) {
		for(int j = 0; j < vectors[0].size(); j++) {
			cout << "vectors[" << i << "][" << j << "] = " << vectors[i][j] << "\t";
		}
		cout << endl;
	}
	cout << endl;*/
}

// returns new number of colors
int Processor::classifyNodes(vector< vector<int> > vectors, vector<int> &nodeColors) {
	// first let`s find how many unique types of vectors are out there
	vector< vector<int> > vectorTypes;
	vectorTypes.push_back(vectors[0]);

	for(int i = 1; i < nodeColors.size(); i++) {
		bool add = 1;
		for(int j = 0; j < vectorTypes.size(); j++) {
			if(vectors[i] == vectorTypes[j]) {add = 0;}
		}
		if(add == 1) {vectorTypes.push_back(vectors[i]);}
	}
/*
	// output unique vector types
	for(int i = 0; i < vectorTypes.size(); i++) {
		for(int j = 0; j < vectorTypes[0].size(); j++) {
			cout << "vectorTypes[" << i << "][" << j << "] = " << vectorTypes[i][j] << "\t";
		}
		cout << endl;
	}
	cout << endl;
*/
	// now let`s reshuffle node types for next step
	for(int i = 1; i < nodeColors.size(); i++) {
		for(int j = 0; j < vectorTypes.size(); j++) {
			if(vectors[i] == vectorTypes[j]) {nodeColors[i] = j;}
		}
	}

	return vectorTypes.size();
}

void Processor::findGroupoids(int numberOfNodes, vector< vector<int> > groupoidConnections, vector<int> &nodeColors) {
	// defining all colors initially to be same
	int numberOfColors = 1;
	while(1) {
		// create 2D vector array to store all vectors belonging to each node
		/* Explanation why array is of size numberOfNodes x (numberOfColors * numberOfWeights). There are two ways how to do it.
		Either it can be done as a 3D array and then we will need two realisations for weighted and non-weighted design.
		Or vectors themselves can be formed in a bit weird way, but we will classify nodes comparing vectors not worrying about their structure.
		It improves readability and simpliness only paying with the strange enumeration of array */
		vector< vector<int> > vectors(numberOfNodes);
		for(int i = 0; i < numberOfNodes; i++)
			vectors[i].resize(numberOfColors * (weighted?numberOfWeights:1));

		calculateVectors(nodeColors, vectors);
		int nOC = classifyNodes(vectors, nodeColors);
		if(nOC == numberOfColors) {break;}
		else {numberOfColors = nOC;}
	}
}
