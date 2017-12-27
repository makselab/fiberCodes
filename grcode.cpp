#include <iostream>
#include <fstream>
#include <string>
#include <vector>
using namespace std;

#define inputFile "adjacency.txt"

void calculateVectors(vector< vector<int> > connections, vector<int> nodeColors, vector< vector<int> > &vectors, bool directed, int numberOfWeights);
int classifyNodes(vector< vector<int> > vectors, vector<int> &nodeColors);
void readConfigFile(int &numberOfNodes, int &numberOfConnections, bool &directed, bool &weighted, int &numberOfWeights);
void readConnectionsFile(vector< vector<int> > &connections, bool weighted);

int main() {
	int numberOfNodes;
	int numberOfConnections;
	bool directed;
	bool weighted;
	int numberOfWeights;
	readConfigFile(numberOfNodes, numberOfConnections, directed, weighted, numberOfWeights);

	// create 2D vector array to store all connections
	vector< vector<int> > connections(numberOfConnections);
	for(int i = 0; i < numberOfConnections; i++)
		connections[i].resize(weighted?3:2);
	readConnectionsFile(connections, weighted);
	// print connections
	/*for(int i = 0; i < numberOfConnections; i++) {
		cout << "Connection " << i << ": " << connections[i][0] + 1 << " -> " << connections[i][1] + 1;
		if(weighted) {cout << ". Weight = " << connections[i][2];}
		cout << endl;
	}*/

	// defining all colors initially to be same
	// different colors here just mean different type of nodes
	vector<int> nodeColors(numberOfNodes, 0);
	int numberOfColors = 1;

	while(1) {
		// create 2D vector array to store all vectors belonging to each node
		/* Explanation why array is of size numberOfNodes x (numberOfColors * numberOfWeights). There are two ways how to do it.
		Either it can be done as a 3D array and then we will need two realisations for weighted and non-weighted design.
		Or vectors themselves can be formed in a bit weird way, but we will classify nodes comparing vectors not worrying about their structure.
		It improves readability and simpliness only paying with the strange enumeration of array */
		vector< vector<int> > vectors(numberOfNodes);
		for(int i = 0; i < numberOfNodes; i++)
			vectors[i].resize(numberOfColors * weighted?numberOfWeights:1);

		calculateVectors(connections, nodeColors, vectors, directed, weighted?numberOfWeights:0);
		int nOC = classifyNodes(vectors, nodeColors);
		if(nOC == numberOfColors) {break;}
		else {numberOfColors = nOC;}
	}

	for(int i = 0; i < numberOfNodes; i++)
		cout << i << "\t" << nodeColors[i] << endl;
}

void readConfigFile(int &numberOfNodes, int &numberOfConnections, bool &directed, bool &weighted, int &numberOfWeights) {
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
	numberOfWeights = stoi(line);

	numberOfConnections = 0;
	while(1) {
		if(!std::getline(config, line, '\n')) {break;}
		numberOfConnections++;
	}
	config.close();
}

void readConnectionsFile(vector< vector<int> > &connections, bool weighted) {
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
		i++;
	}
	config.close();
}

void calculateVectors(vector< vector<int> > connections, vector<int> nodeColors, vector< vector<int> > &vectors, bool directed, int numberOfWeights) {
	for(int i = 0; i < connections.size(); i++) {
		if(directed == false) {
			vectors[connections[i][0]][nodeColors[connections[i][1]] * numberOfWeights + connections[i][2]]++;
		}
		vectors[connections[i][1]][nodeColors[connections[i][0]] * numberOfWeights + connections[i][2]]++;
	}
/*
	// output vector value
	for(int i = 0; i < vectors.size(); i++) {
		for(int j = 0; j < vectors[0].size(); j++) {
			cout << "vectors[" << i << "][" << j << "] = " << vectors[i][j] << "\t";
		}
		cout << endl;
	}
	cout << endl;*/
}

// returns new number of colors
int classifyNodes(vector< vector<int> > vectors, vector<int> &nodeColors) {
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
