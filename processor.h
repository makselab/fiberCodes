#ifndef PROCESSOR_H
#define PROCESSOR_H
#include <vector>
#include "node.h"
#include "blocks.h"
using namespace std;

#define inputFile "adjacency.txt"

class Processor {
private:
	Processor();
	static Processor *p_Processor;
public:
	static Processor* getProcessor();
private:
	int numberOfNodes;
	vector<Node> nodes;
	int numberOfConnections;
	void addConnection(int source, int destination);
	bool directed;
	bool weighted;
	int numberOfWeights;
	void readConfigFile();
private:
	// create 2D vector array to store all connections
	vector< vector<int> > connections;
	void readConnectionsFile();
private:
	void calculateVectors(vector<int> nodeColors, vector< vector<int> > &vectors);
	int classifyNodes(vector< vector<int> > vectors, vector<int> &nodeColors);
	int findGroupoids(int numberOfNodes, vector< vector<int> > groupoidConnections, vector<int> &nodeColors);
private:
	vector<BuildingBlock> blocks;
public:
	void run();
};

#endif
