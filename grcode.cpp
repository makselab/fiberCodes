#include <iostream>
#include <fstream>
#include <string>
#include <vector>
using namespace std;

#define inputFile "adjacency.txt"

void calculateVectors(vector< vector<int> > connections, vector<int> nodeColors, vector< vector<int> > &vectors, bool directed);
int classifyNodes(vector< vector<int> > vectors, vector<int> &nodeColors);
void readConfigFile(int &numberOfNodes, bool &directed, int &numberOfConnections);
void readConnectionsFile(vector< vector<int> > &connections);

int main() {
    int numberOfNodes;
    int numberOfConnections;
    bool directed;
    readConfigFile(numberOfNodes, directed, numberOfConnections);

    // create 2D vector array to store all connections
    vector< vector<int> > connections(numberOfConnections);
    for(int i = 0; i < numberOfConnections; i++)
        connections[i].resize(2);
    readConnectionsFile(connections);
    // print connections
    for(int i = 0; i < numberOfConnections; i++) {
        for(int j = 0; j < 2; j++)
            cout << "connections[" << i << "][" << j << "] = " << connections[i][j] << "\t";
        cout << endl;
    }
    cout << endl;

    // defining all colors initially to be same
    vector<int> nodeColors(numberOfNodes, 0);
    int numberOfColors = 1;

    while(1) {
        // create 2D vector array to store all vectors belonging to each node
        vector< vector<int> > vectors(numberOfNodes);
        for(int i = 0; i < numberOfNodes; i++)
            vectors[i].resize(numberOfColors);

        calculateVectors(connections, nodeColors, vectors, directed);
        int nOC = classifyNodes(vectors, nodeColors);
        if(nOC == numberOfColors) {break;}
        else {numberOfColors = nOC;}
    }

    // we add 1 here, because we want to enumerate output from 1 to n
    for(int i = 0; i < numberOfNodes; i++)
        cout << i + 1 << " " << nodeColors[i] << endl;
}

void readConfigFile(int &numberOfNodes, bool &directed, int &numberOfConnections) {
// First line is number of nodes, second one is if graph is directed(true or false), then connections follow up
    string line;
    ifstream config;

    config.open(inputFile, ifstream::in);

    getline(config, line, '\n');
    numberOfNodes = stoi(line);

    getline(config, line, '\n');
    directed = stoi(line);

    numberOfConnections = 0;
    while(1) {
        if(!std::getline(config, line, '\n')) {break;}
        numberOfConnections++;
    }
    config.close();
}

void readConnectionsFile(vector< vector<int> > &connections) {
    string line;
    ifstream config;

    config.open(inputFile, ifstream::in);

    // skip 2 lines with config data
    getline(config, line, '\n');
    getline(config, line, '\n');

    int i = 0;
    while(1) {
        if(!getline(config, line, '\n')) {break;}
        // we add -1 here, because in adjacency file we enumerate from 1 to n
        connections[i][0] = line[0] - '0' - 1;
        connections[i][1] = line[2] - '0' - 1;
        i++;
    }
    config.close();
}

void calculateVectors(vector< vector<int> > connections, vector<int> nodeColors, vector< vector<int> > &vectors, bool directed) {
    for(int i = 0; i < connections.size(); i++) {
        if(directed == false) {
            vectors[connections[i][0]][nodeColors[connections[i][1]]]++;
        }
        vectors[connections[i][1]][nodeColors[connections[i][0]]]++;
    }

    // output vector value
    for(int i = 0; i < vectors.size(); i++) {
        for(int j = 0; j < vectors[0].size(); j++) {
            cout << "vectors[" << i << "][" << j << "] = " << vectors[i][j] << "\t";
        }
        cout << endl;
    }
    cout << endl;
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

    // output unique vector types
    for(int i = 0; i < vectorTypes.size(); i++) {
        for(int j = 0; j < vectorTypes[0].size(); j++) {
            cout << "vectorTypes[" << i << "][" << j << "] = " << vectorTypes[i][j] << "\t";
        }
        cout << endl;
    }
    cout << endl;

    // now let`s reshuffle node types for next step
    for(int i = 1; i < nodeColors.size(); i++) {
        for(int j = 0; j < vectorTypes.size(); j++) {
            if(vectors[i] == vectorTypes[j]) {nodeColors[i] = j;}
        }
    }

    return vectorTypes.size();
}
