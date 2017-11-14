#include <iostream>
#include <vector>
using namespace std;

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

int main() {
    int numberOfNodes = 4;
    int numberOfConnections = 6;
    bool directed = 1;

    // create 2D vector array to store all connections
    vector< vector<int> > connections(numberOfConnections);
    for(int i = 0; i < numberOfConnections; i++)
        connections[i].resize(2);

    // defining all colors initially to be same
    vector<int> nodeColors(numberOfNodes, 0);
    int numberOfColors = 1;

    // define connections
    connections[0][0] = 0; connections[0][1] = 1;
    connections[1][0] = 0; connections[1][1] = 2;
    connections[2][0] = 1; connections[2][1] = 3;
    connections[3][0] = 2; connections[3][1] = 0;
    connections[4][0] = 2; connections[4][1] = 3;
    connections[5][0] = 3; connections[5][1] = 0;
    for(int i = 0; i < numberOfConnections; i++) {
        for(int j = 0; j < 2; j++)
            cout << "connections[" << i << "][" << j << "] = " << connections[i][j] << "\t";
        cout << endl;
    }
    cout << endl;

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

    for(int i = 0; i < numberOfNodes; i++)
        cout << i << " " << nodeColors[i] << endl;
}
