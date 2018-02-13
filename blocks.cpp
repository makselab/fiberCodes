#include "blocks.h"
#include <iostream>

bool BuildingBlock::addNode(int id) {
	for(int i = 0; i < nodes.size(); i++) {
		if(nodes[i] == id) {return 0;}
	}
	nodes.push_back(id);
	return 1;
}

void BuildingBlock::print() {
	cout << "Building block id = " << id << ". Nodes:"<< endl;
	for(int i = 0; i < nodes.size(); i++) {
		cout << nodes[i] << endl;
	}
}
