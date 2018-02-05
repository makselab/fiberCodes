#include "blocks.h"
#include <iostream>

void BuildingBlock::addNode(int id) {
	nodes.push_back(id);
}

void BuildingBlock::print() {
	cout << "Building block id = " << id << ". Nodes:"<< endl;
	for(int i = 0; i < nodes.size(); i++) {
		cout << nodes[i] << endl;
	}
	cout << "Colors:" << endl;
	for(int i = 0; i < colors.size(); i++) {
		cout << colors[i] << endl;
	}
}
