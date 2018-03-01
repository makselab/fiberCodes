#include "blocks.h"
#include <iostream>

bool BuildingBlock::addNode(int id, int color) {
	for(int i = 0; i < nodes.size(); i++) {
		if(nodes[i] == id) {return 0;}
	}
	nodes.push_back(id);
	if(color != -1) {
		if(color > colors.size()) {
			colors.resize(color + 1);
		}
		colors[color]++;
	}
	return 1;
}

int BuildingBlock::getAdditionalColor() {
	for(int i = 0; i < colors.size(); i++) {
		if(colors[i] > 1) {
			colors[i] = 0;
			return i;
		}
	}
	return -1;
}

void BuildingBlock::print() {
	cout << "Building block id = " << id << ". Nodes:" << endl;
	for(int i = 0; i < nodes.size(); i++) {
		cout << nodes[i] << endl;
	}
	/*cout << "Colors:" << endl;
	for(int i = 0; i < colors.size(); i++) {
		cout << i << ":\t" << colors[i] << endl;
	}*/
}
