#include "blocks.h"
#include <iostream>

bool BuildingBlock::addNode(int id) {
	for(int i = 0; i < nodes.size(); i++) {
		if(nodes[i] == id) {return 0;}
	}
	nodes.push_back(id);
	return 1;
}

void BuildingBlock::setColors(int color1, int color2) {
	colors[0] = color1;
	colors[1] = color2;
}

bool BuildingBlock::colorFits(int color) {
	if(color == colors[0] || color == colors[1]) {return 1;}
	return 0;
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
