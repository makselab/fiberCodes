#include "blocks.h"
#include <iostream>

bool BuildingBlock::addNode(int id, int color) {
	for(int i = 0; i < nodes.size(); i++) {
		if(nodes[i] == id) {return 0;}
	}
	nodes.push_back(id);
	if(color != -1) {
		if(color >= colors.size()) {
			colors.resize(color + 1);
		}
		colors[color]++;
	}
	return 1;
}

bool BuildingBlock::isAddableColor(int color) {
	if(color >= colors.size()) {
		return 0;
	}
	if(colors[color] > 1) {
		return 1;
	} else {
		return 0;
	}
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
