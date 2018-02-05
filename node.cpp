#include "node.h"
#include <iostream>

//TODO: check for duplicated connections
void Node::addInput(Node* entry) {
	input.resize(input.size() + 1);
	input[input.size() - 1] = entry;
}

void Node::addOutput(Node* entry) {
	output.resize(output.size() + 1);
	output[output.size() - 1] = entry;
}

void Node::printNode() {
	cout << "Printing node " << id << ". Color = " << color << endl;
	cout << "Number of connections (i/o) = " << input.size() << "/" << output.size() << endl;
}
