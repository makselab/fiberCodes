#ifndef NODE_H
#define NODE_H

#include <vector>

class Node {
public:
	Node(int iD, int coloR) : id(iD), color(coloR) {}
	int getColor()	{return color;}
	int getId()		{return id;}
	void addInput(Node* entry);
	void addOutput(Node* entry);
private:
	std::vector<Node*> output;
	std::vector<Node*> input;
	int color;
	int id;
};

#endif
