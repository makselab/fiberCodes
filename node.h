#ifndef NODE_H
#define NODE_H

#include <vector>
using namespace std;

class Node {
public:
	Node(int iD, int coloR) : id(iD), color(coloR) {}
	int getColor()	{return color;}
	int getId()		{return id;}
	void setColor(int coloR) {color = coloR;}
	void addInput(Node* entry);
	void addOutput(Node* entry);
	void printNode();
private:
	vector<Node*> output;
	vector<Node*> input;
	int color;
	int id;
};

#endif
