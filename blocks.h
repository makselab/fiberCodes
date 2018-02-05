#ifndef BUILDINGBLOCK_H
#define BUILDINGBLOCK_H
#include <vector>
using namespace std;

class BuildingBlock {
private:
	int id;
	vector<int> nodes;
	vector<int> colors;
public:
	BuildingBlock(int iD) {id = iD;};
	void addNode(int id);
	void print();
};

#endif
