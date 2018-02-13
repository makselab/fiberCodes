#ifndef BUILDINGBLOCK_H
#define BUILDINGBLOCK_H
#include <vector>
using namespace std;

class BuildingBlock {
private:
	int id;
	vector<int> nodes;
public:
	BuildingBlock(int iD) {id = iD;};
	bool addNode(int id);
	void print();
};

#endif
