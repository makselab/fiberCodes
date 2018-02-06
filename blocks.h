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
	BuildingBlock(int iD) {id = iD; colors.resize(2);};
	bool addNode(int id);
	void setColors(int color1, int color2);
	bool colorFits(int color);
	void print();
};

#endif
