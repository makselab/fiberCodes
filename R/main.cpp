#include "processor.h"
#include <cstdlib>

int main(int argc, char *argv[]) {
	Processor *processor = processor->getProcessor(atoi(argv[1]));
	processor->run();
	return 0;
}
