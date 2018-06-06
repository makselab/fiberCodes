library(testthat)

file.remove("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/adjacency.txt")
file.remove("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/buildingBlocks.txt")
file.remove("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/fibers.txt")
file.remove("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
system("g++ -std=c++11 /home/ian/Desktop/groupoid_finding_codes/fibers/R/main.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/processor.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/node.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/blocks.cpp -o /home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
test_dir("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat")
