library(testthat)

file.remove("~/Dropbox/groupoid_finding_codes/fibers/tests/testthat/adjacency.txt")
file.remove("~/Dropbox/groupoid_finding_codes/fibers/tests/testthat/buildingBlocks.txt")
file.remove("~/Dropbox/groupoid_finding_codes/fibers/tests/testthat/fibers.txt")
file.remove("~/Dropbox/groupoid_finding_codes/fibers/tests/testthat/exec")
system("g++ -std=c++11 ~/Dropbox/groupoid_finding_codes/fibers/R/main.cpp ~/Dropbox/groupoid_finding_codes/fibers/R/processor.cpp ~/Dropbox/groupoid_finding_codes/fibers/R/node.cpp ~/Dropbox/groupoid_finding_codes/fibers/R/blocks.cpp -o ~/Dropbox/groupoid_finding_codes/fibers/tests/testthat/exec")
test_dir("~/Dropbox/groupoid_finding_codes/fibers/tests/testthat")
