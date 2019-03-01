# Author: Ian Leifer <ianleifer93@gmail.com>

library(tidyr)
library(dplyr)
source("~/Dropbox/groupoid_finding_codes/fibers/R/functionsPar.R")

# TODO: this all code can be made much faster with a proper use of igraph. I would rewrite the whole code
runFiberCode <- function(fileNames, configuration, parId) {
  currentDirectory <- getwd()
  setwd("~/Dropbox/groupoid_finding_codes/fibers/R")
  network <- readNetworkFile(configuration)

  nodeMap <- createNodeMap(network, configuration)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  } else {
    weightMap <- NULL
  }

  connectivity <- getTransformedConnectivity(configuration, network, nodeMap, weightMap)
  writeToAdjacencyFile(configuration, nodeMap, weightMap, connectivity, fileNames)

  codePreactions(fileNames)
  # TODO: properly check if code returned 1
  system(paste("g++ -std=c++11 main.cpp processor.cpp node.cpp blocks.cpp -o exec", parId, sep = ""))
  print(paste("Process ", parId, ": Running code", sep = ""))
  system(paste("./exec", parId, " ", parId, sep = ""))
  system(paste("rm exec", parId, sep = ""))

  print(paste("Process ", parId, ": Reading code output", sep = ""))
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  if(configuration$BuildingBlocks == "1") {
    buildingBlocks <- getBuildingBlocksFromCodeOutput(nodeMap, fileNames)
  }
  print(paste("Process ", parId, ": Writing to output files", sep = ""))
  writeOutputToFiles(configuration, fibers, buildingBlocks, nodeMap, network, fileNames)
  setwd(currentDirectory)
}

main <- function() {
  fileNames <- getFileNames()
  configuration <- readConfigurationFile()

  runFiberCode(fileNames, configuration, 1)
}

#main()

