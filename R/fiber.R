library(tidyr)
library(dplyr)

setwd("/home/ian/Desktop/groupoid finding codes/fibers/R")
source("functions.R")

main <- function() {
  fileNames <- getFileNames()
  configuration <- readConfigurationFile()
  network <- readNetworkFile(configuration)
  nodeMap <- createNodeMap(network)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  }
  
  connectivity <- getTransformedConnectivity(configuration, network, nodeMap, weightMap)
  writeToAdjacencyFile(configuration, nodeMap, weightMap, connectivity, fileNames)
  
  codePreactions(fileNames)
  # TODO: make the key to recompile the code or not
  # TODO: properly check if code returned 1
  #system("g++ -std=c++11 main.cpp processor.cpp node.cpp blocks.cpp -o exec")
  system("./exec")
  
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  buildingBlocks <- getBuildingBlocksFromCodeOutput(nodeMap, fileNames)
  writeOutputToFiles(configuration, fibers, buildingBlocks, nodeMap, network, fileNames)
}

main()
