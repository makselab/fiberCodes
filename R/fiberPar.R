library(tidyr)
library(dplyr)
source("~/Dropbox/groupoid_finding_codes/fibers/R/functionsPar.R")

runFiberCode <- function(fileNames, configuration, parId) {
  currentDirectory <- getwd()
  setwd("~/Dropbox/groupoid_finding_codes/fibers/R")
  network <- readNetworkFile(configuration)

  nodeMap <- createNodeMap(network)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  } else {
    weightMap <- NULL
  }

  connectivity <- getTransformedConnectivity(configuration, network, nodeMap, weightMap)
  writeToAdjacencyFile(configuration, nodeMap, weightMap, connectivity, fileNames)

  codePreactions(fileNames)
  # TODO: make the key to recompile the code or not
  # TODO: properly check if code returned 1
  system(paste("g++ -std=c++11 main.cpp processor.cpp node.cpp blocks.cpp -o exec", parId, sep = ""))
  system(paste("./exec", parId, " ", parId, sep = ""))
  system(paste("rm exec", parId, sep = ""))

  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  if(configuration$BuildingBlocks == "1") {
    buildingBlocks <- getBuildingBlocksFromCodeOutput(nodeMap, fileNames)
  }
  writeOutputToFiles(configuration, fibers, buildingBlocks, nodeMap, network, fileNames)
  setwd(currentDirectory)
}

main <- function() {
  fileNames <- getFileNames()
  configuration <- readConfigurationFile()

  runFiberCode(fileNames, configuration)
}

#main()

