library(tidyr)
library(dplyr)

setwd("/home/ian/Desktop/groupoid_finding_codes/fibers/R")
source("functions.R")

main <- function() {
  print("Reading configuration...")
  fileNames <- getFileNames()
  configuration <- readConfigurationFile()
  print("Reading network...")
  network <- readNetworkFile(configuration)

  print("Creating network maps...")
  nodeMap <- createNodeMap(network)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  }

  print("Transforming connectivity...")
  connectivity <- getTransformedConnectivity(configuration, network, nodeMap, weightMap)
  writeToAdjacencyFile(configuration, nodeMap, weightMap, connectivity, fileNames)

  codePreactions(fileNames)
  print("Running fibration finding code...")
  # TODO: make the key to recompile the code or not
  # TODO: properly check if code returned 1
  #system("g++ -std=c++11 main.cpp processor.cpp node.cpp blocks.cpp -o exec")
  system("./exec")

  print("Reading fibration code output...")
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  buildingBlocks <- getBuildingBlocksFromCodeOutput(nodeMap, fileNames)
  print("Printing output to output files...")
  writeOutputToFiles(configuration, fibers, buildingBlocks, nodeMap, network, fileNames)
}

main()
