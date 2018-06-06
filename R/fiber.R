library(tidyr)
library(dplyr)

setwd("/home/ian/Desktop/groupoid_finding_codes/fibers/R")
source("functions.R")

writeComment <- function(text, start.time) {
  print(text)
  now.time <- Sys.time()
  time.taken <- now.time - start.time
  print(time.taken)
}

main <- function() {
  start.time <- Sys.time()
  writeComment("Reading configuration...", start.time)
  fileNames <- getFileNames()
  configuration <- readConfigurationFile()
  writeComment("Reading network...", start.time)
  network <- readNetworkFile(configuration)

  writeComment("Creating network maps...", start.time)
  nodeMap <- createNodeMap(network)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  }

  writeComment("Transforming connectivity...", start.time)
  connectivity <- getTransformedConnectivity(configuration, network, nodeMap, weightMap)
  writeToAdjacencyFile(configuration, nodeMap, weightMap, connectivity, fileNames)

  codePreactions(fileNames)
  writeComment("Running fibration finding code...", start.time)
  # TODO: make the key to recompile the code or not
  # TODO: properly check if code returned 1
  #system("g++ -std=c++11 main.cpp processor.cpp node.cpp blocks.cpp -o exec")
  system("./exec")

  writeComment("Reading fibration code output...", start.time)
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  buildingBlocks <- getBuildingBlocksFromCodeOutput(nodeMap, fileNames)
  writeComment("Printing output to output files...", start.time)
  writeOutputToFiles(configuration, fibers, buildingBlocks, nodeMap, network, fileNames)
}

main()
