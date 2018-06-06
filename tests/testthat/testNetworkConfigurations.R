source("/home/ian/Desktop/groupoid_finding_codes/fibers/R/functions.R")

context("Check outputs for test networks")

getTestFileNames <- function() {
  columnNames <- c("AdjacencyFile", "FiberFile", "BuildingBlocksFile")
  fileNames <- data.frame(matrix(vector(), nrow = 1, ncol = length(columnNames), dimnames = list(c(), columnNames)), stringsAsFactors = F)
  fileNames$AdjacencyFile <- "/home/ian/Desktop/groupoid_finding_codes/fibers/R/adjacency.txt"
  fileNames$FiberFile <- "/home/ian/Desktop/groupoid_finding_codes/fibers/R/fibers.txt"
  fileNames$BuildingBlocksFile <- "/home/ian/Desktop/groupoid_finding_codes/fibers/R/buildingBlocks.txt"
  return(fileNames)
}

getTestConfiguration <- function(directed, weighted, testNetworkId) {
  columnNames <- c("Directed", "Weighted", "CSV", "Gephi", "InputFile", "OutputFile")
  configuration <- data.frame(matrix(vector(), nrow = 1, ncol = length(columnNames), dimnames = list(c(), columnNames)), stringsAsFactors = F)
  configuration$Directed <- as.character(directed)
  configuration$Weighted <- as.character(weighted)
  configuration$InputFile <- paste("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/testNetworks/test", testNetworkId, ".txt", sep = "")
  configuration$OutputFile <- paste("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/testNetworks/out", testNetworkId, ".txt", sep = "")
  return(configuration)
}

test_that("Check fibers of undirected unweighted network", {
  configuration <- getTestConfiguration(0, 0, 1)

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
  system("g++ -std=c++11 /home/ian/Desktop/groupoid_finding_codes/fibers/R/main.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/processor.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/node.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/blocks.cpp -o /home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  system("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  expect_match(as.character(fibers[1, 2]), "a, c, g, i")
  expect_match(as.character(fibers[2, 2]), "b, d, f, h")
  expect_match(as.character(fibers[3, 2]), "e")
})

test_that("Check fibers of undirected weighted network", {
  configuration <- getTestConfiguration(0, 1, 2)

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
  system("g++ -std=c++11 /home/ian/Desktop/groupoid_finding_codes/fibers/R/main.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/processor.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/node.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/blocks.cpp -o /home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  system("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  expect_match(as.character(fibers[1, 2]), "a, c, g, i")
  expect_match(as.character(fibers[2, 2]), "b, d, f, h")
  expect_match(as.character(fibers[3, 2]), "e")
})

test_that("Check fibers of directed unweighted network", {
  configuration <- getTestConfiguration(1, 0, 3)

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
  system("g++ -std=c++11 /home/ian/Desktop/groupoid_finding_codes/fibers/R/main.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/processor.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/node.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/blocks.cpp -o /home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  system("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  expect_match(as.character(fibers[1, 2]), "a, b, c")
})

test_that("Check fibers of directed weighted network", {
  configuration <- getTestConfiguration(1, 1, 4)

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
  system("g++ -std=c++11 /home/ian/Desktop/groupoid_finding_codes/fibers/R/main.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/processor.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/node.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/blocks.cpp -o /home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  system("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  expect_match(as.character(fibers[1, 2]), "1, 2, 3")
  expect_match(as.character(fibers[2, 2]), "4, 5")
  expect_match(as.character(fibers[3, 2]), "6")
})

test_that("Check fibers in more complicated undirected weighted network", {
  configuration <- getTestConfiguration(0, 1, 5)

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
  system("g++ -std=c++11 /home/ian/Desktop/groupoid_finding_codes/fibers/R/main.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/processor.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/node.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/blocks.cpp -o /home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  system("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  expect_match(as.character(fibers[1, 2]), "a, b, g, h")
  expect_match(as.character(fibers[2, 2]), "c, f")
  expect_match(as.character(fibers[3, 2]), "d, e")
})

test_that("Check fibers in more complicated directed weighted network with slaves", {
  configuration <- getTestConfiguration(1, 1, 6)

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
  system("g++ -std=c++11 /home/ian/Desktop/groupoid_finding_codes/fibers/R/main.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/processor.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/node.cpp /home/ian/Desktop/groupoid_finding_codes/fibers/R/blocks.cpp -o /home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  system("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  expect_match(as.character(fibers[1, 2]), "3")
  expect_match(as.character(fibers[2, 2]), "1, 2")
  expect_match(as.character(fibers[3, 2]), "10, 11, 12, 13, 15, 16, 17")
  expect_match(as.character(fibers[4, 2]), "14")
  expect_match(as.character(fibers[5, 2]), "18, 19")
  expect_match(as.character(fibers[6, 2]), "20, 21, 22, 23, 24")
  expect_match(as.character(fibers[7, 2]), "4, 5")
  expect_match(as.character(fibers[8, 2]), "6, 7")
  expect_match(as.character(fibers[9, 2]), "8, 9")
})
