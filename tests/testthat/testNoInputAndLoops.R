library(tidyr)
library(dplyr)
source("/home/ian/Desktop/groupoid_finding_codes/fibers/R/functions.R")

context("Check that code works correctly on networks with nodes without inputs or with the loop")

getTestFileNames <- function() {
  columnNames <- c("AdjacencyFile", "FiberFile", "BuildingBlocksFile")
  fileNames <- data.frame(matrix(vector(), nrow = 1, ncol = length(columnNames), dimnames = list(c(), columnNames)), stringsAsFactors = F)
  fileNames$AdjacencyFile <- "/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/adjacency.txt"
  fileNames$FiberFile <- "/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/fibers.txt"
  fileNames$BuildingBlocksFile <- "/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/buildingBlocks.txt"
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

test_that("Two trees with the same leaves. One head has loop, other one doesn't", {
  fileNames <- getTestFileNames()
  configuration <- getTestConfiguration(1, 1, 7)

  network <- readNetworkFile(configuration)
  nodeMap <- createNodeMap(network)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  }
  connectivity <- getTransformedConnectivity(configuration, network, nodeMap, weightMap)
  writeToAdjacencyFile(configuration, nodeMap, weightMap, connectivity, fileNames)

  codePreactions(fileNames)
  system("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  expect_match(as.character(fibers[1, 2]), "8")
  expect_match(as.character(fibers[2, 2]), "1, 2, 3")
  expect_match(as.character(fibers[3, 2]), "4, 5")
  expect_match(as.character(fibers[4, 2]), "6, 7, 9")
})

test_that("Network with 2 not connected synchronized stars. Checking that they are in a different fiber", {
  fileNames <- getTestFileNames()
  configuration <- getTestConfiguration(1, 1, 8)

  network <- readNetworkFile(configuration)
  nodeMap <- createNodeMap(network)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  }
  connectivity <- getTransformedConnectivity(configuration, network, nodeMap, weightMap)
  writeToAdjacencyFile(configuration, nodeMap, weightMap, connectivity, fileNames)

  codePreactions(fileNames)
  system("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  expect_match(as.character(fibers[1, 2]), "0, 1, 2, 3")
  expect_match(as.character(fibers[2, 2]), "4, 5, 6, 7")
})


test_that("This test reminds us that there is a bug with 2 2-BTFs shown as being synchronous", {
  fileNames <- getTestFileNames()
  configuration <- getTestConfiguration(1, 0, 9)

  network <- readNetworkFile(configuration)
  nodeMap <- createNodeMap(network)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  }
  connectivity <- getTransformedConnectivity(configuration, network, nodeMap, weightMap)
  writeToAdjacencyFile(configuration, nodeMap, weightMap, connectivity, fileNames)

  codePreactions(fileNames)
  system("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  expect_match(as.character(fibers[1, 2]), "0, 1")
  expect_match(as.character(fibers[2, 2]), "2, 3")
})

test_that("This test reminds us that there is a bug with 2 3-BTFs shown as being synchronous", {
  fileNames <- getTestFileNames()
  configuration <- getTestConfiguration(1, 0, 10)

  network <- readNetworkFile(configuration)
  nodeMap <- createNodeMap(network)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  }
  connectivity <- getTransformedConnectivity(configuration, network, nodeMap, weightMap)
  writeToAdjacencyFile(configuration, nodeMap, weightMap, connectivity, fileNames)

  codePreactions(fileNames)
  system("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  expect_match(as.character(fibers[1, 2]), "0, 1, 2")
  expect_match(as.character(fibers[2, 2]), "3, 4, 5")
})


test_that("This test reminds us that there is a bug with 2 CF shown as being synchronous", {
  fileNames <- getTestFileNames()
  configuration <- getTestConfiguration(1, 0, 11)

  network <- readNetworkFile(configuration)
  nodeMap <- createNodeMap(network)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  }
  connectivity <- getTransformedConnectivity(configuration, network, nodeMap, weightMap)
  writeToAdjacencyFile(configuration, nodeMap, weightMap, connectivity, fileNames)

  codePreactions(fileNames)
  system("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/exec")
  nodeMap <- getFibersFromCodeOutput(nodeMap, fileNames)
  fibers <- prepareFibersOutput(nodeMap)
  expect_match(as.character(fibers[1, 2]), "0, 1")
  expect_match(as.character(fibers[2, 2]), "2, 3")
})
