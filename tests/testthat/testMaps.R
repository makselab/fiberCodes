source("/home/ian/Desktop/groupoid finding codes/fibers/R/functions.R")

context("Checking that the node and weight maps are created properly")

getTestConfiguration <- function(directed, weighted, testNetworkId) {
  columnNames <- c("Directed", "Weighted", "CSV", "Gephi", "InputFile", "OutputFile")
  configuration <- data.frame(matrix(vector(), nrow = 1, ncol = length(columnNames), dimnames = list(c(), columnNames)), stringsAsFactors = F)
  configuration$Directed <- as.character(directed)
  configuration$Weighted <- as.character(weighted)
  configuration$InputFile <- paste("/home/ian/Desktop/groupoid finding codes/fibers/tests/testthat/testNetworks/test", testNetworkId, ".txt", sep = "")
  configuration$OutputFile <- paste("/home/ian/Desktop/groupoid finding codes/fibers/tests/testthat/testNetworks/out", testNetworkId, ".txt", sep = "")
  return(configuration)
}

test_that("Check maps of undirected unweighted network", {
  configuration <- getTestConfiguration(0, 0, 1)
  
  network <- readNetworkFile(configuration)
  nodeMap <- createNodeMap(network)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  }
  
  expect_match(nodeMap[1, 1], "a")
  expect_match(nodeMap[2, 1], "b")
  expect_match(nodeMap[3, 1], "c")
  expect_match(nodeMap[4, 1], "d")
  expect_match(nodeMap[5, 1], "e")
  expect_match(nodeMap[6, 1], "f")
  expect_match(nodeMap[7, 1], "g")
  expect_match(nodeMap[8, 1], "h")
  expect_match(nodeMap[9, 1], "i")
})

test_that("Check maps of undirected weighted network", {
  configuration <- getTestConfiguration(0, 1, 2)
  
  network <- readNetworkFile(configuration)
  nodeMap <- createNodeMap(network)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  }
  expect_match(nodeMap[1, 1], "a")
  expect_match(nodeMap[2, 1], "b")
  expect_match(nodeMap[3, 1], "c")
  expect_match(nodeMap[4, 1], "d")
  expect_match(nodeMap[5, 1], "e")
  expect_match(nodeMap[6, 1], "f")
  expect_match(nodeMap[7, 1], "g")
  expect_match(nodeMap[8, 1], "h")
  expect_match(nodeMap[9, 1], "i")
  expect_match(weightMap[1, 1], "activator")
  expect_match(weightMap[2, 1], "repressor")
})

test_that("Check maps of directed unweighted network", {
  configuration <- getTestConfiguration(1, 0, 3)
  
  network <- readNetworkFile(configuration)
  nodeMap <- createNodeMap(network)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  }
  
  expect_match(nodeMap[1, 1], "a")
  expect_match(nodeMap[2, 1], "b")
  expect_match(nodeMap[3, 1], "c")
})

test_that("Check maps of directed weighted network", {
  configuration <- getTestConfiguration(1, 1, 4)
  
  network <- readNetworkFile(configuration)
  nodeMap <- createNodeMap(network)
  if(configuration$Weighted == "1") {
    weightMap <- createWeightMap(network)
  }
  
  expect_match(nodeMap[1, 1], "1")
  expect_match(nodeMap[2, 1], "2")
  expect_match(nodeMap[3, 1], "3")
  expect_match(nodeMap[4, 1], "4")
  expect_match(nodeMap[5, 1], "5")
  expect_match(nodeMap[6, 1], "6")
  expect_match(weightMap[1, 1], "activator")
  expect_match(weightMap[2, 1], "dual")
  expect_match(weightMap[3, 1], "repressor")
})
