library(tidyr)
library(dplyr)
source("/home/ian/Desktop/groupoid_finding_codes/fibers/R/functions.R")

context("Checking that we read network files properly")

getTestConfiguration <- function(directed, weighted, testNetworkId) {
  columnNames <- c("Directed", "Weighted", "CSV", "Gephi", "InputFile", "OutputFile")
  configuration <- data.frame(matrix(vector(), nrow = 1, ncol = length(columnNames), dimnames = list(c(), columnNames)), stringsAsFactors = F)
  configuration$Directed <- as.character(directed)
  configuration$Weighted <- as.character(weighted)
  configuration$InputFile <- paste("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/testNetworks/test", testNetworkId, ".txt", sep = "")
  configuration$OutputFile <- paste("/home/ian/Desktop/groupoid_finding_codes/fibers/tests/testthat/testNetworks/out", testNetworkId, ".txt", sep = "")
  return(configuration)
}

test_that("Check loading of undirected unweighted network", {
  configuration <- getTestConfiguration(0, 0, 1)

  network <- readNetworkFile(configuration)
  expect_match(network[1, 1], "a")
  expect_match(network[2, 1], "a")
  expect_match(network[3, 1], "b")
  expect_match(network[4, 1], "b")
  expect_match(network[1, 2], "b")
  expect_match(network[2, 2], "d")
  expect_match(network[3, 2], "c")
  expect_match(network[4, 2], "e")
})

test_that("Check loading of undirected weighted network", {
  configuration <- getTestConfiguration(0, 1, 2)

  network <- readNetworkFile(configuration)
  expect_match(network[1, 1], "a")
  expect_match(network[2, 1], "a")
  expect_match(network[3, 1], "b")
  expect_match(network[4, 1], "b")
  expect_match(network[1, 2], "b")
  expect_match(network[2, 2], "d")
  expect_match(network[3, 2], "c")
  expect_match(network[4, 2], "e")
  expect_match(network[1, 3], "activator")
  expect_match(network[2, 3], "activator")
  expect_match(network[3, 3], "activator")
  expect_match(network[4, 3], "repressor")
})

test_that("Check loading of directed unweighted network", {
  configuration <- getTestConfiguration(1, 0, 3)

  network <- readNetworkFile(configuration)
  expect_match(network[1, 1], "b")
  expect_match(network[2, 1], "b")
  expect_match(network[3, 1], "c")
  expect_match(network[1, 2], "a")
  expect_match(network[2, 2], "b")
  expect_match(network[3, 2], "a")
})

test_that("Check loading of directed weighted network", {
  configuration <- getTestConfiguration(1, 1, 4)

  network <- readNetworkFile(configuration)
  expect_match(network[3, 1], "2")
  expect_match(network[4, 1], "2")
  expect_match(network[5, 1], "3")
  expect_match(network[6, 1], "3")
  expect_match(network[3, 2], "3")
  expect_match(network[4, 2], "5")
  expect_match(network[5, 2], "1")
  expect_match(network[6, 2], "6")
  expect_match(network[3, 3], "activator")
  expect_match(network[4, 3], "repressor")
  expect_match(network[5, 3], "activator")
  expect_match(network[6, 3], "dual")
})
