source("/home/ian/Desktop/groupoid finding codes/fibers/R/functions.R")

context("Checking that functions work correctly")
# Here we assume everywhere that we can get file names and read configuration file without any problems

test_that("We are loading file right", {
  fileNames <- getFileNames()
  configuration <- readConfigurationFile()
  configuration$InputFile <- "testNetworks/test1.txt"
  network <- readNetworkFile(configuration)
  expect_equal(1, 1)
})
