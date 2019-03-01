library(tidyr)
library(dplyr)
library(igraph)
library(foreach)
library(doParallel)

source("~/Dropbox/groupoid_finding_codes/fibers/R/fiberPar.R")
source("~/Dropbox/groupoid_finding_codes/fibers/R/classifier.R")

checkDatasets <- function(files, outputFolder) {
  newDatasets <- gsub(".txt", "", files)
  if(file.exists(paste(outputFolder, "summaryTable", sep = "/"))) {
    runHistory <- read.table(paste(outputFolder, "summaryTable", sep = "/"), sep = "\t", header = T, check.names = F, stringsAsFactors = F)
    datasetHistory <- runHistory$Dataset
  } else {
    datasetHistory <- NULL
  }
  return(!newDatasets %in% datasetHistory)
}

getFileNames <- function(parId) {
  fileNames <- data.frame(matrix(c(paste("parallel/adjacency", parId, ".txt", sep = ""),
                                   paste("parallel/fibers", parId, ".txt", sep = ""),
                                   paste("parallel/buildingBlocks", parId, ".txt", sep = "")),
                                 nrow = 1, ncol = 3,
                                 dimnames = list(c(), c("AdjacencyFile", "FiberFile", "BuildingBlocksFile"))),
                          stringsAsFactors = F)
  return(fileNames)
}

createConfiguration <- function(directed, weighted, buildingBlocksNeeded, parInputFile, parOutputFile) {
  configuration <- data.frame(matrix(c(directed,
                                       weighted,
                                       buildingBlocksNeeded,
                                       parInputFile,
                                       parOutputFile),
                                     nrow = 1, ncol = 5,
                                     dimnames = list(c(), c("Directed", "Weighted", "BuildingBlocks", "InputFile", "OutputFile"))),
                              stringsAsFactors = F)
  return(configuration)
}

# this code is only good for unweighted graphs
folderToRun <- "~/Dropbox/groupoid_finding_codes/naturePhysRuns/automaticRunData"
outputFolder <- paste(folderToRun, "output", sep = "/")
fiberCodeFolder <- "~/Dropbox/groupoid_finding_codes/fibers/R"
files <- list.files(folderToRun, pattern = ".txt")
files <- files[checkDatasets(files, outputFolder)]

if(length(files) != 0) {
  directed <- 1
  if(directed == 1) {
    buildingBlocksNeeded = 1
  } else {
    buildingBlocksNeeded = 0
  }

  myCluster <- makeCluster(detectCores() - 1, outfile = "")
  registerDoParallel(myCluster)

  pb <- txtProgressBar(0, length(files), style = 3)

  start.time <- Sys.time()

  fileParameters <- foreach(f = 1:length(files), .combine = 'rbind') %dopar% {
    library(tidyr)
    library(dplyr)
    library(igraph)
    rawFileData <- read.delim(paste(folderToRun, "/", files[f], sep = ""), header = F, sep = " ", quote = "", stringsAsFactors = F)
    numberOfEdges <- nrow(rawFileData)
    numberOfNodes <- length(unique(c(rawFileData$V1, rawFileData$V2)))
    if(ncol(rawFileData) == 2) {
      weighted = 0
    } else {
      weighted = 1
    }

    network <- graph_from_edgelist(as.matrix(rawFileData[, 1:2]), directed = TRUE)
    nodes <- components(network, mode = "strong")$membership
    nodes <- as.data.frame(nodes)
    numberOfSCC <- max(nodes$nodes)
    nodes <- nodes %>%
      group_by(nodes) %>%
      summarise(n = n()) %>%
      filter(n > 1)
    biggestSCC <- max(nodes$n)
    if(biggestSCC < 1) {biggestSCC = 1}

    runCode <- !dir.exists(paste(outputFolder, "/", gsub(".txt","", files[f]), "buildingBlocks", sep = ""))
    setTxtProgressBar(pb, f/3)
    c(numberOfNodes, numberOfEdges, weighted, numberOfSCC, biggestSCC, runCode)
  }

  foreach(f = 1:length(files)) %dopar% {
    if(fileParameters[f, 6] == F) {return(0)}
    library(tidyr)
    library(dplyr)
    library(igraph)

    fileNames <- getFileNames(f)
    configuration <- createConfiguration(directed, fileParameters[f, 3], buildingBlocksNeeded,
                                         paste(folderToRun, "/", files[f], sep = ""),
                                         paste(outputFolder, "/", files[f], sep = ""))

    runFiberCode(fileNames, configuration, f)
    setTxtProgressBar(pb, length(files) / 3 + f/3)
    return(1)
  }

  setTxtProgressBar(pb, 2 * length(files) / 3)

  blockData <- foreach(f = 1:length(files), .combine = 'rbind') %dopar% {
    print(paste("Classifing ", files[f], " file"))
    library(tidyr)
    library(dplyr)
    outputPrefix <- gsub(".txt", "", paste(outputFolder, "/", files[f], sep = ""))
    blocks <- getBlocks(outputPrefix)
    if(!is.null(blocks)) {
      runSummary <- blocks %>%
        group_by(Class) %>%
        summarise(n = n()) %>%
        mutate(Dataset = gsub(".txt", "", files[f]))
      numberOfBlocks <- nrow(blocks)
      runSummary <- rbind(runSummary, c("Number of blocks", numberOfBlocks, gsub(".txt", "", files[f])))
    } else {
      runSummary <- data.frame(matrix(vector(), nrow = 1, ncol = 3,
                                      dimnames = list(c(), c("Class", "n", "Dataset"))),
                               stringsAsFactors = F)
      runSummary <- rbind(runSummary, c("Number of blocks", 0, gsub(".txt", "", files[f])))
    }
    runSummary <- rbind(runSummary, c("Number of nodes", fileParameters[f, 1], gsub(".txt", "", files[f])))
    runSummary <- rbind(runSummary, c("Number of edges", fileParameters[f, 2], gsub(".txt", "", files[f])))
    runSummary <- rbind(runSummary, c("Number of SCC", fileParameters[f, 4], gsub(".txt", "", files[f])))
    runSummary <- rbind(runSummary, c("Size of biggest SCC", fileParameters[f, 5], gsub(".txt", "", files[f])))
    runSummary <- runSummary[!is.na(runSummary$Class), ]
    setTxtProgressBar(pb, length(files) * 2 / 3 + f/3)
    runSummary
  }

  now.time <- Sys.time()
  stopCluster(myCluster)
  time.taken <- now.time - start.time
  print(time.taken)

  # we are reading summary history here to make the table of all datasets
  if(file.exists(paste(outputFolder, "summaryTable", sep = "/"))) {
    summaryHistory <- read.table(paste(outputFolder, "summaryTable", sep = "/"), sep = "\t", header = T, check.names = F, stringsAsFactors = F)
    summaryHistory <- summaryHistory %>%
      gather(key = Class, value = n, -1) %>%
      select(2, 3, 1)

    blockData <- rbind(blockData, summaryHistory)
    blockData <- blockData[!duplicated(blockData[, c(1, 3)]), ]
  }

  summaryTable <- blockData %>%
    spread(Class, n) %>%
    arrange(Dataset)

  summaryTable[is.na(summaryTable)] <- "0"

  summaryTable[, 2:ncol(summaryTable)] <- sapply(summaryTable[, 2:ncol(summaryTable)], function(x) as.integer(x))
  write.table(summaryTable, paste(outputFolder, "/summaryTable", sep = "/"), row.names = F, quote = F, sep = "\t")
  write.table(summaryTable, "~/Dropbox/Research/PhD work/shared folders/FIBRATIONS/IAN-NETWORKS/summaryTable", row.names = F, quote = F, sep = "\t")
} else {
  print("No new datasets, please see summary history file for history")
}
