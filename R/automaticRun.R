library(tidyr)
library(dplyr)
library(igraph)
library(foreach)
library(doParallel)

source("~/Dropbox/groupoid_finding_codes/fibers/R/fiberPar.R")
source("~/Dropbox/groupoid_finding_codes/fibers/R/classifier.R")

checkDatasets <- function(files, outputFolder) {
  newDatasets <- gsub(".txt", "", files)
  if(file.exists(paste(outputFolder, "summaryTableNL", sep = "/"))) {
    runHistory <- read.table(paste(outputFolder, "summaryTableNL", sep = "/"), sep = "\t", header = T, check.names = F, stringsAsFactors = F)
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

datasets <- read.csv(paste(outputFolder, "datasets.csv", sep = "/"), stringsAsFactors = F)
datasets <- datasets[!is.na(datasets$Directed), ]
files <- files[files %in% datasets$File.name]

if(length(files) != 0) {
  myCluster <- makeCluster(detectCores() - 1, outfile = "")
  registerDoParallel(myCluster)

  pb <- txtProgressBar(0, length(files), style = 3)

  start.time <- Sys.time()

  fileParameters <- foreach(f = 1:length(files), .combine = 'rbind') %dopar% {
    library(tidyr)
    library(dplyr)
    library(igraph)
    rawFileData <- read.delim(paste(folderToRun, "/", files[f], sep = ""), header = F, sep = " ", quote = "", stringsAsFactors = F, colClasses = "character")
    numberOfEdges <- nrow(rawFileData)
    numberOfNodes <- length(unique(c(rawFileData$V1, rawFileData$V2)))
    if(numberOfNodes > 30000) {
      notRun = T
      print(paste("File ", files[f], "has", numberOfNodes, "nodes which is more than 30000, not running to avoid RAM overflow"))
    }
    if(ncol(rawFileData) == 2) {
      weighted = 0
      notRun = F
    } else {
      weighted = 1
      notRun = F
      if(length(unique(rawFileData$V3)) > 10) {
        notRun = T
        print(paste("File ", files[f], "has", length(unique(rawFileData$V3)), "which is more than 10 unique weights, please consider running as unweighted to avoid RAM overflow"))
      }
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

    runCode <- !file.exists(paste(outputFolder, "/", gsub(".txt","", files[f]), ".txt", sep = "")) & !notRun
    datasets <- read.csv(paste(outputFolder, "datasets.csv", sep = "/"), stringsAsFactors = F)
    directed <- datasets$Directed[grep(paste("^", files[f], "$", sep = ""), datasets$File.name)]
    datasets[grep(paste("^", files[f], "$", sep = ""), datasets$File.name), ]
    if(directed == 1) {
      buildingBlocksNeeded = 1
    } else {
      buildingBlocksNeeded = 0
    }
    setTxtProgressBar(pb, f/3)
    c(numberOfNodes, numberOfEdges, weighted, numberOfSCC, biggestSCC, runCode, directed, buildingBlocksNeeded)
  }

  foreach(f = 1:length(files)) %dopar% {
    if(fileParameters[f, 6] == F) {return(0)}
    print(paste("Running ", files[f], " file"))
    library(tidyr)
    library(dplyr)
    library(igraph)

    fileNames <- getFileNames(f)
    configuration <- createConfiguration(fileParameters[f, 7], fileParameters[f, 3], fileParameters[f, 8],
                                         paste(folderToRun, "/", files[f], sep = ""),
                                         paste(outputFolder, "/", files[f], sep = ""))

    runFiberCode(fileNames, configuration, f)
    setTxtProgressBar(pb, length(files) / 3 + f/3)
    return(1)
  }

  setTxtProgressBar(pb, 2 * length(files) / 3)

  blockData <- foreach(f = 1:length(files), .combine = 'rbind', .errorhandling = "remove") %dopar% {
    print(paste("Classifing ", files[f], " file"))
    library(tidyr)
    library(dplyr)
    outputPrefix <- gsub(".txt", "", paste(outputFolder, "/", files[f], sep = ""))
    if(fileParameters[f, 8] == 0) {
      blocks <- NULL
    } else {
      blocks <- getBlocks(outputPrefix)
    }
    if(!is.null(blocks)) {
      runSummary <- blocks %>%
        group_by(nl) %>%
        summarise(n = n()) %>%
        mutate(Dataset = gsub(".txt", "", files[f]))
      numberOfBlocks <- nrow(blocks)
      runSummary <- rbind(runSummary, c("Number of blocks", numberOfBlocks, gsub(".txt", "", files[f])))
    } else {
      runSummary <- data.frame(matrix(vector(), nrow = 1, ncol = 3,
                                      dimnames = list(c(), c("nl", "n", "Dataset"))),
                               stringsAsFactors = F)
      nodes <- read.csv(paste(outputPrefix, "_nodes.csv", sep = ""))
      nodes <- nodes %>%
        group_by(FiberId) %>%
        summarise(n = n()) %>%
        filter(n > 1) %>%
        summarise(n())
      runSummary <- rbind(runSummary, c("Number of blocks", max(nodes), gsub(".txt", "", files[f])))
    }
    nodes <- read.csv(paste(outputPrefix, "_nodes.csv", sep = ""))
    nodes <- nodes %>%
      group_by(FiberId) %>%
      summarise(n = n()) %>%
      filter(n > 1)
    runSummary <- rbind(runSummary, c("Number of nodes in fibers", sum(nodes$n), gsub(".txt", "", files[f])))
    runSummary <- rbind(runSummary, c("Number of nodes", fileParameters[f, 1], gsub(".txt", "", files[f])))
    runSummary <- rbind(runSummary, c("Number of edges", fileParameters[f, 2], gsub(".txt", "", files[f])))
    runSummary <- rbind(runSummary, c("Number of SCC", fileParameters[f, 4], gsub(".txt", "", files[f])))
    runSummary <- rbind(runSummary, c("Size of biggest SCC", fileParameters[f, 5], gsub(".txt", "", files[f])))
    runSummary <- runSummary[!is.na(runSummary$nl), ]
    setTxtProgressBar(pb, length(files) * 2 / 3 + f/3)
    runSummary
  }

  now.time <- Sys.time()
  stopCluster(myCluster)
  time.taken <- now.time - start.time
  print(time.taken)

  # we are reading summary history here to make the table of all datasets
  if(file.exists(paste(outputFolder, "summaryTableNL", sep = "/"))) {
    summaryHistory <- read.table(paste(outputFolder, "summaryTableNL", sep = "/"), sep = "\t", header = T, check.names = F, stringsAsFactors = F)
    summaryHistory <- summaryHistory %>%
      gather(key = nl, value = n, -1) %>%
      select(2, 3, 1)

    blockData <- rbind(blockData, summaryHistory)
    blockData <- blockData[!duplicated(blockData[, c(1, 3)]), ]
  }

  summaryTable <- blockData %>%
    spread(nl, n) %>%
    arrange(Dataset)

  summaryTable[is.na(summaryTable)] <- "0"

  summaryTable[, 2:ncol(summaryTable)] <- sapply(summaryTable[, 2:ncol(summaryTable)], function(x) as.integer(x))
  write.table(summaryTable, paste(outputFolder, "/summaryTableNL", sep = "/"), row.names = F, quote = F, sep = "\t")
  write.table(summaryTable, "~/Dropbox/Research/PhD work/shared folders/FIBRATIONS/IAN-NETWORKS/summaryTableNL", row.names = F, quote = F, sep = "\t")
} else {
  print("No new datasets, please see summary history file for history")
}

#summaryTable <- read.table(paste(outputFolder, "summaryTableNL", sep = "/"), sep = "\t", header = T, check.names = F, stringsAsFactors = F)
ds <- read.csv(paste(outputFolder, "datasets.csv", sep = "/"), stringsAsFactors = F)
ds$File.name <- gsub(".txt$", "", ds$File.name)
colnames(ds)[1] <- "Dataset"
summary <- merge(ds, summaryTable, by="Dataset")
summary <- summary[, c(1, 3, 6, 114, 113, 112, 115, 116, 117, 12:111, 11, 10, 118)]

write.table(summary, "~/Dropbox/Research/PhD work/shared folders/FIBRATIONS/IAN-NETWORKS/summaryNL", row.names = F, quote = F, sep = "\t")
