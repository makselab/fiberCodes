library(tidyr)
library(dplyr)
setwd("/home/ian/Desktop/groupoid finding codes/code")

readConfigurationFile <- function() {
  configuration <- read.delim(file = "fiberConfig.txt", header = F, stringsAsFactors = F)
  configuration <- configuration %>%
    separate(1, c("Parameter", "Value"), sep = ":[ \t]")
  configuration$Parameter <- gsub(" ", "", configuration$Parameter)
  configuration <- data.frame(t(configuration), stringsAsFactors = F)
  colnames(configuration) <- configuration[1, ]
  configuration <- configuration[-1, ]
  rownames(configuration) <- c()
  return(configuration)
}

getFileNames <- function() {
  fileNames <- read.delim("fileNames.txt", header = F)
  fileNames <- fileNames %>%
    separate(1, c("Type", "Path"), sep = ":[ \t]")
  fileNames$Type <- gsub(" ", "", fileNames$Type)
  fileNames <- data.frame(t(fileNames), stringsAsFactors = F)
  colnames(fileNames) <- fileNames[1, ]
  fileNames <- fileNames[-1, ]
  rownames(fileNames) <- c()
  return(fileNames)
}

# TODO: treat csv and not csv case properly
readNetworkFile <- function() {
  if(configuration$Weighted == "1") {
    numberOfColumns <- 3
  } else {
    numberOfColumns <- 2
  }
  
  rawInput <- read.delim(configuration$InputFile, header = F)
  network <- rawInput %>%
    separate(1, paste(c(1:numberOfColumns), sep = ", "), sep = "[ \t]")
  colnames(network)[1] <- "Source"
  colnames(network)[2] <- "Target"
  if(configuration$Weighted == "1") {
    colnames(network)[3] <- "Weight"
  }
  return(network)
}

# TODO: treat properly the case of having same names in a different case i.e. araC and AraC
createNodeMap <- function() {
  uniqueSource <- data.frame(unique(network$Source), stringsAsFactors = F)
  uniqueTarget <- data.frame(unique(network$Target), stringsAsFactors = F)
  colnames(uniqueSource)[1] <- "Name"
  colnames(uniqueTarget)[1] <- "Name"
  uniqueNodes <- rbind(uniqueSource, uniqueTarget)
  uniqueNodes <- data.frame(unique(uniqueNodes), stringsAsFactors = F)
  nodeMap <- uniqueNodes %>%
    arrange(Name) %>%
    mutate(Id = row_number(Name) - 1)
  return(nodeMap)
}

createWeightMap <- function() {
  weightMap <- data.frame(unique(network$Weight), stringsAsFactors = F)
  colnames(weightMap)[1] <- "Name"
  weightMap <- weightMap %>%
    arrange(Name) %>%
    mutate(Id = row_number(Name) - 1)
  return(weightMap)
}

getNodeIdByName <- function(nodeName) {
  return(nodeMap[grep(paste("^", nodeName, "$", sep = ""), nodeMap$Name), 2])
}

getWeightIdByName <- function(weightName) {
  return(nodeMap[grep(paste("^", weightName, "$", sep = ""), weightMap$Name), 2])
}

getTransformedConnectivity <- function(network) {
  connectivity <- network
  for(i in 1:nrow(connectivity)) {
    connectivity[i, 1] <- getNodeIdByName(connectivity[i, 1])
    connectivity[i, 2] <- getNodeIdByName(connectivity[i, 2])
    if(configuration$Weighted == "1") {
      connectivity[i, 3] <- getWeightIdByName(connectivity[i, 3])
    }
  }
  return(connectivity)
}

writeToAdjacencyFile <- function() {
  # adjacency.txt structure
  # 1: size in cells
  # 2: directed/undirected
  # 3: weighted/not weighted
  # 4: number of weights
  # 5..inf: adjacency matrix
  write(nrow(nodeMap), file = fileNames$AdjacencyFile, append = F)
  write(configuration$Directed, file = fileNames$AdjacencyFile, append = T)
  write(configuration$Weighted, file = fileNames$AdjacencyFile, append = T)
  write(nrow(weightMap), file = fileNames$AdjacencyFile, append = T)
  write.table(connectivity, file = fileNames$AdjacencyFile, col.names = F, row.names = F, quote = F, sep = "\t", append = T)
}

getFibersFromCodeOutput <- function() {
  fibers <- read.delim(fileNames$FiberFile, header = F)
  colnames(fibers)[1] <- "Id"
  colnames(fibers)[2] <- "FiberId"
  fibers <- cbind(nodeMap, fibers)[, c(-2, -3)]
  fibers <- arrange(fibers, FiberId)
  fibers <- fibers[, c(2, 1)]
  fibers <- fibers %>%
    group_by(FiberId) %>%
    summarise(Nodes = paste(Name, collapse = ", "))
  return(fibers)
}

fileNames <- getFileNames()
configuration <- readConfigurationFile()
network <- readNetworkFile()
nodeMap <- createNodeMap()
if(configuration$Weighted == "1") {
  weightMap <- createWeightMap()
}
connectivity <- getTransformedConnectivity(network)
writeToAdjacencyFile()
# TODO: make the key to recompile code or not
# TODO: properly check if code returned 1
#system("g++ -std=c++11 main.cpp processor.cpp node.cpp blocks.cpp -o exec")
#system("./exec")
fibers <- getFibersFromCodeOutput()
fibers
