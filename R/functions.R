getFileNames <- function() {
  fileNames <- read.delim("/home/ian/Desktop/groupoid_finding_codes/fibers/R/fileNames.txt", header = F)
  fileNames <- fileNames %>%
    separate(1, c("Type", "Path"), sep = ":[ \t]")
  fileNames$Type <- gsub(" ", "", fileNames$Type)
  fileNames <- data.frame(t(fileNames), stringsAsFactors = F)
  colnames(fileNames) <- fileNames[1, ]
  fileNames <- fileNames[-1, ]
  rownames(fileNames) <- c()
  return(fileNames)
}

readConfigurationFile <- function() {
  configuration <- read.delim(file = "/home/ian/Desktop/groupoid_finding_codes/fibers/R/fiberConfig.txt", header = F, stringsAsFactors = F)
  configuration <- configuration %>%
    separate(1, c("Parameter", "Value"), sep = ":[ \t]")
  configuration$Parameter <- gsub(" ", "", configuration$Parameter)
  configuration <- data.frame(t(configuration), stringsAsFactors = F)
  colnames(configuration) <- configuration[1, ]
  configuration <- configuration[-1, ]
  rownames(configuration) <- c()
  return(configuration)
}

# TODO: treat csv and not csv case properly
readNetworkFile <- function(configuration) {
  if(configuration$Weighted == "1") {
    numberOfColumns <- 3
  } else {
    numberOfColumns <- 2
  }

  rawInput <- read.delim(configuration$InputFile, header = F, sep = "\n")
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
createNodeMap <- function(network) {
  uniqueSource <- data.frame(unique(network$Source), stringsAsFactors = F)
  uniqueTarget <- data.frame(unique(network$Target), stringsAsFactors = F)
  colnames(uniqueSource)[1] <- "Label"
  colnames(uniqueTarget)[1] <- "Label"
  uniqueNodes <- rbind(uniqueSource, uniqueTarget)
  uniqueNodes <- data.frame(unique(uniqueNodes), stringsAsFactors = F)
  nodeMap <- uniqueNodes %>%
    arrange(Label) %>%
    mutate(Id = row_number(Label) - 1)
  return(nodeMap)
}

createWeightMap <- function(network) {
  weightMap <- data.frame(unique(network$Weight), stringsAsFactors = F)
  colnames(weightMap)[1] <- "Name"
  weightMap <- weightMap %>%
    arrange(Name) %>%
    mutate(Id = row_number(Name) - 1)
  return(weightMap)
}

getNodeIdByLabel <- function(nodeLabel, nodeMap) {
  return(nodeMap[grep(paste("^", nodeLabel, "$", sep = ""), nodeMap$Label), "Id"])
}

getNodeLabelById <- function(id, nodeMap) {
  return(nodeMap[grep(paste("^", id, "$", sep = ""), nodeMap$Id), "Label"])
}

getWeightIdByName <- function(weightName, weightMap) {
  return(weightMap[grep(paste("^", weightName, "$", sep = ""), weightMap$Name), 2])
}

isNodeInBlockByLabel <- function(label, block) {
  return(grep(paste("[ ^]", label, "[$,]", sep = ""), block))
}

getTransformedConnectivity <- function(configuration, network, nodeMap, weightMap) {
  connectivity <- network
  for(i in 1:nrow(connectivity)) {
    connectivity[i, 1] <- getNodeIdByLabel(connectivity[i, 1], nodeMap)
    connectivity[i, 2] <- getNodeIdByLabel(connectivity[i, 2], nodeMap)
    if(configuration$Weighted == "1") {
      connectivity[i, 3] <- getWeightIdByName(connectivity[i, 3], weightMap)
    }
    if((i %% 10000) == 0) {print(paste("Transformed ", i, "/", nrow(connectivity), " connectivity lines", sep = ""))}
  }
  return(connectivity)
}

writeToAdjacencyFile <- function(configuration, nodeMap, weightMap, connectivity, fileNames) {
  # adjacency.txt structure
  # 1: size in cells
  # 2: directed/undirected
  # 3: weighted/not weighted
  # 4: number of weights
  # 5..inf: adjacency matrix
  write(nrow(nodeMap), file = fileNames$AdjacencyFile, append = F)
  write(configuration$Directed, file = fileNames$AdjacencyFile, append = T)
  write(configuration$Weighted, file = fileNames$AdjacencyFile, append = T)
  if(configuration$Weighted == "1") {
    write(nrow(weightMap), file = fileNames$AdjacencyFile, append = T)
  } else {
    write(0, file = fileNames$AdjacencyFile, append = T)
  }
  write.table(connectivity, file = fileNames$AdjacencyFile, col.names = F, row.names = F, quote = F, sep = "\t", append = T)
}

codePreactions <- function(fileNames) {
  # clear fiber and building block files before running code
  if(file.exists(fileNames$BuildingBlocksFile)) {file.remove(fileNames$BuildingBlocksFile)}
  if(file.exists(fileNames$FiberFile)) {file.remove(fileNames$FiberFile)}
}

getFibersFromCodeOutput <- function(nodeMap, fileNames) {
  fibers <- read.delim(fileNames$FiberFile, header = F, sep = "\t")
  colnames(fibers)[1] <- "Id"
  colnames(fibers)[2] <- "FiberId"
  nodeMap <- cbind(nodeMap, fibers)[, c(2, 1, 4)]
  return(nodeMap)
}

prepareFibersOutput <- function(nodeMap) {
  fibers <- nodeMap[, -1]
  fibers <- arrange(fibers, FiberId)
  fibers <- fibers[, c(2, 1)]
  fibers <- fibers %>%
    group_by(FiberId) %>%
    summarise(Nodes = paste(Label, collapse = ", "))
  return(fibers)
}

getBuildingBlocksFromCodeOutput <- function(nodeMap, fileNames) {
  # we need nodeMap here to run getNodeLabelById to get real names for nodes from building block
  buildingBlocks <- read.delim(fileNames$BuildingBlocksFile, header = F, sep = "\n")
  buildingBlocks <- buildingBlocks %>%
    separate(1, c("Id", "Nodes"), sep = ":[ \t]")

  for(i in 1:nrow(buildingBlocks)) {
    block <- data.frame(strsplit(buildingBlocks$Nodes[i], ", "), stringsAsFactors = F)
    colnames(block)[1] <- "NodeId"
    for(j in 1:nrow(block)) {
      block$NodeName[j] <- getNodeLabelById(block$NodeId[j], nodeMap)
    }
    block <- block %>%
      select(2) %>%
      summarise(Nodes = paste(NodeName, collapse = ", "))
    buildingBlocks$Nodes[i] <- block$Nodes
  }
  return(buildingBlocks)
}

writeOutputToFiles <- function(configuration, fibers, buildingBlocks, nodeMap, network, fileNames) {
  configuration$BlocksOutputFile <- gsub(".txt", "_blocks.txt", configuration$OutputFile)
  configuration$NodesOutputFile <- gsub(".txt", "_nodes.csv", configuration$OutputFile)
  configuration$EdgesOutputFile <- gsub(".txt", "_edges.csv", configuration$OutputFile)
  write.table(fibers, file = configuration$OutputFile, quote = F, row.names = F, col.names = F, sep = ":\t")
  write.table(buildingBlocks, file = configuration$BlocksOutputFile, quote = F, row.names = F, col.names = F, sep = ":\t")

  csvNodeMap <- nodeMap
  csvNodeMap$Id <- csvNodeMap$Label
  write.csv(csvNodeMap, file = configuration$NodesOutputFile, quote = F, row.names = F)

  csvNetwork <- network
  if(configuration$Directed == "1") {
    csvNetwork$Type <- "directed"
  } else {
    csvNetwork$Type <- "undirected"
  }
  write.csv(csvNetwork, file = configuration$EdgesOutputFile, quote = F, row.names = F)
}
