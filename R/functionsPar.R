library(tidyr)
library(dplyr)

# TODO: treat csv and not csv case properly
readNetworkFile <- function(configuration) {
  if(configuration$Weighted == "1") {
    numberOfColumns <- 3
  } else {
    numberOfColumns <- 2
  }

  rawInput <- read.delim(configuration$InputFile, header = F, sep = "\n", quote = "")
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
createNodeMap <- function(network, configuration) {
  graph <- graph_from_edgelist(as.matrix(network[, 1:2]), directed = configuration$Directed)
  nodeMap <- as.data.frame(vertex_attr(graph, "name"), stringsAsFactors = F)
  nodeMap$Id <- 1:nrow(nodeMap) - 1
  colnames(nodeMap)[1] <- "Label"

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

getNodeFiberIdByLabel <- function(nodeLabel, nodeMap) {
  return(nodeMap[grep(paste("^", nodeLabel, "$", sep = ""), nodeMap$Label), "FiberId"])
}

getNodeLabelById <- function(id, nodeMap) {
  return(nodeMap[grep(paste("^", id, "$", sep = ""), nodeMap$Id), "Label"])
}

getWeightIdByName <- function(weightName, weightMap) {
  return(weightMap[grep(paste("^", weightName, "$", sep = ""), weightMap$Name), 2])
}

getTransformedConnectivity <- function(configuration, network, nodeMap, weightMap) {
  connectivity <- network

  graph <- graph_from_edgelist(as.matrix(connectivity[, 1:2]), directed = configuration$Directed)
  connectivity <- as.data.frame(as_edgelist(graph, names = F), stringsAsFactors = F)
  if(configuration$Weighted == "1") {
    connectivity <- cbind(connectivity, network$Weight)
    connectivity <- connectivity %>%
      mutate(`network$Weight` = as.character(`network$Weight`))
    colnames(connectivity) = c("Source", "Target", "Weight")
  } else {
    colnames(connectivity) = c("Source", "Target")
  }
  connectivity[, 1:2] <- connectivity[, 1:2] - 1

  if(configuration$Weighted == "1") {
    for(i in 1:nrow(connectivity)) {
      connectivity[i, 3] <- getWeightIdByName(connectivity[i, 3], weightMap)
    }
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

  nodeMap$FiberId <- fibers$FiberId[nodeMap$Id + 1]
  nodeMap <- nodeMap[, c(2, 1, 3)]
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
  if(!file.exists(fileNames$BuildingBlocksFile)) {
    buildingBlocks <- data.frame(matrix(vector(), nrow = 0, ncol = 2, dimnames = list(c(), c("Id", "Nodes"))), stringsAsFactors = F)
    print("There are no building blocks")
    return(buildingBlocks)
  }
  buildingBlocks <- read.delim(fileNames$BuildingBlocksFile, header = F, sep = "\n")
  buildingBlocks <- buildingBlocks %>%
    separate(1, c("Id", "Nodes"), sep = ":[ \t]")

  for(i in 1:nrow(buildingBlocks)) {
    block <- data.frame(strsplit(buildingBlocks$Nodes[i], ", "), stringsAsFactors = F)
    colnames(block)[1] <- "NodeId"
    block$NodeName <- nodeMap[as.integer(block$NodeId) + 1, 2]
    block <- block %>%
      select(2) %>%
      summarise(Nodes = paste(NodeName, collapse = ", "))
    buildingBlocks$Nodes[i] <- block$Nodes
  }
  return(buildingBlocks)
}

writeOutputToFiles <- function(configuration, fibers, buildingBlocks, nodeMap, network, fileNames) {
  configuration$BlocksOutputFile <- gsub(".txt$", "_blocks.txt", configuration$OutputFile)
  configuration$NodesOutputFile <- gsub(".txt$", "_nodes.csv", configuration$OutputFile)
  configuration$EdgesOutputFile <- gsub(".txt$", "_edges.csv", configuration$OutputFile)
  write.table(fibers, file = configuration$OutputFile, quote = F, row.names = F, col.names = F, sep = ":\t")
  if(configuration$BuildingBlocks == "1") {
    write.table(buildingBlocks, file = configuration$BlocksOutputFile, quote = F, row.names = F, col.names = F, sep = ":\t")
  }

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
  if(configuration$BuildingBlocks == "1") {
    writeBuldingBlocksToFiles(configuration, buildingBlocks, nodeMap, csvNetwork)
  }
}

library(igraph)

writeBuldingBlocksToFiles <- function(configuration, buildingBlocks, nodeMap, csvNetwork) {
  if(nrow(buildingBlocks) == 0) {return()}
  configuration$OutputPath <- gsub(".[A-z]{3}$", "", configuration$OutputFile)
  configuration$OutputPath <- paste(configuration$OutputPath, "buildingBlocks", sep = "")
  system(paste("mkdir ", configuration$OutputPath, sep = ""))

  for(i in 1:nrow(buildingBlocks)) {
    # get nodes data
    block <- data.frame(strsplit(buildingBlocks$Nodes[i], ", "), stringsAsFactors = F)
    colnames(block)[1] <- "Id"
    block$Label <- block$Id
    for(j in 1:nrow(block)) {
      block$FiberId[j] <- getNodeFiberIdByLabel(block$Label[j], nodeMap)
    }
    # write to nodes file
    fileName <- paste(configuration$OutputPath, "/", buildingBlocks$Id[i], "_nodes.csv", sep = "")
    write.csv(block, file = fileName, quote = F, row.names = F)

    # get edges data
    columnNames <- colnames(csvNetwork)
    blockConnections <- data.frame(matrix(vector(), nrow = 0, ncol = length(columnNames), dimnames = list(c(), columnNames)), stringsAsFactors = F)
    bbNodes <- as.data.frame(strsplit(buildingBlocks$Nodes[i], ", "), stringsAsFactors = F)
    blockConnections <- csvNetwork[csvNetwork$Source %in% bbNodes[, 1] & csvNetwork$Target %in% bbNodes[, 1], ]

    # write to edges file
    fileName <- paste(configuration$OutputPath, "/", buildingBlocks$Id[i], "_edges.csv", sep = "")
    write.csv(blockConnections, file = fileName, quote = F, row.names = F)
    nodes <- block
    edges <- blockConnections
    network <- graph_from_data_frame(d = edges, vertices = nodes, directed = as.integer(configuration$Directed))
    V(network)$label.size <- 30
    V(network)$color <- group_indices(nodes, FiberId)
    if(configuration$Weighted == "1") {
      edge.col <- group_indices(edges, Weight)
    }

    png(filename = paste(configuration$OutputPath, "/", buildingBlocks$Id[i], ".png", sep = ""), width = 1280, height = 720)
    plot(network, edge.color = edges$color, vertex.label.cex = 2.5)
    dev.off()
  }
}
