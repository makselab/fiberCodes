library(tidyr)
library(dplyr)

setwd("~/Desktop/groupoid_finding_codes/Rruns/output/")
prefix <- "ourOperonNetwork"
# read fibers and building blocks
fiberFile <- paste(prefix, ".txt", sep = "")
blocksFile <- paste(prefix, "_blocks.txt", sep = "")

fibers <- read.table(fiberFile, sep = "\t", stringsAsFactors = F)
fibers$V1 <- gsub(":", "", fibers$V1)
colnames(fibers)[1] <- "Id"
colnames(fibers)[2] <- "Node"
fibers$Id <- as.integer(fibers$Id)

maxFiberSize <- 300
tidyFibers <- fibers %>%
  separate(Node, paste("V", c(1:maxFiberSize), sep = ""), sep = ", ") %>%
  gather(key = key, value = Node, -Id) %>%
  filter(!is.na(Node)) %>%
  arrange(Id) %>%
  select(c(1, 3))

blocks <- read.table(blocksFile, sep = "\t", stringsAsFactors = F)
blocks$V1 <- gsub(":", "", blocks$V1)
colnames(blocks)[1] <- "Id"
colnames(blocks)[2] <- "Node"
blocks$Id <- as.integer(blocks$Id)

maxBlockSize <- 300
tidyBlocks <- blocks %>%
  separate(Node, paste("V", c(1:maxBlockSize), sep = ""), sep = ", ") %>%
  gather(key = key, value = Node, -Id) %>%
  filter(!is.na(Node)) %>%
  arrange(Id) %>%
  select(c(1, 3))

isOnlyMainFiber <- function(block, fiberId) {
  regulatorIds <- block[block$FiberId != fiberId, "FiberId"]
  return(anyDuplicated(regulatorIds) == 0)
}

isFiberSendingToRegulators <- function(edges) {
  feedback <- edges %>%
    filter(SourceType == "Fiber") %>%
    filter(TargetType == "Regulator")
  return(nrow(feedback) != 0)
}

areAllNodesFromBlockInFiber <- function(block) {
  return(nrow(block[block$NodeType == "Regulator", ]) == 0)
}

isSizeOfInputSetOne <- function(block, edges) {
  fiberNode <- block %>%
    filter(NodeType == "Fiber") %>%
    select(Node) %>%
    summarise(Node = first(Node))
  fiberNode <- as.character(fiberNode)
  return(length(edges[edges$Target == fiberNode, "Source"]) == 1)
}

doFibersSendToFibers <- function(edges){
  fiberFiberInteractions <- edges %>%
    filter(SourceType == "Fiber") %>%
    filter(TargetType == "Fiber")
  return(nrow(fiberFiberInteractions) > 0)
}

fiberHasOneInput <- function(edges) {
  NumberOfInputs <- edges %>%
    filter(TargetType == "Fiber") %>%
    group_by(Target) %>%
    summarise(NumberOfInputs = n()) %>%
    ungroup() %>%
    summarise(NumberOfInputs = first(NumberOfInputs))
  return(NumberOfInputs$NumberOfInputs[1] == 1)
}

isOneNodeFromFiberRegulator <- function(edges) {
  numberOfFiberInputs <- edges %>%
    filter(SourceType == "Fiber") %>%
    filter(TargetType == "Fiber") %>%
    group_by(Target) %>%
    summarise(NumberOfInputs = n()) %>%
    ungroup() %>%
    summarise(NumberOfInputs = first(NumberOfInputs))

  return(numberOfFiberInputs$NumberOfInputs == 1)
}

chainCondition <- function(block, edges) {
  # chain condition: only one node has no output, only one node has 2 outputs, all other nodes have 1 input and 1 output
  for(i in 1:nrow(block)) {
    numberOfOutputs <- edges %>%
      filter(Source == block$Node[i]) %>%
      summarise(NumberOfOutputs = n())

    numberOfInputs <- edges %>%
      filter(Target == block$Node[i]) %>%
      summarise(NumberOfInputs = n())

    block$NumberOfOutputs[i] <- numberOfOutputs$NumberOfOutputs[1]
    block$NumberOfInputs[i] <- numberOfInputs$NumberOfInputs[1]
  }
  loopbackNode <- edges %>%
    filter(Source == Target)
  loopbackNode <- loopbackNode$Source[1]

  loopTwoOutputs <- block[block$Node == loopbackNode, "NumberOfOutputs"] == 2
  oneNoOutputNode <- nrow(block[block$NumberOfOutputs == 0, ]) == 1
  otherNodesOneOne <- (nrow(block[(block$NumberOfOutputs != 0) & (block$NumberOfOutputs != 2), ]) == nrow(block) - 2)

  return(loopTwoOutputs & oneNoOutputNode & otherNodesOneOne)
}

allSameInput <- function(edges) {
  sources <- edges %>%
    group_by(Source) %>%
    summarise(N = n())
  return(nrow(sources) == 1)
}

start.time <- Sys.time()
# starting work with unique block
for(id in 0:max(tidyBlocks$Id)) {
  #print(id)
  #id <- 2
  # first gather data about the block
  block <- tidyBlocks %>%
    filter(Id == id) %>%
    select("Node")

  for(i in 1:nrow(block)) {
    block$FiberId[i] <- tidyFibers[grep(paste("^", block$Node[i], "$", sep = ""), tidyFibers$Node), "Id"]
  }
  # here we know, that the first line of block is the node, which belongs to main fiber, because that is how we form it using stack in building blocks section of C code
  # if that part of C code is changed, this part needs to be rewritten
  fiberId <- tidyFibers$Id[grep(paste("^", block$Node[1], "$", sep = ""), tidyFibers$Node)]

  block <- block %>%
    mutate(NodeType = FiberId)
  block[block$NodeType == fiberId, "NodeType"] <- "Fiber"
  block[block$NodeType != "Fiber", "NodeType"] <- "Regulator"

  blocks$Fiber[id + 1] <- paste(sort(block[block$NodeType == "Fiber", "Node"]), collapse = ", ")
  blocks$Regulators[id + 1] <- paste(sort(block[block$NodeType == "Regulator", "Node"]), collapse = ", ")

  edgesFileName <- paste(prefix, "buildingBlocks/", id, "_edges.csv", sep = "")
  edges <- read.csv(edgesFileName, stringsAsFactors = F)

  for(i in 1:nrow(edges)) {
    edges$SourceType[i] <- block[grep(paste("^", edges$Source[i], "$", sep = ""), block$Node), "NodeType"]
    edges$TargetType[i] <- block[grep(paste("^", edges$Target[i], "$", sep = ""), block$Node), "NodeType"]
  }

  # this big structure of ifs is hard to understand, but it is drawn in block diagram in file blockdiagram.xml
  if(isFiberSendingToRegulators(edges)) {
    blocks$Class[id + 1] <- "Feedback Fiber"
    blocks$BlockName[id + 1] <- "Feedback Fiber"
    blocks$nl[id + 1] <- "Not n-l classifiable"
  } else {
    if(!isOnlyMainFiber(block, fiberId)) {
      blocks$Class[id + 1] <- "Multi-layered Fiber"
      blocks$BlockName[id + 1] <- "Multi-layered Fiber"
      blocks$nl[id + 1] <- "Not n-l classifiable"
    } else {
      if(areAllNodesFromBlockInFiber(block)) {
        if(isSizeOfInputSetOne(block, edges)) {
          if(chainCondition(block, edges)) {
            blocks$Class[id + 1] <- "Chain"
            K <- nrow(block)
            blocks$BlockName[id + 1] <- paste(K, "Chain", sep = "-")
            blocks$nl[id + 1] <- "n = 1, l = 0"
          } else {
            if(allSameInput(edges)) {
              blocks$Class[id + 1] <- "Synchronized Star Fiber"
              K <- nrow(block) - 1
              blocks$BlockName[id + 1] <- paste(K, "SSF", sep = "-")
              blocks$nl[id + 1] <- "n = 1, l = 0"
            } else {
              blocks$Class[id + 1] <- "Chain-Star"
              blocks$BlockName[id + 1] <- "Chain-Star"
              blocks$nl[id + 1] <- "n = 1, l = 0"
            }
          }
        } else {
          blocks$Class[id + 1] <- "n > 1"
          blocks$BlockName[id + 1] <- "n > 1"
          blocks$nl[id + 1] <- "n > 1"
        }
      } else {
        if(doFibersSendToFibers(edges)) {
          if(isOneNodeFromFiberRegulator(edges)) {
            L <- nrow(block[block$NodeType == "Regulator", ])
            K <- nrow(block[block$NodeType == "Fiber", ])
            blocks$Class[id + 1] <- "FFF"
            blocks$BlockName[id + 1] <- paste(L, K, "FFF", sep = "-")
            blocks$nl[id + 1] <- paste("n = 1, l = ", L, sep = "")
          } else {
            blocks$Class[id + 1] <- "Unclassified"
            blocks$BlockName[id + 1] <- "Unclassified"
            blocks$nl[id + 1] <- "Unclassified"
          }
        } else {
          if(fiberHasOneInput(edges)) {
            blocks$Class[id + 1] <- "Unsynchronized Star Fiber"
            K <- nrow(block) - 1
            blocks$BlockName[id + 1] <- paste(K, "USF", sep = "-")
            blocks$nl[id + 1] <- "n = 0, l = 1"
          } else {
            L <- nrow(block[block$NodeType == "Regulator", ])
            K <- nrow(block[block$NodeType == "Fiber", ])
            blocks$Class[id + 1] <- "FAN Fiber"
            blocks$BlockName[id + 1] <- paste(L, K, "FAN", sep = "-")
            blocks$nl[id + 1] <- paste("n = 0, l = ", L, sep = "")
          }
        }
      }
    }
  }
}
now.time <- Sys.time()
time.taken <- now.time - start.time
print(time.taken)

#blocks <- arrange(blocks, Class, BlockName)

paperBlocks <- arrange(blocks, Class, BlockName)
paperBlocks %>%
  group_by(Class) %>%
  summarise(numberOfBlocks = n())

FANNM <- paperBlocks %>%
  filter(Class == "FAN Fiber") %>%
  select(-2, -3) %>%
  mutate(N = BlockName) %>%
  mutate(M = BlockName)
FANNM$N <- as.integer(gsub("([0-9]+)-[0-9-]+FAN", "\\1", FANNM$N))
FANNM$M <- as.integer(gsub("[0-9]+-([0-9]+)-FAN", "\\1", FANNM$M))
FANNM <- FANNM %>%
  filter(M < 20)

library(ggplot2)
ggplot(data = FANNM) +
  geom_histogram(mapping = aes(x = N), binwidth = 1, color = "white", fill = "blue")

ggplot(data = FANNM[]) +
  geom_histogram(mapping = aes(x = M), binwidth = 1, color = "white", fill = "blue")

FFFNM <- paperBlocks %>%
  filter(Class == "FFF") %>%
  select(-2, -3) %>%
  mutate(N = BlockName) %>%
  mutate(M = BlockName)
FFFNM$N <- as.integer(gsub("([0-9]+)-[0-9-]+FFF", "\\1", FFFNM$N))
FFFNM$M <- as.integer(gsub("[0-9]+-([0-9]+)-FFF", "\\1", FFFNM$M))

ggplot(data = FFFNM) +
  geom_histogram(mapping = aes(x = N), binwidth = 1, color = "white", fill = "blue")

ggplot(data = FFFNM) +
  geom_histogram(mapping = aes(x = M), binwidth = 1, color = "white", fill = "blue")

uSSFN <- paperBlocks %>%
  filter(Class == "Unsynchronized Star Fiber") %>%
  select(-2, -3) %>%
  mutate(N = BlockName)
uSSFN$N <- as.integer(gsub("([0-9]+)-USF", "\\1", uSSFN$N))
uSSFN <- uSSFN %>%
  filter(N < 35)

ggplot(data = uSSFN) +
  geom_histogram(mapping = aes(x = N), binwidth = 7, color = "white", fill = "blue")

SSFN <- paperBlocks %>%
  filter(Class == "Synchronized Star Fiber") %>%
  select(-2, -3) %>%
  mutate(N = BlockName)
SSFN$N <- as.integer(gsub("([0-9]+)-SSF", "\\1", SSFN$N))

ggplot(data = SSFN) +
  geom_histogram(mapping = aes(x = N), binwidth = 6, color = "white", fill = "blue")

blockDistribution <- read.csv("/home/ian/Dropbox/Research/PhD work/shared folders/LARGE-SCALE-GROUPOIDS/Human_yeast_paper/block distribution.csv", stringsAsFactors = F)

blockDistribution <- blockDistribution %>%
  gather("Dataset", "value", -1)

blockDistribution <- blockDistribution %>%
  group_by(Dataset) %>%
  mutate(value = value / sum(value) * 100)

ggplot(data = blockDistribution) +
  geom_col(mapping = aes(x = Structure.type, y = value, fill = Dataset), pos = "dodge")

# result analysis
classifiedByHand <- read.csv("human_HINT_classification.csv", stringsAsFactors = F)
classifiedByHand <- cbind(blocks, classifiedByHand)
classifiedByHand <- classifiedByHand[, -4]
diff <- classifiedByHand %>%
  filter(Class != Name)
