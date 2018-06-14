library(tidyr)
library(dplyr)

setwd("~/Desktop/groupoid_finding_codes/Rruns/output/")
prefix <- "human_HINT_all_curated"
# read fibers and building blocks
fiberFile <- paste(prefix, ".txt", sep = "")
blocksFile <- paste(prefix, "_blocks.txt", sep = "")

fibers <- read.table(fiberFile, sep = "\t", stringsAsFactors = F)
fibers$V1 <- gsub(":", "", fibers$V1)
colnames(fibers)[1] <- "Id"
colnames(fibers)[2] <- "Node"
fibers$Id <- as.integer(fibers$Id)

maxFiberSize <- 70
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

maxBlockSize <- 71
tidyBlocks <- blocks %>%
  separate(Node, paste("V", c(1:maxBlockSize), sep = ""), sep = ", ") %>%
  gather(key = key, value = Node, -Id) %>%
  filter(!is.na(Node)) %>%
  arrange(Id) %>%
  select(c(1, 3))

# starting work with unique block
id <- 63
block <- tidyBlocks %>%
  filter(Id == id) %>%
  select("Node")

# here we know, that the first line of block is the node, which belongs to main fiber, because that is how we form it using stack in building blocks section of C code
# if that part of C code is changed, this part needs to be rewritten
fiberId <- tidyFibers$Id[grep(paste(block$Node[1]), tidyFibers$Node)]
fiber <- tidyFibers %>%
  filter(Id == fiberId)

log <- logical(nrow(block))

for(i in 1:nrow(fiber)) {
  log <- as.logical(log + grepl(paste("^", fiber$Node[i], "$", sep = ""), block$Node))
}

log <- data.frame(log)
colnames(log) <- "NodeType"
log[log == T] <- "Fiber"
log[log == F] <- "Regulator"

block <- cbind(block, log)
block

classifiedByHand <- read.csv("human_HINT_classification.csv", stringsAsFactors = F)
unique(classifiedByHand$Name)
