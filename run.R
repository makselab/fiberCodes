library("dplyr")
library("tidyr")
library("ggplot2")
library(readr)
setwd("~/Desktop/groupoid finding codes/code")
setwd("~/Dropbox/Research/PhD\ work/shared\ folders/LARGE-SCALE-GROUPOIDS/IAN-CODE")

inputFile <- "../data/regulonDBdata.txt"
outputFilePath <- "../sorted_outputs/"

input <- read.delim(inputFile, header = F, stringsAsFactors = F)
colnames(input)[1] <- "Source"
colnames(input)[2] <- "Target"
colnames(input)[3] <- "Interaction"
input <- input %>%
  select(1:3) %>%
  filter(Interaction != "?")
# we change h-ns, cause we know it has problems
input$Source[grepl("H-NS", input$Source)] <- "hns"

nodes1 <- as.data.frame(unique(input$Source), stringsAsFactors = F)
nodes2 <- as.data.frame(unique(input$Target), stringsAsFactors = F)
colnames(nodes1)[1] = "name"
colnames(nodes2)[1] = "name"
nodes <- rbind(nodes1, nodes2)
nodes <- as.data.frame(nodes[!duplicated(tolower(nodes$name)), ], stringsAsFactors = F)
colnames(nodes)[1] = "name"
nodes <- arrange(nodes, name)

#weights <- as.data.frame(unique(input$Interaction), stringsAsFactors = F)
#colnames(weights)[1] = "name"

# adjacency <- as.data.frame(matrix(, 0, 3))
# for(i in 1:nrow(input)) {
#   for(j in 1:2) {
#     adjacency[i, j] <- grep(paste("^", input[i, j], "$", sep = ""), nodes$name, ignore.case = T)
#   }
#   adjacency[i, 3] <- grep(paste("^", input[i, 3], "$", sep = ""), weights$name)
# }

fileNameRegulon <- "../data/regulonGeneNames.txt"
genesRegulon <- read.delim(fileNameRegulon, header = F, stringsAsFactors = F)
genesRegulon <- arrange(genesRegulon, V2)
genesRegulon[genesRegulon$V3 == "", 3] <- NA

for(i in 1:nrow(nodes)) {
  id <- genesRegulon[grepl(paste("^", nodes[i, 1], "$", sep = ""), genesRegulon$V2, ignore.case = T), 3]
  if(length(id) != 0) {nodes[i, 2] <- id}
}

# all used ecodata is checked by hand and it is true and it is also compared with ecocyc
# http://www.ecogene.org/old/DatabaseTable.php
fileNameEcodata <- "../data/EcoData040318-132801.txt"
genesEco <- read.delim(fileNameEcodata, header = F, stringsAsFactors = F)
colnames(genesEco)[1] = "Gene"
colnames(genesEco)[2] = "WISC"
colnames(genesEco)[3] = "Syn"
genesEco <- arrange(genesEco, Gene)
genesEco[genesEco$Syn == "Null", 3] <- NA

for(i in 1:nrow(nodes)) {
  id1 <- genesEco[grepl(paste("(^|;)", nodes[i, 1], "($|;)", sep = ""), genesEco$Gene, ignore.case = T), 2]
  id2 <- genesEco[grepl(paste("(^|;)", nodes[i, 1], "($|;)", sep = ""), genesEco$Syn, ignore.case = T), 2]
  if(length(id1) != 0) {nodes[i, 3] <- id1[1]}
  if(length(id2) != 0) {nodes[i, 4] <- id2[1]}
}

# These are operons, we give them separate names
nodes[grepl("DinJ-YafQ", nodes$name), 5]  <- "b5001"
nodes[grepl("FlhDC", nodes$name), 5]      <- "b5002"
nodes[grepl("GadE-RcsB", nodes$name), 5]  <- "b5004"
nodes[grepl("HigB-HigA", nodes$name), 5]  <- "b5005"
nodes[grepl("HipAB", nodes$name), 5]      <- "b5006"
nodes[grepl("IHF", nodes$name), 5]        <- "b5008"
nodes[grepl("MazE-MazF", nodes$name), 5]  <- "b5009"
nodes[grepl("RcsAB", nodes$name), 5]      <- "b5010"
nodes[grepl("RcsB-BglJ", nodes$name), 5]  <- "b5011"
nodes[grepl("RelB-RelE", nodes$name), 5]  <- "b5012"
nodes[grepl("YefM-YoeB", nodes$name), 5]  <- "b5013"

# this guy is just something weird
nodes[grepl("HU", nodes$name), 5] <- "b5007"

# this guy has no K-in, so we put him alone. It is also pseudo gene
nodes[grepl("GatR", nodes$name), 5] <- "b5003"

# all of the guys below are just slaves, which are pseudogenes, so they have no idx, I give them idx by hand
nodes[grepl("efeU_2", nodes$name), 5] <- "b5014"
nodes[grepl("gadF", nodes$name), 5] <- "b5015"
nodes[grepl("sroA", nodes$name), 5] <- "b5020"
nodes[grepl("sroC", nodes$name), 5] <- "b5021"
nodes[grepl("sroD", nodes$name), 5] <- "b5022"

# has no b name in db, but has b name
nodes[grepl("ilvG_1", nodes$name), 5] <- "b3767"
nodes[grepl("ilvG_2", nodes$name), 5] <- "b3768"
nodes[grepl("phnE_2", nodes$name), 5] <- "b4103"
nodes[grepl("molR_1", nodes$name), 5] <- "b2115"
nodes[grepl("yegZ", nodes$name), 5] <- "b2083"

nodes$Blattner <- nodes$V2
nodes$Blattner[is.na(nodes$Blattner)] <- nodes$V3[is.na(nodes$Blattner)]
nodes$Blattner[is.na(nodes$Blattner)] <- nodes$V4[is.na(nodes$Blattner)]
nodes$Blattner[is.na(nodes$Blattner)] <- nodes$V5[is.na(nodes$Blattner)]
nodes <- nodes %>%
  select(1, 6)

operonEdges <- function(operon, genes) {
  input1 <- input[grep(paste("^", genes[1], "$", sep = ""), input$Target, ignore.case = T),]
  input2 <- input[grep(paste("^", genes[2], "$", sep = ""), input$Target, ignore.case = T),]
  newEdges <- as.data.frame(matrix(, 0, 3))
  colnames(newEdges)[1] <- "Source"
  colnames(newEdges)[2] <- "Target"
  colnames(newEdges)[3] <- "Interaction"
  newEdge <- as.data.frame(matrix(, 0, 3))
  colnames(newEdge)[1] <- "Source"
  colnames(newEdge)[2] <- "Target"
  colnames(newEdge)[3] <- "Interaction"
  for(i in 1:nrow(input1)) {
    newIn <- input2[grep(paste("^", input1$Source[i], "$", sep = ""), input2$Source, ignore.case = T), ]
    newIn <- newIn %>%
      filter(!is.na(Source))
    if(nrow(newIn) == 0) next
    if(input1$Interaction[i] != newIn$Interaction) next
    newEdge[1, 1] <- newIn$Source
    newEdge[1, 2] <- operon
    newEdge[1, 3] <- newIn$Interaction
    newEdges <- rbind(newEdges, newEdge)
  }
  newEdges
}

operons <- c("DinJ-YafQ","FlhDC", "GadE-RcsB", "HigB-HigA", "HipAB",
             "IHF", "MazE-MazF", "RcsAB", "RcsB-BglJ", "RelB-RelE", "YefM-YoeB")
genes <- c("DinJ", "YafQ", "FlhD", "FlhC", "GadE", "RcsB", "HigB", "HigA", "HipA", "HipB",
           "IHFA", "IHFB", "MazE", "MazF", "RcsA", "RcsB", "RcsB", "BglJ", "RelB", "RelE", "YefM", "YoeB")
newEdges <- as.data.frame(matrix(, 0, 3))
colnames(newEdges)[1] <- "Source"
colnames(newEdges)[2] <- "Target"
colnames(newEdges)[3] <- "Interaction"
for(i in 1:length(operons)) {
  newEdge <- operonEdges(operons[i], c(genes[2 * i - 1], genes[2 * i]))
  newEdges <- rbind(newEdges, newEdge)
}

input <- rbind(input, newEdges)

# we now change all input names to b numbers and go back, because we want unique genes only in our network
blattnerConnections <- as.data.frame(matrix(, nrow(input), 3))
colnames(blattnerConnections)[1] <- "Source"
colnames(blattnerConnections)[2] <- "Target"
colnames(blattnerConnections)[3] <- "Interaction"
for(i in 1:nrow(input)) {
  blattnerConnections$Source[i] <- nodes[grep(paste("^", input$Source[i], "$", sep = ""), nodes$name, ignore.case = T), 2]
  blattnerConnections$Target[i] <- nodes[grep(paste("^", input$Target[i], "$", sep = ""), nodes$name, ignore.case = T), 2]
  blattnerConnections$Interaction <- input$Interaction
}

#write.table(blattnerConnections, "../data/regulonBlattner.txt", quote = F, row.names = F, col.names = F)

nodes <- nodes[!duplicated(nodes$Blattner), ]

geneNetwork <- as.data.frame(matrix(, nrow(input), 3))
colnames(geneNetwork)[1] <- "Source"
colnames(geneNetwork)[2] <- "Target"
colnames(geneNetwork)[3] <- "Interaction"
for(i in 1:nrow(blattnerConnections)) {
  geneNetwork$Source[i] <- nodes[grepl(paste("^", blattnerConnections$Source[i], "$", sep = ""), nodes$Blattner, ignore.case = T), 1]
  geneNetwork$Target[i] <- nodes[grepl(paste("^", blattnerConnections$Target[i], "$", sep = ""), nodes$Blattner, ignore.case = T), 1]
  geneNetwork$Interaction <- blattnerConnections$Interaction
}
# here we do it by hand, cause we know only this one is not using it`s main name and it can cause problems
geneNetwork$Source[grepl("AlsR", geneNetwork$Source, ignore.case = T)] <- "rpiR"
geneNetwork$Target[grepl("AlsR", geneNetwork$Target, ignore.case = T)] <- "rpiR"
#write.table(geneNetwork, "../data/regulonUniqueGenes.txt", quote = F, row.names = F, col.names = F)

for(i in 1:nrow(geneNetwork)) {
  nodes$koutNoZero[grepl(paste("^", geneNetwork$Source[i], "$", sep = ""), nodes$name, ignore.case = T)] <- 1
}

kOutNodes <- nodes %>%
  filter(!is.na(koutNoZero))

grepBool <- F
for(i in 1:nrow(kOutNodes)) {
  grepBool <- grepBool + grepl(paste("^", kOutNodes$name[i], "$", sep = ""), geneNetwork$Target, ignore.case = T)
}
tfNetwork <- geneNetwork %>%
  filter(as.logical(grepBool))

#write.table(tfNetwork, "../data/regulonTFNetwork.txt", quote = F, row.names = F, col.names = F)

# now we want to find possible operons as those, who have same input and no output
# first we remove those, which have kout
# second we put together only nodes, which have same promoter
# then we find all unique inputs and put nodes with the same input together
fileNamePromoterData <- "../data/regulonPromoters.txt"
promoterData <- read.delim(fileNamePromoterData, header = F, stringsAsFactors = F)
promoterData[grepl("^$", promoterData$V5), 5] <- NA
promoterData <- promoterData %>%
  select(4:5) %>%
  separate(V4, paste("gene", c(1:16), sep = ""), sep = ",")
promoterData <- promoterData %>%
  gather(name, gene, -V5) %>%
  select(c(1, 3)) %>%
  filter(!is.na(gene)) %>%
  filter(!is.na(V5)) %>%
  group_by(V5) %>%
  mutate(numberOfGenes = n()) %>%
  ungroup() %>%
  group_by(gene) %>%
  arrange(desc(numberOfGenes))

possibleOperonNodes <- nodes %>%
  filter(is.na(koutNoZero)) %>%
  select(1:2)

geneInputs <- geneNetwork %>%
  group_by(Target) %>%
  arrange(Source) %>%
  summarise(Input = paste(Source, Interaction, collapse = ';'))

for(i in 1:nrow(geneInputs)) {
  geneInputs$Promoter[i] <- toString(promoterData[grep(paste("^", geneInputs$Target[i], "$", sep = ""), promoterData$gene, ignore.case = T)[1], 1])
}

geneInputs <- geneInputs %>%
  filter(!is.na(Promoter))

filtration <- F
for(i in 1:nrow(possibleOperonNodes)) {
  filtration <- filtration + grepl(paste("^", possibleOperonNodes$name[i], "$", sep = ""), geneInputs$Target,  ignore.case = T)
}

operons <- geneInputs %>%
  filter(as.logical(filtration)) %>%
  group_by(Input, Promoter) %>%
  summarise(OperonGenes = paste(Target, collapse = ';')) %>%
  arrange(OperonGenes)

operons <- operons %>%
  separate(OperonGenes, paste("gene", c(1:12), sep = ""), sep = ";")

operons <- operons %>%
  filter(!is.na(gene2)) %>%
  arrange(Input, Promoter)

# now we want to compare to real operons
# here we compare what we found with regulonData, I checked by hand around half of differences and it seems that regulon doesn`t define operon right
fileNameOperons <- "../data/regulonOperonDB.txt"
regulonOperons <- read.delim(fileNameOperons, header = F, stringsAsFactors = F)
regulonOperons <- regulonOperons %>%
  select(c(1, 6))

regulonOperons <- regulonOperons %>%
  separate(V6, paste("gene", c(1:16), sep = ""), sep = ",") %>%
  gather(id, name, -V1) %>%
  filter(!is.na(name)) %>%
  arrange(V1, name) %>%
  select(c(1, 3)) %>%
  group_by(V1) %>%
  summarise(OperonGenes = paste(name, collapse = ','))

ourOperons <- operons %>%
  mutate(oneGene = gene1) %>%
  select(3:15) %>%
  gather(id, name, -(c(Input,oneGene))) %>%
  filter(!is.na(name)) %>%
  arrange(Input, name) %>%
  select(-3) %>%
  group_by(Input, oneGene) %>%
  summarise(OperonGenes = paste(name, collapse = ','))

for(i in 1:nrow(ourOperons)) {
  ourOperons$regulon[i] <- regulonOperons[grep(paste("(^|,)", ourOperons$oneGene[i], "($|,)", sep = ""), regulonOperons$OperonGenes), 2]
  ourOperons$regulonName[i] <- regulonOperons[grep(paste("(^|,)", ourOperons$oneGene[i], "($|,)", sep = ""), regulonOperons$OperonGenes), 1]
}

same <- ourOperons %>%
  filter(OperonGenes == regulon) %>%
  mutate(OperonName = unlist(regulonName))

giveName <- function(genes) {
  genes <- as.data.frame(genes)
  colnames(genes)[1] <- "a"
  genes <- genes %>%
    separate(a, paste("gene", c(1:5), sep = ""), sep = ",") %>%
    gather(1:5) %>%
    filter(!is.na(value)) %>%
    arrange(value) %>%
    mutate(base = value, add = value)
  
  genes$base <- gsub("[A-Z]*$", "", genes$base)
  genes$add <- gsub("^[a-z]*", "", genes$add)
  
  genes <- genes %>%
    group_by(base) %>%
    summarise(post = paste(add, collapse = '')) %>%
    ungroup() %>%
    mutate(partNames = paste(base, post, sep = "")) %>%
    summarise(name = paste(partNames, collapse = '-'))
  toString(genes)
}

different <- ourOperons %>%
  filter(OperonGenes != regulon) %>%
  mutate(OperonName = giveName(OperonGenes))

operonDefinition <- rbind(same[, c(3, 6)], different[, c(3, 6)])

operonDefinition[duplicated(operonDefinition$OperonName), ]
operonDefinition[grep("glpEG", operonDefinition$OperonName), ]
operonDefinition$OperonName[285] <- "glpEG"
operonDefinition$OperonName[286] <- "gpsA-secB"
operonDefinition$OperonName[287] <- "ilvBN-ivbL"
#OperonName <- c("DinJ-YafQ","FlhDC", "GadE-RcsB", "HigB-HigA", "HipAB", "IHF", "MazE-MazF", "RcsAB", "RcsB-BglJ", "RelB-RelE", "YefM-YoeB")
#OperonGenes <- c("DinJ, YafQ", "FlhD, FlhC", "GadE, RcsB", "HigB, HigA", "HipA, HipB", "IHFA, IHFB", "MazE, MazF", "RcsA, RcsB", "RcsB, BglJ", "RelB, RelE", "YefM, YoeB")
#operonDefinition <- rbind(operonDefinition, data.frame(OperonGenes, OperonName))
#write.table(operonDefinition, "../data/operonDefinition.txt", quote = F, row.names = F, col.names = F, sep = "\t")

# now, final stage, we want to take gene data and to gather all operons
operonNetwork <- as.data.frame(matrix(, nrow(geneNetwork), 3))
colnames(operonNetwork)[1] <- "Source"
colnames(operonNetwork)[2] <- "Target"
colnames(operonNetwork)[3] <- "Interaction"
for(i in 1:nrow(geneNetwork)) {
  operonName <- operonDefinition[grep(paste("(^|,)", geneNetwork$Target[i], "(,|$)", sep = ""), operonDefinition$OperonGenes, ignore.case = F), 2]
  operonNetwork$Source[i] <- geneNetwork$Source[i]
  operonNetwork$Interaction[i] <- geneNetwork$Interaction[i]
  
  if(nrow(operonName) == 0) {
    operonNetwork$Target[i] <- toString(geneNetwork$Target[i])
  } else {
    operonNetwork$Target[i] <- toString(operonName)
  }
}

operonNetwork <- operonNetwork[!duplicated(operonNetwork), ]

#a <- data.frame(unique(operonNetwork$Source))
#b <- data.frame(unique(operonNetwork$Target))
#colnames(a)[1] <- "V1"
#colnames(b)[1] <- "V1"
#c <- rbind(a, b)
#unique(c)
#d <- operonDefinition
#d$OperonGenes <- gsub("[^,]", "", d$OperonGenes)
#d$OperonGenes <- nchar(d$OperonGenes)
#sum(d$OperonGenes)

# write.table(operonNetwork, "../data/ourOperonNetwork.txt", quote = F, row.names = F, col.names = F)

# just fixing groupoids here
groupoidsFileName <- "../sorted_outputs/operons/run_7/groupoids.txt"
groupoids <- read.delim(groupoidsFileName, header = F, stringsAsFactors = F)
groupoids$V1 <- gsub(",$", "", groupoids$V1)
groupoids <- groupoids %>%
  separate(V1, paste("operon", c(1:30), sep = ""), sep = ",")


for(i in 1:nrow(groupoids)) {
  for(j in 2:ncol(groupoids)) {
    if(is.na(groupoids[i, j])) next
    out <- operonDefinition[grep(paste("^", groupoids[i, j], "$", sep = ""), operonDefinition$OperonName, ignore.case = T), 1]
    if(nrow(out) != 0) {
      groupoids[i, j] <- out
    }
  }
}
groupoids <- groupoids %>%
  unite(-1, sep = ",")
groupoids$`-1` <- gsub("(,NA)*$", "", groupoids$`-1`)
groupoids$`-1` <- gsub(":,", ":\t", groupoids$`-1`)
#write.table(groupoids, "../sorted_outputs/operons/run_7/geneGroupoids.txt", quote = F, row.names = F, col.names = F)
