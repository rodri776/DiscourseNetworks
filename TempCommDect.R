library(igraph)
library(rbibutils)
library(ggraph)
library(tidygraph)
library(Rdpack)
library(DynComm)


node_file <-paste("~/Dropbox/Belief networks/Data/networks_plots_all/nodes/thehill//nodes_1214.csv")
edge_file <-paste("~/Dropbox/Belief networks/Data/networks_plots_all/edges/edges200/pcor/thehill/thehill_pcor_1214.csv")

output_file <- pdf(paste('~/Desktop/NetVis/TempCommTest.pdf')) 

nodes <- read.csv(paste(node_file), sep = ",")
NodesList <- (nodes$topic)
NodesList <- NodesList[NodesList != -1] #Exclude -1, BERTopic leftovers 

pCorMat <- as.matrix(read.csv(edge_file, sep="", header = FALSE))
pCorMat <- pCorMat[-1, ]
pCorMat <- pCorMat[, -1]
rownames(pCorMat) <- NodesList
colnames(pCorMat) <- NodesList
row.index <-rownames(pCorMat)
row_sums <- apply(pCorMat, 1, sum)  # Calculate row sums
row.index <- row.index[row_sums[row.index] > 0]
filt <- pCorMat[row.index,row.index] 
y <- filt
y[y < 0] <- 0

#Filtering Nodes
nodes <- nodes[nodes[, 4] %in% row.index, ]

#Use Node Number Column for Data Vis
baseNum <- nodes[, 4]
NodeNum <- as.list(baseNum) #Represented in DataVis as Text

#Use Name Node Column for Data Vis
baseName <- nodes[, 17]
Name <- as.list(baseName) #Represented in DataVis as Text

g <- graph_from_adjacency_matrix(y, mode = "undirected", weighted = TRUE)  
edge_list <- get.edgelist(g)
edge_list <- apply(edge_list, 2, as.double)


Parameters<-matrix(c("e","0.1","w", "FALSE"),ncol=2, byrow=TRUE)
dc<-DynComm(ALGORITHM$LOUVAIN,CRITERION$MODULARITY,Parameters)
dc$addRemoveEdges(edge_list)
## or

comm_list <- dc$communities()
dc_list <- dc$communityMapping(TRUE)
my_list <- as.list(dc_list[,2])
names(my_list) <- dc_list[, 1]
my_list <- unlist(my_list)

# Creates a graph
g <- graph_from_adjacency_matrix(y, mode = "undirected", weighted = TRUE)
# Fixes Label Formatting
V(g)$words3 <- gsub(",", "\n", nodes$words3) 
V(g)$words3 <- gsub(",", "", V(g)$words3) 

graph <- ggraph(g, layout = 'graphopt') +
  geom_edge_fan(aes(edge_linewidth = weight, alpha = weight), edge_colour = "grey66", show.legend = FALSE) +
  geom_node_point(aes(size = centrality_degree(), color = as.factor(my_list)), show.legend = FALSE) +
  geom_node_text(aes(label = words3), vjust = 0.75, repel=FALSE, lineheight = 0.75, size = 0.8) +
  geom_node_text(aes(label = NodeNum), repel = FALSE, vjust = -1, fontface = "bold", size = 1.2) 
plot(graph)
dev.off()
