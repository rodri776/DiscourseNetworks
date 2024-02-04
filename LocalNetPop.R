# This program creates a matrix that shows how prevalent different topics are in each network over time. 
# It looks at the amount of times they appear in the network, as well as the Sum and avg. of their edge weights. 

library(Matrix)
library(ggraph)
library(ggplot2)
library(RColorBrewer)
library(igraph)
library(tidygraph)


#Enter Platform
sites <- list('atlantic','motherjones','breitbart','thehill','gatewaypundit')
my.site <- readline(prompt= "Enter Platform: ")

#Enter Node
my.node <- readline(prompt="Enter Node: ")
my.node <- as.numeric(my.node)

#Get Files
dir_nodes <-paste("~/Dropbox/Belief networks/Data/networks_plots_all/nodes/",my.site,"/",sep="")
dir_edges <-paste("~/Dropbox/Belief networks/Data/networks_plots_all/edges/edges200/pcor/",my.site,sep="")
files_nodes<-list.files(path=dir_nodes, pattern='csv', full.names = TRUE)
files_edges<-list.files(path=dir_edges, pattern='csv', full.names = TRUE)

#Sorting Files
files_edges1 <- as.matrix(files_edges)
xye=c()
xmo=c()
for (i in 1:nrow(files_edges1)) {
  x1 <- unlist(gregexpr('.csv', files_edges1[i]))
  xye[i] <- substr(files_edges1[i], x1-2, x1-1)
  xmo[i] <- substr(files_edges1[i], x1-4, x1-3)
}
files_edges_sorted = data.frame(t(rbind(files_edges, xye, xmo)))
files_edges_sorted = arrange(files_edges_sorted, xye, xmo) 
files_sorted = files_edges_sorted[,1]

# Create the full file path for the CSV file
meas_folder <- "~/Desktop/LocalNetPrevalence"
meas_name <- paste(my.site, "_", my.node, "_prev.csv")
csv_file <- file.path(meas_folder, meas_name)

#Initialize lists for Matrix
NodeNum <- list()
continuous_values <- list() 
discrete_values <- list() 
mean_values <- list() 

#Big File Loop to Make & Export Data Visualizations and Network Measure Matrix
for (file in files_sorted){
  try ({
    #Creates Date 
    x <- unlist(gregexpr(".", file, fixed=TRUE))
    dte <- substr(file[1], x-4, x-1)        
    print(dte) # Prints for Debugging 
    
    # load nodes
    nodes <- read.csv(paste(dir_nodes, "nodes_", dte, ".csv", sep=""), sep = ",")
    NodesList <- (nodes$topic)
    NodesList <- NodesList[NodesList != -1]
    
    # load edges
    pCorMat <- as.matrix(read.csv(file, sep="", header = FALSE))
    pCorMat <- pCorMat[-1, ]
    pCorMat <- pCorMat[, -1]
    rownames(pCorMat) <- NodesList
    colnames(pCorMat) <- NodesList
    row.index <- which(rownames(pCorMat) == my.node)
    filt <- pCorMat[row.index,] > 0 
    filt[row.index] <- TRUE
    x <- rownames(pCorMat)[filt == TRUE]
    y <- pCorMat[x, x]
    y[y < 0] <- 0

    #Filtering Nodes
    rows_to_keep <- c()
    for (i in 1:nrow(nodes)) {
      for (j in 1:length(x)){ 
        if (as.numeric(nodes[i, 'topic']) == as.numeric(x[[j]])) {
          rows_to_keep <- c(rows_to_keep, i)
        } 
      }
    }
    nodes <- nodes[rows_to_keep, ] 
    
    #Create list of Node Names/Numbers
    baseNum <- nodes[, 4]
    baseName <- nodes[, 18]
    for (i in seq_along(baseNum)) {
      Name_Find <- as.character(baseName[i])
      if (!(baseNum[i] %in% NodeNum)) {
        NodeNum[[Name_Find]] <- (baseNum[i])
        NodeNum <- NodeNum[order(as.numeric(NodeNum))]
      }
    }
    
    #Calculate Continuous Variable (Edge Weight Sum)
    for (j in 1:nrow(y)) {
      row_name <- rownames(y)[j]
        if (row_name %in% names(continuous_values)) {
          continuous_values[[row_name]] <- continuous_values[[row_name]] + y[j, as.character(my.node)]
        } else {
          continuous_values[[row_name]] <- y[j, as.character(my.node)]
          continuous_values <- continuous_values[order(as.numeric(names(continuous_values)))]
        }
    }
    
    #Calculate Discrete Variable (Node Prevalence)
    for (k in 1:nrow(y)) {
      row_name <- rownames(y)[k]
      if (row_name %in% names(discrete_values)) {
        discrete_values[[row_name]] <- discrete_values[[row_name]] + 1
      } else {
        discrete_values[[row_name]] <- 1
        discrete_values <- discrete_values[order(as.numeric(names(discrete_values)))]
      }
    }
    #Calculate Mean Variable (Avg. Edge Weight)
    for (l in 1:nrow(y)) {
      row_name <- rownames(y)[l]
      mean_values[[row_name]] <- continuous_values[[row_name]] / discrete_values[[row_name]]
      mean_values <- mean_values[order(as.numeric(names(mean_values)))]
    }
  }) 
}
dev.off()

#Combines Node names and Numbers (For DataVis purposes)
combined_names <- names(NodeNum)
unlisted_NodeNum <- unlist(NodeNum)
combined_names <- paste(combined_names, " (", paste0(unlisted_NodeNum), ")", sep = "")

#plug statistics into matrix
df_row_data <- data.frame(
  "Topic" = combined_names,
  "Continuous (edge weight)" = unlist(continuous_values),
  "Discrete (presence)" = unlist(discrete_values),
  "Mean Prevalence" = unlist(mean_values),
  stringsAsFactors = FALSE)

# Save the graph matrix as a CSV file
write.csv(df_row_data, file = csv_file, row.names = FALSE)

