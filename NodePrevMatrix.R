#Creates matrix  showing the prevalence of nodes across time on any given ego network

library(matlab)
library(Matrix)




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
time <- list()
nodes <- list() 
main_matrix <- list() 




time = [5 8]; #TIME
nodes = [3 6]; # NUMBER OF NODES
main_matrix = [0 2 4 6; 8 10 12 14; 16 18 20 22];
imagesc(time,nodes,main_matrix)

