# Created Program to Merge Node files together; useful for deciphering what each topic represents

library(purrr)
library(dplyr)
library(readr)

sites <- c('atlantic', 'motherjones', 'breitbart', 'thehill', 'gatewaypundit')

merged_data <- list()  # Create an empty list to store the merged data for each site

#Iterates over every site
for (site in sites) {
  path <- paste0("~/Dropbox/networks_plots_all/nodes/", site)
  data_files <- list.files(path, pattern = "*.csv", full.names = TRUE) #Finds node files
  exportPath <- paste0("~/Documents/", site, ".csv") 
  
  #For debugging
  if (length(data_files) == 0) {
    cat("No CSV files found in", path, "\n")
    next
  }
  
  #Loop merges files together 
  data_list <- lapply(data_files, read_csv)
  if (is.null(merged_data[[site]])) {
    merged_data[[site]] <- bind_rows(data_list)  # Create the merged data frame
    write.csv(merged_data[[site]], exportPath, row.names=FALSE)
  } else {
    merged_data <- full_join(merged_data, bind_rows(data_list), by = "id")  # Merge with the existing data frame
  }
  
}

merged_data[['atlantic']]  # Access the merged data for the 'atlantic' site

