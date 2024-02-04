library(EFAtools)
library(paran)
library(Matrix)
library(psych)
library(ggplot2)
Topics <- list('Abortion','Climate','Vaccines')
Plots <- list('AvgEW', 'SumEW')

for (i in Topics) {
  for (j in Plots) {
    File <- paste0(j, "_", i)
    data <-read.csv(paste0("~/Dropbox/Belief networks/Analyses/Juniper/NodePrevGraphs/ScreePlot/", File, ".csv"))
    Platforms <- list('Atlantic','Mother.Jones','The.Hill', 'Breitbart', 'Gateway.Pundit')
    #for (k in 1:ncol(data)) {
    #  column <- data[, k]
    #}
    paran(data, cfa = TRUE)
    
  } 
}

pa_paf <- PARALLEL(test_models$case_11b$cormat, N = 500)
