# Bennett et al 2016 

rm(list = ls())

library(reshape)
source("/Users/christophercarnivale/Desktop/TempleRcourse/Colaboration_assignment/natdb/R/utility.R")

#.bennett.2016 <- function(...) {
#  dl_data <- read.csv(
#    suppdata(
#      x = "10.5061/dryad.hg578",
#     si = "Bennett et al 2016  J Ecol Competition trait data for dryad.csv"),
#    as.is = TRUE
#  )
  
  #metadata is column 1 - ID: identifying label for individual lizards (matched across datasets)
#  meta_data <- dl_data[, c(1, 3:5), drop = FALSE]
  
  
  # data is columns 2 and 3
#  my_data <-  dl_data[, c(2,6:13)]
#  colnames(my_data) <- c("Sex", "snout_vent_length")
#  my_data$species <- "anolis_sangrei" #species name
  #set the units for snout vent length
#  my_units <- c("mm","mm")
  
#  final_data <-  .df.melt(
#    x = my_data,
#    spp = "species",
#    units = my_units,
#    metadata = meta_data
#  )
  

#  return(dl_data)
#}

View(.bennett.2016()[2])

.bennett.2016 <- function (...){
  dl_data <- read.csv(
    suppdata(
      x = "10.5061/dryad.hg578",
      si = "Bennett et al 2016  J Ecol Competition trait data for dryad.csv"),
    as.is = TRUE
  )
  
  # Metadata is columns 1 (community ID number), 3 (treatment), 4 (pot code), 5(replicate)
  meta_data <- dl_data[,c(1,3:5)]
  
  # Data is columns 2 and 3 (sex and snout vent length of individual lizards) 
  my_data <-  dl_data[, c(2, 6:13)]                   # data object
  
  colnames(my_data) <- c("species", "avg_leaf_area", "specific_leaf_area",
                         "shoot_mass", "root_mass", "total_mass", 
                         "root-shoot_ratio", "avg_root_diameter", "specific_root_lenght")  # rename columns with variables
  
  my_units <- c("mm^2", "cm^2/g", "g", "g", "g", "NA", "mm", "cm/g")             # set the units for variables
  
  # Format the final object data and use return function
  final_data <-  .df.melt(                    
    x = my_data,
    spp = "species",
    units = my_units,
    metadata = meta_data
  )
  return(final_data)
}




