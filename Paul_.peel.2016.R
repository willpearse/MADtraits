library(reshape2)
library(natdb)
library(tidyverse)
library(suppdata)
library(fulltext)

source('/Users/carrie/Documents/GitHub/natdb/R/utility.R') #change filepath to where your file is

.peel.2016 <- function(...) { 
  dp_data <- read.csv(suppdata(x="10.5061/dryad.2fp34",si = "Eidolon data 2007_2014_Openaccess.csv"),as.is=TRUE)
  ###Create meta data object------------------------------------------------------------------------------------------------------
  my_metadata <- as.data.frame(dp_data[c(1:12, 23:24, 26, 31, 33)])
  colnames(my_metadata) <- c("Sample", "SamplingEventID", "Samplers", "Samplingdate", "Birthdate", "MthsSinceBirthdate", "Cont_Island", 
                             "Country", "Region", "Location","Latitude", "Longitude", "MotherID", "OffspringID", "BandNo", "GeneticsID",
                             "GenBankAccession")
  ### Create data object------------------------------------------------------------------------------------------------------
  my_data <- dp_data[c(13:22, 25, 27:30, 32, 34:69)] 
  ### Rename columns ------------------------------------------------------------------------------------------------------
  colnames(my_data) <- c("BatWt","Sex","Age", "AgeSpecific", "AgeTooth", "TeethCertaintyScore", "TeethAgeRange", "TeethAgeMths", "AgeMths", 
                         "ReproStatus", "Forearm", "LagosBatVirusScore", "HenipavirusResults", "AchimotaVirus1", "AchimotaVirus2", 
                         "CytochromeB", "T1AlleleSize", "T2AlleleSize", "S1AlleleSize", "S2AlleleSize", "F1AlleleSize", "F2AlleleSize", 
                         "W1AlleleSize", "W2AlleleSize", "N1AlleleSize", "N2AlleleSize", "Q1AlleleSize", "Q2AlleleSize","X1AlleleSize", 
                         "X2AlleleSize", "P1AlleleSize", "P2AlleleSize", "K1AlleleSize", "K2AlleleSize", "Ac1AlleleSize", "Ac2AlleleSize", 
                         "Af1AlleleSize", "Af2AlleleSize", "Ai1AlleleSize", "Ai2AlleleSize", "Ad1AlleleSize", "Ad2AlleleSize", 
                         "Y1AlleleSize", "Y2AlleleSize", "Ag1AlleleSize", "Ag2AlleleSize", "Ah1AlleleSize", "Ah2AlleleSize", 
                         "B1AlleleSize","B2AlleleSize", "M1AlleleSize", "M2AlleleSize")
  ### Add species name column------------------------------------------------------------------------------------------------------
  my_data$species <- "eidolon_helvum" # species names
  ### Add units column ------------------------------------------------------------------------------------------------------
  my_units <- c("g", NA, NA, NA, NA, "mo", "mo", NA, "mm", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  
  ### df.melt ------------------------------------------------------------------
  to_return_data <- .df.melt(x = my_data, spp = "species", units = my_units, metadata = my_metadata)
  return(to_return_data)
}

.peel.2016()