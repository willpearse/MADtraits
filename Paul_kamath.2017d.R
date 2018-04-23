
# loading packages 

library(reshape2)
library(devtools)
install_github("willpearse/fulltext")
library(fulltext)
source('/path/to/natdb/R/utility.R')

#loading paper to get data from
data <- read.delim("~/Desktop/PanTHERIA_1-0_WR05_Aug2008.txt")
