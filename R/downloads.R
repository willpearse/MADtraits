#####################
# ADD DOIs ##########
#####################
#Headers
#' @importFrom fulltext ft_get_si
#' @importFrom gdata read.xls
#' @importFrom utils read.csv
#' @importFrom testdat sanitize_text
# -- this last import must be removed, because testdat isn't on cran (devtools::install_github("ropensci/testdat"))

#Deleted Kew because needs download (?...?)
.wright.2004 <- function(...){
    raw <- read.xls("http://www.nature.com/nature/journal/v428/n6985/extref/nature02403-s2.xls", as.is=TRUE, skip=7)
    raw$metadata <- with(raw, paste(Dataset,BIOME,sep="_"))
    raw <- raw[,!names(raw) %in% c("Code","Dataset","BIOME","X","X.1","X.2","X.3","X.4","X.5","X.6")]
    raw$Species <- gsub(" ", "_", tolower(raw$Species))
    output <- .df.melt(raw, "Species")
    class(output) <- "natdb"
    return(output)
}
.zanne.2014 <- function(...){
    wood <- read.csv("http://datadryad.org/bitstream/handle/10255/dryad.59002/GlobalWoodinessDatabase.csv?sequence=1")
    names(wood)[3] <- "metadata"
    phenol <- read.csv("http://datadryad.org/bitstream/handle/10255/dryad.59005/GlobalLeafPhenologyDatabase.csv?sequence=1")
    output <- merge(wood, phenol, by.x="gs", by.y="Binomial", all.x=TRUE, all.y=TRUE)
    output$gs <- gsub(" ", "_", tolower(output$gs))
    return(.df.melt(output, "gs"))
}
.hintze.2013 <- function(...){
    data <- read.csv("http://www.sciencedirect.com/science/MiamiMultiMediaURL/1-s2.0-S1433831913000218/1-s2.0-S1433831913000218-mmc1.txt/273233/html/S1433831913000218/6bd947d6c0ccb7edd11cd8bf73648447/mmc1.txt", sep=";", as.is=TRUE)
    data$metadata <- seq_len(nrow(data))
    data$name <- sapply(strsplit(tolower(sanitize_text(data$name)),split=" "), function(x) paste(x[1:2], collapse="_"))
    data <- data[,!names(data) %in% c("comment","family","citation_total","citation_prop_ane","citation_prop_dyso","citation_prop_endo","citation_prop_epi","citation_prop_hem","citation_prop_hydro","citation_prop_other")]
    return(.df.melt(data, "name"))
}
.bezeng.2015 <- function(...){
    data <- read.xls("http://datadryad.org/bitstream/handle/10255/dryad.84999/Table%20S2.xls?sequence=1", as.is=TRUE)
    data$metadata <- sapply(strsplit(data$Species, "_"), function(x) x[3])
    data$Species <- sapply(strsplit(data$Species, "_"), function(x) paste(x, collapse="_"))
    return(.df.melt(data, "Species"))
}

.gossner.2015 <- function(...){
  file<-tempfile()
  download.file("http://datadryad.org/bitstream/handle/10255/dryad.76638/ArthropodSpeciesTraits.txt?sequence=1",file)
  data <- read.delim(file)
  metaData <- data[,c(1:3)]
  data <- data[,-c(1:3,5,17)]
  units <- c("mm",rep(NA,10))
  data <- .df.melt(data,"SpeciesID",units=units, metadata)
}

.sherratt.2013<-function(...){
  file<-tempfile()
  download.file("https://datadryad.org/bitstream/handle/10255/dryad.47130/Ontogenetic%20allometry%20data.csv?sequence=1",file)
  data<-read.csv(file)
  data<-data[,-c(2:3,5)]
  units=c("mm","cm","cm")
  data<-.df.melt(data,"Species",units=units)
}
