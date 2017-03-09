####################
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

.cariveau.2016 <- function(...){
  
  link = "http://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0151482.s003"
  file = "C:/crap/journal.pone.0151482.s003.XLSX"
  sheet = "TableS1_v2"
  data = read.xlsx(ft_get_si("10.1371/journal.pone.0151482", 3), sheetName=sheet)

  data = .df.melt(data, "species", c(NA, NA, NA, NA, NA, "#", "#", "#", "#", "mm","mm","mm","mm",NA,NA))
  
  return(data)
}


# Elton traits
# written by Sylvia
.wilman.2014a  <- function(...){
    data <- read.delim(ft_get_si("E095-178", "BirdFuncDat.txt", "esa_archives"))
    data <- data[,-c(23,34)]
    units <- sample(c("SpecID", "PassNonPass", "IOCOrder", "BLFamilyLatin", "BLFamilyEnglish", "BLFamSequID", "Taxo", "Scientific", "English", "Diet.Inv", "Diet.Vend", "Diet.Vect", "Diet.Vfish", "Diet.Vunk", "Diet.Scav", "Diet.Fruit", "Diet.Nect", "Diet.Seed", "Diet.PlantO", "Diet.5Cat", "Diet.Source", "Diet.Certainty", "ForStrat.watbelowsurf", "ForStrat.wataroundsurf", "ForStrat.ground", "ForStrat.understory", "ForStrat.midhigh", "ForStrat.canopy", "ForStrat.aerial", "PelagicSpecialist", "ForStrat.Source", "ForStrat.SpecLevel", "Nocturnal", "BodyMass.Value", "BodyMass.Source", "BodyMass.SpecLevel", "BodyMass.Comment", "Record.Comment"),length(names(data))-1,TRUE)
    return(.df.melt(data, "Scientific"))
}

.wilman.2014b  <- function(...){
    data <- read.delim(ft_get_si("E095-178", "MamFuncDat.txt", "esa_archives"))
    data <- data[,-c(1)]
    units <- sample(c("Scientific","MSWFamilyLatin","Diet.Inv","Diet.Vend","Diet.Vect","Diet.Vfish","Diet.Vunk","Diet.Scav","Diet.Fruit","Diet.Nect","Diet.Seed","Diet.PlantO","Diet.Source","Diet.Certainty","ForStrat.Value","ForStrat.Certainty","ForStrat.Comment","Activity.Nocturnal","Activity.Crepuscular","Activity.Diurnal","Activity.Source","Activity.Certainty","BodyMass.Value","BodyMass.Source","BodyMass.SpecLevel"),length(names(data))-1,TRUE)
    return(.df.melt(data, "Scientific"))
}

# panTHERIA
# written by Will during meeting, added by Sylvia
.jones.2009a <- function(...){
    data <- read.delim(ft_get_si("E090-184", "PanTHERIA_1-0_WR05_Aug2008.txt", "esa_archives"))
    for(i in 1:ncol(data))
        data[data[,i]==-999 | data[,i]=="-999",i] <- NA
    units <- sample(c("g","m^2"),length(names(data))-1,TRUE)
    data <- .df.melt(data, "MSW05_Binomial", units=units)
    return(data)
}

.jones.2009b <- function (...){
    data <- read.delim(ft_get_si("E090-184", "PanTHERIA_1-0_WR93_Aug2008.txt", "esa_archives"))
    for(i in 1:ncol(data))
        data[data[,i]==-999 | data[,i]=="-999",i] <- NA
    units <- sample(c("g","m^2"),length(names(data))-1,TRUE)
    data <- .df.melt(data, "MSW05_Binomial", units=units)
    return(data)
}

# high impact invaders
# written by Sylvia
.case.2016 <- function(...){
    data <- read.csv("http://datadryad.org/bitstream/handle/10255/dryad.103099/Traits.2Feb2015.csv?sequence=1", sep = ",", header = TRUE)
    #units <- length(names(data))
    data <- .df.melt(data, "species")
    return(data) 
}
