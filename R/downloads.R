####################
# ADD DOIs ##########
#####################
#Headers
#' @importFrom fulltext ft_get_si
#' @importFrom gdata read.xls
#' @importFrom utils read.csv
#' @importFrom testdat sanitize_text
# -- this last import must be removed, because testdat isn't on cran (devtools::install_github("ropensci/testdat"))
.wright.2004 <- function(...){
    raw <- read.xls("http://www.nature.com/nature/journal/v428/n6985/extref/nature02403-s2.xls", as.is=TRUE, skip=7)
    metadata <- data.frame(raw[,c("Code","Dataset","BIOME")],need_permission=TRUE)
    raw <- raw[,!names(raw) %in% c("Code","Dataset","BIOME","X","X.1","X.2","X.3","X.4","X.5","X.6")]
    raw$Species <- gsub(" ", "_", tolower(raw$Species))
    output <- .df.melt(raw, "Species", units=c(NA,NA,NA,NA,NA,"log10(mo)", "log10(g m-2)", "log10(%)", "log10(g m-2)", "log10(%)", "log10(%)", "log10(nmol g-1s-1)", "log10(micromol m-2s-1)", "?", "log10(nmol g-1s-1)", "log10(micromol m-2s-1)", "ppm"), metadata)
    class(output) <- "natdb"
    return(output)
}

.zanne.2014 <- function(...){
    wood <- read.csv(ft_get_si("10.5061/dryad.63q27.2", "GlobalWoodinessDatabase.csv"))
    phenol <- read.csv(ft_get_si("10.5061/dryad.63q27.2", "GlobalLeafPhenologyDatabase.csv"))
    output <- merge(wood, phenol, by.x="gs", by.y="Binomial", all.x=TRUE, all.y=TRUE)
    output$gs <- gsub(" ", "_", tolower(output$gs))
    metadata <- data.frame(output$woodiness.count); output <- output[,-3]
    return(.df.melt(output, "gs", metadata=metadata))
}

.hintze.2013 <- function(...){
    data <- read.csv("http://www.sciencedirect.com/science/MiamiMultiMediaURL/1-s2.0-S1433831913000218/1-s2.0-S1433831913000218-mmc1.txt/273233/html/S1433831913000218/6bd947d6c0ccb7edd11cd8bf73648447/mmc1.txt", sep=";", as.is=TRUE)
    data$metadata <- seq_len(nrow(data))
    data$name <- sapply(strsplit(tolower(sanitize_text(data$name)),split=" "), function(x) paste(x[1:2], collapse="_"))
    data <- data[,!names(data) %in% c("comment","family","citation_total","citation_prop_ane","citation_prop_dyso","citation_prop_endo","citation_prop_epi","citation_prop_hem","citation_prop_hydro","citation_prop_other")]
    return(.df.melt(data, "name"))
}

.bezeng.2015 <- function(...){
    data <- read.xls(ft_get_si("10.5061/dryad.6t7t6","Table%20S2.xls"), as.is=TRUE)
    metadata <- data.frame(GenBankAccession=sapply(strsplit(data$Species, "_"), function(x) x[3]))
    data$Species <- sapply(strsplit(data$Species, "_"), function(x) paste(x, collapse="_"))
    return(.df.melt(data, "Species",c(NA,"?","?",NA,NA,NA,NA,NA,NA),metadata))
}

.cariveau.2016 <- function(...){ 
    data <- read.xls(ft_get_si("10.1371/journal.pone.0151482", 3), sheet="TableS1_v2")
    metadata <- data[,c(1:3,6)]
    data$species <- with(data, tolower(paste(genus, species, sep="_")))
    data <- data[-c(1:4,6)]
    data <- .df.melt(data, "species", units=c(NA, NA, NA, NA, NA, "#", "#", "#", "#", "mm","mm","mm","mm",NA,NA), metadata)
    return(data)
}

# Elton traits
# written by Sylvia
.wilman.2014a  <- function(...){
    data <- read.delim(ft_get_si("E095-178", "BirdFuncDat.txt", "esa_archives"))
    data <- data[,-c(1,23,34)]
    units <- sample(c("NA", "NA", "NA", "NA", "NA", "Taxo", "NA", "NA", "%", "%", "%", "%", "%", "%", "%", "%", "%", "%", "NA", "Diet.Source", "NA", "%", "%", "%", "%", "%", "%", "%", "boolean", "NA", "boolean", "boolean", "NA", "NA", "boolean", "NA", "NA"),length(names(data))-1,TRUE)
    return(.df.melt(data, "Scientific"))
}
.wilman.2014b  <- function(...){
    data <- read.delim(ft_get_si("E095-178", "MamFuncDat.txt", "esa_archives"))
    data <- data[,-c(1)]
    units <- sample(c("NA","NA","%","%","%","%","%","%","%","%","%","%","NA","NA","NA","NA","NA","boolean","boolean","boolean","NA","NA","NA","NA","NA"),length(names(data))-1,TRUE)
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

.ingram.2016 <- function(...){
  color <- read.csv(ft_get_si("10.5061/dryad.9vr0c", "Dewlap_data_archive.csv"))
  color <- color[,-c(1,3,4,7:11)]
  units <- c('cm', 'cm^2')
  return(.df.melt(color, "Species", units=units))
}

.munoz.2014 <- function(...){
  data <- read.table(ft_get_si("10.5061/dryad.q39h2", "Munoz_2014_AmNat_Dryad.txt"), header=T)
  names(data) <- c('species', 'clade', 'island','latitude','longitude','elevation','svl')
  units <- c('degrees', 'degrees', 'm','mm', rep(NA, 3))
  data <- .df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.artacho.2015 <- function(...){
  data <- read.csv2(ft_get_si("10.5061/dryad.qg062", "phenotypictraits.csv"), sep=';')
  data <- data[,-c(1:5,10,12:13)]
  data$species <- 'Z.vivipara'
  units <- c('mm', 'mm', 'g','C', 'J/h')
  data$SVL <- as.numeric(data$SVL)
  data$TTL <- as.numeric(data$TTL)
  data$weight <- as.numeric(data$weight)
  data$PBT <- as.numeric(data$PBT)
  data$RMR <- as.numeric(data$RMR)
  return(.df.melt(data, "species", units=units))
}

.kolbe.2011 <- function(...){
  data <- read.table(ft_get_si("10.5061/dryad.1d24c","21%20species%20means.txt"),header=T,sep = '\t')
  units <- c(rep('mm',20))
  data<-.df.melt(data, "Species", units=units)
  data$character$units <- NA
  return(data)
  }

.winchell.2016 <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.h234n","winchell_evol_phenshifts.csv"))
  data <- data[,-c(1:2,15:16)]
  data$species <- 'A.cristatellus'
  data$perch.diam.cm <- as.numeric(data$perch.diam.cm)
  units <- c(rep('C',3),'%','cm','cm','g',rep('mm',15), rep(NA, 3))
  data<-.df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.kamath.2016 <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.9vk07","KamathLososEvol_AnolissagreiMorphAvg.csv"))
  data <- data[,-c(1)]
  data$species <- 'A.sagrei'
  units <- c('mm','NA','mm^2','mm',NA)
  data<-.df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.husak.2016 <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.2d960","HusakFergusonLovern_Anolis_training_diet_alldata.csv"))
  data <- data[,-c(1)]
  data$species <- 'A.carolinensis'
  units <- c('NA',rep('mm',4), rep('g',3),rep('s',2),'mm','%',rep('mg',4),'psi',rep('ng/mL',2),'NA','mg',rep('NA',4))
  data<-.df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

# high impact invaders
# written by Sylvia
# Konrad tried to update units, turns out they're not reported with the data or in the paper WTF!
.case.2016 <- function(...){
    data <- read.csv("http://datadryad.org/bitstream/handle/10255/dryad.103099/Traits.2Feb2015.csv?sequence=1", sep = ",", header = TRUE)
    #units <- length(names(data))
    data <- .df.melt(data, "species")
    return(data) 
}

# Will fix me please!!!
#issue with the setup
.Tian.2016 <- function(...){
  data <- read.xls("~/Documents/Programming/NerdClub/Trait_sheets/srep19703-s2.xls", as.is=TRUE, skip=XXX)
  for(i in 1:ncol(data))
    data[ifelse(is.na(data[,i]== "—" | data[,i]== "—§"), FALSE, data[,i]== "—" | data[,i]== "—§"),i] <- NA
  data[,-c(1,5,9,12)]
  data$Space <- NULL
  units <- c("sites", "species", "family", "IVI", "cm^2", "mg individual^-1", "Space","mm^2 mg^-1", "μm", "mm^2", "%", "Space", "μm", "%", "%", "Space", "classification", "Space", "Needle/Broad")
  data <- .df.melt(data, "plant_spp", units=units)
  return(data)
}

#M.A.Hagadorn: First Attempt, needs to be verified by Pearse
.Pearse.2014 <- function(...){
  data <- read.csv(ft_get_si("10.6084/m9.figshare.979288", 4), sep = ",", na.strings = c("","NA"))
  species <- rep(c("Carcinus_maenas"), nrow(data))
  data <- data.frame(species, data)
  metadata <- data[,c(2:3,8:15)]
  data <- data[,-c(2:3,8:15)]
  units <- c("species",rep(NA,7), "min", rep(NA,6))
  data <- .df.melt(data, "species", units=units, metadata)
  return(data)
}

.Simmons.2014 <- function(...){
  data <- read.xls(ft_get_si("10.5061/dryad.42pg7", "Simmons%20%26%20Buzatto%202013.xlsx"), as.is=TRUE)
  species <- rep(c("Onthophagus_taurus"), nrow(data))
  data <- data.frame(species, data)
  metadata <- data[,11]
  data <- data[,-c(10:11)]
  units <- c("species", rep(NA,2), "mg", "mm", "mg", rep(NA, 5))
  data <- .df.melt(data, "species", units, metadata)
  return(data)
}

.simpson.2015 <- function(...){
  data <- read.csv("http://datadryad.org/bitstream/handle/10255/dryad.99379/Plant%20trait%20data.csv?sequence=1")
  metadata <- data[,c(2:3)]
  data <- data[-c(2:3)]
  data$Species = gsub(" ","_",data$Species)
  data <- .df.melt(data, "Species", units = c(NA, NA, "m", "m", "g", "g", "g/g", "g/cm", "SA/vol", "kJ/g"), metadata)
  return(data)
}

.martin.2016 <- function(...){
  data <- read.csv("http://datadryad.org/bitstream/handle/10255/dryad.127965/Martin%20et%20al.%20Functional%20Ecology.txt?sequence=1", sep = "\t")
  data$species <- rep("Coffea_arabica", nrow(data))
  metadata <- data[,c(1:10,12)]
  data <- data[-c(1:10,12)]
  units = c("#","cm","cm","mm","mm","#","#","#","m^2","mm","m","mg","g","#","#","#",
            "#","#","#","#","#","#","#","#","g/m^2","#","g/cm^3","#","#","g/cm^2",
            "#","#","#","#","#", NA)
  data <- .df.melt(data, "species", units = units, metadata)
  return(data)
}
