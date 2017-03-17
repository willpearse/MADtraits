####################
# ADD DOIs ##########
#####################
#Headers
#' @importFrom fulltext ft_get_si
#' @importFrom gdata read.xls
#' @importFrom utils read.csv read.csv2 read.delim read.table
#' @importFrom testdat sanitize_text
# -- this last import must be removed, because testdat isn't on cran (devtools::install_github("ropensci/testdat"))

.mesquita.2016 <- function(...){
  data <- read.table('http://datadryad.org/bitstream/handle/10255/dryad.108670/Data%20%28revised%29.txt?sequence=1', header=T, sep='\t')
  data <- data[,-c(2:4,7,9:10,12:13,17,25)]
  data$Species <- tolower(gsub(" ", "_", (data$Species)))
  colnames(data) <- c('species','longitude','latitude','average_female_adult_weight','mean_female_svl_adults','female_svl_at_maturity','offspring_svl','mean_clutch_size','mode_of_reproduction','clutch_per_year','clutch_frequency','relative_clutch_mass','foraging_mode','distribution','prefered_habitat_type')
  units <- c(rep(NA,2),'g', rep('mm',3),rep(NA,3),'g',rep(NA,5))
  test <- .df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.Broeckhoven.2016 <- function(...){
  data <- read.xls('http://datadryad.org/bitstream/handle/10255/dryad.124881/Data%20for%20Dryad.xlsx?sequence=1', skip=1)
  data <- data[,-c(23:102)]
  data$Species <- tolower(gsub(" ", "_", (data$Species)))
  names(data) <- tolower(gsub("\\.", "_", names(data)))
  units <- c(rep('mm',21))
  test <- .df.melt(data, "species", units=units)
  return(data)
}

.rutschmann.2016 <- function(...){
  data <- read.csv('http://datadryad.org/bitstream/handle/10255/dryad.102425/Rutschmann_AdaptationofPhenologyacrossPopulations_dataset.csv?sequence=1',sep=';')
  names(data) <- tolower(gsub("X\\.", "", names(data)))
  data <- data[,-c(1:3)]
  data$species <- 'zootoca_vivipara'
  colnames(data) <- c('altitude','svl','weight','parturition_year', 'parturition_day', 'mid_gestation_temperatures', 'forest_cover_coefficient','mountain_chain','species')
  units <- c('m','mm','g',NA,NA,'°',NA,NA)
  data <- .df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.kuo.2014 <- function(...){
  data <- read.csv('http://datadryad.org/bitstream/handle/10255/dryad.70378/personality_date1.csv?sequence=1')
  data <- data[,-c(1:4,16)]
  colnames(data) <- c('diet','svl','latency','perch_inspected','percent_time_newzone','percent_time_onperch','bite_force','tail_diameter', 'personality_principal_component_1','personality_principal_component_2','transformed_principal_component_1')
  data$species <- 'anolis_sagrei'
  units <- c('cm','min',NA,rep('%',2), 'N','cm',rep(NA,4))
  data <- .df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

# .klomp.2016 <- function(...){
#   data <- read.csv('http://datadryad.org/bitstream/handle/10255/dryad.117914/draco_comparative%20data.csv?sequence=1')
#   names(data) <- tolower(gsub("\\.", "_", names(data)))
#   
#   colnames(data) <- c('species', 'female_dewlap_area', 'male_dewlap_area_', sexual_dimorphism_in_dewlap_area__natural_logged_ sexual_size_dimorphism__svl_ sexual_dichromatism__chromatic_contrast_
#   data <- data[,-c(1:4,16)]
#   colnames(data) <- c('diet','svl','latency','perch_inspected','percent_time_newzone','percent_time_onperch','bite_force','tail_diameter', 'personality_principal_component_1','personality_principal_component_2','transformed_principal_component_1')
#   data$species <- 'anolis_sagrei'
#   units <- c('cm','min',NA,rep('%',2), 'N','cm',rep(NA,4))
#   data <- .df.melt(data, "species", units=units)
#   data$character$units <- NA
#   return(data)
# }

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

#KW first attempt to be verified
.bellobedoy.2015a <- function(...){
    data <- read.csv(
        ft_get_si("10.6084/m9.figshare.1190766.v2","Mating success H americana.csv")
        )
    data$species <- "hetaerina_americana"
    metadata <- data.frame(id=data$id)
    data$id <- NULL
    return(.df.melt(data, "species", c(NA, "#", "mm^2 ", "mm^2", "mm"), metadata))
}
.bellobedoy.2015b <- function(...){
    data <- read.xls(
        ft_get_si("10.6084/m9.figshare.1190766.v2","mating occurrencePzoe_2010.xls")
        )
    data$species <- "paraphlebia zoe"
    metadata <- data.frame(data$Individual)
    data$Individual <- NULL
    levels(data$Status) <- c("satellite.male", "territorial.male")
    return(.df.melt(data, "species", c(NA, NA, "mm", "mm", "?", "?", "log10(?)"), metadata))
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
  data$species <- 'zootoca_vivipara'
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
  data$species <- 'anolis_cristatellus'
  data$perch.diam.cm <- as.numeric(data$perch.diam.cm)
  units <- c(rep('C',3),'%','cm','cm','g',rep('mm',15), rep(NA, 3))
  data<-.df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.kamath.2016 <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.9vk07","KamathLososEvol_AnolissagreiMorphAvg.csv"))
  data <- data[,-c(1)]
  data$species <- 'anolis_sagrei'
  units <- c('mm','NA','mm^2','mm',NA)
  data<-.df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.husak.2016 <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.2d960","HusakFergusonLovern_Anolis_training_diet_alldata.csv"))
  data <- data[,-c(1)]
  data$species <- 'anolis_carolinensis'
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
#.Tian.2016 <- function(...){
#  data <- read.xls("~/Documents/Programming/NerdClub/Trait_sheets/srep19703-s2.xls", as.is=TRUE)
#  for(i in 1:ncol(data))
#    data[ifelse(is.na(data[,i]== "â" | data[,i]== "âÂ§"), FALSE, data[,i]== "â" | data[,i]== "âÂ§"),i] <- NA
#  data[,-c(1,5,9,12)]
#  data$Space <- NULL
#  units <- c("sites", "species", "family", "IVI", "cm^2", "mg individual^-1", "Space","mm^2 mg^-1", "Î¼m", "mm^2", "%", "Space", "Î¼m", "%", "%", "Space", "classification", "Space", "Needle/Broad")
#  data <- .df.melt(data, "plant_spp", units=units)
#  return(data)
#}

.pearse.2014 <- function(...){
  data <- read.csv(ft_get_si("10.6084/m9.figshare.979288", 4), sep = ",", na.strings = c("","NA"))
  species <- rep(c("Carcinus_maenas"), nrow(data))
  data <- data.frame(species, data)
  metadata <- data[,c(2:3,8:15)]
  data <- data[,-c(2:3,8:15)]
  units <- c(rep(NA,7), "min", rep(NA,6))
  data <- .df.melt(data, "species", units=units, metadata)
  return(data)
}

.simmons.2014 <- function(...){
  data <- read.xls(ft_get_si("10.5061/dryad.42pg7", "Simmons%20%26%20Buzatto%202013.xlsx"), as.is=TRUE)
  species <- rep(c("Onthophagus_taurus"), nrow(data))
  data <- data.frame(species, data)
  metadata <- data[,11]
  data <- data[,-c(10:11)]
  units <- c(rep(NA,2), "mg", "mm", "mg", rep(NA, 5))
  data <- .df.melt(data, "species", units, metadata)
  return(data)
}

.grootemaat.2015 <- function(...){
  data <- read.xls(ft_get_si('10.5061/dryad.m41f1', 'Grootemaat%202015_FE_Dryad.xlsx'), sheet ='RAWdata', as.is = TRUE)
  data <- data[-c(1),]
  metadata <- data[,c(1,3)]
  data <- data[,-c(1,3)]
  units <- c(rep('mm',4), rep('g',2), rep('cm^2',2), 'cm^3', '1/cm', rep('g/cm^3',2), "odw", "cm^2/g", rep("%",4), 'NA', rep('s',4), 'NA', 'NA')
  data <- .df.melt(data, "species", units, metadata)
  return(data)
}

.arnold.2016 <- function(...){
  data <- read.xls(ft_get_si("10.5061/dryad.t3d52", "Arnold_etal_2016_functecol_dataset.xlsx"), as.is = TRUE, skip = 3)
  species <- rep(c("Tribolium_castaneum"), nrow(data))
  data <- data.frame(species, data)
  metadata <- data[,c(2:7,24:25)]
  data <- data[,-c(2:7,23:25)]
  units <- c("µL CO2/h", "counts/h", "mg", rep("mm",7), rep("mm/s",2), rep("count",3), "NA", "NA", "NA", "NA", "days", "NA", "NA", "NA")
  data <- .df.melt(data, "species", units, metadata)
  return(data)
}

.cariveau.2016 <- function(...){
  
  link = "http://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0151482.s003"
  file = "C:/crap/journal.pone.0151482.s003.XLSX"
  sheet = "TableS1_v2"
  data = read.xlsx(ft_get_si("10.1371/journal.pone.0151482", 3), sheetName=sheet)
}

.simpson.2015 <- function(...){
  data <- read.csv("http://datadryad.org/bitstream/handle/10255/dryad.99379/Plant%20trait%20data.csv?sequence=1")
  metadata <- data[,c(2:3)]
  data <- data[-c(2:3)]
  data$Species <- gsub(" ","_",data$Species)
  data <- .df.melt(data, "Species", units = c(NA, NA, "m", "m", "g", "g", "g/g", "g/cm", "SA/vol", "kJ/g"), metadata)
  return(data)
}

.martin.2016 <- function(...){
  data <- read.csv("http://datadryad.org/bitstream/handle/10255/dryad.127965/Martin%20et%20al.%20Functional%20Ecology.txt?sequence=1", sep = "\t")
  data$species <- rep("Coffea_arabica", nrow(data))
  metadata <- data[,c(1:10,12)]
  data <- data[-c(1:10,12)]
  units <- c("#","cm","cm","mm","mm","#","#","#","m^2","mm","m","mg","g","#","#","#",
            "#","#","#","#","#","#","#","#","g/m^2","#","g/cm^3","#","#","g/cm^2",
            "#","#","#","#","#", NA)
  data <- .df.melt(data, "species", units = units, metadata)
  return(data)
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
  units <- c("mm","cm","cm")
  data<-.df.melt(data,"Species",units=units)
}

.mccullough.2015<-function(...){
  file<-tempfile()
  download.file("http://www.sciencedirect.com/science/MiamiMultiMediaURL/1-s2.0-S0003347215003103/1-s2.0-S0003347215003103-mmc1.xlsx/272524/html/S0003347215003103/0bb76368c8bbec26cf11858140abe3e8/mmc1.xlsx",file)
  data<-read.xls(xls=file)
  
  data<-read.xls(xls="C:/Users/water/Downloads/mmc1.xlsx")
  units<-c("mm","mm")
  data<-.df.melt(data,"Species",units=units)
}

.anderson.2015(...){
  file<-tempfile()
  download.file("http://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0166714.s002",file)
  data<-read.csv(file)
  metadata<-data[,c(2:4,44:46)]
  data<-data[,-c(2:4,44:46)]
  units<-c("cm","g","cm^2",rep("?",4),rep("cm^2",7),"?",rep("g",7),rep("mg/g",7),rep("cm^2/g",7),rep("g",4))
  data<-.df.melt(data,"Species",units=units,metadata=metadata)
}

.plourde.2014(...){
  file<-tempfile()
  download.file("http://datadryad.org/bitstream/handle/10255/dryad.65737/complete.individual.data.txt?sequence=1",file)
  data<-read.delim(file)
  metadata<-data[,c(1:3,14,16)]
  data<-data[,-c(1:3,14,16,31:39)]
  units<-c("g","cm^3",rep("g",2),"cm^3",rep("g",2),"cm^3","g",rep("cm",8),rep("g/cm^3",3),rep("cm^2",4))
  data<-.df.melt(data,"species",units=units,metadata=metadata)
}

.buzzard.2015 <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.s8f38", "FEBuzzardSpTraits.csv"), sep = ",", as.is = TRUE, na.strings = c("","NA"))
  data <- unite(data, species, genus, species, remove = FALSE)
  metadata <- data[,c(1:2,4:9,16:18)]
  data <- data[,-c(1:2,4:9,16:18)]
  units <- c("NA", "cm^2 g^-1", "NA", "mg g-1", rep("NA",3),"years", rep("NA", 5), "mg ha^–1", "?", rep("NA",3))
  data <- .df.melt(data, "species", units, metadata)
  return(data)
}

.philipson.2014 <- function(...){
  data <- read.delim(ft_get_si("10.5061/dryad.qn814", "EcologyEvolution_IntensivePlotsData_forDryad.txt"), sep = ",", as.is = TRUE)
  species <- tolower(gsub(" ", "_", data$Species, ignore.case = TRUE))
  data <- data.frame(species, data[,-1])
  metadata <- data[,c(2, 9:12)]
  data <- data[,-c(2, 9:12)]
  units <- c("%", "mm", "mm", "cm", "day_since_transplant", "survival_since_transplant", rep("NA", 4), "categorical_measure_occasion")
  data <- .df.melt(data, "species", units, metadata)
  return(data)
}

#KW attempt 2
.Aubret.2012a <- function(...){
  data <- read.xls(ft_get_si("10.5061/dryad.14cr5345", "Aubret%2053172.xlsx"), as.is=TRUE, sheet=1)
  data$species <- "notechis_scutatus"
  metadata <- data.frame(data$POPULATION, data$DATE)
  data$POPULATION <- NULL
  data$DATE <- NULL
  names(data) <- c("sex", "body_mass", "body_mass", "snout_vent_length", "snout_vent_length", "species")
  units <- c(NA, "g", "log10(g)", "cm", "log10(cm)")
  return(.df.melt(data, "species", units, metadata))
}

#KW
#.Aubret.2012b <- function(...){
#  data <- read.xls(ft_get_si("10.5061/dryad.14cr5345", "Aubret%2053172.xlsx"), as.is=TRUE, sheet=2)
#  data$X <- data$X.1 <- data$X.2 <- data$X.3 <- NA <- NA <- NA <- NA <- NA <- NA <- NA <- NA  NA <- NULL
#  data$species <- "notechis_scutatus"
#  metadata <- data[,c("POPULATION", "MOTHER", "BIRTH.DATE")]
#  data <- data[,!names(data) %in% names(metadata)]
#  names(data) <- c("total_litter_body_mass", "total_litter_body_mass", "total_litter_snout_vent_length", "total_litter_snout_vent_length", "total_litter_jaw_length", "total_litter_jaw_length")
#  units <- c("mean(g)", "log10(mean(g))", "mean(cm)", "log10(mean(cm))", "mean(cm)", "log10(mean(cm))")
#  return(.df.melt(data, "species", units, metadata))
#}

#.Aubret.2012c <- function(...){
#  data <- read.xls(ft_get_si("10.5061/dryad.14cr5345", "Aubret%2053172.xlsx"), as.is=TRUE, sheet=3)
#  names(data) <- c("genus_species", )
#  units <- c(NA, "g", "log10(g)", "cm", "log10(cm)")
#  return(.df.melt(data, "species", units, metadata))
#}

# Elton traits
.wilman.2014a  <- function(...){
    data <- read.delim(ft_get_si("E095-178", "BirdFuncDat.txt", "esa_archives"))
    data <- data[,-c(23,34)]
    units <- sample(c("SpecID", "PassNonPass", "IOCOrder", "BLFamilyLatin", "BLFamilyEnglish", "BLFamSequID", "Taxo", "Scientific", "English", "Diet.Inv", "Diet.Vend", "Diet.Vect", "Diet.Vfish", "Diet.Vunk", "Diet.Scav", "Diet.Fruit", "Diet.Nect", "Diet.Seed", "Diet.PlantO", "Diet.5Cat", "Diet.Source", "Diet.Certainty", "ForStrat.watbelowsurf", "ForStrat.wataroundsurf", "ForStrat.ground", "ForStrat.understory", "ForStrat.midhigh", "ForStrat.canopy", "ForStrat.aerial", "PelagicSpecialist", "ForStrat.Source", "ForStrat.SpecLevel", "Nocturnal", "BodyMass.Value", "BodyMass.Source", "BodyMass.SpecLevel", "BodyMass.Comment", "Record.Comment"),length(names(data))-1,TRUE)
    return(.df.melt(data, "Scientific"))
}
  
  ## WIP
.winchell.2016 <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.h234n","winchell_evol_phenshifts.csv"))
  data <- data[,-c(1:2,15:16)]
  data$species <- 'A.cristatellus'
  data$perch.diam.cm <- as.numeric(data$perch.diam.cm)
  units <- c(rep('C',3),'%','cm','cm','g',rep('mm',15), rep(NA, 3))
  return(.df.melt(data, "species", units=units))
}

.yin.2015 <- function(...){
  data <- read.xls(ft_get_si("10255/dryad.86209","Species-XylemAnatomy.xlsx"), fileEncoding="UTF-8")
  metaData <- data[,2]
  data <- data[,-c(2,13)]
  units <- c(NA, NA, rep('um',6), NA, NA, NA)
  return(.df.melt(data,"Species",units=units, metadata))
}

.shibata.2015a <- function(...){
  data <- read.xls(ft_get_si("10.5061/dryad.rj480","FEShibataDataForAnalyses.xls"), fileEncoding="UTF-8")
  metadata <- data[,14]
  data <- data[,-c(14)]
  units <- c(rep('%',6), NA, NA, 'cm^2', 'g m^-2', 'MN m-2', '%', 'g cm^-3')
  return(.df.melt(data,"Species",units=units, metadata))
}

.shibata.2015b <- function(...){
  data <- read.xls(ft_get_si("10.5061/dryad.rj480","FEShibataDataForAnalyses.xls"), fileEncoding="UTF-8", sheet=2)
  metadata <- data[,15]
  data <- data[,-c(15)]
  units <- c(NA, NA, rep('cm^2',3),'g cm^-3', 'g m^-2', 'MN m^-2', rep('%',5), NA)
  return(.df.melt(data,"Species",units=units, metadata))
}

.cavender.2015a <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.855pg","Freezing.vulnerability.csv"), fileEncoding="UTF-8")
  data <- data[1:758,]
  metadata <- data[,c(2,3)]
  data <- data[,-c(2,3)]
  units <- c('C', 'C', '%', '%')
  return(.df.melt(data,"Species",units=units, metadata))
}

.cavender.2015b <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.855pg","Tree.height.csv"))
  data <- data[1:109,1:8]
  metadata <- data[,2:6]
  data <- data[,-c(2:6)]
  units <- c("m", 'cm')
  return(.df.melt(data,"Species",units=units, metadata))
}

#principal components are metadata
.cavender.2015c <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.855pg","PCA_All_Virentes.csv"))
  data <- data[1:109,1:8]
  metadata <- data[,2:6]
  data <- data[,-c(2:6)]
  units <- c("m", 'cm')
  return(.df.melt(data,"Species",units=units, metadata))
}

.limpens.2013a <- function(...){
  data <- read.xls(ft_get_si("doi:10.5061/dryad.926nd", "Motherfile.xls"), as.is = TRUE)
  species <- gsub("gla", "picea_glauca", data$Species)
  species <- gsub("mar", "picea_mariana", species)
  species <- gsub("rub", "picea_rubens", species)
  species <- gsub("sit", "picea_sitchensis", species)
  species <- gsub("ban", "pinus_banksiana", species)
  species <- gsub("nig", "pinus_nigra", species)
  species <- gsub("syl", "pinus_slyvestris", species)
  data <- data.frame(species, data[,-3])
  metadata <- data[,c(2:4)]
  data <- data[,-c(2:4)]
  units <- c(rep("cm",2), "mm day^-1", "mg", rep("cm",2), "mm day^-1", "mg", "cm/cm", "mg/mg", "mg", rep("NA",3))
  data <- .df.melt(data, "species", units, metadata)
  return(data)
}

.fitzgerald.2017 <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.67140", "traits.csv"), sep = ",", as.is = TRUE)
  species <- tolower(gsub(".", "_", data$species, fixed = TRUE))
  species <- gsub("sp_", "sp.", species)
  data <- data.frame(species, data[,-1])
  data <- data[,-c(2, 9:12)]
  units <- c(rep("log_mm", 5), "sqrt_mm", "cubert_mm", "mm", "sqrt_mm", "mm", "sqrt_mm", "mm", "inverse_mm", "log_mm", "mm", "mm", "log_mm", rep("mm", 3), "log_mm", rep("mm", 4), "inverse_mm", "log_mm", "cubert_mm", rep("sqrt_mm",2), "cubert_mm", rep("mm",2), "inverse_mm", rep("sqrt_mm",2), "mm", "sqrt_mm", rep("fourthroot_mm",2), "mm", "log_mm", rep("mm",3))
  data <- .df.melt(data, "species", units)
  return(data)
}
