####################
# ADD DOIs ##########
#####################
#Headers
#' @importFrom fulltext ft_get_si
#' @importFrom gdata read.xls
#' @importFrom utils read.csv
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

.ingram.2016 <- function(...){
  color <- read.csv('https://datadryad.org/bitstream/handle/10255/dryad.131337/Dewlap_data_archive.csv?sequence=1')
  color <- color[,-c(1,3,4,7:11)]
  units <- c('cm', 'cm^2')
  return(.df.melt(color, "Species", units=units))
}

.munoz.2014 <- function(...){
  data <- read.table('https://datadryad.org/bitstream/handle/10255/dryad.67309/Munoz_2014_AmNat_Dryad.txt?sequence=1', header=T)
  names(data) <- c('species', 'clade', 'island','latitude','longitude','elevation','svl')
  units <- c('degrees', 'degrees', 'm','mm', rep(NA, 3))
  data <- .df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.artacho.2015 <- function(...){
  data <- read.csv2('https://datadryad.org/bitstream/handle/10255/dryad.86947/phenotypictraits.csv?sequence=1', sep=';')
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
  data <- read.table('https://datadryad.org/bitstream/handle/10255/dryad.34389/21%20species%20means.txt?sequence=1',header=T,sep = '\t')  
  units <- c(rep('mm',20))
  data<-.df.melt(data, "Species", units=units)
  data$character$units <- NA
  return(data)
  }

.winchell.2016 <- function(...){
  data <- read.csv('https://datadryad.org/bitstream/handle/10255/dryad.115900/winchell_evol_phenshifts.csv?sequence=1')
  data <- data[,-c(1:2,15:16)]
  data$species <- 'anolis_cristatellus'
  data$perch.diam.cm <- as.numeric(data$perch.diam.cm)
  units <- c(rep('C',3),'%','cm','cm','g',rep('mm',15), rep(NA, 3))
  data<-.df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.kamath.2016 <- function(...){
  data <- read.csv('https://datadryad.org/bitstream/handle/10255/dryad.133489/KamathLososEvol_AnolissagreiMorphAvg.csv?sequence=1')
  data <- data[,-c(1)]
  data$species <- 'anolis_sagrei'
  units <- c('mm','NA','mm^2','mm',NA)
  data<-.df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.husak.2016 <- function(...){
  data <- read.csv('https://datadryad.org/bitstream/handle/10255/dryad.109876/HusakFergusonLovern_Anolis_training_diet_alldata.csv?sequence=1')
  data <- data[,-c(1)]
  data$species <- 'anolis_carolinensis'
  units <- c('NA',rep('mm',4), rep('g',3),rep('s',2),'mm','%',rep('mg',4),'psi',rep('ng/mL',2),'NA','mg',rep('NA',4))
  data<-.df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}


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

.klomp.2016 <- function(...){
  data <- read.csv('http://datadryad.org/bitstream/handle/10255/dryad.117914/draco_comparative%20data.csv?sequence=1')
  names(data) <- tolower(gsub("\\.", "_", names(data)))
  colnames(data) <- c('species', 'female_dewlap_area', 'male_dewlap_area', "sexual_dimorphism_in_dewlap_area", "sexual_size_dimorphism_svl", "sexual_dichromatism_chromatic_contrast",'sexual_dichromatism_achromatic_contrast', 'female_dewlap_chromatic_contrast', 'male_dewlap_chromatic_contrast','female_dewlap_achromatic_contrast', 'male_dewlap_achromatic_contrast', 'light_level_auc','predation_category')
  data$species <- tolower(gsub(" ", "_", (data$species)))
  data$species <- gsub("\\(", "", (data$species))
  data$species <- gsub("\\)", "", (data$species))
  units <- c(rep('cm^2',2), 'cm^2 (nl)', 'cm', rep('JND', 6), 'nl', NA)
  data <- .df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.dmitriew.2014 <- function(...){
  data <- read.csv('http://datadryad.org/bitstream/handle/10255/dryad.72954/DataDmitriew%26BlanckenhornJEB12488%202014.csv?sequence=1',sep=';')
  data$species <- 'sepsis_punctum'
  data <- data[,-c(4)]
  colnames(data) <- c('population', 'food', 'sex', 'family', "sire",'dam','development_time','fore_femur_width','mittibial_length', 'species')
  units <- c(rep(NA, 3), 'days', 'mm','mm',rep(NA,4))
  data <- .df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.zhang.2014 <- function(...){
  data <- read.xls('http://datadryad.org/bitstream/handle/10255/dryad.73881/data_traits.xlsx?sequence=1')
  data <- data[,-c(1:2,5:6,8:11,16)]
  data$species <- 'sterna_hirundo'
  colnames(data) <- c('age', 'sex', 'lifespan', 'egg_volume', 'clutch_size', 'brood_size', 'no_fledglings', 'species')
  units <- c('years',NA,'years', rep(NA,3))
  data <- .df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

