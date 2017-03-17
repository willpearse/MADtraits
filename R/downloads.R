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

.grutters.2017<-function(...){
  link <- "http://datadryad.org/bitstream/handle/10255/dryad.128398/FE-2016-00091-Data-plant-traits-and-consumption.xlsx?sequence=1"
  data<-read.xls(link, na.strings=c(""," ","NA"))
  vars <- c("species", "native", "clade", "latitude_origin", "acquisition", "shared_pomacea", "consumption_rate_lymnaea", "shared_lymnaea",
           "consumption_rate_pomacea","c","n","p","dmc","total_phenolics","cn","cp","np","n_total_pheno")
  data <- data[-c(1),] #remove row with unit names
  data$plantScientificName = gsub(" ","_",data$plantScientificName) #replace spaces with underscore
  data$plantScientificName = gsub("-","_",data$plantScientificName) #replace hyphens with underscore
  data$plantScientificName <- sapply(strsplit(data$plantScientificName, "_"), function(x) paste(x[1:2], collapse="_"))
  data$plantScientificName = tolower(data$plantScientificName)
  colnames(data)<-vars
  metadata <- data[,c(2:6,8)]
  data <- data[,-c(2:6,8)]
  units <- c("mg g^-1 d^-1","mg g^-1 d^-1","percent_dry_weight", "percent_dry_weight",
            "percent_dry_weight", "g g^-1", "mg g^-1", "mol mol^-1","mol mol^-1","mol mol^-1", "mg g^-1 (mg g^-1)^-1")
  for(i in 2:ncol(data)){
    data[,i] = as.numeric(data[,i])
  }
  data <- .df.melt(data, species="species",units=units, metadata = metadata)
  return(data)
}

.molinari.2014<-function(...){
  link <- "http://datadryad.org/bitstream/handle/10255/dryad.55089/Dryad_Final.xlsx?sequence=1"
  data <- read.xls(link)
  vars = c("species", "native", "life_form","abundance_2007","abundance_2008", "peak_flowering", "height", "seed_mass", "specific_leaf_area")
  colnames(data) = vars
  data$species = tolower(gsub(" ","_",data$species))
  metadata = data[,c(4:5)]
  data = data[,-c(4:5)]
  units = c(NA,NA,"month","m","mg","cm^2/g")
  data = .df.melt(data, "species",units,metadata)
  return(data)
}

.anderson.2015 <- function (...){
  file<-tempfile()
  download.file("http://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0166714.s002",file)
  data<-read.csv(file)
  metadata<-data[,c(2:4,44:46)]
  data<-data[,-c(2:4,44:46)]
  units<-c("cm","g","cm^2",rep("?",4),rep("cm^2",7),"?",rep("g",7),rep("mg/g",7),rep("cm^2/g",7),rep("g",4))
  data<-.df.melt(data,"Species",units=units,metadata=metadata)
}

.plourde.2014 <- function (...){
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

.mesquita.2015 <- function(...){
    data <- read.delim(unzip(ft_get_si("E096-058-D1","Life_history_data_of_lizards_of_the_world.txt.zip"), "Data files/Data (revised).txt"))
    metadata <- data[,c("Species","Genus","Family","Population","Longitude","Latitude","Source","Sample.Size.Female.adult.weight","Sample.size.Mean.F.SVL.adults","Sample.size.Clutch.Size.")]
    data <- data[,!names(data) %in% names(metadata)]
    data$Species.1 <- tolower(gsub(" ", "_", data$Species.1))
    return(.df.melt(data, "Species.1", c("g", "g", "mm", "mm", "mm", "mm", "#", NA, "#", NA, "cc/g", NA, NA, NA), metadata))
}

#gossner.2015 <- function(...){
#    data <- read.delim(ft_get_si("E096-102", "HeteropteraMorphometricTraitsRAW.txt", "esa_archives"), as.is=TRUE, fileEncoding="UTF-8")
#}

.falster.2015 <- function(...){
    data <- read.csv(unzip(ft_get_si("E096-128","baad_data.zip", "esa_archives"), "baad_data/baad_data.csv"))
    # Note: units "m^2/m^2 are taken from the meta-data
    units <- c("NA","mm","deg","NA","m^2/m^2","NA","NA","NA","NA","yr","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m^2","m","m","m","m","m","m","m","kg","kg","kg","kg","kg","kg","kg","kg","kg","kg","kg","m^2","kg/m^2","kg/m^3","kg/m^3","kg/m^3","kg/m^3","kg/kg","kg/kg","kg/kg","kg/kg","kg/kg","kg/kg")
    metadata <- data[,c(c("studyName","location","latitude","longitude","species","family"))]
    data$speciesMatched <- tolower(gsub(" ", "_", data$speciesMatched))
    data <- data[,!names(data) %in% names(metadata)]
    return(.df.melt(data, "speciesMatched", units, metadata))
}

.kelt.2015 <- function(...){
    data <- read.delim(ft_get_si("E096-155","Mammal_Home_Ranges.txt", "esa_archives"))
    names(data) <- c("taxon","order","family","trophic_group","body_mass","home_range","references","notes")
    data$taxon <- tolower(gsub(" ", "_", data$taxon))
    metadata <- data[,c("order","family","references","notes")]
    data <- data[,!names(data) %in% names(metadata)]
    return(.df.melt(data, "taxon", c(NA,"log10(g)","log10(ha)"), metadata))
}

.edwards.2015a <- function(...){
    data <- read.csv(ft_get_si("E096-202", "Table1.csv", "esa_archives"))
    metadata <- data[,c("isolate","taxon","synonym", "c_citation")]
    data <- data[,!names(data) %in% names(metadata)]
    units <- c(NA, "°C", "µmol photons m-2 s-1", "hr", "µm^3", "µm^3", "µmol cell-1", "day^-1", "day^-1", "µmol L-1", "µmol L-1", "µmol N cell-1 day-1", "µmol N µmol C-1 day -1", "µmol N cell-1", "µmol N µmol C-1", "µmol N cell-1", "µmol N µmol C-1", "day-1", "day-1", "µmol L-1", "µmol L-1", "µmol N cell-1 day-1", "µmol N µmol C-1 day-1", "µmol N cell-1", "µmol N µmol C-1", "µmol N cell-1", "µmol N µmol C-1", "day-1", "day-1", "µmol L-1", "µmol L-1", "µmol P cell-1 day-1", "µmol P µmol C-1 day-1", "µmol P cell-1", "µmol P µmol C-1", "µmol P cell-1", "µmol P µmol C-1", "citation")
    return(.df.melt(data, "species", units, metadata))
}
.edwards.2015b <- function(...){
    data <- read.csv(ft_get_si("E096-202", "Table3.csv", "esa_archives"))
    metadata <- data[,c("isolate","volume_citation")]
    data$isolate <- data$volume_citation <- NULL
    data$species <- tolower(gsub(" ", "_", data$species))
    return(.df.melt(data, "species", "µm3", metadata))
}

.albouy.2015 <- function(...){
    data <- read.csv(unzip(ft_get_si("E096-203","Functional_data.zip", "esa_archives"), "Functional_data.csv"), sep=";")
    for(i in 11:17)
        data[,i] <- as.logical(data[,i])
    names(data)[18:19] <- c("minimum_depth", "maximum_depth")
    metadata <- data[,c("id","Super_class","Order","Family","Genus")]
    data <- data[,!names(data) %in% names(metadata)]
    data$Species <- tolower(gsub("_", " ", data$Species))
    return(.df.melt(data, "Species", c(NA,"cm","cm",rep(NA,8),"m","m",rep(NA,9)), metadata))
}

.yin.2015 <- function(...){
  data <- read.xls(ft_get_si("10255/dryad.86209","Species-XylemAnatomy.xlsx"), fileEncoding="UTF-8")
  metaData <- data[,2]
  data <- data[,-c(2,13)]
  units <- c(NA, NA, rep('um',6), NA, NA, NA)
  return(.df.melt(data,"Species",units=units, metadata))
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
  units <- c('years',NA,'years', rep(NA,4))
  data <- .df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
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
  metadata <- data[,c(1,2,4:9,11:18)]
  data <- data[,-c(1,2,4:9,11:18)]
  names <- data['Species']
  for(i in 1:17){
    p <- c('BR', 'FU', 'GE', 'MN', 'OL', 'SA','VI','FU2', 'OL2', 'SA2', 'VI2', 'VI3', 'MN2', 'VI4', 'GE1', 'GE3', 'HY')
    r <- c("q_brandegeei","q_fusiformis", "q_geminata", "q_minima", "q_oleoides", "q_sagraena", "q_virginiana", "q_fusiformis2", "q_oleoides2", "q_sagraena2", "q_virginiana2", "q_virginiana3","q_minima2", "q_virginiana4", "q_geminata1", "q_geminata3", "hybrid")
    names[,1] <- gsub(p[i], r[i], names[,1])
  }
  units <- c("mm^2", 'mm^2','C','C','%','%',rep('C',7),rep('mm',3),'NA',rep('mm',4), 'C', 'C', '%', 'NA', rep('C',7), rep('mm',3), 'NA', rep('mm',4))
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

.pigot.2015 <- function(...){
    data <- read.xls(ft_get_si("10.5061/dryad.fd986","Database%20S1%20Pigot%2c%20Trisos%20and%20Tobias.xls"), as.is=TRUE)
    data <- data[,-c(26:28)]
        for(i in 12:15)
    data[,i] <- as.logical(data[,i])
    names(data)[3:11] <- c("Min.Elevation","Max.Elevation","log.Bill.Length","log.Bill.Width","log.Bill.Depth","log.Tarsus.Length","log.Kipps.Distance","log.Wing.Length","log.Tail.Length")
    names(data)[26] <- "Museum.Institution.codes"
    units <- c("NA","m","m",rep('mm',7),rep('NA',4),rep('%',10),"NA")
    metadata <- data[,c(1,3:4,26)]
    data <- .df.melt(data, "Binomial", units=units, metadata=metadata)
}

.marx.2016 <- function(...){
    data <- read.csv("https://ndownloader.figshare.com/files/6854532")
    names(data)[3:7] <- c("Seed.Mass","Maximum.Height","","Leaf.Size","Leaf.Nitrogen")
    units <- c("native/invasive","mg","m","cm2/g","cm2","specific_leaf_area")
    metadata <- data[,2]
    data <- .df.melt(data, "Species", units=units, metadata=metadata)
}

.olli.2015 <- function(...){
    data <- read.table("https://datadryad.org/bitstream/handle/10255/dryad.90019/fd.txt?sequence=1", header = T, sep = '\t')
    for(i in c(1:9,11))
        data[,i] <- as.logical(data[,i])
    units <- c(rep('NA',9), "micrometer", "NA")
    data$species <- rownames(data)
    data <- .df.melt(data, "species", units=units)
}

#KW
.Aubret.2012b <- function(...){
  data <- read.xls(ft_get_si("10.5061/dryad.14cr5345", "Aubret%2053172.xlsx"), as.is=TRUE, sheet=2)
  data$X <- data$X.1 <- data$X.2 <- data$X.3 <- data$BCI <- NULL
  data$species <- "notechis_scutatus"
  metadata <- data[,c("POPULATION", "MOTHER", "BIRTH.DATE")]
  data <- data[,!names(data) %in% names(metadata)]
  names(data) <- c("snout_vent_length", "body_mass_pre", "body_mass_pre", "body_mass_post", "body_mass_post", "bm_body_mass_post", "rcm", "total_litter_body_mass", "total_litter_body_mass", "total_litter_snout_vent_length", "total_litter_snout_vent_length", "total_litter_jaw_length", "total_litter_jaw_length","species")
  units <- c("log10(cm)", "g", "log10(g)", "log10(mean(cm))", "g", "log10(g)", "?", "?", "mean(g)", "log10(mean(g))", "mean(cm)", "log10(mean(cm))", "mean(cm)", "log10(mean(cm))", NA)
  return(.df.melt(data, "species", units, metadata))
}

.Aubret.2012c <- function(...){
  data <- read.xls(ft_get_si("10.5061/dryad.14cr5345", "Aubret%2053172.xlsx"), as.is=TRUE, sheet=3)
  names(data) <- c("genus_species", "body_mass", "max_circumference")
  units <- c("g", "cm")
  return(.df.melt(data, "genus_species", units))
}

#KW
.Jennings.2015 <- function(...){
  data <- read.csv(ft_get_si("E096-226", "SIA_N_C_Atlantic_marine_fishes_squids_20150105_v1.csv", "esa_archives"))
    metadata <- data[,c("record", "year", "DOY", "latitude", "longitude", "sea")]
    data <- data[,!names(data) %in% names(metadata)]
    units <- c(NA, "g", "%", "%", "?", "?")
    return(.df.melt(data, "species", units, metadata))
}

.deraison.2014 <- function(...){
    data <- read.xls(ft_get_si("10255/dryad.72345","Plant%20traits.xls"), sheet = 2)
    name.data <- read.xls(ft_get_si("10255/dryad.72345","Plant%20traits.xls"), sheet = 1)
    data$Plant.species <- name.data$Species.name[1:22]
    names(data) <- c("species","leaf_dry_matter", "leaf_nitrogen_content", "leaf_carbon_content", "leaf_carbon_nitrogen_ratio", "leaf_thickness", "leaf_area", "perimeter_leaf_length_ratio")
    data <- data[,1:8]
    units <- c("%", rep("%_dry_mass",2), "NA", "mm", "cm2", "NA")
    data <- .df.melt(data, "species", units=units)
}

#.ameztegui.2016 <- function(...){
#    data <- read.xls(ft_get_si("10.5061/dryad.12b0h","FunctionalTraits_Dryad.xlsx"))
#    data <- data[,-c(1,6,7)]
#    names(data)[4:16] <- c("Phylum","leaf_habit","","leaf_mass_area","photosynthetic_capacitity_per_unit_leaf_mass","N_content_per_unit_mass","P_content_per_unit_mass","leaf_lifespan","leaf_length","seed_mass","wood_density","max_tree_height")
#
#}

.plourde.2015 <- function(...){
  data <- read.delim(ft_get_si("10.5061/dryad.sv181", "complete.individual.data.txt"))
  data <- unite(data, species, genus, species, remove = FALSE)
  data <-data[,-c(4:5)]
  data$species <- tolower(data$species)
  metadata <- data[,c(1:2,13,15,30:34,38)]
  data <- data[,-c(1:2,13,15,30:34,38)]
  units <- c("g", "cm^3", rep("g",2), "cm^3", rep("g",2), "cm^3", "g", "cm", rep("cm", 7), rep("g/cm^3",3), rep("cm^2", 4),  "?", "?", "%", "NA", "NA", "NA", "Na", "intercept", "y/x", "r^2", "pvalue", "class", "y/x")
  data <- .df.melt(data, "species", units, metadata)
  return(data) 
}

.delgado.2016 <- function(...){
  data <- read.xls(ft_get_si("10.5061/dryad.1tj60","Delagado_etal_2016_Appendix2.xlsx"), fileEncoding='UTF-8',sheet=1)
  data <- data[1:109,1:8]
  metadata <- data[,2:6]
  data <- data[,-c(2:6)]
  units <- c("m", 'cm')
  return(.df.melt(data,"Species",units=units, metadata))
}
 
.kefi.2016 <- function(...){
  link = "http://datadryad.org/bitstream/handle/10255/dryad.116249/chilean_metadata.xls?sequence=1"
  data <- read.xls(link)
  vars <- c("id", "species", "body_mass", "sessile_mobile", "cluster", "shore_height_conservative", "shore_height_C_ordinal",
            "shore_height_C_breadth", "shore_height_2_restrictive", "shore_height_R_ordinal", "shore_height_r_breadth",
            "phyllum", "subphyllum", "trophic")
  colnames(data) <- vars
  data$species <- tolower(glob(" ","_", data$species))
  metadata <- data[,c(1, 6, 9, 12:13)]
  data <- data[,-c(1, 6, 9, 12:13)]
  units <- c("?",NA,"?", "?","?","?","?",NA)
  data <- .df.melt(data, "species", units, metadata)
  
  return(data)
}

.petry.2016 <- function(...){
  link <- "http://datadryad.org/bitstream/handle/10255/dryad.119003/PollenMovement.csv?sequence=1"
  data <- read.csv(link)
  data$species = rep("valeriana_edulis", nrow(data))
  metadata <- data[,c(1)]
  data <- data[,-c(1)]
  units <- c("#","#","#","#","#","#","#","#","#","#","m","m","m","m","m","m")
  data <- .df.melt(data, "species", units, metadata)
  
  return(data)
}

.maire.2016 <- function(...){
  link <- "http://datadryad.org/bitstream/handle/10255/dryad.119139/globamax_data_160609%20%28for%20GEB%20ms%29.xlsx?sequence=1"
  data <- read.xls(link, sheet = "Data")
  legend <- read.xls(link, sheet ="Legend")
  data <- data[legend$Variable]
  data <- data[,order(names(data))]
  units <- c("µmol m^-2 s^-1","% of ECEC","nmol g^-1 s^-1","mm m^-1","kg dm^-3","g  kg^-1","cmol+ kg^-1",
             "cmolc kg^-1",NA,"%wt","gC gN^-1",NA,"gC kg^-1",NA,NA,"cm","m",NA,NA,NA,NA,"%wt","mmol m^-2 s^-1",
             NA,NA,NA,NA,"mm mm^-1","mm mm^-1","gN m^-2","%","gN kg^-1",NA,"W m^-2","gP m^-2","mgP2O5 kg^-1",
             "mm month^-1","mm month^-1",NA,"%","mm","mm","mm","mm",NA,"km",NA,NA,NA,"W m-2","%","dS m^-1","%wt",
             "%","%wt",NA,"cm2 g^-1","% of ECEC",NA,"%","%","%","%",NA,"cmol kg^-1","ºC",NA,"ºC","ºC","ºC","#","ºC")
  
  metadata = data[,c("Country","Latitude","Longitude","Cite", "Continent", "Dataset", "Expt_Remark", "Family","Genus","Location", "P.Method","P.retention.class", "P.Source", "Seeding_Sapling","SITECODE")]
  units <- units[!names(data) %in% c("Genus.spp",names(metadata))]
  data <- data[,!names(data) %in% names(metadata)]
  data$Genus.spp <- tolower(gsub(" ","_",data$Genus.spp))
  data <- .df.melt(data, "Genus.spp",units,metadata)

  return(data)
}

.myhrvold.2015 <- function(...){
    data <- read.csv("~/Downloads/Amniote_Database_Aug_2015.csv", as.is=TRUE)
    data <- read.csv(ft_get_si("E096-269","Data_Files/Amniote_Database_Aug_2015.csv", "esa_archives", cache=FALSE))
    for(i in seq_len(ncol(data)))
        data[data[,i]==-999 | data[,i]=="-999",i] <- NA
    metadata <- data[,c("class","order","family","genus","species","subspecies","common_name")]
    species <- ifelse(is.na(data$subspecies), "", data$subspecies)
    data$binomial <- tolower(paste(data$genus, data$species, data$subspecies, sep="_"))
    data <- data[,!names(data) %in% names(metadata)]
    units <- c("days", "#", "#/year", "g", "years", "days", "days", "g", "g", "g", "days", "days", "years","days", "years", "g", "g", "g", "mm", "mm", "g", "cm", "cm", "cm", "cm", "cm", "g", "cm", "days")
    return(.df.melt(data, "binomial", units, metadata))
}

.benesh.2017 <- function(...){
    data <- read.csv(unzip(ft_get_si("10.1002/ecy.1680", 1), "CLC_database_lifehistory.csv"))
    metadata <- data[,c("Parasite.genus", "Parasite.group", "Development.remarks", "Size.reported.as", "Size.remarks", "Author", "Year", "Journal", "Volume", "Pages")]
    data <- data[,!names(data) %in% names(metadata)]
    return(.df.melt(data, "Parasite.species", c(NA, "#", NA, NA, "days", "°C", "mm", "mm", "mm", "mm", "n", NA, NA, NA), metadata))
}

.lu.2016a <- function(...){
    data <- read.csv(unzip(ft_get_si("10.1002/ecy.1600", 2), "GCReW_Allom_General_Data.csv"))
    data$binomial <- data$Spp_Orig
    data$binomial <- gsub("PHAU", "phragmites_australis", data$binomial)
    data$binomial <- gsub("SCAM", "schoenoplectus_americanus", data$binomial)
    data$binomial <- gsub("SPAL", "spartina_alterniflora", data$binomial)
    data$binomial <- gsub("SPCY", "spartina_cynosuroides", data$binomial)
    data$binomial <- gsub("TYAN", "typha_angustifolia", data$binomial)
    data$binomial <- gsub("AMCA", "amaranthus_cannabinus", data$binomial)
    data$binomial <- gsub("ATPA", "atriplex_patula", data$binomial)
    data$binomial <- gsub("KOVI", "kosteletzkya_virginica", data$binomial)
    data$binomial <- gsub("POHY", "polygonum_hydropiper", data$binomial)
    data$binomial <- gsub("SOSE", "solidago_sempervirens", data$binomial)
    data$binomial <- gsub("IVFR", "iva_frutescens", data$binomial)
    data$Width[data$Width==-99] <- NA
    metadata <- data[,c("Species","Spp_Orig","Decile","ID","Year")]
    data <- data[,!names(data) %in% names(metadata)]
    names(data)[1] <- "height"
    return(.df.melt(data, "binomial", c("cm","mm","g"), metadata))
}

.lu.2016b <- function(...){
    data <- read.csv(unzip(ft_get_si("10.1002/ecy.1600", 2), "GCReW_Allom_Other_Data.csv"))
    data$binomial <- data$Species
    data$binomial <- gsub("PHAU", "phragmites_australis", data$binomial)
    data$binomial <- gsub("SCAM", "schoenoplectus_americanus", data$binomial)
    data$binomial <- gsub("SPAL", "spartina_alterniflora", data$binomial)
    data$binomial <- gsub("SPCY", "spartina_cynosuroides", data$binomial)
    data$binomial <- gsub("TYAN", "typha_angustifolia", data$binomial)
    data$binomial <- gsub("AMCA", "amaranthus_cannabinus", data$binomial)
    data$binomial <- gsub("ATPA", "atriplex_patula", data$binomial)
    data$binomial <- gsub("KOVI", "kosteletzkya_virginica", data$binomial)
    data$binomial <- gsub("POHY", "polygonum_hydropiper", data$binomial)
    data$binomial <- gsub("SOSE", "solidago_sempervirens", data$binomial)
    data$binomial <- gsub("IVFR", "iva_frutescens", data$binomial)
    data$Width[data$Width==-99] <- NA
    metadata <- data[,c("Species","ID","Year")]
    data <- data[,!names(data) %in% names(metadata)]
    names(data)[1] <- "height"
    return(.df.melt(data, "binomial", c("cm","mm","g"), metadata))
}

.lu.2016c <- function(...){
    data <- read.csv(unzip(ft_get_si("10.1002/ecy.1600", 2, cache=FALSE), "GCReW_Allom_SCAM_Data.csv"))
    data$binomial <- "schoenoplectus_americanus "
    data$Width[data$Width==-99] <- NA
    metadata <- data[,c("Species","ID","Expt","ExptAge","Year","Community","Chamber","Quadrat","InOut","CO2","N","Treatment")]
    data <- data[,!names(data) %in% names(metadata)]
    names(data)[1] <- "height"
    data$Cut_Stem <- as.logical(data$Cut_Stem)
    return(.df.melt(data, "binomial", c("cm","cm","mm","g","g","g",NA), metadata))
}

.valido.2011 <- function(...){
  link = "http://datadryad.org/bitstream/handle/10255/dryad.89498/Dryad_database.xls?sequence=1"
  data = read.xls(link,sheet="Traits")
  metadata = data[,c(2:7)]
  data = data[,-c(2:7)]
  vars = c("species","height","cover","leaf_size","LDMC","specific_leaf_area","lchl","lnc","C13","SDMC","WD", "RDMC", "SRL")
  colnames(data) = vars
  units = c("m","m^2","cm^2","g g^-1","m^2 Kg^-1", "µg g^-1", "%","%","g g^-1","g cm^-3","g g^-1","m g^-1")
  data$species = tolower(gsub(" ","_", data$species))
  data = .df.melt(data, "species", units, metadata)
  return(data)
}

.jennings.2016a <- function(...){
  link = "http://datadryad.org/bitstream/handle/10255/dryad.112638/spiders.csv?sequence=1"
  data = read.csv(link)
  data$species = rep("sosippus_floridanus", nrow(data))
  vars = c("web_area","web_height","diff_trich","diff_trap","sum_trap", "species")
  units = c("cm^2","cm","#/cm^2","cm^2","cm^2")
  metadata = data[,c(1:7,10)]
  data = data[,-c(1:7,10)]
  colnames(data) = vars
  data = .df.melt(data, "species", units, metadata)
}
  
.lessard.2016 <- function(...){
  data <- read.delim(ft_get_si("10.5061/dryad.t897q", "hummer_traits_Lessard.txt"), sep = " ", row.names = NULL)
  species <- tolower(data[,1])
  data <- data.frame(species, data[,-c(1)])
  units <- c("g", "mm", "annual_mean_temperature", "annual_precipitation", "range")
  data <- .df.melt(data, "species", units)
  return(data) 
}

.jennings.2016a <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.m23g6", "spiders.csv"), sep = ",", as.is = TRUE, na.strings = c("","NA"))
  species <- rep(c("sosippus_floridanus"), nrow(data))
  data <- data.frame(species, data)
  metadata <- data[,c(2:6)]
  data <- data[,-c(2:6)]
  units <- c("abundance", "sqrt_abundance", "cm^2", "cm", "abundance", "number_of_trichomes/cm^2", "cm^2","cm^2", "NA", "NA", "abundance", "condition", "condition")
  data <- .df.melt(data, "species", units, metadata)
  return(data)
}

.jennings.2016b <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.m23g6", "sundews.csv"), sep = ",", as.is = TRUE, na.strings = c("","NA"))
  species <- rep(c("drosera_capillaris"), nrow(data))
  data <- data.frame(species, data)
  metadata <- data[,c(2:6)]
  data <- data[,-c(2:6)]
  units <- c("abundance", "sqrt_abundance", "cm^2", "cm", "abundance", "number_of_trichomes/cm^2", "cm^2","cm^2", "NA", "NA", "abundance", "condition", "condition")
  data <- .df.melt(data, "species", units, metadata)
  return(data)
}

.jennings.2016c <- function(...){
  data <- read.csv(ft_get_si("10.5061/dryad.m23g6", "toads.csv"), sep = ",", as.is = TRUE, na.strings = c("","NA"))
  species <- rep(c("anaxyrus_quercicus"), nrow(data))
  data <- data.frame(species, data)
  metadata <- data[,c(2:6)]
  data <- data[,-c(2:6)]
  units <- c("cm^2", "cm", "abundance", "sqrt_abundance", "g",  "proportion_initial_mass(g)", "abundance", "number_of_trichomes/cm^2", "cm^2","cm^2", "NA", "NA", "abundance", "condition", "condition")
  data <- .df.melt(data, "species", units, metadata)
  return(data)
}

.vanier.2013 <- function(...){
  data <- read.delim(ft_get_si("E094-246", "Mass_volume_data.txt", "esa_archives"), sep = "", as.is = TRUE, na.strings = c("","NA"))
  data$Individual_Species <- tolower(data$Individual_Species)
  data$Species_Groups <- tolower(data$Species_Groups)
  metadata <- data[,c(1:2,4:5)]
  data <- data[,-c(1:2,4:5)]
  units <- c("g", "m^3", "Status", "survivor_status", "life_form_type", "?", "NA", "NA", "treatmet_group")
  data <- .df.melt(data, "Individual_Species", units, metadata)
  return(data)
}

.castillo.2016 <- function(...){
  data <- read.csv('http://datadryad.org/bitstream/handle/10255/dryad.116802/Castillo%20and%20Delph%20isofemale%20data.csv?sequence=1')
  names(data) <- tolower(names(data))
  data$species <- 'caenorhabditis_remanei'
  units <- c('μm','μm','sec','sec','sec', rep(NA,3))
  data <- .df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.nandy.2013 <- function(...){
  data <- read.xls('http://datadryad.org/bitstream/handle/10255/dryad.53849/Nandy%20et%20al.%2013-0268.R2_data.xlsx?sequence=1')
  colnames(data) <- c('selection_regime', 'block', 'total_mass_dry')
  names(data) <- tolower(names(data))
  data$species <- 'drosophila_melanogaster'
  units <- c(NA,'mg',NA)
  data <- .df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.comeault.2013 <- function(...){
  data <- read.table('http://datadryad.org/bitstream/handle/10255/dryad.54681/Tcris_FHA_phenotypes.txt?sequence=1',header=TRUE)
  data <- data[,-c(1,4)]
  colnames(data) <- c('sex','phenotype','hue_green_color_chip', 'saturation_green_color_chip', 'brightness_green_color_chip', 'lateral_hue_average', 'lateral_saturation_average', 'lateral_brightness_average', 'midsaggital_hue_average', 'midsaggital_saturation_average', 'midsaggital_brightness_average','body_length','body_width','head_width','proportion_striped')
  data$species <- 'timema_cristinae'
  units <- c(rep(NA,9),rep('cm',4),rep(NA,2))
  data <- .df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.fargevieille.2017 <- function(...){
  data <- read.csv('http://datadryad.org/bitstream/handle/10255/dryad.138322/ColorTraitValuesECE3-2822.csv?sequence=1',sep=';')
  data <- data[,-c(3,4)]
  levels(data$pop)<- c('d_muro','e_muro','e_pirio','d_rouviere')
  colnames(data) <- c('year','population','male_blue_brightness','female_blue_brightness','male_blue_hue','female_blue_hue','male_blue_uv_chroma','female_blue_uv_chroma','male_yellow_brightness','female_yellow_brightness','male_yellow_contrast','female_yellow_contrast')
  data$species <- 'cyanistes_caeruleus'
  units <- c(rep(NA,12))
  data <- .df.melt(data, "species", units=units)
  data$character$units <- NA
  return(data)
}

.engemann.2016 <- function(...){
    data <- read.delim(unzip(ft_get_si("10.1002/ecy.1569", 1), "DataS1/GrowthForm_Final.txt"))
    metadata <- data[,c("FAMILY_STD", "CONSENSUS", "ID", "SOURCES")]
    data <- data[,!names(data) %in% names(metadata)]
    data$SPECIES_STD <- tolower(gsub(" ", "_", data$SPECIES_STD))
    return(.df.melt(data, "SPECIES_STD", rep(NA,2), metadata))
}

.pfautsch.2016 <- function(...){
    first <- read.csv(unzip(ft_get_si("10.1890/16-0147.1", 1), "Eucalyptus_vessel_anatomy_800cm.csv"), as.is=TRUE)
    first$measurement_height <- "800cm"
    names(first)[19] <- "diameter_at_breast_height"
    second <- read.csv(unzip(ft_get_si("10.1890/16-0147.1", 1), "Eucalyptus_vessel_anatomy_130cm.csv"), as.is=TRUE)
    second$measurement_height <- "130cm"
    names(second)[19] <- "diameter_at_breast_height"
    data <- rbind(first, second)
    names(data)[5:6] <- c("latitude", "longitude")
    metadata <- data[,c("location","country","state","latitude","longitude","elevation","tree","image","measurement_height")]
    data <- data[,!names(data) %in% names(metadata)]
    units <- c("#", "#", "µm^2", "µm^2", "µm^2", "# cm^-2", "%", "µm", "µm", "cm", "cm", "cm", "g cm^-3", "g cm^-3", "°C", "°C", "mm", "mm", "mm", "mm", "mm", "mm", "")
    return(.df.melt(data, "species", units, metadata))
}

.hebert.2016 <- function(...){
    data <- read.csv(unzip(ft_get_si("10.1890/15-1275.1",1), "zooplankton_traits.csv"), sep=";", as.is=TRUE, dec=",")
    data$binomial <- tolower(paste(data$Genus, data$Species, sep="_"))
    metadata <- data[,c("Genus","Species","Replicate.number","Group","Ref.tg","Ref.bl","Ref.dm","Ref.C","Ref.N","Ref.P","Ref.NP","Ref.prot","Ref.lip","Ref.resp","Ref.N.ex","Ref.P.ex","Ref.NPex")]
    data <- data[,!names(data) %in% names(metadata)]
    units <- c(NA, NA, "mm", "mm", "mm","mg", "mg", "mg", "%", "%", "%", "mg", "%", "%", "%", "mg", "%","%", "%", "mg", ":", ":", ":", "%", "%", "%", "%", "%", "%", "µl O2 ind-1 h-1", "µl O2 mgDM -1 h-1","µl O2 mgDM -1 h-1", "µl O2 mgDM -1 h-1", "°C", "°C", "°C", "µg N-NH4+ ind-1 h-1", "µg N-NH4+ mgDM-1 h-1", "µg N-NH4+ mgDM-1 h-1", "µg N-NH4+ mgDM-1 h-1", "nmol N-NH4+ mg DM-1 h-1", "nmol N-NH4+ ind-1 h-1", "nmol N-NH4+ ind-1 h-1", "nmol N-NH4+ ind-1 h-1", "°C", "°C", "°C", "µg P-PO43- ind-1 h-1", "ug P-PO43- mg DM-1 h-1", "ug P-PO43- mg DM-1 h-1", "ug P-PO43- mg DM-1 h-1", "nmol P-PO43- mg DM-1 h-1", "nmol P-PO43- ind-1 h-1", "nmol P-PO43- ind-1 h-1", "nmol P-PO43- ind-1 h-1", "°C", "°C", "°C", ":", ":", ":")
    return(.df.melt(data, "binomial", units, metadata))
}

#.neuheimer.2016 <- function(...){
#    data <- read.csv(unzip(ft_get_si("10.1890/15-1261.1", 1), "15-1261_Neuheimer_SizeDatabase.csv"), as.is=TRUE)
#    data$binomial <- tolower(paste(data$Genus, data$Species, sep="_"))
#    metadata <- data[,c("Phylum", "Class", "Order", "SubOrder", "Family", "Genus", "Species")]
#    combined <- cbind(data[,c("AdultSize1", "AdultSizeUnit1")])
#    units <- c("AdultSize1", "AdultSizeUnit1", "AdultSizeDescription1", "AdultSizeVariation1", "AdultSizeVariationUnit1", "AdultSizeVariationDescription1", "AdultSize2", "AdultSizeUnit2", "AdultSizeDescription2", "AdultSizeVariation2", "AdultSizeVariationUnit2", "AdultSizeVariationDescription2", "ProgenySize1", "ProgenySizeUnit1", "ProgenySizeDescription1", "ProgenySizeVariation1", "ProgenySizeVariationUnit1", "ProgenySizeVariationDescription1", "ProgenySize2", "ProgenySizeUnit2", "ProgenySizeDescription2", "ProgenySizeVariation2", "ProgenySizeVariationUnit2", "ProgenySizeVariationDescription2", "Reference")
#}

#.ebert.2013 <- function(...){
#    data <- read.delim(ft_get_si("E094-193","dissections%2054_09.txt", "esa_archives"), as.is=TRUE, fileEncoding="latin1")
#    metadata <- data[,c()]
#    data <- data[,!names(data) %in% names(metadata)]    
#}