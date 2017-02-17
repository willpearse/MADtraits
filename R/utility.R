#' @importFrom stats model.matrix

#' Unzips a file from a downloaded zip file
#' @param file name of file to be extracted from zip
#' @param zip location and name of zip file (e.g.,
#'     ~/Downlaods/a_file.zip)
#' @param to.save.dir directory to save resulting file (DEFAULT: a new
#'     temporary directory will be used)
#' @param to.save.name name to save the file as (DEFAULT: it will be
#'     named paste(zip,file, sep='_'))
#' @return Complete path to unzipped file
#' @importFrom utils unzip
.unzip <- function(file, zip, to.save.dir, to.save.name){
    if(missing(to.save.dir))
        to.save.dir <- tempdir()
    if(missing(to.save.name))
        to.save.name <- paste(file, zip, sep='_')
    
    files <- unzip(zip, list=TRUE)
    if(!file %in% files$Name)
        stop("Required file not in zipfile ", zip)
    
    file <- unzip(zip, file)
    file.rename(file, file.path(to.save.dir, to.save.name))
    return(file.path(to.save.dir, to.save.name))
}

.fac.sim <- function(x){
    x <- Filter(Negate(is.na), x)
    x <- x[x != "" & x != " "]
    x <- unique(x)
    return(paste(x,collapse="_"))
}
.expand.factor <- function(factor_to_expand, name){
    names <- rep(name, length(unique(factor_to_expand)))
    output <- model.matrix(~factor_to_expand-1)
    colnames(output) <- paste(names, gsub("factor_to_expand", "", colnames(output)), sep="_")
    return(as.data.frame(output))
}
.df.melt <- function(x, species){
    numeric <- x[,sapply(x, is.numeric) | names(x) %in% c(species,"metadata")]
    if(ncol(numeric) > 2){
        numeric <- melt(numeric, id.vars=c(species,"metadata"))
        numeric <- numeric[!is.na(numeric$value),]
        names(numeric)[1] <- "species"
    } else numeric <- NULL
    character <- x[,sapply(x, Negate(is.numeric)) | names(x) %in% c(species,"metadata")]
    if(ncol(character) > 2){
        character <- melt(character, id.vars=c(species,"metadata"))
        character <- character[!is.na(character$value),]
        names(character)[1] <- "species"
    } else character <- NULL
    return(list(numeric=numeric,character=character))
}
