#' @importFrom utils unzip
.unzip <- function(file, zip, to.save.dir, to.save.name){
    if(missing(to.save.dir))
        to.save.dir <- tempdir()
    if(missing(to.save.name))
        to.save.name <- file
    
    files <- unzip(zip, list=TRUE)
    if(!file %in% files$Name)
        stop("Required file not in zipfile ", zip)

    file <- unzip(zip, file)
    file.rename(file, file.path(to.save.dir, to.save.name))
    return(file.path(to.save.dir, to.save.name))
}

#' @importFrom stats setNames
#' @importFrom reshape2 melt
.df.melt <- function(x, spp, units, metadata){
    # Meta-data and units
    if(missing(units)){
        units <- setNames(as.character(rep(NA, length(names(x)))), names(x))
    } else {
        units <- setNames(units, setdiff(names(x),c(spp,"metadata")))
    }

    if(!missing(metadata)){
        metadata <- apply(sapply(seq_along(names(metadata)), function(y) paste(names(metadata)[y],metadata[,y],sep=":")), 1, paste, collapse=";")
    } else metadata <- rep(NA, nrow(x))

    # Numeric data
    numeric <- x[,sapply(x, is.numeric) | names(x) == spp,drop=FALSE]
    if(ncol(numeric) > 1){
        numeric$metadata <- metadata
        numeric <- melt(numeric, id.vars=c(spp,"metadata"))
        numeric$variable <- as.character(numeric$variable) # impossible to stop this coercion in melt!
        numeric <- numeric[!is.na(numeric$value),]
        names(numeric)[1] <- "species"
        numeric$units <- units[numeric$variable]
        numeric$species <- as.character(numeric$species)
    } else numeric <- NULL
    
    # Character data
    character <- x[,sapply(x, Negate(is.numeric)) | names(x) == spp,drop=FALSE]
    if(ncol(character) > 1){
        character$metadata <- metadata
        character <- melt(character, id.vars=c(spp,"metadata"))
        character$variable <- as.character(character$variable) # impossible to stop this coercion in melt!
        character <- character[!is.na(character$value),]
        names(character)[1] <- "species"
        character$units <- units[character$variable]
        character$species <- as.character(character$species)
   } else character <- NULL

    #Cleanup and return
    output <- list(numeric=numeric,character=character)
    class(output) <- "natdb"
    return(output)
}
prog.bar <- function(x, y){
    if(y < 100){
        cat(".")} else {
            z <- Filter(function(z) z>=0, seq(1,y,length.out=100)-x)
            if(length(z) > 0)
                tryCatch(if(z[1] < 1) if((length(z) %% 10)==0) cat("|") else cat("."), error=function(z) cat("."))
        }
}
