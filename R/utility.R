#' @importFrom utils unzip
#' @importFrom stats model.matrix

.unzip <- function(zip, dir, save.name, cache, si, list=FALSE){
    files <- unzip(zip, list=TRUE)
    if(list){
        cat("Files in ZIP:")
        print(files)
    }
    if(!si %in% files$Name)
        stop("Required file not in zipfile ", zip)
    file <- unzip(file, si)
    file.rename(file, file.path(dir, save.name))
    return(file.path(dir, save.name))
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
