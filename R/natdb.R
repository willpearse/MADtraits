#' Builds a trait database
#'
#' The key function of the natdb package. When run with defaults, it
#' will download and build a database of species' traits from all the
#' manuscript sources in the package. This totals XXX
#' manuscripts/databases, XXX species, and XXX traits. Please note
#' that all parameters are interactive; thus specifying \code{species}
#' and \code{traits} constraints will constraint according to both,
#' for example. Please also note that specifying any kind of
#' constraints makes use of the package's built-in cache of what
#' species and traits information are available in each database;
#' making use of this on the GitHub (developer) build of this package
#' is not advisable, and (further) it is impossible for us to verify
#' whether the datasets NATDB searches have been updated since the
#' package was last built.
#' 
#' @param datasets Character vector of datasets to be searched for
#'     trait data. If not specified (the default) all trait datasets
#'     will be downloaded and returned.
#' @param species Character vector of species to be searched for trait
#'     data. If not specified (the default) data for all species will
#'     be downloaded and returned.
#' @param traits Character vector of traits to be searched for
#'     data. If not specified (the default) data for all traits will
#'     be downloaded and returned.
#' @return natdb.data object. XXX
#' @author Will Pearse; USU Biology Nerd Group (XXX)
#' #@examples
#' # Limit the scope of these as they have to work online on servers!...
#' #@seealso 
#' @export

natdb <- function(datasets, species, traits){
    #Check datasets
    if(missing(datasets)){
        datasets <- Filter(Negate(is.function), ls(pattern="^\\.[a-z]*\\.[0-9]+", name="package:natdb", all.names=TRUE))
    } else {
        datasets <- paste0(".", tolower(datasets))
        datasets <- gsub("..", ".", datasets, fixed=TRUE)
    }
    if(!all(datasets %in% datasets)){
        missing <- setdiff(datasets, ls.funs())
        stop("Error: ", paste(missing, collapse=", "), "not in natdb")
    }
    
    #Do work and return
    output <- vector("list", length(datasets))
    for(i in seq_along(datasets))
        output[[i]] <- eval(as.name(datasets))()
    numeric <- do.call(rbind, lapply(output, function(x) x[[1]]))
    categorical <- do.call(rbind, lapply(output, function(x) x[[1]]))
    output <- list(numeric=numeric, categorical=categorical)
    return(output)
}

print.natdb <- function(x, ...){
    # Argument handling
    if(!inherits(x, "natdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'natdb'")
    
    # Do main summary
    output <- matrix(0, 3, 3, dimnames=list(c("Numeric","Categorical","Total"),c("Species","Traits","Data-points:")))
    try(output[1,] <- c(length(unique(x$numeric$species)), length(unique(x$numeric$variable)), nrow(x$numeric)), silent=TRUE)
    try(output[2,] <- c(length(unique(x$character$species)), length(unique(x$character$variable)), nrow(x$character)), silent=TRUE)
    output[3,] <- colSums(output)
    cat("A Trait DataBase containing:\n")
    print(output)

    # Supplemental summaries
    printer <- FALSE
    if(!all(is.na(c(x$character$metadata,x$numeric$metadata)))){
        printed <- TRUE
        cat("Meta-data present. ")
    }
    if(!all(is.na(c(x$character$units,x$numeric$units)))){
        printed <- TRUE
        cat("Units present. ")
    }
    if(printed)
        cat("\n")
    invisible(output)
}

summary.natdb <- function(x, ...){
    print.natdb(x, ...)
}

"[.natdb" <- function(x, spp, traits){
    # Argument handling
    if(!inherits(x, "natdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'natdb'")

    # Species
    if(!missing(spp)){
        if(any(x$numeric$species %in% spp))
            x$numeric <- x$numeric[x$numeric$species %in% spp,] else
                                                                    x$numeric <- NULL
        if(any(x$character$species %in% spp))
            x$character <- x$character[x$character$species %in% spp,] else
                                                                          x$character <- NULL
    }
    
    # Traits
    if(!missing(traits)){
        if(any(x$numeric$variable %in% traits))
            x$numeric <- x$numeric[x$numeric$variable %in% traits,] else
                                                                        x$character <- NULL
        if(any(x$character$variable %in% traits))
            x$character <- x$character[x$character$variable %in% traits,] else
                                                                              x$character <- NULL
    }

    output <- list(character=x$character, numeric=x$numeric)
    class(output) <- "natdb"
    return(output)
}

species <- function(x, ...){
    if(!inherits(x, "natdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'natdb'")
    return(unique(c(x$numeric$species,x$character$species)))
}
traits <- function(x, ...){
    if(!inherits(x, "natdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'natdb'")
    return(unique(c(x$numeric$variable,x$character$variable)))
}


citations <- function(...){
    # load file w/ authors & bibtex
    # check for matching name, year
    # return bibtex
}
