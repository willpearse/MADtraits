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
