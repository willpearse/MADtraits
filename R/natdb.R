#' Builds a trait database
#'
#' The key function of the natdb package. When run with defaults, it
#' will download and build a database of species' traits from all the
#' manuscript sources in the package. This totals XXX
#' manuscripts/databases, XXX species, and XXX traits. *Please* make
#' use the the \code{cache} feature, as it will massively speed and
#' ease your use of the package.
#' 
#' @param datasets Character vector of datasets to be searched for
#'     trait data. If not specified (the default) all trait datasets
#'     will be downloaded and returned.
#' @param cache Specify an existing directory/folder where datasets
#'     can be downloaded to and stored. If a dataset is already
#'     present in this directory, it will not be downloaded from the
#'     server but instead loaded locally. We *STRONGLY* advise you to
#'     specify a cache location.
#' @param delay How many seconds to wait between downloading and
#'     processing each dataset (default: 5). This delay may seem
#'     large, but if you specify a \code{cache} (see above) you only
#'     need do it once, and specifying a large delay ensures you don't
#'     over-stretch servers. Keeping servers happy is good for you
#'     (they won't reject you!) and good for them (they can help
#'     everyone).
#' @return natdb.data object. XXX
#' @author Will Pearse; USU Biology Nerd Group (XXX)
#' @examples
#' \dontrun{
#' # Download all NATDB data, and cache (save) it on your hard-drive for use later
#' data <- natdb(cache="Documents/natdb/cache")
#' # Perform basic checks and cleaning on that data
#' clean.data <- clean.natdb(data)
#'
#' # Subset data down to the traits you want (notice the comma position)
#' subset.data <- clean.data[,c("specific_leaf_area","height")]
#' # Subset data down to the species you want (notice the comma position)
#' subset.data <- clean.data[c("quercus_robur","quercus_ilex"),]
#' # Subset multiple things at once
#' clean.data[c("quercus_robur","quercus_ilex"),"specific_leaf_area"]
#'
#' # Convert that into a data.frame for use in an analysis
#' neat.data <- as.data.frame(clean.data)
#' }
#' @seealso clean.natdb convert.natdb.units lookup.natdb.species
#' @export
#' @importFrom gdata ls.funs
#' @importFrom utils capture.output
natdb <- function(cache, datasets, delay=5){
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

    .warn.func <- function(x){
        warning("Could not download from ", datasets[i], "; ignoring")
        return(NULL)
    }
    
    #Do work and return
    cat("Downloading/loading data\n")
    cat("'.' --> 1%; '|' --> 10% complete\n")
    output <- vector("list", length(datasets))
    for(i in seq_along(datasets)){
        prog.bar(i, length(datasets))
        if(!missing(cache)){
            path <- file.path(cache,paste0(sub("\\.","",datasets[i]), ".RDS"))
            if(file.exists(path)){
                output[[i]] <- readRDS(path)
            } else {
                capture.output(output[[i]] <- tryCatch(
                                   eval(as.name(datasets[i]))()),
                               error=.warn.func)
                if(!is.null(output[[i]]))
                    saveRDS(output[[i]], path)
                print(output[[i]])
                Sys.sleep(delay)
            }
        } else {
            capture.output(output[[i]] <- tryCatch(
                                   eval(as.name(datasets[i]))()),
                           error=.warn.func)
            Sys.sleep(delay)
        }
        
        if(!is.null(output[[i]]$numeric))
            output[[i]]$numeric$dataset <- datasets[i]
        if(!is.null(output[[i]]$character))
            output[[i]]$character$dataset <- datasets[i]
    }
    numeric     <- do.call(rbind,
                           lapply(Filter(function(y) !is.null(y[[1]]), output), function(x) x[[1]])
                           )
    categorical <- do.call(rbind,
                           lapply(Filter(function(y) !is.null(y[[2]]), output), function(x) x[[2]])
                           )
    output <- list(numeric=numeric, categorical=categorical)
    class(output) <- "natdb"
    cat("\n")
    return(output)
}

print.natdb <- function(x, ...){
    # Argument handling
    if(!inherits(x, "natdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'natdb'")
    
    # Do main summary
    output <- matrix(0, 3, 3, dimnames=list(c("Numeric","Categorical","Total"),c("Species","Traits","Data-points:")))
    try(output[1,] <- c(length(unique(x$numeric$species)), length(unique(x$numeric$variable)), nrow(x$numeric)), silent=TRUE)
    try(output[2,] <- c(length(unique(x$categorical$species)), length(unique(x$categorical$variable)), nrow(x$categorical)), silent=TRUE)
    output[3,] <- colSums(output)
    output[3,1] <- length(unique(c(x$numeric$species,x$categorical$species)))
    cat("A Trait DataBase containing:\n")
    print(output)

    # Supplemental summaries
    printer <- FALSE
    if(!all(is.na(c(x$categorical$metadata,x$numeric$metadata)))){
        printed <- TRUE
        cat("Meta-data present. ")
    }
    if(!all(is.na(c(x$categorical$units,x$numeric$units)))){
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
            x$numeric <- x$numeric[x$numeric$species %in% spp,,drop=FALSE] else
                                                                    x$numeric <- NULL
        if(any(x$categorical$species %in% spp))
            x$categorical <- x$categorical[x$categorical$species %in% spp,,drop=FALSE] else
                                                                          x$categorical <- NULL
    }
    
    # Traits
    if(!missing(traits)){
        if(any(x$numeric$variable %in% traits))
            x$numeric <- x$numeric[x$numeric$variable %in% traits,,drop=FALSE] else
                                                                        x$categorical <- NULL
        if(any(x$categorical$variable %in% traits))
            x$categorical <- x$categorical[x$categorical$variable %in% traits,,drop=FALSE] else
                                                                              x$categorical <- NULL
    }

    output <- list(categorical=x$categorical, numeric=x$numeric)
    class(output) <- "natdb"
    return(output)
}

species <- function(x, ...){
    if(!inherits(x, "natdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'natdb'")
    return(unique(c(x$numeric$species,x$categorical$species)))
}

traits <- function(x, ...){
    if(!inherits(x, "natdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'natdb'")
    return(unique(c(x$numeric$variable,x$categorical$variable)))
}

#' @param x \code{natdb} object to be printed
#' @param num.func To summarise data at the species level (which is
#'     done by default), a function is needed to summarise the
#'     continuous data at the species level. This argument specifies
#'     this function; while the default is \code{\link[base]{mean}},
#'     you could change this to \code{\link[stats]{median}} or specify
#'     your own standard error function.
#' @param cat.func To summarise data at the species level (which is
#'     done by default), a function is needed to summarise the
#'     continuous data at the species level. This argument specifies
#'     this function; while the default is to just return the modal
#'     value, other options could be used.
#' @param data Which variables to summarise into a data.frame. If
#'     numeric, the top \code{data} variables with the most data will
#'     be summarised (default: top ten variables). You can also
#'     specify variable names if you wish. Note: there are over a
#'     thousand variables and over 100,000 species in NATDB - you *do
#'     not* want to turn all of this data into a single
#'     \code{data.frame}, as it will be unmanageable. Subset your data
#'     down first to your species or traits of interest (see
#'     "examples")
#' @param row.names Ignored
#' @param optional Ignored
#' @param ... ignored
#' @rdname natdb
#' @method as.data.frame natdb
#' @export
as.data.frame.natdb <- function(x, row.names, optional, num.func=mean, cat.func=function(x) names(which.max(table(x))), data=10, ...){
    # Argument handling
    if(!inherits(x, "natdb"))
        stop("'", deparse(substitute(x)), "' must be of type 'natdb'")    
    if(!is.function(num.func))
        stop("'", deparse(substitute(num.func)), "' must be a function to summarise data")
    if(!is.function(cat.func))
        stop("'", deparse(substitute(cat.func)), "' must be a function to summarise data")

    # Subset data
    if(is.numeric(data))
        data <- names(sort(table(c(x$numeric$variable,x$categorical$variable)), decreasing=TRUE)[seq_len(data)])
    if(!is.character(data))
        stop("Either too few/many variables requested, or data not requested by name")
    x <- x[,data]

    # Average data
    numeric <- with(x$numeric, as.data.frame(tapply(value, list(species,variable), num.func, ...)))
    categorical <- with(x$categorical, as.data.frame(tapply(value, list(species,variable), cat.func, ...)))

    # Cleanup and return
    numeric$species <- rownames(numeric)
    categorical$species <- rownames(categorical)
    output <- merge(categorical, numeric, by="species")
    return(output)
}
