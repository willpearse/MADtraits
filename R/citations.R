#' @name MADtraits_citations
#' @title Citations for all datasets within MADTRAITS
#' @description Citations for all datasets within MADTRAITS. These *MUST* be
#'     cited when publishing anything that makes use of this package:
#'     cite the data in preference to this package, if needs be. You
#'     will rarely use this data directly; instead, use the
#'     \code{\link{citations}} function.
#' @docType data
#' @keywords datasets
#' @usage MADtraits_citations
#' @format \code{MADtraits_citations} contains a \code{data.frame} of
#'     citations and paper information.
#' @seealso citations
NULL

#' Returns citations for the datasets in MADTRAITS
#'
#' This will generate citations (in BibTeX format) for a given MADTRAITS
#' dataset.
#' 
#' @param x The MADTRAITS database object for which you want citations
#' @return Character vector of citations in BibTeX format
#' @author Will Pearse; Sylia Kinosian
#' #@examples
#' # Limit the scope of these as they have to work online on servers!...
#' # Also give an example (notrun) of how to use the cache
#' #@seealso 
#' @export
citations <- function(x){
    if(!inherits(x, "MADtraits"))
        stop("'", deparse(substitute(x)), "' must be of type 'MADtraits'")
    
    data("MADtraits_citations", envir=environment())
    datasets <- unique(c(x$categorical$dataset,x$numeric$dataset))
    citations <- with(MADtraits_citations, BibTeX.citation[match(datasets, author_year)])
    return(unique(citations[!is.na(citations)]))
}

