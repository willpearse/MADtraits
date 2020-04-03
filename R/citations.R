#' @name MADtraits_citations
#' @title Citations for all datasets within MADtraits
#' @description Citations for all datasets within MADtraits. These *MUST* be
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
#' @param x The MADtraits database object for which you want citations
#' @param format One of "bibtex", "ris", "citeproc", or "schema.org",
#'     which is the format in which your citations will be
#'     returned. Default is "bibtex"
#' @return Character vector of citations in chosen format.
#' @author Will Pearse; Sylia Kinosian
#' @export
#' @importFrom handlr bibtex_reader bibtex_writer ris_writer
#'     citeproc_writer schema_org_writer
citations <- function(x, format=c("bibtex","ris", "citeproc","schema.org")){
    if(!inherits(x, "MADtraits"))
        stop("'", deparse(substitute(x)), "' must be of type 'MADtraits'")
    if(!("dataset" %in% names(x$numeric) & "dataset" %in% names(x$categorical)))
        stop("'", deparse(substitute(x)), "' must be an output from MADtraits, not a single function/dataset")
    format <- match.arg(format)
    
    data("MADtraits_citations", envir=environment())
    datasets <- unique(c(x$categorical$dataset,x$numeric$dataset))
    citations <- with(MADtraits_citations, BibTeX.citation[match(datasets, author_year)])
    citations <- bibtex_reader(unique(citations[!is.na(citations)]))
    return(switch(format,
                bibtex = bibtex_writer(citations),
                ris = ris_writer(citations),
                citeproc = citeproc_writer(citations),
                citeproc = schema_org_writer(citations)
                ))
}

