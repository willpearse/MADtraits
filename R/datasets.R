#' @name natdb_datasets
#' @title Get vector of the IDs for all datasets available within
#'     NATDB
#' @description Returns a vector of all IDs for the datasets that
#'     NATDB can download. This can be used to identify particular
#'     datasets you wish to download using \code{\link{natdb}}.
#' @keywords datasets
#' @usage natdb_datasets()
#' @seealso citations natdb
#' @return Vector of dataset IDs
#' @author Will Pearse; Matthew Helmus
#' @examples
#' \dontrun{
#' datasets <- natdb_datasets()
#' data <- natdb(datasets = datasets[1:2])
#' }
#' 
#' @export
natdb_datasets <- function(){
    datasets <- Filter(Negate(is.function), ls(pattern="^\\.[a-z]*\\.[0-9]+", name="package:natdb", all.names=TRUE))
  return(datasets)
} 
