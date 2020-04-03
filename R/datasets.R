#' @name MADtraits_datasets
#' @title Get vector of the IDs for all datasets available within
#'     MADtraits
#' @description Returns a vector of all IDs for the datasets that
#'     MADtraits can download. This can be used to identify particular
#'     datasets you wish to download using \code{\link{MADtraits}}.
#' @keywords datasets
#' @usage MADtraits_datasets()
#' @seealso citations MADtraits
#' @return Vector of dataset IDs
#' @author Will Pearse; Matthew Helmus
#' @examples
#' \dontrun{
#' datasets <- MADtraits_datasets()
#' data <- MADtraits(datasets = datasets[1:2])
#' }
#' 
#' @export
MADtraits_datasets <- function(){
    datasets <- Filter(Negate(is.function), ls(pattern="^\\.[a-z]*\\.[0-9]+", name="package:MADtraits", all.names=TRUE))
  return(datasets)
} 
