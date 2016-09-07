natdb <- function(datasets, species, traits){
    #Check datasets
    datasets <- paste0(".", tolower(datasets))
    datasets <- gsub("..", ".", datasets, fixed=TRUE)
    namespaces <- intersect(search(), c("package:natdb",".GlobalEnv") #Makes debugging easier
    functions <- Filter(Negate(is.function), ls(pattern="^\\.", all.names=TRUE, name=namespaces))
    if(!all(datasets %in% functions)){
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
