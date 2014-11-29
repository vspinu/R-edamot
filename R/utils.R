
get_ns <- function(ns){
    ## ns is always non-null; pre_handler assigns it
    if( ns %in% search() )
        as.environment(ns)
    else if (grepl("^namespace:", ns))
        asNamespace(gsub("^namespace:", "", ns))
    else {
        if (exists(ns, envir = globalenv()))
            get(ns, envir = globalenv())
        else
            backToTopError("No such ns: %s", ns)
    }
}

get_in_ns <- function(symbol, ns, transport){
    symbol <- as.character(symbol)
    ns_obj <- get_ns(ns)
    if(is.environment(ns_obj)){
        if (exists(symbol, ns_obj, inherits = FALSE)){
            get(symbol, envir = ns_obj)
        } else {
            backToTopError("Object '%s' does not exist in '%s'", symbol, ns)
        }
    } else ns_obj[[symbol]]
}

object_info <- function(obj){
    list(type = typeof(obj),
         class = class(obj),
         length = length(obj),
         `vector?` = is.vector(obj),
         `recursive?` = is.recursive(obj), 
         `table?` = !is.null(dim(obj)),
         `nr-rows` = nrow(obj),
         `nr-cols` = ncol(obj),
         names = if (is.environment(obj)) ls(obj, all.names=T)
                 else names(obj))
}

struct_encode <- function(obj, recursive = TRUE){
    info <- object_info(obj)
    if (recursive && is.recursive(obj))
        ## do not convert to bendict here. If no names, leave 
        obj <- lapply(obj, struc_encode, recursive = recursive)
    bendict(info = info, data = obj)
}


