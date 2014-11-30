
get_ns <- function(ns){
    ## ns is always non-null; pre_handler assigns it
    if( ns %in% search() )
        as.environment(ns)
    else if (grepl("^namespace:", ns))
        asNamespace(gsub("^namespace:", "", ns))
    else {
        if (exists(ns, envir = globalenv()))
            get(ns, envir = globalenv())
        else {
            backToTopError("No such ns: %s", ns)
            NULL
        }
    }
}

get_in_ns <- function(symbol, ns){
    if(is.null(ns)){
        ns <- ".GlobalEnv"
        inher <- TRUE
    } else
        inher <- FALSE
    symbol <- as.character(symbol)
    ns_obj <- get_ns(ns)
    if(is.environment(ns_obj)){
        if (exists(symbol, ns_obj, inherits = inher))
            get(symbol, envir = ns_obj)
        else
            backToTopError("Object '%s' does not exist in '%s'", symbol, ns)
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
        obj <- lapply(obj, struct_encode, recursive = recursive)
    bendict(info = info, data = obj)
}

ns_vars <- function (pattern = NULL, ns = NULL, mode = NULL) {
    if (is.null(pattern)) pattern <- ".*"
    if (is.null(mode)) mode <- "any"
    accum <- character(0L)
    .ls1 <- function(ns_obj){
        li <- grep(pattern, ls(pos = ns_obj, all.names = TRUE), value = TRUE)
        if (length(li) > 0 && mode != "any")
            li <- li[sapply(li, exists, where = ns_obj, inherits = FALSE, mode = mode)]
        li
    }
    out <-
        if (!is.null(ns))
            .ls1(get_ns(ns))
        else
            unlist(lapply(search(), .ls1), use.names=F)
    sort(out)
}


ns_list <- function (pattern = NULL, type = NULL){
    if(is.null(pattern)) pattern <- ".*"
    if(is.null(type)) type <- "search"
    switch(type,
           search = grep(pattern, search(), value = T),
           package = grep(pattern, grep("^package:", search(), value = T), value = T), 
           namespace = paste0("namespace:", grep(pattern, loadedNamespaces(), value = T)), 
           environment = ns_vars(pattern = pattern, mode = "environment"),
           list = ns_vars(pattern = pattern, mode = "list"),
           backToTopError("Invalid `type` argument (%s).\n Accepted types are 'search', 'package', 'namespace', 'environment' and 'list'.",
                          type))
}

## ns_list(typ = "namespace");
## ns_list(typ = "package")
## ns_list("[z].*", type = "environment")
## ns_list("^[a-z].*", type = "environment")


