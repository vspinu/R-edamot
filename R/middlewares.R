library(bencode)
library(nREPL)
source("~/VC/R-edamot/R/utils.R")


## INFO
mw_info <- middleware("info", handles = c("info"),
                      fun = function(h){
                          function(op, tr, symbol = NULL, ns = NULL, ...){
                              msg <- list(...)
                              if (op == "info"){
                                  obj <- get_in_ns(symbol, ns)
                                  info <- object_info(obj)
                                  tr$write(respfor(msg, ns = ns, symbol = symbol,
                                                   status = list("done"),
                                                   lst = info))
                              } else {
                                  msg <- nREPL:::assoc(
                                      msg, op = op, tr = tr,
                                      symbol = symbol, ns = ns,
                                      non_null = TRUE)
                                  do.call(h, msg)
                              }
                          }
                      })
assign("info", mw_info, envir = nREPL::middlewares)

## test_middleware(mw_info, op = "info", symbol = "iris")
## test_middleware(mw_info, op = "info", symbol = "iris", ns = "datasets")
## test_middleware(mw_info, op = "info", symbol = "iris", ns = "package:datasets")
## test_middleware(mw_info, op = "blabla", symbol = "iris", ns = "package:datasets")
## test_middleware(mw_info, op = "info", symbol = "plot", ns = "package:graphics")
## test_middleware(mw_info, op = "info", symbol = "plot", ns = "iris")
## test_middleware(mw_info, op = "info", symbol = "Sepal.Length", ns = "iris")
## test_middleware(mw_info, op = "info", symbol = ".GlobalEnv", ns = "namespace:base")


### FETCH
mw_fetch <- middleware("fetch", handles = c("fetch"),
                       fun = function(h){
                           function(op, tr, symbol = NULL, ns = NULL, recursive = NULL, ...){
                               msg <- list(...)
                               if (op == "fetch"){
                                   if (is.null(recursive)) recursive <- TRUE
                                   obj <- get_in_ns(symbol, ns)
                                   info <- struct_encode(obj, recursive = recursive)
                                   tr$write(respfor(msg, ns = ns, symbol = symbol,
                                                    status = list("done"),
                                                    lst = info))
                               } else {
                                   msg <- nREPL:::assoc(
                                       msg, op = op, tr = tr,
                                       symbol = symbol, ns = ns, recursive = recursive, 
                                       non_null = TRUE)
                                   do.call(h, msg)
                               }
                           }
                       })
assign("fetch", mw_fetch, envir = nREPL::middlewares)

## test_middleware(mw_fetch, op = "fetch", symbol = "iris", ns = "package:datasets")
## test_middleware(mw_info, op = "info", symbol = "iris", ns = "datasets")
## test_middleware(mw_info, op = "info", symbol = "iris", ns = "package:datasets")
## test_middleware(mw_info, op = "blabla", symbol = "iris", ns = "package:datasets")
## test_middleware(mw_info, op = "info", symbol = "plot", ns = "package:graphics")
## test_middleware(mw_info, op = "info", symbol = "plot", ns = "iris")
## test_middleware(mw_info, op = "info", symbol = "Sepal.Length", ns = "iris")
## test_middleware(mw_info, op = "info", symbol = ".GlobalEnv", ns = "namespace:base")


### NS
mw_ns <- middleware("ns", handles = c("ns-list", "ns-vars"),
                    fun = function(h){
                        function(op, tr, ns = NULL, pattern = NULL, type = NULL, ...){
                            msg <- list(...)
                            if( op %in% c("ns-list", "ns-vars")){
                                value <- switch(op,
                                                'ns-vars' = ns_vars(pattern, ns, type),
                                                'ns-list' = ns_list(pattern, type))
                                tr$write(respfor(msg, ns = ns, type = type,
                                                 status = list("done"),
                                                 value = as.list(value)))
                            } else {
                                msg <- nREPL:::assoc(
                                    msg, op = op, tr = tr,
                                    ns = ns, pattern = pattern, type = type, 
                                    non_null = TRUE)
                                do.call(h, msg)
                            }
                        }
                    })
assign("ns", mw_ns, envir = nREPL::middlewares)

## test_middleware(mw_ns, op = "ns-list")
## test_middleware(mw_ns, op = "ns-list", type = "package")
## test_middleware(mw_ns, op = "ns-list", pattern = "sta", type = "package")
## test_middleware(mw_ns, op = "ns-list", pattern = "middddd", type = "environment")
## test_middleware(mw_ns, op = "ns-list", pattern = "mid", type = "environment")
## test_middleware(mw_ns, op = "ns-list", pattern = "ir", type = "list")
## test_middleware(mw_ns, op = "ns-vars", ns = "iris")
