library(bencode)
library(nREPL)
source("~/VC/R-edamot/R/utils.R")


## INFO
mw_info <- middleware("info", handles = c("info"),
                      fun = function(h){
                          function(op, transport, symbol = NULL, ns = NULL, ...){
                              msg <- list(...)
                              if (op == "info"){
                                  obj <- get_in_ns(symbol, ns, transport)
                                  info <- object_info(obj)
                                  transport$write(respfor(msg, ns = ns, symbol = symbol,
                                                          status = list("done"),
                                                          lst = info))
                              } else {
                                  msg <- nREPL:::assoc(
                                      msg, op = op, transport = transport,
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
                           function(op, transport, symbol = NULL, ns = NULL, recursive = NULL, ...){
                               msg <- list(...)
                               if (op == "fetch"){
                                   if (is.null(recursive)) recursive <- TRUE
                                   obj <- get_in_ns(symbol, ns, transport)
                                   info <- struct_encode(obj, recursive = recursive)
                                   transport$write(respfor(msg, ns = ns, symbol = symbol,
                                                           status = list("done"),
                                                           lst = info))
                               } else {
                                   msg <- nREPL:::assoc(
                                       msg, op = op, transport = transport,
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
