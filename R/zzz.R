# .onLoad <- function(...) {
#   
#   requireNamespace("ggplot2", quietly = TRUE)
#   
#   # backport_unit_methods()
#   # if (requireNamespace("ggplot2", quietly = TRUE)) {
#   #   
#   # }
# }
# 
# requireNamespace("caret")
# 
# read.dcf()
# file.path()
# 
# library.dynam
# 
# ples
# ## Which DLLs were dynamically loaded by packages?
# library.dynam()
# 
# ## More on library.dynam.unload() :
# require(nlme)
# nlme:::.onUnload # shows library.dynam.unload() call
# detach("package:nlme")  # by default, unload=FALSE ,  so,
# tail(library.dynam(), 2)# nlme still there
# 
# ## How to unload the DLL ?
# ## Best is to unload the namespace,  unloadNamespace("nlme")
# ## If we need to do it separately which should be exceptional:
# pd.file <- attr(packageDescription("nlme"), "file")
# library.dynam.unload("nlme", libpath = sub("/Meta.*", '', pd.file))
# tail(library.dynam(), 2)# 'nlme' is gone now
# unloadNamespace("nlme") # now gives warning
