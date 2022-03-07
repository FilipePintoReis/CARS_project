library(reticulate)

source_python("inst/python/mmHLA.py")


# R Test
start <- Sys.time()
mmHLA(c('1','2'), c('1','2'), c('1','2'), c('1','2'), c('1','2'), c('1','3'))
end <- Sys.time()
res = end - start
print("Results for R")
print(res)

# Python Test
start <- Sys.time()
py$mmHLA(c(1,2), c(1,2), c(1,2), c(1,2), c(1,2), c(1,3))
end <- Sys.time()
res = end - start
print("Results for Python")
print(res)
