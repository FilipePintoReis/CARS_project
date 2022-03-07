library(reticulate)
library(dplyr)

load(file="data/cabs.RDA")
source_python("inst/python/xmatch.py")

#############################################################################################


# R Test
start <- Sys.time()
xmatch_r(c('1','2'),
             c('1','2'),
             c('1','2'),
             cabs)
end <- Sys.time()
res = end - start
print("Results for R")
print(res)


# Python Test
start <- Sys.time()
py$xmatch(c(1,2), c(1,2), c(1,2), "data/csv/cabs.csv", "None")
end <- Sys.time()
res = end - start
print("Results for Python")
print(res)
