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
py$xmatch(c(1,2), c(1,2), c(1,2), paste("data", "csv", "cabs.csv", sep = .Platform$file.sep), "None")
end <- Sys.time()
res = end - start
print("Results for Python")
print(res)
