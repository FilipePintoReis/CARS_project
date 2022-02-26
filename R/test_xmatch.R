library(reticulate)
library(dplyr)

load(file="data/cabs.RDA")
source_python("inst/python/xmatch.py")

xmatch <- function(dA = c('1','2'),
                   dB = c('1','2'),
                   dDR = c('1','2'),
                   df.abs = cabs){

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop(
      "Package \"dplyr\" must be installed to use this function.",
      call. = FALSE
    )
  }

  dhla <- c(paste0('A',dA[1]),
            paste0('A',dA[2]),
            paste0('B',dB[1]),
            paste0('B',dB[2]),
            paste0('DR',dDR[1]),
            paste0('DR',dDR[2]))


  df.abs %>%
    dplyr::mutate(res = abs %in% dhla) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(xm = ifelse(sum(res)>0, "POS","NEG")) %>%
    dplyr::ungroup()

}
#############################################################################################


# R Test
start <- Sys.time()
xmatch(c('1','2'),
             c('1','2'),
             c('1','2'),
             cabs)
end <- Sys.time()
res = end - start
print(res)


# Python Test
start <- Sys.time()
py$xmatch(c(1,2), c(1,2), c(1,2), "data/csv/cabs.csv", "None")
end <- Sys.time()
res = end - start
print(res)
