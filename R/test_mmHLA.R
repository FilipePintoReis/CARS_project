library(reticulate)

source_python("inst/python/mmHLA.py")

#' number of HLA mismatchs
#'
#' @description Computes the number of HLA mismatchs between donor and candidate
#' @param dA donor's HLA-A typing
#' @param dB donor's HLA-B typing
#' @param dDR donor's HLA-DR typing
#' @param cA candidate's HLA-A typing
#' @param cB candidate's HLA-B typing
#' @param cDR candidate's HLA-DR typing
#' @return mmA number of HLA-A mismatchs between \code{dA} and \code{cA};
#' mmB number of HLA-B mismatchs between \code{dB} and \code{cB};
#' mmDR number of HLA-DR mismatchs between \code{dA}DRand \code{cDR};
#' and mmHLA as the sum of mmA + mmB + mmDR
#' @examples
#' mmHLA(dA = c('1','2'), dB = c('5','7'), dDR = c('1','4'), cA = c('1','2'), cB = c('03','15'), cDR = c('04','07'))
#' @export
mmHLA <- function(dA = c('1','2'), dB = c('5','7'), dDR = c('1','4'),
                  cA = c('1','2'), cB = c('3','15'), cDR = c('4','7')){

  mmA = NULL
  mmB = NULL
  mmDR = NULL

  # verify function parameters
  if(!is.character(dA)){stop("donor's HLA-A typing is not valid!\n")}
  if(!is.character(dB)){stop("donor's HLA-B typing is not valid!\n")}
  if(!is.character(dDR)){stop("donor's HLA-DR typing is not valid!\n")}
  if(!is.character(cA)){stop("candidate's HLA-A typing is not valid!\n")}
  if(!is.character(cB)){stop("candidate's HLA-B typing is not valid!\n")}
  if(!is.character(cDR)){stop("candidate's HLA-DR typing is not valid!\n")}

  # compute missmatches
  mmA<-dplyr::if_else((dA[1] %in% cA & dA[2] %in% cA) | (dA[1] %in% cA & (is.na(dA[2]) | dA[2] == "")), 0,
                      dplyr::if_else(dA[1] %in% cA | dA[2] %in% cA, 1,
                                     dplyr::if_else(!dA[1] %in% cA & (is.na(dA[2]) | dA[2] == ""), 1,
                                                    dplyr::if_else(dA[1] == dA[2], 1,2))))

  mmB<-dplyr::if_else((dB[1] %in% cB & dB[2] %in% cB) | (dB[1] %in% cB & (is.na(dB[2]) | dB[2] == "")), 0,
                      dplyr::if_else(dB[1] %in% cB | dB[2] %in% cB, 1,
                                     dplyr::if_else(!dB[1] %in% cB & (is.na(dB[2]) | dB[2] == ""), 1,
                                                    dplyr::if_else(dB[1] == dB[2], 1,2))))

  mmDR<-dplyr::if_else((dDR[1] %in% cDR & dDR[2] %in% cDR) | (dDR[1] %in% cDR & (is.na(dDR[2]) | dDR[2] == "")), 0,
                       dplyr::if_else(dDR[1] %in% cDR | dDR[2] %in% cDR, 1,
                                      dplyr::if_else(!dDR[1] %in% cDR & (is.na(dDR[2]) | dDR[2] == ""), 1,
                                                     dplyr::if_else(dDR[1] == dDR[2],1,2))))

  # resume results
  mmHLA = mmA + mmB + mmDR
  mm = c(mmA,mmB,mmDR,mmHLA)
  names(mm) <- c("mmA","mmB","mmDR","mmHLA")

  return(mm)
}

# R Test
start <- Sys.time()
mmHLA(c('1','2'), c('1','2'), c('1','2'), c('1','2'), c('1','2'), c('1','3'))
end <- Sys.time()
res = end - start
print(res)

# Python Test
start <- Sys.time()
py$mmHLA(c(1,2), c(1,2), c(1,2), c(1,2), c(1,2), c(1,3))
end <- Sys.time()
res = end - start
print(res)
