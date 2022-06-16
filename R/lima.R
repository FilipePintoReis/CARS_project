#' Candidates' Color Priority
#'
#' @description Classification of candidates according to waiting list
#' time on dialysis' quartiles and two cPRA cutoff values.
#' @param data A data frame with information for candidates' waiting list.
#' @param q2 A numerical value for the median of candidates' waiting list.
#' @param q3 A numerical value for the third quartile of candidates' waiting list.
#' @param cPRA1 A numerical value (0-100) for the lower cPRA cutoff.
#' @param cPRA2 A numerical value (0-100) for the higher cPRA cutoff. cPRA2
#' must be greater than cPRA1.
#' @return A data frame with a new column 'cp' (color priority)
#' @examples
#' cp(data = candidates, q2 = 60, q3 = 100, cPRA1 = 50, cPRA2 = 85)
#' @export
cp <- function(data = candidates,
               q2 = 60,
               q3 = 100,
               cPRA1 = 50,
               cPRA2 = 85){

  # verify function parameters
  if(cPRA2 < cPRA1){
    stop("higher cPRA cutoff value (cPRA2) must be greater than lower cPRA cutoff (cPRA1)!\n")
  } else if (q2 >= q3){
    stop("median time on dialysis quartiles must be lower than third quartile: q2 < q3!\n")
    }

  data <- data %>%
    dplyr::mutate(cp = ifelse(urgent == 1, 1,
                            ifelse(cPRA >= cPRA2 | dialysis >= q3, 2,
                                   ifelse(cPRA >= cPRA1 | dialysis >= q2, 3, 4)
                                   )
                            ),
                  cp = factor(cp, levels = 1:4,
                              labels = c('Red', 'Orange', 'Yellow', 'Green')
                              )
                  )

  return(data)
  }

#' Candidates' ordering according to Lima's algorithm 'USING data.table'
#'
#' @description Ordering of waitlisted candidates for a given donor and
#' according to to Lima's algorithm.
#' @param iso A logical value for isogroupal compatibility.
#' @param dABO A character value with ABO blood group.
#' @param dA donor's HLA-A typing.
#' @param dB donor's HLA-B typing.
#' @param dDR donor's HLA-DR typing.
#' @param dage A numeric value with donor's age.
#' @param data A data frame containing demographics and medical information
#' for a group of waitlisted transplant candidates with
#' color priority classification.
#' @param df.abs A data frame with candidates' antibodies.
#' @param n A positive integer to slice the first candidates.
#' @param q2 A numerical value for the median of candidates' waiting list.
#' @param q3 A numerical value for the third quartile of candidates' waiting list.
#' @param cPRA1 A numerical value (0-100) for the lower cPRA cutoff.
#' @param cPRA2 A numerical value (0-100) for the higher cPRA cutoff. cPRA2
#' @return An ordered data frame with a column 'cp' (color priority),
#' 'sp', 'hi' and 'mmHLA'.
#' @examples
#' lima(iso = TRUE, dABO = "O",
#' dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"),
#' dage = 60, df.abs = cabs,
#' data = candidates, n = 2)
#' @export
lima <- function(iso = TRUE
                  , dABO = "O"
                  , dA = c("1","2"), dB = c("15","44"), dDR = c("1","4")
                  , dage = 60
                  , df.abs = cabs
                  , data = candidates
                  , n = 2
                  , q2 = 60
                  , q3 = 100
                  , cPRA1 = 50
                  , cPRA2 = 85){

  n <- max(1, n)

  data <- cp(data = data
            , q2 = q2
            , q3 = q3
            , cPRA1 = cPRA1
            , cPRA2 = cPRA2) %>% # Isto pode ser feito antes do for loop de candidato vs dador
    as.data.frame()

  xm <- xmatch_r(dA = dA, dB = dB, dDR = dDR, df.abs = df.abs)

  data.table::setDT(data, key = 'ID')
  data.table::setDT(xm, key = 'ID')

  data[, ID := as.character(ID)] # ensure ID as a character
  xm[, ID := as.character(ID)] # ensure ID as a character

  data <- merge(data, xm,
                by = 'ID',
                all.x=TRUE)

  data[, `:=`(
    donor_age = dage,
    SP = sp(cage = age, dage = dage),
    HI = hiper(cPRA = cPRA), # Isto pode ser feito antes do for loop de candidato vs dador
    compBlood = abo(iso = iso, dABO = dABO, cABO = bg)
    ), by = 'ID'][, row_n := 1:nrow(data)]

  l <- list()

  for (i in 1:nrow(data)){
    res <- mmHLA(dA = dA,
                 dB = dB,
                 dDR = dDR,
                 cA = c(data$A1[i], data$A2[i]),
                 cB = c(data$B1[i], data$B2[i]),
                 cDR = c(data$DR1[i], data$DR2[i])
    )

    l = append(l, res)
  }

  data[, `:=`(
    mmA = unlist(l[(1 + (row_n - 1) * 4)]),
    mmB = unlist(l[2 + (row_n - 1) * 4]),
    mmDR = unlist(l[3 + (row_n - 1) * 4]),
    mmHLA = unlist(l[4 + (row_n - 1) * 4])
  )]

  return(
    data[compBlood == TRUE & (xm == 'NEG' | is.na(xm)) & SP < 3,]
    [order(-SP, cp, mmHLA, -dialysis)]
    [1:n]
    [!is.na(ID),]
    [, .(ID,
         bg,
         A1,
         A2,
         B1,
         B2,
         DR1,
         DR2,
         mmA,
         mmB,
         mmDR,
         mmHLA,
         age,
         donor_age,
         dialysis,
         cPRA,
         HI,
         cp,
         SP)
    ]
  )
}
