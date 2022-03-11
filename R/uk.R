#' donor-recipient Risk Index Combination
#'
#' @description computes Risk Index Combination for each pair donor-recipient
#' @param DRI Donor RisK Index group from options: 'D1','D2','D3','D4'
#' @param data A data file with candidates information for UK transplant
#' @param D1R1 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D1R2 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D1R3 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D1R4 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D2R1 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D2R2 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D2R3 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D2R4 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D3R1 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D3R2 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D3R3 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D3R4 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D4R1 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D4R2 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D4R3 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D4R4 A numeric value (0-1000) for the combination of indexes DiRj
#' @return A tibble with a new column 'ric' that gives the  Risk Index Combination.
#' @examples
#' ric(DRI = 'D1', data =candidates.uk,
#' D1R1 = 1000, D1R2 = 700, D1R3 = 350, D1R4 = 0,
#' D2R1 = 700, D2R2 = 1000, D2R3 = 500, D2R4 = 350,
#' D3R1 = 350, D3R2 = 500, D3R3 = 1000, D3R4 = 700,
#' D4R1 = 0, D4R2 = 350, D4R3 = 700, D4R4 = 1000)
#' @export
ric<-function(DRI = 'D1',
              data =candidates.uk,
              D1R1 = 1000, D1R2 = 700, D1R3 = 350, D1R4 = 0,
              D2R1 = 700, D2R2 = 1000, D2R3 = 500, D2R4 = 350,
              D3R1 = 350, D3R2 = 500, D3R3 = 1000, D3R4 = 700,
              D4R1 = 0, D4R2 = 350, D4R3 = 700, D4R4 = 1000
) {

  # verify function parameters
  if(!DRI %in% c('D1','D2','D3','D4')){stop("DRI is not a valid option! Select of 'D1','D2','D3','D4' \n")
  } else if (D1R1 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D1R2 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D1R3 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D1R4 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D2R1 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D2R2 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D2R3 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D2R4 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D3R1 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D3R2 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D3R3 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D3R4 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D4R1 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D4R2 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D4R3 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  } else if (D4R4 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  }

  if (DRI == 'D1') {
    data <- data %>% dplyr::mutate(ric = dplyr::case_when(RRI == 'R1' ~ D1R1,
                                                          RRI == 'R2' ~ D1R2,
                                                          RRI == 'R3' ~ D1R3,
                                                          RRI == 'R4' ~ D1R4))
  } else if (DRI == 'D2') {
    data <- data %>% dplyr::mutate(ric = dplyr::case_when(RRI == 'R1' ~ D2R1,
                                                          RRI == 'R2' ~ D2R2,
                                                          RRI == 'R3' ~ D2R3,
                                                          RRI == 'R4' ~ D2R4))
  } else if (DRI == 'D3') {
    data <- data %>% dplyr::mutate(ric = dplyr::case_when(RRI == 'R1' ~ D3R1,
                                                          RRI == 'R2' ~ D3R2,
                                                          RRI == 'R3' ~ D3R3,
                                                          RRI == 'R4' ~ D3R4))
  } else {
    data <- data %>% dplyr::mutate(ric = dplyr::case_when(RRI == 'R1' ~ D4R1,
                                                          RRI == 'R2' ~ D4R2,
                                                          RRI == 'R3' ~ D4R3,
                                                          RRI == 'R4' ~ D4R4))
  }
  return(data)
}


#' Donor recipient age difference
#'
#' @description computes punctuation according to donor-recipient age difference
#' @param dage A numeric value with donor's age.
#' @param cage A numeric value with candidate's age.
#' @return A numeric value.
#' @examples
#' age_diff(dage = 60, cage = 50)
#' @export
age_diff<-function(dage = 60,
                   cage = 50){
  # verify ages
  if(!is.numeric(dage) | dage < 18 | dage > 99) {stop("donor's age is not valid!\n")}
  if(!is.numeric(cage) | cage < 18 | cage > 99) {stop("candidate's age is not valid!\n")}

  res<-NULL

  res<- (-1/2)*((dage-cage)^2)

  return(res)

}

#' blood group B match points
#'
#' @description computes penalization when donor's group O and  candidate's group B
#' @param cABO A character from 'A', 'B', 'AB', 'O'
#' @param dABO A character from 'A', 'B', 'AB', 'O'
#' @param tier A character value for UK transplant TIER classification
#' @param pts A negative value with penalization for B candidates
#' @return A numeric value.
#' @examples
#' b_blood(dABO = "B", cABO = "O", tier = "B", pts = -1000)
#' @export
b_blood<-function(dABO = "B",
                  cABO = "O",
                  tier = "B",
                  pts = -1000){
  if(!blood_group_checker(cABO)){stop("candidate's blood group is not valid!")}
  if(!blood_group_checker(dABO)){stop("donor's blood group is not valid!")}
  if(!is.numeric(pts) | pts>=0){stop('pts must be a negative value!')}

  res=NULL

  res<-ifelse(cABO == 'B' & dABO == 'O' & tier == 'B', pts, 0)

  return(res)
}



#' test for ABO compatibility on UK transplant
#'
#' @description ABO compatibility test between donor and candidate according to
#' TIER classification
#' @param cABO A character from 'A', 'B', 'AB', 'O', for candidate ABO group
#' @param dABO A character from 'A', 'B', 'AB', 'O', for donor ABO group
#' @param tier A character value for UK transplant candidate's TIER classification
#' (options A and B)
#' @return A logical value T/F
#' @examples
#' abo_uk(dABO = "A", cABO = "A", tier = "B")
#' @export
abo_uk<-function(dABO = "A", cABO = "A", tier = "B"){

  res = NULL
  # verify function parameters
  if (!dABO %in% c("A", "B", "AB", "O")) {stop("donor's group is not a valid option!\n")}
  if (!cABO %in% c("A", "B", "AB", "O")) {stop("candidate's group is not a valid option!\n")}
  if (! tier %in% c('A','B')) {stop("candidate's Tier is not a valid option!\n")}

  if(tier == 'B'){
    res<-ifelse(dABO == "O" & (cABO == "O" | cABO == "B"), TRUE,
                ifelse(dABO == "A" & (cABO == "A" | cABO == "AB"), TRUE,
                       ifelse(dABO == "B" & cABO == "B", TRUE,
                              ifelse(dABO == "AB" & cABO == "AB", TRUE, FALSE)
                       )
                )
    )
  } else {res<-ifelse(dABO == "O", TRUE,
                      ifelse(dABO == "A" & (cABO == "A" | cABO == "AB"), TRUE,
                             ifelse(dABO == "B" & cABO == "B", TRUE,
                                    ifelse(dABO == "AB" & cABO == "AB", TRUE, FALSE)
                             )
                      )
  )
  }

  return(res)
}

#' resume function for UK algorithm punctuation
#'
#' @description Ordering of waitlisted candidates for a given donor and according
#' to UK transplant algorithm.
#' @param DRI Donor RisK Index group from options: 'D1','D2','D3','D4'
#' @param dABO A character value with ABO blood group.
#' @param dA donor's HLA-A typing.
#' @param dB donor's HLA-B typing.
#' @param dDR donor's HLA-DR typing.
#' @param dage A numeric value with donor's age.
#' @param data A data frame containing demographics and medical information for
#' a group of waitlisted transplant for UK transplant.
#' @param D1R1 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D1R2 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D1R3 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D1R4 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D2R1 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D2R2 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D2R3 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D2R4 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D3R1 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D3R2 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D3R3 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D3R4 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D4R1 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D4R2 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D4R3 A numeric value (0-1000) for the combination of indexes DiRj
#' @param D4R4 A numeric value (0-1000) for the combination of indexes DiRj
#' @param ptsDial A numeric value for the points corresponding to each month
#' on dialysis
#' @param a1 A numeric value for HLA match and age combined formula:
#' b1*cos(age/18)+a1
#' @param a2 A numeric value for HLA match and age combined formula:
#' b2*cos(age/18)+a2
#' @param b1 A numeric value for HLA match and age combined formula:
#' b1*cos(age/18)+a1
#' @param b2 A numeric value for HLA match and age combined formula:
#' b2*cos(age/18)+a2
#' @param b3 A numeric value for HLA match and age combined formula:
#' b3*sin(age/50)
#' @param m A numeric value for matchability formula: m * (1+(MS/nn)^o)
#' @param nn A numeric value for matchability formula: m * (1+(MS/nn)^o)
#' @param o A numeric value for matchability formula: m * (1+(MS/nn)^o)
#' @param mm1 A numeric value to penalize 1 mm
#' @param mm23 A numeric value to penalize 2-3 mm
#' @param mm46 A numeric value to penalize 4-6 mm
#' @param pts A negative value with penalization for B candidates
#' @param df.abs A data frame with candidates' antibodies.
#' @param n A positive integer to slice the first candidates.
#' @examples
#' uk1_v0(DRI = 'D1', dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"),
#' dABO = "O", dage = 65, data = candidates.uk,
#' D1R1 = 1000, D1R2 = 700, D1R3 = 350, D1R4 = 0,
#' D2R1 = 700, D2R2 = 1000, D2R3 = 500, D2R4 = 350,
#' D3R1 = 350, D3R2 = 500, D3R3 = 1000, D3R4 = 700,
#' D4R1 = 0, D4R2 = 350, D4R3 = 700, D4R4 = 1000,
#' ptsDial = 1,
#' a1 = 2300,  a2 = 1500, b1 = 1200, b2 = 750, b3 = 400,
#' m = 40, nn = 4.5, o = 4.7,
#' mm1 = -100, mm23 = -150, mm46 = -250,
#' pts = -1000,
#' df.abs = cabs, n = 2)
#' @export
uk1_v0<-function(DRI = 'D1',
              dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"),
              dABO = "O",
              dage = 65,
              data = candidates.uk,
              D1R1 = 1000, D1R2 = 700, D1R3 = 350, D1R4 = 0,
              D2R1 = 700, D2R2 = 1000, D2R3 = 500, D2R4 = 350,
              D3R1 = 350, D3R2 = 500, D3R3 = 1000, D3R4 = 700,
              D4R1 = 0, D4R2 = 350, D4R3 = 700, D4R4 = 1000,
              ptsDial = 1,
              a1 = 2300,  a2 = 1500, b1 = 1200, b2 = 750, b3 = 400,
              m = 40, nn = 4.5, o = 4.7,
              mm1 = -100, mm23 = -150, mm46 = -250,
              pts = -1000,
              df.abs = cabs,
              n = 2
              ){

  data <- merge(data,
                xmatch_r(dA = dA, dB = dB, dDR = dDR, df.abs = df.abs),
                all.x=TRUE)

  data <- ric(DRI = DRI, D1R1 = D1R1, D1R2 = D1R2, D1R3 = D1R3, D1R4 = D1R4,
             D2R1 = D2R1, D2R2 = D2R2, D2R3 = D2R3, D2R4 = D2R4,
             D3R1 = D3R1, D3R2 = D3R2, D3R3 = D3R3, D3R4 = D3R4,
             D4R1 = D4R1, D4R2 = D4R2, D4R3 = D4R3, D4R4 = D4R4,
             data = data)

  data <- data %>%
    #as.data.frame() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(donor_age = dage,
                  compBlood = abo_uk(dABO = dABO, cABO = bg, tier = Tier),
                  mmA = mmHLA_r(dA = dA, dB = dB, dDR = dDR,
                                cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmA"],
                  mmB = mmHLA_r(dA = dA, dB = dB, dDR = dDR,
                                cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmB"],
                  mmDR = mmHLA_r(dA = dA, dB = dB, dDR = dDR,
                                 cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmDR"],
                  mmHLA = mmA + mmB + mmDR,
                  level = dplyr::case_when(mmA + mmB + mmDR == 0 ~ 1,
                                    (mmDR == 0 & mmB <=1) | (mmDR == 1 & mmB == 0) ~ 2,
                                    (mmDR == 0 & mmB == 2) |(mmDR == 1 & mmB == 1) ~ 3,
                                    TRUE ~ 4),
                  pts.hla.age = dplyr::case_when(level == 1 ~ b1*cos(age/18)+a1,
                                          level == 2 ~ b2*cos(age/18)+a2,
                                          TRUE ~ b3*sin(age/50)
                                          ),
                  total.HLA = dplyr::case_when(mmA + mmB + mmDR == 0 ~ 0,
                                        mmA + mmB + mmDR == 1 ~ mm1,
                                        mmA + mmB + mmDR < 4 ~ mm23,
                                        TRUE ~ mm46),
                  matchability = round(m * (1+(MS/nn)^o),1),  # compute matchability points from Match Score
                  pts.age = age_diff(dage = dage, cage = age),
                  pts.abo = b_blood(dABO = dABO, cABO = bg, tier = Tier, pts = pts),
                  pointsUK = round(ifelse(Tier == "A",
                                          9999,
                                          ric + pts.hla.age + matchability + pts.age + total.HLA + pts.abo),1)
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(compBlood == TRUE & (xm == 'NEG' | is.na(xm))) %>%
    dplyr::arrange(Tier, desc(pointsUK), desc(matchability), desc(dialysis)) %>%
    dplyr::slice(1:n) %>%
    dplyr::select(ID, bg,
                  A1, A2, B1, B2, DR1, DR2,
                  matchability,
                  mmA, mmB, mmDR, mmHLA,
                  age, donor_age, dialysis, cPRA, Tier,
                  pointsUK)

  return(data)

}
