#' Mismatch Probability (MMP) from ETKAS
#'
#' @description Mismatch Probability (MMP) is a calculation of the probability of receiving a kidney offer with
#' 0 and 1 broad HLA-A, -B or split DR mismatches based on 1000 kidneys offered,
#' taking into account AB0 blood group rules and PRA screening. Patients receive
#' between 0-100 MMPs
#' @source \url{https://www.eurotransplant.org/wp-content/uploads/2020/01/H4-Kidney.pdf}
#' @param data A data frame containing demographics and medical information for a group of waitlisted transplant candidates.
#' @param hlaA A data frame with HLA-A allele frequencies
#' @param hlaB A data frame with HLA-B allele frequencies
#' @param hlaDR A data frame with HLA-DR allele frequencies
#' @param abo_freq A data frame with ABO blood group frequencies
#' @examples
#' et_mm(data = candidates,
#' hlaA = hlaApt, hlaB = hlaBpt, hlaDR = hlaDRpt,
#' abo_freq = ABOpt)
#' @export
et_mmp<-function(data = candidates,
                 hlaA = hlaApt, hlaB = hlaBpt, hlaDR = hlaDRpt,
                 abo_freq = ABOpt){

  # compute the sum of squared frequencies for each loci with PT frequencies
  SallA <-sum((hlaA %>% drop_na() %>% .$freq)^2)
  SallB <-sum((hlaB %>% drop_na() %>% .$freq)^2)
  SallDR <-sum((hlaDR %>% drop_na() %>% .$freq)^2)

  data<-data %>% left_join(hlaA %>% select(A,freq), by = c("A1" = "A"))
  data<- data %>% rename(a1=freq)
  data<-data %>% left_join(hlaA %>% select(A,freq), by = c("A2" = "A"))
  data<- data %>% rename(a2=freq)
  data<-data %>% left_join(hlaB %>% select(B,freq), by = c("B1" = "B"))
  data<- data %>% rename(b1=freq)
  data<-data %>% left_join(hlaB %>% select(B,freq), by = c("B2" = "B"))
  data<- data %>% rename(b2=freq)
  data<-data %>% left_join(hlaDR %>% select(DR,freq), by = c("DR1" = "DR"))
  data<- data %>% rename(dr1=freq)
  data<-data %>% left_join(hlaDR %>% select(DR,freq), by = c("DR2" = "DR"))
  data<- data %>% rename(dr2=freq)
  data<-data %>% left_join(abo_freq, by = c("bg" = "abo"))
  data<- data %>% rename(abo=freq)

  # compute MMP2 and add it to the data file
  data$MMP2 <- with(data,
                    (((2*(a1+a2)*(1 - a1 - a2)) - a1^2 - a2^2 + SallA) /
                       ((a1+a2)^2))
                    + (((2*(b1+b2)*(1 - b1 - b2)) - b1^2 - b2^2 + SallB) /
                         ((b1+b2)^2))
                    + (((2*(dr1+dr2)*(1 - dr1 - dr2) ) - dr1^2 - dr2^2 + SallDR) /
                         ((dr1+dr2)^2))
  )

  # compute MMP0 and add it to the data file
  data$MMP0 <- with(data,
                    (a1+a2)^2 * (b1+b2)^2 * (dr1+dr2)^2)

  # compute MMP1 and add it to the data file
  data$MMP1 <- with(data,
                    MMP0 * MMP2)

  # compute MMP and add it to the data file
  data$MMP<-with(data,
                 100 * (1-(abo * (1-cPRA/100) * (MMP0 + MMP1)))^1000
  )

  return(data)


}


#' ET points for mmHLA
#'
#' @description Punctuation given for HLA mismatches within ET Kidney allocation system
#' @param dA donor's HLA-A typing
#' @param dB donor's HLA-B typing
#' @param dDR donor's HLA-DR typing
#' @param cA candidate's HLA-A typing
#' @param cB candidate's HLA-B typing
#' @param cDR candidate's HLA-DR typing
#' @param mm0 A numeric value with points for 0 HLA mm on ETKAS points table
#' @param mm1 A numeric value with points for 1 HLA mm on ETKAS points table
#' @param mm2 A numeric value with points for 2 HLA mm on ETKAS points table
#' @param mm3 A numeric value with points for 3 HLA mm on ETKAS points table
#' @param mm4 A numeric value with points for 4 HLA mm on ETKAS points table
#' @param mm5 A numeric value with points for 5 HLA mm on ETKAS points table
#' @param mm6 A numeric value with points for 6 HLA mm on ETKAS points table
#' @examples
#' et_mmHLA(dA = c("01","02"), dB = c("03","05"), dDR = c("04","06"),
# cA = c("01","02"), cB = c("03","05"), cDR = c("04","06"),
# mm0 = 400, mm1 = 333.33, mm2 = 266.67, mm3 = 200,
# mm4 = 133.33, mm5 = 66.67, mm6 = 0)
#' @export
et_mmHLA<-function(dA = c("01","02"), dB = c("03","05"), dDR = c("04","06"),
                   cA = c("01","02"), cB = c("03","05"), cDR = c("04","06"),
                   mm0 = 400,
                   mm1 = 333.33,
                   mm2 = 266.67,
                   mm3 = 200,
                   mm4 = 133.33,
                   mm5 = 66.67,
                   mm6 = 0){

  # verify function parameters
  if(!is.numeric(mm0) | mm0 < 0 | mm0 > 501){
    stop("points for 0 mmHLA (full match) is not valid!\n")}
  if(!is.numeric(mm1) | mm1 < 0 | mm1 > 501){
    stop("points for 1 mmHLA is not valid!\n")}
  if(!is.numeric(mm2) | mm2 < 0 | mm2 > 501){
    stop("points for 2 mmHLA is not valid!\n")}
  if(!is.numeric(mm3) | mm3 < 0 | mm3 > 501){
    stop("points for 3 mmHLA is not valid!\n")}
  if(!is.numeric(mm4) | mm4 < 0 | mm4 > 501){
    stop("points for 4 mmHLA is not valid!\n")}
  if(!is.numeric(mm5) | mm5 < 0 | mm5 > 501){
    stop("points for 5 mmHLA is not valid!\n")}
  if(!is.numeric(mm6) | mm6 < 0 | mm6 > 501){
    stop("points for 6 mmHLA is not valid!\n")}

  # apply mmHLA function
  mm<-mmHLA(dA = dA, dB = dB, dDR = dDR,
            cA = cA, cB = cB, cDR = cDR)

  pts<-dplyr::if_else(sum(mm[4]) == 0, mm0,
                      dplyr::if_else(sum(mm[4]) == 1, mm1,
                                     dplyr::if_else(sum(mm[4]) == 2, mm2,
                                                    dplyr::if_else(sum(mm[4]) == 3, mm3,
                                                                   dplyr::if_else(sum(mm[4]) == 4, mm4,
                                                                                  dplyr::if_else(sum(mm[4]) == 5, mm5,mm6))))))
  return(pts)
}


#' ET points for time on dialysis (in months)
#'
#' @description Punctuation given for each month on dialysis, within ET Kidney allocation system
#' @param dial A numeric value with candidate's time on dialysis, in months (between 0 and 500)
#' @param month A numeric value with the punctuation for each month (between 0 and 10)
#' @examples
#' et_dial(dial = 100, month = 2.78)
#' @export
et_dial<-function(dial = 0, month = 2.78){

  # verify function parameters
  if(!is.numeric(dial) | dial < 0 | dial > 499){
    stop("value for time on dialysis is not valid!\n")}
  if(!is.numeric(month) | month < 0 | month > 10){
    stop("attributed points for each month on dialysis is not valid!\n")}

  pts<-dial * month

  return(pts)

}


#' resume function for ET algorithm punctuation
#'
#' @description Ordering of waitlisted candidates for a given donor and according to ETKAS algorithm.
#' @param iso A logical value for isogroupal compatibility.
#' @param dABO A character value with ABO blood group.
#' @param dA donor's HLA-A typing.
#' @param dB donor's HLA-B typing.
#' @param dDR donor's HLA-DR typing.
#' @param dage A numeric value with donor's age.
#' @param data A data frame containing demographics and medical information for a group of waitlisted transplant candidates.
#' @param month A numeric value with the punctuation for each month (between 0 and 10)
#' @param mm0 A numeric value with points for 0 HLA mm on ETKAS points table
#' @param mm1 A numeric value with points for 1 HLA mm on ETKAS points table
#' @param mm2 A numeric value with points for 2 HLA mm on ETKAS points table
#' @param mm3 A numeric value with points for 3 HLA mm on ETKAS points table
#' @param mm4 A numeric value with points for 4 HLA mm on ETKAS points table
#' @param mm5 A numeric value with points for 5 HLA mm on ETKAS points table
#' @param mm6 A numeric value with points for 6 HLA mm on ETKAS points table
#' @param df.abs A data frame with candidates' antibodies.
#' @param n A positive integer to slice the first candidates.
#' @return An ordered data frame with a column 'cp' (color priority), 'sp', 'hi' and 'mmHLA'.
#' @examples
#' et1(iso = TRUE, dABO = "A", dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"),
# dage = 65, cdata = candidates, month = 2.78,
# mm0 = 400, mm1 = 333.33, mm2 = 266.67, mm3 = 200,
# mm4 = 133.33, mm5 = 66.67, mm6 = 0,
# df.abs = abs, n = 2)
#' @export
et1<-function(iso = TRUE, dABO = "A", dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"),
              dage = 65,
              data = candidates, month = 2.78,
              mm0 = 400, mm1 = 333.33, mm2 = 266.67, mm3 = 200,
              mm4 = 133.33, mm5 = 66.67, mm6 = 0,
              df.abs = abs,n = 2
              ){
  data<-cdata %>%
    left_join(xmatch_r.v2(df.abs = df.abs,
                        dA = dA, # donor's HLA-A typing
                        dB = dB, # donor's HLA-B typing
                        dDR = dDR))

  n <- max(1, n)

  merge(data,
        xmatch_r(dA = dA, dB = dB, dDR = dDR, df.abs = df.abs),
        all.x=TRUE) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(donor_age = dage,
           SP = ifelse(sp(dage = dage, cage = age) == 1, 1, 0),
           AM = ifelse(SP == 0 & cPRA >= 85, 1, 0),
           compBlood=ifelse(AM == 1, compABO(iso = FALSE, dABO = dABO, cABO = bg),
                            compABO(iso = iso, dABO = dABO, cABO = bg)),
           pointsHLA = et_mmHLA(dA = dA, dB = dB, dDR = dDR,
                                cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2),
                                mm0 = mm0,
                                mm1 = mm1,
                                mm2 = mm2,
                                mm3 = mm3,
                                mm4 = mm4,
                                mm5 = mm5,
                                mm6 = mm6),
           mmA = mmHLA(dA = dA, dB = dB, dDR = dDR,
                       cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmA"],
           mmB = mmHLA(dA = dA, dB = dB, dDR = dDR,
                       cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmB"],
           mmDR = mmHLA(dA = dA, dB = dB, dDR = dDR,
                        cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmDR"],
           mmHLA = mmA + mmB + mmDR,
           mm000 = ifelse(mmA + mmB + mmDR == 0, 1, 0),
           pointsDial = et_dial(month = month, dial = dialysis),
           pointsETx = round(pointsHLA + pointsDial + MMP)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(compBlood == TRUE & (xm == FALSE | is.na(xm))) %>%
    # mutate(pointsET = case_when(SP == 1 ~ dialysis,
    #                             TRUE ~ pointsETx)) %>%
    dplyr::mutate(pointsET = ifelse(SP == 1, dialysis, pointsETx),
           HI = hi(cPRA = cPRA, cutoff = 85)) %>%
    dplyr::arrange(desc(SP),desc(AM), desc(mm000), desc(pointsET)) %>%
    dplyr::slice(1:n) %>%
    dplyr::select(ID, bg,
           A1, A2, B1, B2, DR1, DR2,
           mmA, mmB, mmDR, mmHLA,
           age, donor_age, dialysis, cPRA, HI,
           pointsET, SP, AM)


}
