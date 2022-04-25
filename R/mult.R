#' Candidates' selection according to any algorithm for multiple donors
#'
#' @description Ordering of waitlisted candidates for a given donor and
#' according to any algorithm.
#' @param iso A logical value for isogroupal compatibility.
#' @param df.donors A data frame containing demographics and medical information
#' for a poll of donors
#' @param df.candidates A data frame containing demographics and medical information
#' for a group of waitlisted transplant candidates with
#' color priority classification.
#' @param algoritm The name of the function to use
#' @param df.abs A data frame with candidates' antibodies.
#' @param n A positive integer to slice the first candidates.
#' @return An ordered data frame with a column 'cp' (color priority),
#' 'sp', 'hi' and 'mmHLA'.
#' @examples
#' calculate_donor_to_candidate_matchability(iso = TRUE,
#' df.donors = donors,
#' df.abs = cabs,
#' df.candidates = candidates,
#' n = 2)
#' @export
calculate_donor_to_candidate_matchability <- function(...){ 
  # Tem de ter sempre 
  # donors 
  # candidates.
  # n (que é sempre o tamanho do número de candidatos)

  # defenir condições de validação dos inputs
  # ver função stopifnot()

  df.donors <- ...[["df.donors"]]

  df <- df.donors %>%
    dplyr::mutate(dABO = bg,
                  dA = purrr::map2(.x = A1,
                            .y = A2,
                            ~c(.x,.y)),
                  dB = purrr::map2(.x = B1,
                            .y = B2,
                            ~c(.x,.y)),
                  dDR = purrr::map2(.x = DR1,
                             .y = DR2,
                             ~c(.x,.y)),
                  dage = age
                  ) %>%
    dplyr::select(dABO, dA, dB, dDR, dage)

  if (...[["function_name"]] == "uk1_v1") {
    stopifnot(1 == 1)

    lst <- purrr::pmap(df, ...[["algorithm"]],
                     n = ...[["n"]],
                     df.abs = ...[["df.abs"]],
                     data = ...[["df.candidates"]],
                     D1R1 = ...[["D1R1"]],
                     D1R2 = ...[["D1R2"]],
                     D1R3 = ...[["D1R3"]],
                     D1R4 = ...[["D1R4"]],
                     D2R1 = ...[["D2R1"]],
                     D2R2 = ...[["D2R2"]],
                     D2R3 = ...[["D2R3"]],
                     D2R4 = ...[["D2R4"]],
                     D3R1 = ...[["D3R1"]],
                     D3R2 = ...[["D3R2"]],
                     D3R3 = ...[["D3R3"]],
                     D3R4 = ...[["D3R4"]],
                     D4R1 = ...[["D4R1"]],
                     D4R2 = ...[["D4R2"]],
                     D4R3 = ...[["D4R3"]],
                     D4R4 = ...[["D4R4"]],
                     ptsDial = ...[["ptsDial"]],
                     a1 = ...[["a1"]],
                     a2 = ...[["a2"]],
                     b1 = ...[["b1"]],
                     b2 = ...[["b2"]],
                     b3 = ...[["b3"]],
                     m =  ...[["m"]],                   
                     nn = ...[["nn"]],
                     o = ...[["o"]],
                     mm1 = ...[["mm1"]],
                     mm23 = ...[["mm23"]], 
                     mm46 = ...[["mm46"]],
                     pts = ...[["pts"]])
  }

  else if (...[["function_name"]] == "lima1_v1") {
    stopifnot(1 == 1)

    lst <- purrr::pmap(df, ...[["algorithm"]],
                     iso = ...[["iso"]],
                     n = ...[["n"]],
                     df.abs = ...[["df.abs"]],
                     data = ...[["df.candidates"]])
  }

  else if (...[["function_name"]] == "et1_v1") {
    stopifnot(1 == 1)

    lst <- purrr::pmap(df, ...[["algorithm"]],
                     iso = ...[["iso"]],
                     n = ...[["n"]],
                     df.abs = ...[["df.abs"]],
                     data = ...[["df.candidates"]],
                     month = ...[["month"]], 
                     mm0 = ...[["mm0"]], 
                     mm1 = ...[["mm1"]], 
                     mm2 = ...[["mm2"]], 
                     mm3 = ...[["mm3"]], 
                     mm4 = ...[["mm4"]], 
                     mm5 = ...[["mm5"]], 
                     mm6 = ...[["mm6"]])
  }

    else if (...[["function_name"]] == "pt1_v1") {
      stopifnot(1 == 1)
      
      lst <- purrr::pmap(df, ...[["algorithm"]],
                     iso = ...[["iso"]],
                     n = ...[["n"]],
                     df.abs = ...[["df.abs"]],
                     data = ...[["df.candidates"]],
                     pts.80 = ...[["pts.80"]],
                     pts.50 = ...[["pts.50"]],
                     pts.dial = ...[["pts.dial"]],
                     pts.age = ...[["pts.age"]])
  }

  names(lst) <- df.donors$ID

  return(lst)
}

several <- function(iteration.number = 10, ...){
  # defenir condições de validação dos inputs
  # ver função stopifnot()
  
  df.donors <- ...[["df.donors"]]
  df.donors$nrow <- 1:nrow(df.donors)

  pre_calculated_mappings <- calculate_donor_to_candidate_matchability(...)

  res.mul <- list()

  for (. in 1:iteration.number){
    shuffled_donors <- sample(df.donors$nrow)

    # Create used candidates.
    cc <- NULL
    # Create Local Results.
    res <- NULL

    for (j in 1:length(shuffled_donors)){
      tmp <- pre_calculated_mappings[[shuffled_donors[j]]] %>%
        dplyr::filter(!ID %in% cc) %>% # This can be optimized
        dplyr::slice(1:2)

      res <- dplyr::bind_rows(res, tmp)

      cc <- c(cc, tmp$ID)
    }

    res.mul <- append(res.mul, list(res))
  }

  mean_age <- res.mul %>% purrr::map(., ~mean(.x$age)) %>% unlist()
  mean_dialysis <- res.mul %>% purrr::map(., ~mean(.x$dialysis)) %>% unlist()
  mean_cPRA <- res.mul %>% purrr::map(., ~mean(.x$cPRA)) %>% unlist()
  freq_mmHLA <- res.mul %>% purrr::map(., ~table(.x$mmHLA)) #%>% unlist()
  freq_mmA <- res.mul %>% purrr::map(., ~table(.x$mmA)) #%>% unlist()
  freq_mmB <- res.mul %>% purrr::map(., ~table(.x$mmB)) #%>% unlist()
  freq_mmDR <- res.mul %>% purrr::map(., ~table(.x$mmDR)) #%>% unlist()
  freq_ABO <- res.mul %>% purrr::map(., ~table(.x$bg))
  freq_HI <- res.mul %>% purrr::map(., ~table(.x$HI))
  freq_color <- res.mul %>% purrr::map(., ~table(.x$cp))
  freq_SP <- res.mul %>% purrr::map(., ~table(.x$SP))

  return(list(age = mean_age,
              dialysis = mean_dialysis,
              cPRA = mean_cPRA,
              mmHLA = freq_mmHLA,
              mmA = freq_mmA,
              mmB = freq_mmB,
              mmDR = freq_mmDR,
              ABO = freq_ABO,
              HI = freq_HI,
              color = freq_color,
              SP = freq_SP))
}


uk_several <- function(){
  return(
    several(iteration.number = 10, 
            list(
              df.donors = donors.uk, 
              df.abs = cabs, 
              df.candidates = candidates.uk, 
              algorithm = uk1_v1,
              n = nrow(candidates.uk), 
              function_name = "uk1_v1",
              D1R1 = 1000,
              D1R2 = 700,
              D1R3 = 350,
              D1R4 = 0,
              D2R1 = 700,
              D2R2 = 1000,
              D2R3 = 500,
              D2R4 = 350,
              D3R1 = 350,
              D3R2 = 500,
              D3R3 = 1000,
              D3R4 = 700,
              D4R1 = 0,
              D4R2 = 350,
              D4R3 = 700,
              D4R4 = 1000,
              ptsDial = 1,
              a1 = 2300,
              a2 = 1500,
              b1 = 1200,
              b2 = 750,
              b3 = 400,
              m = 40,
              nn = 4.5,
              o = 4.7,
              mm1 = -100,
              mm23 = -150,
              mm46 = -250,
              pts = -1000
            )
          )
        )
}

lima_several <- function(){
  return(
    several(
      iteration.number = 10, 
      list(
        df.donors = donors, 
        df.abs = cabs, 
        df.candidates = candidates, 
        algorithm = lima1_v2, 
        n = nrow(candidates), 
        function_name = "lima1_v2", 
        iso = TRUE
        )
      )
    )
}

et_several <- function(){
  return(
    several(
      iteration.number = 10, 
      list(df.donors = donors
        , df.abs = cabs
        , df.candidates = candidates
        , algorithm = et1_v1
        , n = nrow(candidates)
        , function_name = "et1_v1"
        , iso = TRUE
        , month = 2
        , mm0 = 400 
        , mm1 = 333.33 
        , mm2 = 266.67 
        , mm3 = 200
        , mm4 = 133.33 
        , mm5 = 66.67 
        , mm6 = 0)
      )
    )
}

pt_several <- function(){
  return(
    several(
      iteration.number = 10, 
      list(df.donors = donors
        , df.abs = cabs
        , df.candidates = candidates
        , algorithm = pt1_v1
        , n = nrow(candidates)
        , function_name = "pt1_v1"
        , iso = TRUE
        , pts.80 = 8
        , pts.50 = 4
        , pts.dial = 0.1
        , pts.age = 4
        )
      )
    )
}

