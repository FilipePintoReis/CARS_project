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

  if (...[["function_name"]] == "uk") {
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

  else if (...[["function_name"]] == "lima") {
    stopifnot(1 == 1)

    lst <- purrr::pmap(df, ...[["algorithm"]],
                     iso = ...[["iso"]],
                     n = ...[["n"]],
                     df.abs = ...[["df.abs"]],
                     data = ...[["df.candidates"]],
                     q2 = ...[["q2"]],
                     q3 = ...[["q3"]],
                     cPRA1 = ...[["cPRA1"]],
                     cPRA2 = ...[["cPRA2"]])
  }

  else if (...[["function_name"]] == "et") {
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
                     mm6 = ...[["mm6"]],
                     hlaA = ...[["hlaA"]],
                     hlaB = ...[["hlaB"]],
                     hlaDR = ...[["hlaDR"]],
                     abo_freq = ...[["abo_freq"]])
  }

  else if (...[["function_name"]] == "pt") {
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

  else {
    stop("The function specified in 'function_name' does not exist.")
  }


  names(lst) <- df.donors$ID

  return(lst)
}

#' Generic function that runs the matchability between all combinations of donors and candidates.
#' Runs an arbitrary number of times to provide statistics
#'
#' @description Generic function that runs the matchability between all combinations of donors and candidates.
#' Runs an arbitrary number of times to provide statistics
#' @param iteration.number Number of times the matchability runs.
#' @return Statistics related to all the times the function ran.
#' @examples
#' several(iteration.number = 10,
#' ...)
#' @export
several <- function(iteration.number = 10, ...){
  # defenir condições de validação dos inputs
  # ver função stopifnot()

  df.donors <- ...[["df.donors"]]
  df.donors$nrow <- 1:nrow(df.donors)

  pre_calculated_mappings <- calculate_donor_to_candidate_matchability(...)

  all.statistics <- list()

  for (. in 1:iteration.number){
    used.candidates <- NULL
    current.iteration.statistics <- NULL
    shuffled_donors <- sample(df.donors$nrow)

    for (j in 1:length(shuffled_donors)){
      tmp <- pre_calculated_mappings[[shuffled_donors[j]]] %>%
        dplyr::filter(!ID %in% used.candidates) %>% # This can be optimized
        dplyr::slice(1:2)

      current.iteration.statistics <- dplyr::bind_rows(current.iteration.statistics, tmp)

      used.candidates <- c(used.candidates, tmp$ID)
    }

    all.statistics <- append(all.statistics, list(current.iteration.statistics))
  }

  mean_age <- all.statistics %>% purrr::map(., ~mean(.x$age)) %>% unlist()
  mean_dialysis <- all.statistics %>% purrr::map(., ~mean(.x$dialysis)) %>% unlist()
  mean_cPRA <- all.statistics %>% purrr::map(., ~mean(.x$cPRA)) %>% unlist()
  freq_mmHLA <- all.statistics %>% purrr::map(., ~table(.x$mmHLA)) #%>% unlist()
  freq_mmA <- all.statistics %>% purrr::map(., ~table(.x$mmA)) #%>% unlist()
  freq_mmB <- all.statistics %>% purrr::map(., ~table(.x$mmB)) #%>% unlist()
  freq_mmDR <- all.statistics %>% purrr::map(., ~table(.x$mmDR)) #%>% unlist()
  freq_ABO <- all.statistics %>% purrr::map(., ~table(.x$bg))
  freq_HI <- all.statistics %>% purrr::map(., ~table(.x$HI))
  freq_color <- all.statistics %>% purrr::map(., ~table(.x$cp))
  freq_SP <- all.statistics %>% purrr::map(., ~table(.x$SP))

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

# No export, testing purposes
uk_several <- function(){
  return(
    several(iteration.number = 10,
            list(
              df.donors = donors.uk,
              df.abs = cabs,
              df.candidates = candidates.uk,
              algorithm = uk,
              n = nrow(candidates.uk),
              function_name = "uk",
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

# No export, testing purposes
lima_several <- function(){
  return(
    several(
      iteration.number = 10,
      list(iso = TRUE
        , df.donors = donors
        , df.abs = cabs
        , df.candidates = candidates
        , algorithm = lima
        , n = nrow(candidates)
        , function_name = "lima"
        , q2 = 60
        , q3 = 100
        , cPRA1 = 50
        , cPRA2 = 85
        )
      )
    )
}

# No export, testing purposes
et_several <- function(){
  return(
    several(
      iteration.number = 10,
      list(df.donors = donors
        , df.abs = cabs
        , df.candidates = candidates
        , algorithm = et
        , n = nrow(candidates)
        , function_name = "et"
        , iso = TRUE
        , month = 2
        , mm0 = 400
        , mm1 = 333.33
        , mm2 = 266.67
        , mm3 = 200
        , mm4 = 133.33
        , mm5 = 66.67
        , mm6 = 0
        , hlaA = hlaApt
        , hlaB = hlaBpt
        , hlaDR = hlaDRpt
        , abo_freq = ABOpt)
      )
    )
}

# No export, testing purposes
pt_several <- function(){
  return(
    several(
      iteration.number = 10,
      list(df.donors = donors
        , df.abs = cabs
        , df.candidates = candidates
        , algorithm = pt
        , n = nrow(candidates)
        , function_name = "pt"
        , iso = TRUE
        , pts.80 = 8
        , pts.50 = 4
        , pts.dial = 0.1
        , pts.age = 4
        )
      )
    )
}

##############
#' função que cria os os pares dador receptor para cada dador
#' deve ter como inputs os data frames de dadores, candidatos e anticorpos,
#' o número de candidatos a selecionar para cada dador (quando n = 0 seleciona todos)
#' e os parametros de cada algoritmo
donor_rec_pairs <- function(df.donors = donors,
                            df.candidates = candidates,
                            df.abs = cabs,
                            algorithm = lima,
                            n = 2, ...){
  if(!is.numeric(n))stop("'n' is not a valid numeric value!")

  df.donors <- df.donors %>%
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

  if(n == 0) n <- nrow(df.candidates)

  lst <- purrr::pmap(df.donors,
                     data = df.candidates,
                     df.abs = df.abs,
                     algorithm,
                     n=n,
                     ...)

  names(lst) <- df.donors$ID

  lst
}

## função several revisitada
# está sempre a dar o mesmo resultado


##
several_v1 <- function(iteration.number = 10,
                       df.donors = donors,
                       df.candidates = candidates,
                       df.abs = cabs,
                       algorithm = lima,
                       n = 0, ...){
  # defenir condições de validação dos inputs
  # ver função stopifnot()

  #df.donors <- ...[["df.donors"]]
  df.donors$nrow <- 1:nrow(df.donors)

  pre_calculated_mappings <- donor_rec_pairs(df.donors = df.donors,
                                             df.candidates = df.candidates,
                                             df.abs = df.abs,
                                             algorithm = algorithm,
                                             n = n, ...)

  all.statistics <- list()

  for (. in 1:iteration.number){
    used.candidates <- NULL
    current.iteration.statistics <- NULL
    shuffled_donors <- sample(df.donors$nrow)

    for (j in 1:length(shuffled_donors)){
      tmp <- pre_calculated_mappings[[shuffled_donors[j]]] %>%
        dplyr::filter(!ID %in% used.candidates) %>% # This can be optimized
        dplyr::slice(1:2)

      current.iteration.statistics <- dplyr::bind_rows(current.iteration.statistics, tmp)

      used.candidates <- c(used.candidates, tmp$ID)
    }

    all.statistics <- append(all.statistics, list(current.iteration.statistics))
  }

  mean_age <- all.statistics %>% purrr::map(., ~mean(.x$age)) %>% unlist()
  mean_dialysis <- all.statistics %>% purrr::map(., ~mean(.x$dialysis)) %>% unlist()
  mean_cPRA <- all.statistics %>% purrr::map(., ~mean(.x$cPRA)) %>% unlist()
  freq_mmHLA <- all.statistics %>% purrr::map(., ~table(.x$mmHLA)) #%>% unlist()
  freq_mmA <- all.statistics %>% purrr::map(., ~table(.x$mmA)) #%>% unlist()
  freq_mmB <- all.statistics %>% purrr::map(., ~table(.x$mmB)) #%>% unlist()
  freq_mmDR <- all.statistics %>% purrr::map(., ~table(.x$mmDR)) #%>% unlist()
  freq_ABO <- all.statistics %>% purrr::map(., ~table(.x$bg))
  freq_HI <- all.statistics %>% purrr::map(., ~table(.x$HI))
  freq_color <- all.statistics %>% purrr::map(., ~table(.x$cp))
  freq_SP <- all.statistics %>% purrr::map(., ~table(.x$SP))

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

###

several_v2 <- function(iteration.number = 10,
                       df.donors = donors,
                       df.candidates = candidates,
                       df.abs = cabs,
                       algorithm = lima,
                       n = 0, ...){
  # defenir condições de validação dos inputs
  # ver função stopifnot()

  #df.donors <- ...[["df.donors"]]
  df.donors$nrow <- 1:nrow(df.donors)

  pre_calculated_mappings <- donor_rec_pairs(df.donors = df.donors,
                                             df.candidates = df.candidates,
                                             df.abs = df.abs,
                                             algorithm = algorithm,
                                             n = n, ...)

  all.statistics <- list()

  for (. in 1:iteration.number){
    used.candidates <- NULL
    current.iteration.statistics <- NULL
    shuffled_donors <- sample(df.donors$nrow)

    for (j in 1:length(shuffled_donors)){
      tmp <- pre_calculated_mappings[[shuffled_donors[j]]][
        !ID %in% used.candidates,][
          1:2,]

      current.iteration.statistics <- data.table::rbindlist(list(current.iteration.statistics,
                                                                 tmp))

      used.candidates <- c(used.candidates, tmp$ID)
    }

    all.statistics <- append(all.statistics, list(current.iteration.statistics))
  }

  mean_age <- all.statistics %>% purrr::map(., ~mean(.x$age)) %>% unlist()
  mean_dialysis <- all.statistics %>% purrr::map(., ~mean(.x$dialysis)) %>% unlist()
  mean_cPRA <- all.statistics %>% purrr::map(., ~mean(.x$cPRA)) %>% unlist()
  freq_mmHLA <- all.statistics %>% purrr::map(., ~table(.x$mmHLA)) #%>% unlist()
  freq_mmA <- all.statistics %>% purrr::map(., ~table(.x$mmA)) #%>% unlist()
  freq_mmB <- all.statistics %>% purrr::map(., ~table(.x$mmB)) #%>% unlist()
  freq_mmDR <- all.statistics %>% purrr::map(., ~table(.x$mmDR)) #%>% unlist()
  freq_ABO <- all.statistics %>% purrr::map(., ~table(.x$bg))
  freq_HI <- all.statistics %>% purrr::map(., ~table(.x$HI))
  freq_color <- all.statistics %>% purrr::map(., ~table(.x$cp))
  freq_SP <- all.statistics %>% purrr::map(., ~table(.x$SP))

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

