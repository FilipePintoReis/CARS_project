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
#' mult(iso = TRUE,
#' df.donors = donors,
#' df.abs = cabs,
#' df.candidates = candidates,
#' n = 2)
#' @export
mult <- function(iso = TRUE
                         , df.donors = donors
                         , df.abs = cabs
                         , df.candidates = candidates
                         , algorithm = lima1_v2
                         , n = 2){

  # defenir condições de validação dos inputs
  # ver função stopifnot()

  id <- df.donors$ID

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

  lst <- purrr::pmap(df, algorithm,
                     iso = iso,
                     n = n,
                     df.abs = df.abs,
                     data = df.candidates)
  names(lst) <- id

  return(lst)
}

several <- function(iso = TRUE
                         , df.donors = donors
                         , df.abs = cabs
                         , df.candidates = candidates
                         , algorithm = lima1_v2
                         , iteration.number = 10){

  # defenir condições de validação dos inputs
  # ver função stopifnot()

  df.donors$nrow <- 1:nrow(df.donors)


  pre_calculated_mappings <- mult(iso = iso
                         , df.donors = df.donors
                         , df.abs = df.abs
                         , df.candidates = df.candidates
                         , algorithm = algorithm
                         , n = nrow(df.candidates))

  res.mul <- list()

  for (. in 1:iteration.number){
    #shuffled_donors <- df.donors[sample(1:nrow(df.donors)), ]
    shuffled_donors <- sample(df.donors$nrow)

    # Create used candidates.
    cc <- NULL
    # Create Local Results.
    res <- NULL

    for (j in 1:length(shuffled_donors)){ # como agora trabalho um vector uso a função length
      #tmp <- pre_calculated_mappings[[ shuffled_donors[j,]$nrow ]] %>%
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

  #return(res.mul)

}
