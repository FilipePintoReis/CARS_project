#' Candidates' selection according to any algorithm for multiple donors
#'
#' @description Ordering of waitlisted candidates for a given donor and
#' according to to any algorithm.
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
                         , algorithm = et1_v1
                         , n = 2){

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
                         , algorithm = et1_v1
                         , iteration.number = 2){

  df.donors$nrow <- 1:nrow(df.donors)

  pre_calculated_mappings <- mult(iso = iso
                         , df.donors = df.donors
                         , df.abs = df.abs
                         , df.candidates = df.candidates
                         , algorithm = algorithm
                         , n = nrow(df.candidates))

  # print(pre_calculated_mappings)
  # return()
  
  # Criar objeto stats que é uma lista de estatísticas
  stats <- list()

  for (i in 1:iteration.number){
    shuffled_donors <- df.donors[sample(1:nrow(df.donors)), ]

    # Create used candidates.
    cc <- NULL
    # Create Local Results.
    res <- NULL
    
    for (j in 1:nrow(shuffled_donors)){
      tmp <- pre_calculated_mappings[[ shuffled_donors[j,]$nrow ]] %>%
        dplyr::filter(!ID %in% cc) %>% # This can be optimized
        dplyr::slice(1:2)

      # print(tmp)
      # return()
      res <- dplyr::bind_rows(res, tmp)
  
      cc <- c(cc, tmp$ID)

      # if (!is.null(res)){
      #   res <- mapply(c, res, tmp, SIMPLIFY = FALSE)
      # }
      # else {
      #   res <- tmp
      # }
    }

    # temos o res.
    # tmp_stats = tirar estatísticas do res (e.g., média de idades.)
    tmp_stats <- NULL

    tmp_stats$A1_mean        <- sapply(list(sapply(res$A1, as.integer)) , mean)
    tmp_stats$A2_mean        <- sapply(list(sapply(res$A2, as.integer)) , mean)
    tmp_stats$B1_mean        <- sapply(list(sapply(res$B1, as.integer)) , mean)
    tmp_stats$B2_mean        <- sapply(list(sapply(res$B2, as.integer)) , mean)
    tmp_stats$DR1_mean       <- sapply(list(sapply(res$DR1, as.integer)), mean)
    tmp_stats$DR2_mean       <- sapply(list(sapply(res$DR2, as.integer)), mean)
    tmp_stats$mmA_mean       <- sapply(list(res$mmA)                    , mean)  
    tmp_stats$mmB_mean       <- sapply(list(res$mmB)                    , mean)  
    tmp_stats$mmDR_mean      <- sapply(list(res$mmDR)                   , mean)   
    tmp_stats$mmHLA_mean     <- sapply(list(res$mmHLA)                  , mean)     
    tmp_stats$age_mean       <- sapply(list(res$age)                    , mean)      
    tmp_stats$donor_age_mean <- sapply(list(res$donor_age)              , mean)          
    tmp_stats$dialysis_mean  <- sapply(list(res$dialysis)               , mean)       
    tmp_stats$cPRA_mean      <- sapply(list(res$cPRA)                   , mean)   
    # tmp_stats$cp        <- sapply(res$cp       , mean)
    tmp_stats$SP_mean        <- sapply(list(res$SP)                     , mean)      


    tmp_stats$A1_median        <- sapply(list(sapply(res$A1, as.integer)) , median)
    tmp_stats$A2_median        <- sapply(list(sapply(res$A2, as.integer)) , median)
    tmp_stats$B1_median        <- sapply(list(sapply(res$B1, as.integer)) , median)
    tmp_stats$B2_median        <- sapply(list(sapply(res$B2, as.integer)) , median)
    tmp_stats$DR1_median       <- sapply(list(sapply(res$DR1, as.integer)), median)
    tmp_stats$DR2_median       <- sapply(list(sapply(res$DR2, as.integer)), median)
    tmp_stats$mmA_median       <- sapply(list(res$mmA)                    , median)  
    tmp_stats$mmB_median       <- sapply(list(res$mmB)                    , median)  
    tmp_stats$mmDR_median      <- sapply(list(res$mmDR)                   , median)   
    tmp_stats$mmHLA_median     <- sapply(list(res$mmHLA)                  , median)     
    tmp_stats$age_median       <- sapply(list(res$age)                    , median)      
    tmp_stats$donor_age_median <- sapply(list(res$donor_age)              , median)          
    tmp_stats$dialysis_median  <- sapply(list(res$dialysis)               , median)       
    tmp_stats$cPRA_median      <- sapply(list(res$cPRA)                   , median)   
    # tmp_stats$cp        <- sapply(res$cp       , median)
    tmp_stats$SP_median        <- sapply(list(res$SP)                     , median)  

    # print(tmp_stats)     
    stats <- append(stats, list(tmp_stats))
    # Fazer append de tmp_stats a stats.
  }

  print(stats)
  # Temos distribuição de stats.

}