#' Tests if element is a valid Blood Group character
#' @param input_string A character from 'A', 'B', 'AB', 'O'
#' @return A logical value T/F
#' @noRd
blood_group_checker <- function(input_string){

  valid_blood_groups = c('O', 'A', 'B', 'AB')

  if (input_string %in% valid_blood_groups){
    return(TRUE)
    } else{return(FALSE)}
  }

#' Validates that the age of a person is not negative.
#' @param input_number A numeric value between 1 and 99 from 'A', 'B', 'AB', 'O'
#' @return A logical value T/F
#' @noRd
age_checker <- function(input_number){
  if (!is.numeric(input_number) | input_number < 1 | input_number > 99){
    return(FALSE)
    } else {return(TRUE)}
  }

#' Tests if element is a valid Tier character
#' @param input_string A character from 'A', 'B'
#' @return A logical value T/F
#' @noRd
tier_checker <- function(input_string){

  valid_tiers = c('A', 'B')

  if (input_string %in% valid_tiers){
    return(TRUE)
    } else {return(FALSE)}
  }

#' Validates that the RRI is within the correct range of values
#' @param input_string A character from 'R1', 'R2', 'R3', 'R4'
#' @return A logical value T/F
#' @noRd
rri_checker <- function(input_string){

  valid_rris = c('R1', 'R2', 'R3', 'R4')

  if (input_string %in% valid_rris){
    return(TRUE)
    } else {return(FALSE)}
  }

#' Checks if new_id is present in id_set or not. If it is not, adds it to id_set
#' @param id_set
#' @param new_id
#' @return A logical value T/F
#' @noRd
id_uniqueness <- function(id_set, new_id){

}
