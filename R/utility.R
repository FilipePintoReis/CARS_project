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
  if (!is.numeric(input_number) | input_number < env$person.minimum.age | input_number > env$person.maximum.age){
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


#' Validates the Candid file.
#' Makes sure the header matches the header that a candid file should have.
#' For each line, call blood group and age checks.
#' @param candidate.dataframe a dataframe
#' @return A logical value T/F
#' @noRd
candidate_dataframe_check <- function(candidate.dataframe){
  candidate.fields <- c(
    'ID',
    'bg',
    'A1',
    'A2',
    'B1',
    'B2',
    'DR1',
    'DR2',
    'age',
    'dialysis',
    'cPRA',
    'urgent')

  for (i in 1:length(candidate.fields)){
    if (!candidate.fields[i] %in% colnames(candidate.dataframe)){
      stop('Column ', candidate.fields[i], ' is not present in the file.')
    }
  }

  if (length(candidate.fields) != length(colnames(candidate.dataframe))){
    stop('There are unexpected columns in the file. Expected: ', candidate.fields, ' ', collapse = ", ")
  }

  candidate.datatable <- data.table::setDT(candidate.dataframe, key = 'ID')
  duplication.location <- anyDuplicated(candidate.datatable)

  if(duplication.location != 0){
    stop(paste('Duplicated ID in line', duplication.location))
  }

  for (i in 1:nrow(candidate.dataframe)){
    if (!blood_group_checker(candidate.dataframe$bg[i])){
      stop(paste('Invalid blood group in line', i), paste('\nSupported groups are', paste(valid_blood_groups, collapse = ", ")))
    }
    if (!age_checker(candidate.dataframe$age[i])){
      stop('Negative age in line', i)
    }
  }

  return(TRUE)
}

#' Validates the Candids
#' @param file_name name of the file
#' @param file_type type of the file
#' @return A logical value T/F
#' @noRd
validate_candid <- function(file_name, file_type){
  candidate.dataframe <- read.csv(file_name, sep = ";")
  candidate_dataframe_check(candidate.dataframe)
}

#' Validates the CandidUK file.
#' Makes sure the header matches the header that a candid file should have.
#' For each line, call blood group and age checks.
#' @param candidate.dataframe candidate's dataframe
#' @return A logical value T/F
#' @noRd
uk_candidate_dataframe_check <- function(candidate.dataframe){
  candid_uk_columns <- c(
    'ID',
    'bg',
    'A1',
    'A2',
    'B1',
    'B2',
    'DR1',
    'DR2',
    'age',
    'dialysis',
    'cPRA',
    'Tier',
    'MS',
    'RRI',
    'urgent')

  for (i in 1:length(candid_uk_columns)){
    if (!candid_uk_columns[i] %in% colnames(candidate.dataframe)){
      stop(paste('Column', candid_uk_columns[i], 'is not present in the file.'))
    }
  }

  if (length(candid_uk_columns) != length(colnames(candidate.dataframe))){
    stop('There are unexpected columns in the file. Expected:\n', paste(candid_uk_columns, collapse = ", "))
  }

  candidate.datatable <- data.table::setDT(candidate.dataframe, key = 'ID')
  duplication.location <- anyDuplicated(candidate.datatable)

  if(duplication.location != 0){
    stop(paste('Duplicated ID in line', duplication.location))
  }

  for (i in 1:nrow(candidate.dataframe)){
    if (!blood_group_checker(candidate.dataframe$bg[i])){
      stop(paste('Invalid blood group in line', i), paste('Supported groups are', paste(valid_blood_groups, collapse = ", ")))
    }

    if (!tier_checker(candidate.dataframe$Tier[i])){
      stop(paste('Invalid tier in line', i), paste('Supported tiers are', paste(valid_tiers, collapse = ", ")))
    }

    if (!age_checker(candidate.dataframe$age[i])){
      stop(paste('Negative age in line', i))
    }

    if (!rri_checker(candidate.dataframe$RRI[i])){
      stop(paste('Invalid RRI in line', i), paste('Supported RRIs are', paste(valid_rris, collapse = ", ")))
    }
  }

  return(TRUE)
}

#' Validates the CandidUK file.
#' @param file_name name of the file
#' @param file_type type of the file
#' @return A logical value T/F
#' @noRd
validate_candid_uk <- function(file_name, file_type){
  file <- read.csv(file_name, sep = ";")
  uk_candidate_dataframe_check(file)
}

