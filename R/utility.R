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
    } 
    
  else {
      return(FALSE)
    }
  }

#' Validates that the RRI is within the correct range of values
#' @param input_string A character from 'R1', 'R2', 'R3', 'R4'
#' @return A logical value T/F
#' @noRd
rri_checker <- function(input_string){

  valid_rris = c('R1', 'R2', 'R3', 'R4')

  if (input_string %in% valid_rris){
    return(TRUE)
    } 
  
  else {
    return(FALSE)
    }
  }

#' Checks if new_id is present in id_set or not. If it is not, adds it to id_set
#' TODO
#' @param id_set verify id
#' @param new_id verify id
#' @return A logical value T/F
#' @noRd
id_uniqueness <- function(id_set, new_id){

}

#' Validates the Candid file.
#' Makes sure the header matches the header that a candid file should have.
#' For each line, call blood group and age checks.
#' FUNCAO NAO ACABADA
#' @param csv_file a .csv file
#' @return A logical value T/F
#' @noRd
candid_check <- function(csv_file){
  candid_columns <- c(
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

  for (i in 1:length(candid_columns)){
    if (!candid_columns[i] %in% colnames(csv_file)){
      print(paste('Column', candid_columns[i], 'is not present in the file.'))
      return(FALSE)
    }
  }

  if (length(candid_columns) != length(colnames(csv_file))){
    print('There are unexpected columns in the file. Expected:')
    print(paste(candid_columns, collapse = ", "))
    return(FALSE)
  }

  for (i in 1:nrow(csv_file)){
    if (!blood_group_checker(csv_file$bg[i])){
      print(paste('Invalid blood group in line', i))
      print(paste('Supported groups are', paste(valid_blood_groups, collapse = ", ")))
      break
    }
    if (!age_checker(csv_file$age[i])){
      print(paste('Negative age in line', i))
      break
    }
  }
}

#' Validates the Candids
#' FUNCAO NAO ACABADA
#' @param file_name name of the file
#' @param file_type type of the file
#' @return A logical value T/F
#' @noRd
validate_candid <- function(file_name, file_type){
  file <- read.csv(file_name, sep = ";")
  candid_check(file)
}

#' Validates the CandidUK file.
#' Makes sure the header matches the header that a candid file should have.
#' For each line, call blood group and age checks.
#' FUNCAO NAO ACABADA
#' @param csv_file a .csv file
#' @return A logical value T/F
#' @noRd
candid_uk_check <- function(csv_file){
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
    if (!candid_uk_columns[i] %in% colnames(csv_file)){
      print(paste('Column', candid_uk_columns[i], 'is not present in the file.'))
      return(FALSE)
    }
  }

  if (length(candid_uk_columns) != length(colnames(csv_file))){
    print('There are unexpected columns in the file. Expected:')
    print(paste(candid_uk_columns, collapse = ", "))
    return(FALSE)
  }

  for (i in 1:nrow(csv_file)){
    if (!blood_group_checker(csv_file$bg[i])){
      print(paste('Invalid blood group in line', i))
      print(paste('Supported groups are', paste(valid_blood_groups, collapse = ", ")))
      break
    }

    if (!tier_checker(csv_file$Tier[i])){
      print(paste('Invalid tier in line', i))
      print(paste('Supported tiers are', paste(valid_tiers, collapse = ", ")))
      break
    }

    if (!age_checker(csv_file$age[i])){
      print(paste('Negative age in line', i))
      break
    }

    if (!rri_checker(csv_file$RRI[i])){
      print(paste('Invalid RRI in line', i))
      print(paste('Supported RRIs are', paste(valid_rris, collapse = ", ")))
      break
    }
  }
}

#' Validates the CandidUK file.
#' FUNCAO NAO ACABADA
#' @param file_name name of the file
#' @param file_type type of the file
#' @return A logical value T/F
#' @noRd
validate_candid_uk <- function(file_name, file_type){
  file <- read.csv(file_name, sep = ";")
  candid_uk_check(file)
}
