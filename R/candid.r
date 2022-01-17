valid_blood_groups = c('O', 'A', 'B', 'AB')

# Tests if element is a valid Blood Group character
blood_group_checker <- function(input_string){
  if (input_string %in% valid_blood_groups){
    return(TRUE)
  }
  return(FALSE)
}

# Validates that the age of a person is not negative.
age_checker <- function(input_number){
  if (input_number < 0){
    return(FALSE)
  }
  return(TRUE)
}

id_uniqueness <- function(id_set, new_id){

}

# Validates the Candid file.
# Makes sure the header matches the header that a candid file should have.
# For each line, call blood group and age checks.
candid_check <- function(csv_file){
  candid_columns <- list('ID', 'bg', 'A1', 'A2', 'B1', 'B2', 'DR1', 'DR2', 'age', 'dialysis', 'cPRA')

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

file_validation <- function(file_name, file_type){
  file <- read.csv(file_name, sep = ";")
  candid_check(file)
}
