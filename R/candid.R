# Validates the Candid file.
# Makes sure the header matches the header that a candid file should have.
# For each line, call blood group and age checks.
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
    'cPRA')#,
    #'urgent')

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

validate_candid <- function(file_name, file_type){
  file <- read.csv(file_name, sep = ";")
  candid_check(file)
}
