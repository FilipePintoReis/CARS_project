ValidBloodGroups = c('O', 'A', 'B', 'AB')

# Tests if element is a valid Blood Group character
BloodGroupChecker <- function(InputString){
  if (InputString %in% ValidBloodGroups){
    return(TRUE)
  }
  return(FALSE)
}

# Validates that the age of a person is not negative.
AgeChecker <- function(InputNumber){
  if (InputNumber < 0){
    return(FALSE)
  }
  return(TRUE)
}

IdUniqueness <- function(IdSet, NewId){
  
}
 
# Validates the Candid file.
# Makes sure the header matches the header that a candid file should have.
# For each line, call blood group and age checks.
CandidCheck <- function(CSVFile){
  CandidColumns <- list('ID', 'bg', 'A1', 'A2', 'B1', 'B2', 'DR1', 'DR2', 'age', 'dialysis', 'cPRA')
  
  for (i in 1:length(CandidColumns)){
    if (!CandidColumns[i] %in% colnames(CSVFile)){
      print(paste('Column', CandidColumns[i], 'is not present in the file.'))
      return(FALSE)
    }
  }
  
  if (length(CandidColumns) != length(colnames(CSVFile))){
    print('There are unexpected columns in the file. Expected:')
    print(paste(CandidColumns, collapse = ", "))
    return(FALSE)
  }
  
  for (i in 1:nrow(CSVFile)){
    if (!BloodGroupChecker(CSVFile$bg[i])){
      print(paste('Invalid blood group in line', i))
      print(paste('Supported groups are', paste(ValidBloodGroups, collapse = ", ")))
      break
    }
    if (!AgeChecker(CSVFile$age[i])){
      print(paste('Negative age in line', i))
      break 
    }
  }
}

FileValidation <- function(FileName, FileType){
  file <- read.csv(FileName, sep = ";")
  CandidCheck(file)
}
