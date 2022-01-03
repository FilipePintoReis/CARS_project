ValidBloodGroups = list('O', 'A', 'B', 'AB')
ValidTiers = list('A', 'B')
ValidRRIs = list('R1', 'R2', 'R3', 'R4')

# Tests if element is a valid Blood Group character
BloodGroupChecker <- function(InputString){
  if (InputString %in% ValidBloodGroups){
    return(TRUE)
  }
  return(FALSE)
}

# Tests if element is a valid Tier character
TierChecker <- function(InputString){
  if (InputString %in% ValidTiers){
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

# Validates that the RRI is within the correct range of values .
RRIChecker <- function(InputString){
  if (InputString %in% ValidRRIs){
    return(TRUE)
  }
  return(FALSE)
}

IdUniqueness <- function(IdSet, NewId){
  
}

# Validates the CandidUK file.
# Makes sure the header matches the header that a candid file should have.
# For each line, call blood group and age checks.
CandidUKCheck <- function(CSVFile){
  CandidUKColumns <- list('ID', 
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
                        'RRI'
                        )
  
  for (i in 1:length(CandidUKColumns)){
    if (!CandidUKColumns[i] %in% colnames(CSVFile)){
      print(paste('Column', CandidUKColumns[i], 'is not present in the file.'))
      return(FALSE)
    }
  }
  
  if (length(CandidUKColumns) != length(colnames(CSVFile))){
    print('There are unexpected columns in the file. Expected:')
    print(paste(CandidUKColumns, collapse = ", "))
    return(FALSE)
  }
  
  for (i in 1:nrow(CSVFile)){
    if (!BloodGroupChecker(CSVFile$bg[i])){
      print(paste('Invalid blood group in line', i))
      print(paste('Supported groups are', paste(ValidBloodGroups, collapse = ", ")))
      break
    }
    
    if (!TierChecker(CSVFile$Tier[i])){
      print(paste('Invalid tier in line', i))
      print(paste('Supported tiers are', paste(ValidTiers, collapse = ", ")))
      break
    }
    
    if (!AgeChecker(CSVFile$age[i])){
      print(paste('Negative age in line', i))
      break 
    }
    
    if (!RRIChecker(CSVFile$RRI[i])){
      print(paste('Invalid RRI in line', i))
      print(paste('Supported RRIs are', paste(ValidRRIs, collapse = ", ")))
      break 
    }
  }
}

FileValidation <- function(FileName, FileType){
  file <- read.csv(FileName, sep = ";")
  CandidUKCheck(file)
}
