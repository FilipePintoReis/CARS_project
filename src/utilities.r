# Tests if element is a valid Blood Group character
BloodGroupChecker <- function(InputString){
  ValidBloodGroups = list('O', 'A', 'B', 'AB')
  if (InputString %in% ValidBloodGroups){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

  
MainLoop <- function(CSVFile){
  for (i in 0:nrow(CSVFile)){
    if (!BloodGroupChecker(CSVFile$age[i])){
      print('This is inva')
    }
  }
}