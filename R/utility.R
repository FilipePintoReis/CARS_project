valid_blood_groups = c('O', 'A', 'B', 'AB')
valid_tiers = c('A', 'B')
valid_rris = c('R1', 'R2', 'R3', 'R4')

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

# Tests if element is a valid Tier character
tier_checker <- function(input_string){
  if (input_string %in% valid_tiers){
    return(TRUE)
  }
  return(FALSE)
}

# Validates that the RRI is within the correct range of values .
rri_checker <- function(input_string){
  if (input_string %in% valid_rris){
    return(TRUE)
  }
  return(FALSE)
}

# Checks if new_id is present in id_set or not. If it is not, adds it to id_set
id_uniqueness <- function(id_set, new_id){

}
