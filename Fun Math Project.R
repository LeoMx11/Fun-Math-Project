
## FUNCTIONS ##

add_strings <- function(str1, str2){
  x_val <- as.integer(str_split(str1, ",")[[1]][[1]]) + as.integer(str_split(str2, ",")[[1]][[1]])
  
  y_val <- as.integer(str_split(str1, ",")[[1]][[2]]) + as.integer(str_split(str2, ",")[[1]][[2]])
  
  return(paste(as.character(x_val), ",", as.character(y_val), sep=''))
  
}

get_prime_factors <- function(number){
  
  if (number == 1){
    return(NULL)
  }
  
  if (number %% 2 == 0){
    return(c(2, get_prime_factors(number/2)))
  }
  
  for (i in seq(3, ceiling(sqrt(number)))){
    if (number %% i == 0){
      return(c(i, get_prime_factors(number/i)))
    }
  }
  
  return(number)
  
}

placePoint <- function(number, existingReference){
  
  if (length(get_prime_factors(number)) == 1){
    
    for (i in seq(0,number)){
      
      point_check <- paste("1,", as.character(i), sep='')
      
      if (is.na(existingReference[point_check]) || 
          length(existingReference[point_check]) == 0){
        
        existingReference[point_check] <- c(number)
        return(existingReference)
        
      }
    }
  }
  
  point <- "0,0"
  
  for (factor in get_prime_factors(number)){
    for (key in names(existingReference)){
      if (existingReference[key] == factor){
        index <- key
      }
    }
    
    point <- add_strings(point, index)
    
  }
  
  if (is.na(existingReference[point])){
    existingReference[point] <- number
  } else {
    existingReference[point] <- c(existingReference[point], number)
  }
  
  return(existingReference)
  
}


## TESTING ##

for (i in seq(2,4)){
  pointDict <- placePoint(i, pointDict)
}


pointDict

