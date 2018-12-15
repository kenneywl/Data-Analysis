
#1. The following function calculates all the letter
#values of a vector of data. It's default is to just
#disply the five number summary, but it can display as
#many letter values as you wish, just set the "fiveNsum" 
#flag to F.

#If "fiveNsum = F" then it's default is to calculate
#five letter values, up to C, but you can specify any number
#of letter values you want. If you specify more letter
#values than are possible for the size of the data set,
#it will just output all the possible letter values.
#Out of the alphebet, I removed the second M as it is used
#for the median as the first letter value. I also made
#notation for letter values greater than G, the 26th letter
#value, where the 27th letter value is G27, the 28th is G28 etc.
#This function can output an ANY number of letter values.

#As for how it works. Basicially I first used a while loop to
#calculate the depth of each lower letter value. Once I had that,
#I automatically had the depth of the upper too.
#The depth is the index of the letter value, except where the depth
#is midway between two integers, then it is the average between
#the value for the two indexes.

#This function automatically omits NA inputs.
#It also rounds to three decimal places.
#Finally, the output for this function is a matrix
#or a named vector depending on the value of "fiveNsum"

LetterValues <- function(x, fiveNsum = F, n_letters = 5){
  
  x <- na.omit(x)
  x <- sort(x, decreasing=F)
  
  new <- length(x)
  lower_ind <- NULL
  while(new != 1 & length(lower_ind) < n_letters){
    new <- (floor(new)+1)/2
    lower_ind <- c(lower_ind,new)
  }
  
  lower_vals <- NULL
  for(i in lower_ind){
    if(floor(i) != i){
      lower_vals <- c(lower_vals, (x[floor(i)]+x[ceiling(i)])/2)
    } else {
      lower_vals <- c(lower_vals, x[floor(i)])
    }
  }
  
  upper_ind <- length(x)+1-lower_ind
  
  upper_vals <- NULL
  for(i in upper_ind){
    if(floor(i) != i){
      upper_vals <- c(upper_vals, (x[floor(i)]+x[ceiling(i)])/2)
    } else {
      upper_vals <- c(upper_vals, x[floor(i)])
    }
  }
  
  letters_needed <- length(lower_ind)
  
  letts <- c("M",LETTERS[6:1],LETTERS[26:7])
  letts <- letts[-21]
  
  if(letters_needed <= 26){
    letts <- letts[1:length(lower_ind)]
  } else {
    for(i in 27:letters_needed){
      letts <- c(letts,paste0("G",i,collapse=""))
    }
  }
  
  outer <- (1/2)^(1:letters_needed)
  inner <- 1-2*outer
  
  middle <- round((upper_vals+lower_vals)/2,3)
  lower_vals <- round(lower_vals,3)
  upper_vals <- round(upper_vals,3)
  ans <- matrix(lower_ind)
  ans <- cbind(ans,lower_vals,middle,upper_vals,upper_vals-lower_vals,outer,inner)
  
  rownames(ans) <- letts
  colnames(ans) <- c("Depth","Lower","Middle","Upper","Spread","p(x<Lower)","p(Lower<x<Upper)")
  
  
  if(fiveNsum == T){
    ans_five <- c(min(x),ans[2,2],ans[1,3],ans[2,4],max(x))
    ans_five <- round(ans_five,3)
    attr(ans_five, "names") <- c("Min","Fl","M","Fu","Max")
    
    return(ans_five)
  } else {
    return(ans)
  }
}