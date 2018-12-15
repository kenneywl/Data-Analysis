
#the LetterValues function must be loaded first for this function to work.
#the nletters variable will change how many letter values are used.
#I set it by default to 26, which will make the letter.value function
#compute all the letter values for a reasonably sized dataset.

UredaTrans <- function(x,nletters = 26){
  source("LetterValues.R")
  
  letters <- LetterValues(x,fiveNsum = F, nletters)
  
  rows <- nrow(letters)
  
  Xa <- NULL
  Ya <- NULL
  for(i in 1:rows){
    xa_working <- ((letters[i,4]-letters[1,3])^2+(letters[i,2]-letters[1,3])^2)/4/letters[1,3]
    ya_working <- letters[i,3] - letters[1,3]
    
    Xa <- c(Xa,xa_working)
    Ya <- c(Ya,ya_working)
  }
  
  lm_YaXa <- lm(Ya~Xa)
  
  plot(Ya~Xa, main = "Ureda Method: Transformation for Symmetry")
  abline(lm_YaXa$coefficients)
  
  optimal <- 1 - lm_YaXa$coefficients[2]
  
  ans <- c("If the plot is linear, the optimal transformation is:", round(optimal,3))
  cat(ans)
}
  