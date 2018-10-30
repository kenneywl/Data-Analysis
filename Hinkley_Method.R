#1b This function evaluates the optimal value to transform your
#data set to get it symmeterical. If there are negative values
#for the log transfomation or transformations between the integers,
#a translation of x + abs(min(x)) + is applied. Otherwise
#no translation takes place.

#A plot is optional with the pl flag.
#The particular powers tried are
#defaulted to seq(-4,4,.5), but can be specifed by the user.

#This function returns an invisible NULL.

hinkley.trans <- function(x, pl = T, Powers = seq(-4,4,.5)){
  translate <- function(z){
    if(sum(z<=0) > 1){
      z <- z + abs(min(z)) + 1
    }
    return(z)
  }
  
  Dp <- NULL
  for(p in Powers){
    if(p == 0){
      y <- log(translate(x))
    } else if(floor(p) != p){
      y <- (translate(x)^p-1)/p
    } else {
      y <- (x^p-1)/p
    }
    Dp <- c(Dp,(mean(y,na.rm=T)-median(y,na.rm=T))/IQR(y,na.rm=T))
  }
  
  root <- which.min(abs(Dp))
  
  ans <- c("The Hinkley Method finds the optimal transfomation for symmetry.\n")
  
  trans <- c("No translation was applied for the optimal transformation.")
  if(sum(x<=0) > 1 & (Powers[root] == 0 | floor(Powers[root]) != Powers[root])){
    trans <- c("A translation of x + abs(min(x)) + 1 was applied first.")
  }
  
  ans <- c(ans,trans,"\n")
  
  if(pl == T){
    plot(Dp~Powers,main="Hinkley Method: Transformation for Symmetry",
         sub=trans)
    abline(h=0,v=Powers[root])
  }
  
  if(Powers[root] == 0){
    optimal <- "0 (log transformation)"
  } else {
    optimal <- Powers[root]
  }
  
  ans <- c(ans, paste0("The optimal value is: ", optimal,"\n",collapse=""),
           paste0("With a closeness to zero of: ", round(Dp[root],3),collapse=""))
  
  cat(ans)
}
