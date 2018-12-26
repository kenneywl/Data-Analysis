#This is our spread vs level function.

#The input is a data frame or a matrix-like object.
#If your input has the levels by row, set bycol = F.
#If you don't want to see the graph, set pl = F.

#If there were any negative medians, a translation of
#x+abs(min(x)) + 1 was done so that logs could be taken.

SvL <- function(x, index, pl = T){
  
  x_medians <- by(x,index,median)
  x_IQR <- by(x,index,IQR)
  
  translate <- c("No translation was performed due to all positive medians.")
  if(sum(x_medians <= 0) > 1){
    x_medians <- x_medians + abs(min(x_medians)) + 1
    translate <- c("Medians were translated by x+abs(min(x))+1 due to negative values.")
  }
  
  x_medians <- log(x_medians)
  x_IQR <- log(x_IQR)
  
  p <- lm(x_IQR~x_medians)$coeff

  if(pl == T){
    plot(x_IQR~x_medians, 
         main = "Spread vs Level: Transformation to Stabalize Heteroscedasticity",
         sub = translate,
         xlab = "log(Level Median)",
         ylab = "log(IQR)")
    abline(p)
  }
  
  cat("If the line is linear, the optimal power transformation is: ", round(1-p[2],3) ,"\n",
      translate)
}
