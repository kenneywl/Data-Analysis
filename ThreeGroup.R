ThreeGroup <- function(x, y, pl = T, tol = -1, max_iter = 500,
                        output = "standard", print_out = T){
  
  #x and y are bivarate "paired" data.
  
  #There are four basic modes. 
  # output = c("standard", "diagnostic","residual","trans","safe")
  #"standard" just gives the line equation with a graph. "diagnostic" returns a
  #df with some information about each iteration, and
  #"residuals", gives the residuals of each iteration in a list.
  #"trans" gives a plot of the vector from the hypythetical
  #median to the actual median to get an idea of what transformation to make.
  #and it outputs that trans vector.
  #"safe" is the algorithm modified to be safe from oscillatory behviour
  #and converges much faster. It doesn't rely on x's as much.
  
  #"pl" turns on or off plotting. this plots the line and the three medians
  #"pl" plots each residual when "output == "residual""
  #it does nothing when set to "output == diagnostic".
  
  #"print_out" turns on a cute little verbal printout.
  #it also restricts the residuals from being returned if output == "residual"
  #and it also restricts the transformation vector if output == "trans"
  
  #"tol" is the tolerance to iterate until. it is set to "-1" automatically.
  #this is the adaptive tolerance, which goes until slope adjustment
  #reaches below 1% of the the previous updated slope.
  
  #For each of these modes, the first initial slope and level are
  #considered the zeroth iteration, so when max_iter == 10,
  #you have the initial estimate + 10 iterations. Thats 11 total.
  
  #############################
  #This setup is common to all of them.
  #We have to use the order function
  #instead of the sort function bc x and y
  #are bivariate. we lose that connection
  #when we just sort the x's.
  #Order gives us the index of the highest, then the index
  #of the next highest etc. Since we have the indexes
  #we preserve the connection between x's and y's.
  
  ord <- order(x,decreasing = F) #decreasing = F makes it sort lowest to higest.
  x <- x[ord] #the x's sorted
  y <- y[ord] #this is the y's sorted by the order of the x's
  
  
  #The following figures out where to put the
  #extra points when the number of points is not
  #divisable by 3
  n <- length(x)
  if(n%%3 == 0){
    ind1 <- n/3
    ind2 <- 2*n/3
  } else if(n%%3 ==1){ #in the middle
    ind1 <- floor(n/3)
    ind2 <- ceiling(2*n/3)
  } else {             #in each outer group
    ind1 <- ceiling(n/3)
    ind2 <- floor(2*n/3)
  }
  
  #Now lets find the x medians:
  xm1 <- median(x[1:ind1])
  xm2 <- median(x[(ind1+1):ind2])
  xm3 <- median(x[(ind2+1):n])
  
  ym1 <- median(y[1:ind1])
  ym2 <- median(y[(ind1+1):ind2])
  ym3 <- median(y[(ind2+1):n])
  
  r <- y #It is usually a good idea to keep your original data unmodified.
  
  if(output == "standard"){
    Updated_Slope <- 0
    Updated_Level <- 0
    Slope_Adjustment <- 0
    Level_Adjustment <- 0

    iter <- 1
    tol_set <- tol
    while(abs(Slope_Adjustment) >= tol_set & (iter+1) <= max_iter | iter == 1){
      r <- r - Level_Adjustment - Slope_Adjustment*x
      
      rm1 <- median(r[1:ind1],na.rm=T)
      rm2 <- median(r[(ind1+1):ind2],na.rm=T)
      rm3 <- median(r[(ind2+1):n],na.rm=T)
      
      Slope_Adjustment <- (rm3-rm1)/(xm3-xm1)
      Level_Adjustment <- (rm1+rm2+rm3-Slope_Adjustment*(xm1+xm2+xm3))/3
      Updated_Slope <- Updated_Slope + Slope_Adjustment
      Updated_Level <- Updated_Level + Level_Adjustment
      
      if(tol == -1){tol_set <- Updated_Slope * .01}
      
      iter <- iter + 1
    }
    if(sign(Updated_Slope) == -1){sig <- " - "} else {sig <- " + "}
    lineeq <- paste0(c("y= ",round(Updated_Level,4),sig,
                       abs(round(Updated_Slope,4)),"x"),collapse="")
    
    ###We graph and print a cute little verbal output.
    if(pl == T){
      plot(x,y,main="Three Group Method for Bivariate Data: Linear Approximation")
      points(c(xm1,xm2,xm3),
             c(median(y[1:ind1],na.rm=T),
               median(y[(ind1+1):ind2],na.rm=T),
               median(y[(ind2+1):n],na.rm=T)),col="blue",pch=16)
      abline(Updated_Level,Updated_Slope,col="red",lwd=2)
      abline(v=c((x[ind1]+x[ind1+1])/2,(x[ind2]+x[ind2+1])/2))
      
      #this next if-then determines if it is probably better to put the
      #legend on the topright or topleft. If the slope is positive,
      #it puts it on the topleft, and inversely.
      
      if(sign(Updated_Slope) > 0){position <- "topleft"} else {position <- "topright"}
      
      #and lets put the legend on:
      legend(x=position,legend = c("Three Group Medians",lineeq),
             pch = c(16,NA),
             lwd = c(NA,2),
             col = c("blue","red"))
    }
    if(print_out == T){
      iter_resp <- "This was accomplished in"
      if(iter == max_iter){
        iter_out <- paste(c(iter_resp," max iterations: ",max_iter,"\n",
                            " Set output = 'diagnostic' to inspect each iteration.\n",
                            " Or set output = 'safe' to use the safe algorithm."),collapse = "")
      } else {
        iter_out <- paste(c(iter_resp,iter,"iterations."),collapes = "")
      }
      
      three_med <- paste0(paste0("(",round(c(xm1,xm2,xm3),4)),",",
                                 paste0(round(c(ym1,ym2,ym3),4)),")")
      
      cat("The Three Group Method divides the x values into three groups.\n",
          "It uses the x and y medians of each group to make a linear approximation.\n",
          "The three medians are:",three_med,"\n",
          "The linear approximation for this data is:", lineeq,"\n",iter_out)
    }
  }
  if(output == "diagnostic"){
  
    Updated_Slope <- 0
    Updated_Level <- 0
    Slope_Adjustment <- 0
    Level_Adjustment <- 0
    G1_Median_Index <- 0
    G3_Median_Index <- 0
  
    iter <- 1
    tol_set <- tol
    while(abs(Slope_Adjustment[iter]) >= tol_set & iter <= (max_iter+1) | iter == 1){
      r <- r - Level_Adjustment[iter] - Slope_Adjustment[iter]*x
    
      rm1 <- median(r[1:ind1],na.rm=T)
      rm2 <- median(r[(ind1+1):ind2],na.rm=T)
      rm3 <- median(r[(ind2+1):n],na.rm=T)
    
      g1_median_ord <- order(r[1:ind1],decreasing=F)
      g2_median_ord <- order(r[(ind1+1):ind2],decreasing=F)
      g3_median_ord <- order(r[(ind2+1):n],decreasing=F)
    
      G1_Median_Index <- c(G1_Median_Index,g1_median_ord[c(floor((ind1+1)/2),floor((ind1+1)/2)+1)])
      G3_Median_Index <- c(G3_Median_Index,ind2+g3_median_ord[c(floor((ind1+1)/2),floor((ind1+1)/2)+1)])
    
      Slope_Adjustment <- c(Slope_Adjustment, (rm3-rm1)/(xm3-xm1))
      Level_Adjustment <- c(Level_Adjustment,(rm1+rm2+rm3-Slope_Adjustment[iter+1]*(xm1+xm2+xm3))/3)
      Updated_Slope <- c(Updated_Slope, Updated_Slope[iter] + Slope_Adjustment[iter+1])
      Updated_Level <- c(Updated_Level, Updated_Level[iter] + Level_Adjustment[iter+1])
    
      if(tol == -1){tol_set <- Updated_Slope[iter] * .01}
      
      iter <- iter + 1
    }
  
    g1_med_temp <- NULL
    g3_med_temp <- NULL
    if(ind1 %% 2 == 0){
      for(i in seq(2,length(G1_Median_Index),2)){
        g1_med_temp <- c(g1_med_temp,
                         paste0("mean(",G1_Median_Index[i],",",
                                G1_Median_Index[i+1],")",collapse=""))
        g3_med_temp <- c(g3_med_temp,
                         paste0("mean(",G3_Median_Index[i],",",
                                G3_Median_Index[i+1],")",collapse=""))
      }
    } else {
      for(i in seq(2,length(G1_Median_Index),2)){
        g1_med_temp <- c(g1_med_temp,G1_Median_Index[i])
        g3_med_temp <- c(g3_med_temp,G3_Median_Index[i])
      }
    }
  
    G1_Median_Index <- c(0,g1_med_temp)
    G3_Median_Index <- c(0,g3_med_temp)
  
    info_df <- data.frame(Updated_Slope,Updated_Level,Slope_Adjustment,Level_Adjustment,
                        G1_Median_Index,G3_Median_Index)
    info_df <- info_df[-1,]
    info_df$Slope_Adjustment[1] <- 0
    info_df$Level_Adjustment[1] <- 0
    row.names(info_df) <- 0:(nrow(info_df)-1)
    
    return(info_df)
  }
  if(output == "residual"){
    Updated_Slope <- 0
    Updated_Level <- 0
    Slope_Adjustment <- 0
    Level_Adjustment <- 0
    
    residuals <- list(rep(0,length(r)))
    
    iter <- 1
    tol_set <- tol
    while(abs(Slope_Adjustment) >= tol_set & (iter-1) <= max_iter | iter == 1){
      r <- r - Level_Adjustment - Slope_Adjustment*x
      
      residuals[[iter]] <- r

      rm1 <- median(r[1:ind1],na.rm=T)
      rm2 <- median(r[(ind1+1):ind2],na.rm=T)
      rm3 <- median(r[(ind2+1):n],na.rm=T)
      
      Slope_Adjustment <- (rm3-rm1)/(xm3-xm1)
      Level_Adjustment <- (rm1+rm2+rm3-Slope_Adjustment*(xm1+xm2+xm3))/3
      Updated_Slope <- Updated_Slope + Slope_Adjustment
      Updated_Level <- Updated_Level + Level_Adjustment
      
      if(tol == -1){tol_set <- Updated_Slope * .01}
      
      iter <- iter + 1
    }
    names(residuals) <- paste0("Residuals Iteration ",c(0:(iter-2)))
    
    if(pl == T){
      for(i in 0:(length(residuals)-1)){
        plot(residuals[[i+1]]~x,ylab="Residual y",main=paste0("Residuals Iteration ",i,collapse = ""))
        abline(v=c((x[ind1]+x[ind1+1])/2,(x[ind2]+x[ind2+1])/2))
        abline(h=0)
        press <- readline(prompt="Press [enter] to continue. Press [q] to quit:")
        if(press == "q") break
        }
    }
    
    if(print_out == T) return(residuals)
  }
  if(output == "safe"){
    #This first while loop find the initial values for the first two slopes.
    Updated_Slope <- 0
    Updated_Level <- 0
    Slope_Adjustment <- 0
    Level_Adjustment <- 0
    Slope_Adjustment_Old <- 0
    
    iter <- 1
    while(((sign(Slope_Adjustment_Old) == sign(Slope_Adjustment[iter])) & iter <= max_iter) | iter <= 2){
      Slope_Adjustment_Old <- Slope_Adjustment[iter]
      r <- r - Level_Adjustment[iter] - Slope_Adjustment[iter]*x
      
      rm1 <- median(r[1:ind1])
      rm2 <- median(r[(ind1+1):ind2])
      rm3 <- median(r[(ind2+1):n])
      
      Slope_Adjustment <- c(Slope_Adjustment, (rm3-rm1)/(xm3-xm1))
      Level_Adjustment <- c(Level_Adjustment,(rm1+rm2+rm3-Slope_Adjustment[iter+1]*(xm1+xm2+xm3))/3)
      Updated_Slope <- c(Updated_Slope, Updated_Slope[iter] + Slope_Adjustment[iter+1])
      Updated_Level <- c(Updated_Level, Updated_Level[iter] + Level_Adjustment[iter+1])
      
      iter <- iter + 1
    }
    
    #this second while loop finds the adjustements without the use of x's (mostly)
    #It converges faster, and prevent oscillatory behaviour.
    
    iter2 <- iter #lets hand off the iter value but start a new counter.
    tol_set <- tol
    while((abs(Slope_Adjustment[iter2]) > tol_set & (iter+iter2) <= max_iter) | iter2 == iter){
      rl <- y[1:ind1]-Updated_Slope[iter2-1]*x[1:ind1]
      rr <- y[(ind2+1):n]-Updated_Slope[iter2-1]*x[(ind2+1):n]
      
      delta_b0 <- median(rr)-median(rl)
      
      rl <- y[1:ind1]-Updated_Slope[iter2]*x[1:ind1]
      rr <- y[(ind2+1):n]-Updated_Slope[iter2]*x[(ind2+1):n]
      
      delta_b1 <- median(rr)-median(rl)
      
      Slope_Adjustment <- c(Slope_Adjustment,
                            -delta_b1*(Updated_Slope[iter2]-Updated_Slope[iter2-1])/(delta_b1-delta_b0))
      
      Updated_Slope <- c(Updated_Slope, 
                         Updated_Slope[iter2] + Slope_Adjustment[iter2+1])
      
      Updated_Level <- c(Updated_Level, median(y-Updated_Slope[iter2+1]*x))
      
      #And if tol is set to adaptive, we go until it reaches less than .01 of
      #updated slope.
      
      if(tol == -1){tol_set <- Updated_Slope[iter2] * .01}
      iter2 <- iter2 + 1
    }
    
    #And lets clean everything up.
    info_df <- data.frame(Updated_Slope,Updated_Level,Slope_Adjustment)
    
    info_df <- info_df[-1,]
    info_df$Slope_Adjustment[1] <- 0
    row.names(info_df) <- 0:(nrow(info_df)-1)
    
    if(sign(Updated_Slope[iter2]) == -1){sig <- " - "} else {sig <- " + "}
    
    lineeq <- paste0(c("y= ",round(Updated_Level[iter2],4),sig,
                       abs(round(Updated_Slope[iter2],4)),"x"),collapse="")
    
    ###We graph and print a cute little verbal output.
    if(pl == T){
      plot(x,y,main="Three Group Method for Bivariate Data (fast): Linear Approximation")
      points(c(xm1,xm2,xm3),
             c(median(y[1:ind1]),
               median(y[(ind1+1):ind2]),
               median(y[(ind2+1):n])),col="blue",pch=16)
      abline(Updated_Level[iter2],Updated_Slope[iter2],col="red",lwd=2)
      abline(v=c((x[ind1]+x[ind1+1])/2,(x[ind2]+x[ind2+1])/2))
      
      #this next if-then determines if it is probably better to put the
      #legend on the topright or topleft. If the slope is positive,
      #it puts it on the topleft, and inversely.
      
      if(sign(Updated_Slope[iter2]) > 0){position <- "topleft"} else {position <- "topright"}
      
      #and lets put the legend on:
      legend(x=position,legend = c("Three Group Medians ",lineeq),
             pch = c(16,NA),
             lwd = c(NA,2),
             col = c("blue","red"))
    }
    if(print_out == T){
      iter_resp <- "This was accomplished in"
      if(iter2 == max_iter){
        iter_out <- paste0(c(iter_resp," max iterations: ",max_iter,"\n",
                             "If this method didn't converge there is a bug in the program.\n"),collapse = "")
      } else {
        iter_out <- paste0(c(iter_resp,iter,"iterations. \n"),collapes = "")
      }
      
      three_med <- paste0(paste0("(",round(c(xm1,xm2,xm3),4)),",",
                          paste0(round(c(ym1,ym2,ym3),4)),")")
      
      cat("The Three Group Method divides the x values into three groups.\n",
          "This algorithm has been modifiied for quick convergence.\n",
          "Set 'print_out = F' to get information about each iteration.\n",
          "The linear approximation for this data is:", lineeq,"\n",iter_out)
    } else {return(info_df)}
  }
  if(output == "trans"){
    if(pl == T){
      trans_vector <- c(xm1+xm3,ym1+ym3)/2
      plot(y~x,main="Vector from Perfect Median to Actual Median")
      arrows(trans_vector[1],trans_vector[2],xm2,ym2,col="black")
      points(c(xm1,xm2,xm3),c(ym1,ym2,ym3),col="blue",pch=16)
      points(trans_vector[1],trans_vector[2],col="red",pch=16)
      abline(v=trans_vector[1],h=trans_vector[2])
      legend(x="bottomright",legend=c("Three Actual Medians","Hypothetical Median","Transformation Vector"),
             col=c("blue","red","black"),pch=c(16,16,NA),lwd = c(NA,NA,1))
    }
    if(print_out == T){
      cat("The transformation vector gives the direction to transform.\n",
          "Values above zero suggest a transformation above unity.\n",
          "Values below zero suggest a transformation below unity.\n",
          "The transformation vector is: ",
          paste0("(", round(xm2-trans_vector[1],4),",",round(ym2-trans_vector[2],4),")"))
    }
  }
}
