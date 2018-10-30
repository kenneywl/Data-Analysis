#0) Below is the master file for the midtem. You can follow along here,
#or you can follow along in your terminal. If you follow along in your terminal
#You need to change your working directory to where the folder containing
#these file are located. If you change the working directory now,
#it will only be changed for this session. When you restart R it will
#return to your default location.

#setwd("set to where the files are located")

#For any sourced files, if you actually look at the sourced file there is
#discussion about them, some about how they work, default behavior
#and the entire range of what they do. 

#1a) The letter value function.

source("Letter_Values.R")

#Once the file is loaded, you can look at it here:

letter.values

#and here it is in action:

test <- rchisq(1000,5)
letter.values(test)

#It is default to display 5 number summary,
#but you can display as many as you want,
#if you put an arbitrarilly high number
#it will just display all the possible ones.

#Set the fiveNsum flag off and get all the possible lettter values.
# the number_lv variable will compute that number of letter values.
#or all of them, whichever is less. We can get it compute all
#by setting number_lv to an arbitrily high number.

letter.values(test,fiveNsum = F, number_lv = 30)

#1b) The hinkley method:

source("Hinkley_Method.R")

#1b) Once the file is loaded, you can look at it here:

hinkley.trans

#there is a pl flag which turns on plotting, and a Powers variable,
#which takes the set of powers you want to check. Lets test
#our test set out with the default behavior.

hinkley.trans(test)

#Theoretically it should be sqrt, as the chi squared dist is
#the sum of k standard normals squared.

#take a look at the hist before and after transformation.

par(mfrow=c(1,2))
hist(test)
hist(sqrt(test))

#This function can handle negative values, it automatically
#translates all the values.

test_neg <- test-5 

#I subtracted 5 bc the mean is 5 so we should get a bunch of negatives

hinkley.trans(test_neg)

#and the histograms

test_neg_trans <- log(test_neg + abs(min(test_neg)) + 1)

hist(test_neg,main= "Original Data")
hist(test_neg_trans,main="Transformed")

#We should expect it to be .5 also, but we can see from the hinkley method
#graph above that many of the powers are close to zero. Either way,
#the histogram of the graph after the transformation is pretty 
#symmetrical.

par(mfrow=c(1,1)) #clean up and put the defaults back.


#2a. I copy and pasted the data set in instead of using the scan function,
#because I prefer my scripts to have less moving parts.

data <- c(124,  55,  21, 144,  64, 143, 114, 121,  45,  34,
          41,  28,  46, 248, 133,  80,  64,  77,  48,  87,
          53, 130, 100,  79,  87,  94, 123,  70,  11, 107,
          95, 247,  77,  22, 108,  26,  83,  23)

#A stem and leaf display:

stem(data)

#There appears to be two outliers. 247 and 248. We keep these in
#for the next part but remove them in part c.

#All the letter values for this data set.
#I choose some arbitrarily large number for the number
#of letter values, the function only outputs all possible.

letter.values(data, fiveNsum = F, 30)

#2b) I made two functions, one for the hinkley and one for the ureda
#lets examine the data set.

source("Ureda_Method.R")

#if you wanna look at the code:

ureda.method

#and lets apply them to our dataset.

ureda.method(data)
hinkley.trans(data)

#They both suggest a transformation of a half.

#c. From the stem and leaf it looks like we have two outliers,
#I check them with a quick box plot:

boxplot(data)
boxplot(data)$out

#Looks to be true. Now lets remove them

outliers <- data %in% boxplot(data)$out
data_cleaned <- data[!outliers]

#Now lets call the two transformation methods again:

ureda.method(data_cleaned)
hinkley.trans(data_cleaned)

#They both suggest no transformation at all. This suggests that they both
#do a reasonable job at checking for symmetry. But between the two,
#the hinkley is closer.

#d) Well lets test the data set out:

test2 <- 10^(0:6)

ureda.method(test2)
hinkley.trans(test2)

#It looks like the hinkley did a very good job, the data set is the increasing powers
#of ten, and it suggests a log transformation.  The ureda method suggests no
#transformation at all. It is likely that the ureda can't detect exponential
#data sets.

#Just for kicks lets look at the letter values:

letter.values(test2, fiveNsum = F)

#The lower ones are very small and decrease at a slow rate, the uppers
#are huge and increase at fast rate. This is due to the exponential
#nature of hte dataset.

#3) I prefer to do a little extra work processing the data by hand,
#it keeps my text processing skills sharp. Also, since the data set is small
#I'd prefer to keep it up so I can look at it. Lastly, less moving parts is better.

micro <- c("95  96  92 102 103  93 101  92  95  90 
            184 202 215 204 195 201 201 169 182 192  
            261 279 281 278 269 264 266 261 266 276 
            215 214 197 216 215 208 226 208 216 214 
            26  23  25  25  21  22  27  27  21  25 
            56  55  61  57  60  57  65  55  60  61 
            155 142 146 149 149 146 152 159       
            128 119 119 123 117 122 127 121 122 119")

micro_sp <- unlist(strsplit(micro,split=" "))
micro_sp[c(166,167)] <- 0  #To retain the empty spots when I convert to integers

micro_sp <- micro_sp[micro_sp != ""]
micro_sp <- as.integer(micro_sp)
micro_sp <- micro_sp[!is.na(micro_sp)]

micro_m <- matrix(micro_sp,8,byrow=T)
micro_m[7,9:10] <- NA #I put the NA's back.

rownames(micro_m) <- LETTERS[1:8]
colnames(micro_m) <- 1:10

#Our data set is all ready to go.

micro_m

#3a) By the formula you gave us, we would expect 
#.007*(Number of data points) + .4*(Number of batches) = number of outliers
#But the second part .4*number of batches doesn't seem to make sense.

#by this formula, I have:

(nrow(micro_m)*ncol(micro_m)-2)*.007 + .4*nrow(micro_m) 

#the minus 2 is bc there are 2 NA's

#3.746 outliers, this seems too high. If I were to compute the number of
#outliers directly, I would use the t distribution.
#for each sample of 10 we should get:

r10 <- 2*pt(qt(.25,9) - 1.5*(qt(.75,9)-qt(.25,9)),9)
r10 

#and for the single row of 8 we should have:

r9 <- 2*pt(qt(.25,7) - 1.5*(qt(.75,7)-qt(.25,7)),7)
r9

#Then put all together the total number of outliers should be:

Noutliers <- 10*7*r10+8*1*r9 #(# of data points per sample)*(# of samples)*rate
Noutliers                   #  for each term.

#This looks more reasonable. We should expect one or two outliers.
#Note that this is looking at each sample independantly and finding outliers.

#If we look at the whole dataset, without breaking up into samples we should
#expect the rate to be:

ratet <- 2*pt(qt(.25,77)-1.5*(qt(.75,77)-qt(.25,77)),77)
ratet

#and for our sample we should get:

78*ratet

#about one. This is lower because we are only estimating one mean,
#instead of 8 of them.

#You may take off points, but I believe that Noutliers (1.62) is correct
#when looking at each sample independantly.

#If this is true, the following should result in 1 to 2 marked outliers.
#Lets look at the boxplots.

#3bi) The parallel boxplots are below:

boxplot.matrix(micro_m, use.cols = F, outline = T, 
               main="Parallel Boxplots: Microdetermination of CO", 
               xlab = "Sample", ylab = "Air (ppm)")

#The flag "outline" changes whether the outliers are drawn or not. It is
#on by default, (set to draw outliers), but I was a little redundant and
#set it on anyway. 

#If our data set had an outlier it would look like this:

micro_m[7,9] <- 400 #This data point is NA usually in our data set.
boxplot.matrix(micro_m, use.cols = F, outline = T, 
               main="Parallel Boxplots: Microdetermination of CO", 
               xlab = "Sample", ylab = "Air (ppm)")

#So we can see that our original dataset has none.

micro_m[7,9] <- NA
boxplot.matrix(micro_m, use.cols = F, outline = T, 
               main="Parallel Boxplots: Microdetermination of CO", 
               xlab = "Sample", ylab = "Air (ppm)")

#There are no outliers by the 1.5 iqr criterion. One may say then that the
#distrubution may be more compact than the normal dist, it may be
#a double exponential. But I leave that analysis for another time.

#Now looking at the boxplots we can see that as the medians increase
#we also see an increase in IQR. We should stabalize this...

#3bii) I wrote a Spread vs Level function to do this. The command is SvL
#I set it to automatically consider the columns as the levels,
#bc I think more data-objects are like this. We just have
#to turn the bycol flag to F.

source("Spread_vs_Level.R")

#If you wanna look at the code, here's the guts:

SvL

#Lets call it on our dataset.

SvL(micro_m,bycol=F)

#We can see that the optimal transforamtion is to take square roots.

#3biii) We transform the data by square roots.

micro_m_sqrt <- sqrt(micro_m)
boxplot.matrix(micro_m_sqrt, use.cols = F, outline = T, 
               main="Parallel Boxplots: Microdetermination of CO", 
               xlab = "Sample", ylab = "Air (ppm)")

#It seems to make a tiny difference. Just a little tiny one.

par(mfrow=c(1,2))

boxplot.matrix(micro_m, use.cols = F, outline = T, 
               main="Untransformed Data", 
               xlab = "Sample", ylab = "Air (ppm)")

boxplot.matrix(micro_m_sqrt, use.cols = F, outline = T, 
               main="Transforamed Data", 
               xlab = "Sample", ylab = "Sqrt(Air (ppm))")

par(mfrow=c(1,1)) #just to put our settings back to default.

#The IQR's are a little more consistant by a hair.

#3ci, ii, and iii) I'll use apply for this. The function for it to call would be:

D_value <- function(x){
  letters <- letter.values(x)
  d <- (letters[4]-letters[3])/(letters[3]-letters[2])
}

#and lets "apply" it:

D_stat <- apply(micro_m,1,D_value)
M_stat <- apply(micro_m,1,function(x) letter.values(x)[3])

#Our D's are:

D_stat

#and our medians are:

M_stat

#3cii) Lets plot log(D) vs log(M)

D_stat_log <- log(D_stat)
M_stat_log <- log(M_stat)

plot(D_stat_log~M_stat_log,main="Log(D) vs Log(M)")
abline(h=0)

#For each log(d) greater than zero we should expect a right skew, and
#for each less than zero we should expect a left skew.

#The idea behind the plot is to discover if each level has increasing
#right or left skew as the median increases.  We can see that all the 
#points seem to be uniformly distributed about 0, so we could say that 
#there is no trend.

#If we wanted we could fit a line to these points and look at the slope.

D_lm <- lm(D_stat_log~M_stat_log)

plot(D_stat_log~M_stat_log,main="Log(D) Statistic: Skew by Median Value")
abline(D_lm$coeff)

summary(D_lm)

#The pvlaue of the slope indicates that it is probably zero,
#which we could have intuited from the graph.
#There is no relationship between the median and the skew.


#####But I can't just leave it at that. Lets explore the sensitivity
#of log(D) If we just used the criterion that positve values are right
#and negative are left, we would get that every dist is skewed right
#or left due to sampling error. So I choose to put a tolerance of .5. 
#If log(D) is within plus or minus .5, than we say it's symmetric already.

#So lets check it:

left_skew <- D_stat_log < .5
symmetric <- (-.5<= D_stat_log) & (D_stat_log <= .5)
right_skew <- D_stat_log > .5

skew <- LETTERS[1:8]
skew[left_skew] <- "Left Skew"
skew[symmetric] <- "Symmetric"
skew[right_skew] <- "Right Skew"

par(mfrow=c(3,3))
for(i in 1:8){hist(micro_m[,i],main = skew[i])}
par(mfrow=c(1,1))

#None of them are skewed really. We would have to increase the tolerance to one.

left_skew <- D_stat_log < 1
symmetric <- (-1<= D_stat_log) & (D_stat_log <= 1)
right_skew <- D_stat_log > 1

skew <- LETTERS[1:8]
skew[left_skew] <- "Left Skew"
skew[symmetric] <- "Symmetric"
skew[right_skew] <- "Right Skew"

par(mfrow=c(3,3))
for(i in 1:8){hist(micro_m[,i],main = skew[i])}
par(mfrow=c(1,1))

#This doesn't really work, we would have to find the right tolerance.
#If we chose +- 1 that means that the difference between the Fu and the M
#is 2.7 times greater than the difference between the M and the Fl.
#This stat could work, but currently it is too sensitive.
#Or better said, the statistic needs more research.

