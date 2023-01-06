"
****************************************************************
Name: Gabriela Leon
Student Number: A00239383

QMM1001 Module 7 Applied Activity
****************************************************************
"
height <- read.csv(file = file.choose(), header = TRUE)


### QUESTION 1###

#a. Create a histogram of the student heights Do you think the normal distribution
#assumption is reasonable? Explain

hist(height$ï..Height.cm., col = "blue", main = "Histogram of Heights", xlab = "cms", ylab = "Freq") 

#The data approximately fit a bell curve shape, an outlier is visible but the vast majority of the values 
#are grouped to the curve, this curve has a single peak that is in the center and is symmetrical in relation to its mean.



#b. Calculate the mean and standard deviation of the heights
meanheight <- mean(height$ï..Height.cm.) #169.861
sdheight <- sd(height$ï..Height.cm.)     #12.909



#c. What heights do 95% of students fall between? 
x = meanheight-2*sdheight  #144.0425
y = meanheight+2*sdheight #195.6805

#The heights between are 144.0425 to 195.6805


#d. Calculate the z-scores for the shortest and tallest students in the class - are they
#considered outliers?

min <- min(height) #Min value 149.35
max <- max(height) #Max value 213.35

zmin <- (min-meanheight)/sdheight  #1.58
zmax <- (max-meanheight)/sdheight  #3.36

outlier1 <- meanheight + 3* sdheight   #208.59
outlier2 <- meanheight - 3 * sdheight  #131.13

#The min value is under 3 the smallest value is within 3 standard deviations, it is not considered 
#an outlier, the largest value is beyond 3 standard deviations, therefore it is considered an outlier.


#e. What is the probability that a student is less than 160cm? 
pnorm(160,meanheight,sdheight,lower.tail=TRUE) #0.2224652


#f. What is the probability that a student is taller than 180cm?
pnorm(180,meanheight,sdheight,lower.tail=FALSE) #0.2161236


#g. What is the probability that a student is between 165cm and 175cm?
pnorm(175,meanheight,sdheight,lower.tail=TRUE)-pnorm(165,meanheight,sdheight,lower.tail=TRUE) #0.3014579
pnorm(165,meanheight,sdheight,lower.tail=FALSE)-pnorm(175,meanheight,sdheight,lower.tail=FALSE) #0.3014579
diff(pnorm(c(165,175),meanheight,sdheight,lower.tail=TRUE)) #0.3014579


#h. What height in cm corresponds to the 90th percentile?
qnorm(0.9,meanheight,sdheight,lower.tail=TRUE) #186.40


#i. What height in cm corresponds to the bottom 15% of heights?
qnorm(0.15,meanheight,sdheight,lower.tail=TRUE) #156.48


#j. What TWO heights in cm do the MIDDLE (LAST TWO DIGITS OF STUDENT
#NUMBER)% of the data lie between?

#My student number : A00239383

p83 <- qnorm(0.83, meanheight, sdheight) #182.179
p17 <- qnorm (0.17,meanheight, sdheight) #157.543

#The heights are between 157.543 and 182.179