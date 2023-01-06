

#Determine the following:  Read in the "Health" dataset into R. This dataset 
#gives a sample from 129 individuals of body temperature (in degrees Fahrenheit) 
#and heart rate in beats per minute. Note that 1=male and 2=female.

health <- read.csv(file = file.choose(), header = TRUE)

#a.	(2 marks) Determine the mean and standard deviation of the heart rate.

mb<-mean(health$heart.rate) #mean = 73.79
sb<-sd(health$heart.rate) #standard deviation = 7.08


#b.	(1 mark) Calculate the standard error of the mean.

n <-length(health$heart.rate)
SE<-sb/sqrt(n) #Standar error of mean = 0.6235



#c.(1 mark) How many degrees of freedom does the t-statistic have?

df = n-1  #Degrees freedom = 128


#d.(2 marks) Use a histogram to check the nearly normal condition for heart rate. Is the condition satisfied?

hist(health$heart.rate) # The sample is more tan 40, the t method are safe to use and the histogram isn't extremely skewed.


#e.(3 marks) Determine the 95% confidence interval for the population heart rate mean. Interpret the interval

t.crit<-qt(0.975, n-1) #The t critic value is = 1.97
mb-t.crit*SE #72.55
mb+t.crit*SE #75.02

#the interval is [72.55-75.02],which means with 95% of confidence the mean population heart rate is between
#72.55 and 75.02 beats per minute.



#f.(3 marks) Find a (last two digits of your student number)% confidence interval for the population body 
#temperature mean. Comment on if this interval is wider or narrower than the 95% confidence interval.

mb2<-mean(health$temperature) #mean = 98.2
sb2<-sd(health$temperature) #standard deviation = 0.71
n2 <-length(health$temperature) #129
SE2<-sb2/sqrt(n2) #Standard Error = 0.062
t.crit2<-qt(.83/2+.5, n-1) #The t critic value is = 1.37
mb2-t.crit*SE2 #98.13
mb2+t.crit*SE2 #98.38

#the interval is [98.13-98.38], the confidence interval with my last 2 digits of my student number is
# .83 / 2 +.5 = 0.915 = 91.5% , is narrower than the 95% confidence interval.



#g.	(3 marks) Based on these statistics, how many people should be sampled to estimate the populate 
#mean heart rate within 2 bpm with 99% confidence?

sb<-sd(health$heart.rate) #standard deviation = 7.08
conf <- 0.99/2+.5
ME <- 2

n3<-(qnorm(0.995)*sb/ME)^2
n3<-(qt(0.995, n-1)*sb/ME)^2

ceiling(n3) #the sample size to estimate the populate mean hearth rate within 2bpm with 99% confidence is 86n



