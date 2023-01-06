"
****************************************************************
Name: Gabriela Leon
Student Number: A00239383

QMM1001 Module  9 Applied Activity
****************************************************************
"
#A group of 400 employees is randomly selected at a large company. They are asked if
#they spend greater than 4 hours a day looking at a computer screen. 313 responded yes. 


#1.a  Find a 95% confidence interval for the population proportion of employees that spend
#greater than 4 hours a day looking at a computer screen. (3 marks)
n1 <-400
p.hat1 <- 313/400
q1 <- 1 - p.hat1
SE1 <- sqrt(p.hat1 * q1/n1)
z.crit1 <- qnorm(0.95/2+0.5,0,1,lower.tail =TRUE)
lower1 <- p.hat1 - z.crit1 * SE1 
upper1 <- p.hat1 + z.crit1 * SE1 

#The 95% confidence interval is (0.742,0.8229)


#b. Assume that the sample proportion stays the same but the number of employees
#selected is increased to 1400. Find an 85% confidence interval for the population
#proportion of employees that spend greater than 4 hours a day looking at a computer
#screen. (3 marks)
n2 <-1800
p.hat2 <- 313/400
q2 <- 1 - p.hat2
SE2 <- sqrt(p.hat2 * q2/n2)
z.crit2 <- qnorm(0.85/2+0.5,0,1,lower.tail =TRUE)
lower2 <- p.hat2 - z.crit2 * SE2 
upper2 <- p.hat2 + z.crit2 * SE2 

#The 85% confidence interval is (0.7685, 0.7964)


#c. Is the confidence interval in part b wider or more narrow than the interval in part a?
#Explain the result in relation to the sample size and level of confidence (2 marks)

#If the confidence level is high, then the intervals are wider and therefore the precision decreases. 
#Two ways to narrow the intervals and get more precision from the data is to lower the confidence levels 
#or increase the sample size.
#In exercise a) the sample size is smaller than in exercise b), but its confidence level is higher, the same examples have the same p hat. 
#Which results in their intervals being quite similar in size, but no the same, because when increase the sample size the confidence interval gets smaller. 
#74% to 82% a width about 8%, and the second is about 77% to 80%, a width about 3%. 

#Exercise 	  Lower	   Upper	    n	  Confidence
#   a	        0.742	   0.8229	  400	     95%
#  b	        0.7685 	 0.7964	  1800	   85%


#2. Function, sample proportion, margin error, confidence level 
#: It is believed that 20% of adults over the age of 50 never graduated from
#high school. We wish to see if this percentage is the same among the 25 to 30 age group. How
#many of this younger age group must be surveyed in order to estimate the proportion of nongraduates to within ±5% with 95% confidence?

CI <-function(p.hat3,cl,ME3){
  z.crit3 <-qnorm(cl/2+0.5,0,1,lower.tail=TRUE)
  n <- (z.crit3/ME3)^2*p.hat3*(1-p.hat3)
  return(n)
}
CI(0.2,0.95,0.05) #245 the size of the sample



#3.Find the sample proportion of the number of days that you
#watched the news. Create and INTERPRET a 95% confidence interval for the proportion of days
#that you watch the news

n4 <-89
p.hat4 <- 50/89
q4 <- 1 - p.hat4
SE4 <- sqrt(p.hat4 * q4/n4)
z.crit4 <- qnorm(0.95/2+0.5,0,1,lower.tail =TRUE)
lower4 <- p.hat4 - z.crit4 * SE4  
upper4 <- p.hat4 + z.crit4 * SE4  

#The 85% confidence interval is (0.4587, 0.6648)







