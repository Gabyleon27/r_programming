
 

#	What is the P-value for t???-2.77, when the degrees of freedom are the last 2 digits of your student number.

pt(-2.77, 82, lower.tail=TRUE) #P - VALUE IS 0.00346





#After completing training at a telemarketing company, it is expected that an employee will make an 
#average of 420 calls per day. For a certain employee, the number of calls made daily is tracked over 
#30 days, and the average number of calls made per day is found to be 386, with a standard deviation of 
#85 calls. The manager would like to see if there is evidence that this employee is underperforming.
#Test the manager's hypothesis given a significance level of 0.05.

#a)What are the null and alternative hypotheses?

# Ho = 420
# H1 < 420

#b and c)What is the value of the test statistic? The p-value?

t.stat<-(386-420)/(86/sqrt(30)) #t.stat -2.16541

pt(t.stat, 29, lower.tail=TRUE) #p-value 0.01936 <  significance level 0.05 , 
#reject the Ho the employee is probably underperforming make under 420 calls per day with a 0.05 significance level.





#Read the "Donors.csv" dataset into R. This dataset contains a random sample of 916 donors 
#(from a population of 1.5 million) who have donated to a Canadian charitable organization.
#It includes the variables age (in years), homeowner (H=yes, U=unknown), gender, wealth 
#(1=lowest, 9=highest), children, donated last (0=did not donate, 1=did donate), 
#and amount donated last (in dollars).

donors <- read.csv(file = file.choose(), header = TRUE)


#	a) Since there are 9 categories of wealth, the mean value is 5. An analyst for the organization 
#is interested in whether the mean wealth category of the donor population differs from 5. 
#Test this hypothesis at ??=0.05, and interpret your results using the t.test() function.
#Ho = 5
#H1 !=5 

t.test(donors$Wealth, mu=5, alternative = "two.sided") # t= 3.7438, df = 915, p-value = 0.0001926

qt(0.05/2, 915, lower.tail=TRUE) #lower (negative) value  #-1.9625
qt(0.05/2, 915, lower.tail=FALSE) #upper (positive value) #1.9625

# t = 3.74 > 1.9625 t*,  Reject the null hypothesis
#There is evidence that the mean value of wealth is diferent as 5. 


#b) In Canada, the average age of a person is approximately 39 years old. 
#Using the "Donors.csv" data the Canadian charitable organization would 
#like to know if the average age of their donors is greater than 39 years. 
#Test this hypothesis at ??=0.01, and interpret your results using the t.test() function.
#Ho = 39
#H1 > 39

t.test(donors$Age, alternative = "greater", mu = 39) #t= 43.887 , p-value < 2.2e-16
#There is evidence that the avarege age of the donors are gretarn than 39 years, reject the Ho.



#c) Check the nearly normal condition for both variables (wealth and age). Is it satisfied?

hist(donors$Age, breaks=20, right=FALSE) 
hist(donors$Age) 
boxplot(donors$Age)
#the normal condition of the Age variable, seems to like fair normal, just have one outlier in the boxplot.
#the histogram appears to be skewed, but not enough to not meet the condition

hist(donors$Wealth, breaks=10, right=FALSE) 
hist(donors$Wealth) 
boxplot(donors$Wealth)
#the normal condition of the Welth variable, seems to like fair normal, no outlairs in the boxplot
#the histogram appears to be flat, no bell shape

#both variables satisfy the normal condition

