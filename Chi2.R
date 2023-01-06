

#a. 2 marks) State the null and alterative hypothesis for the chi-square goodness of fit test.
#Test
#H0: The monthly distribution of brithadys across the year is the same
#HA: The monthly distribution of brithadys across the year is not the same



#b. (2 marks) Create vectors for the observed birthdays in each month and expected birthdays 
#n each month. What is the expected number of birthdays per month under the null hypothesis? 
observed<-c(137,121,116,121,126,114,102,165,134,115,105,122)
expected<-rep(sum(observed)/length(observed), length(observed))
#The expected number of birthdays under the null hypothesis is 123.16

#c.(5 marks) Conduct a chi-square goodness of fit test at ?? = 0.05 and decide to reject or
#ail to reject the null hypothesis (using the p-value or critical value). Interpret your decision in the context of the problem.

(chi2<-sum((observed-expected)^2/expected))
pchisq(chi2, df=11,lower.tail=FALSE) 

# p.value.chi = 0.0096 < 0.05 alpha
#Reject the null hypothesis
#there is a difference in the distribution of the birthays across the year


#d.	(1 mark) Calculate and print out the standardized residuals for each month. 

residuals<-observed - expected
residuals.sd<-residuals/sqrt(expected)

#OR Extract Standardized Residuals from the Built-In Function
residuals.sd2<-chisq.test(observed)$residuals

print(residuals.sd)
print(residuals.sd2)


#e.	 (4 marks) Create a bar plot that shows the residual values. Are there any unusual values? 
 #Specifically comment on which unusual values are above the mean and which are below the mean.

meanres <- mean(residuals.sd)

#Residual Barplot
library(ggplot2)
resid<-data.frame(Month=c('a.Jan','b.Feb','c.Mar','d.Apr','e.May','f.Jun','g.Jul','h.Ags','i.Sep','j.Oct','k.Nov','l.Dec'), standardized.residuals=residuals.sd)

ggplot(resid, aes(Month, standardized.residuals, fill=Month))+
  geom_bar(stat="identity")+
  labs(x="Month", y="Standardized Residuals")+
  ggtitle("Residuals for Months")+
  geom_hline(yintercept=c(-3, 3), linetype="dashed", color = "red")
  scale_fill_brewer(blues9)
  
#Regarding the outliers, only one was found in the month of August above 3 
#standard deviations, with a value of 3.7.What reinforces the rejection of 
#the Ho, there is a difference in the months, especially in August.
  

  
  ggplot(resid, aes(Month, standardized.residuals, fill=Month))+
    geom_bar(stat="identity")+
    labs(x="Month", y="Standardized Residuals")+
    ggtitle("Residuals for Months")+
    geom_hline(yintercept=c(-4.8), linetype="dashed", color = "red")
  scale_fill_brewer(blues9)  
  
  
#With respect to the mean of the residuals, which is -4.48, 
#no value is below, the closest from the mean is the month of July with -1.9, 
#but it is still very far from the mean.


  
#f.	(4 marks) Create a grouped bar plot to show the observed and expected 
#distributions for birthdays in each month. Are more players born in August?
#Explain using the plot.

players<-data.frame(Month2=rep(c('a.Jan','b.Feb','c.Mar','d.Apr','e.May','f.Jun','g.Jul','h.Ags','i.Sep','j.Oct','k.Nov','l.Dec'), 2), 
                       Counts=c(observed, expected), 
                       Distribution=c(rep("observed", length(observed)), rep("expected", length(expected))))
ggplot(players, aes(Month2, Counts, fill=Distribution))+
    geom_bar(stat="identity", position="dodge")+
    labs(x="Months", y="Number of Births")+
    ggtitle("Births")

#The observed values are higher than expected in the months of: August, January, September 
#and slightly in May. Being in August the biggest difference.

#The observed values are lower than those expected in the remaining 8 months.

#There is a difference in the 12 months with respect to the observed values to the expected ones.