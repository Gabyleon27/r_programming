

#1.	(17 marks) The data set "mpg.csv" provided in Moodle gives the number of miles per gallon (mpg) 
#for a random sample of U.S. cars vs. Japanese cars. We would like to test the hypothesis that Japanese 
#cars get better mileage than U.S. cars at ?? = 0.01.

#a.	(2 marks) State the null and alterative hypothesis
       #   Ho: mu_japan - mu_usa = 0
       #   H1: mu_japan -mu_usa > 0



#b. (3 marks) Check all assumptions and conditions for performing a two-sample t-test. 
#Are they satisfied? Include histograms for the mileage in each country to check the nearly normal condition.

       #1. Independence Assumption: The data of each group are independently and random, we are comparing to differents groups
       #2. Randomization Condition: I assumed that the data is representative of the population and randomly selected
       #3. 10% condition: I assumed that the population of cars are more than the given sample and the sample given are less than 10%
       #4. Nearly Normal Condition: The distribution for each group are unimodal and seems symmetric. The histogram of the united states 
       #   seems to be skewed to the right

autos <-read.csv(file = file.choose(), header = TRUE)

#create groups and find the mean spend lift for each
japan<-subset(autos, Country=="JAPAN", select=Mileage)
mean(japan$Mileage) #Japan mean = 25.8
usa<-subset(autos, Country=="US", select=Mileage)
mean(usa$Mileage) #USA mean = 15.8

#boxplot by group
boxplot(autos$Mileage~autos$Country, col=c("blue", "green"), outcol="red", ylab="Mileage", xlab="Country",
        main="Boxplot for Mileage by Country")
#histograms for each group
hist(japan$Mileage, main="Japan Autos", xlab="Mileage", ylab="Frequency", col="red")
hist(usa$Mileage, main="U.S. Autos", xlab="Mileage", ylab="Frequency", col="blue")


        #5. Independent groups assumption: The Japan and American groups are independent of each other. The have different means



#c.	(2 marks) Perform the hypothesis test using the t.test() function 
t.test(autos$Mileage~autos$Country, 
       var.equal=FALSE, #independent groups
       paired=FALSE, #independent groups
       conf.level=0.99, #given alpha = 0.01
       alternative="greater") #t = 8.4, p.value = 2.093e-06, df = 10.8



#d. (2 marks) Determine the critical value ???? ???.

qt(0.01, 10.8, lower.tail=FALSE) #t* = 2.7


#e. (2 marks) Make a decision (using the p-value OR critical value) and interpret your results

#p-value = 2.093e-06 < 0.01 = alpha
#Reject the null hypothesis
#There is evidence that Japan cars get better mileage than U.S. cars




#f.(3 marks) Determine the 95% confidence interval for the mean difference in mileage for Japanese 
#and US cars. Interpret the interval 

t.test(autos$Mileage~autos$Country, 
       var.equal=FALSE, #independent groups
       paired=FALSE, #independent groups
       conf.level=0.95, #given alpha = 0.01
       alternative="greater") #t = 8.4, p.value = 2.093e-06, df = 10.8

# 95 percent confidence interval: 7.878591      Inf
#With 95% confidence that the mean difference of Japanese cars in terms of miles per gallon compared to US
#cars, will always start at 7.8 gallons.




#g. (3 marks) Create a bar plot of the mean mileage for US and Japanese cars with error bars showing the 
#95% confidence intervals. Add proper labels
#To create a grouped barplot, load the necessary packages
install.packages("ggplot2")
install.packages("Hmisc")
library(ggplot2)
library(Hmisc)



g <- ggplot(autos, aes(Country,Mileage))
g + geom_boxplot(varwidth = T, fill='plum')+
  labs (title = 'Bar Plot Compare Means',
        subtitle = ' Japan vs Us',
        x='Country',
        y='Mileage')
      
  
