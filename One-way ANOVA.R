

df <- data.frame(Group = c("Placebo","Placebo","Placebo","Placebo","Placebo","Placebo","Placebo","Placebo","Placebo","50mg","50mg","50mg","50mg","50mg","50mg","50mg","50mg","50mg","100mg","100mg","100mg","100mg","100mg","100mg","100mg","100mg","100mg"),
                 Result = c(81,80,72,82,83,89,76,88,83,92,86,87,76,80,87,92,83,84,86,93,97,81,94,89,98,90,91)
                 )
print(df)

# a. (2 marks) State the null and alternative hypotheses. 

#        ANOVA
#        Ho: ?placebo = ?50mg = ?100mg                   
#        H1: At least one mean is different



# b.(2 marks) Perform a one-way ANOVA in R and print out the completed summary table. 

dfanova <- aov(Result~Group, data= df)
summary(dfanova)


#c. (1 mark) Determine the critical F value. 

df(0.95, df1=2, df2=24)

#The F.value is  7.289 and the critical value is 0.371



#d.	(2 marks) At ???? = 0.05 state your decision regarding the null hypothesis 
#(using the p-value or critical value and interpret your result in the context of the problem.

#p.value = 0.00336 < 0.05 alpha, the P.value  is less than alpha, we can conclude that there is a mean difference in at least 
#one of the 3 obervations groups.
#To find the groups with differences, I need to run the Tukey Test. 



#e. (3 marks) Check the independence, equal variance, and normal population assumptions. Explain if the assumptions are met or not 
#(show and describe all plots that you create). 


boxplot(Result~Group, data=df) #The are not significant differences, but apparently the 100mg group 
#achieved a better focus of attention among the participants, that took it since it had the highest mean.
#I can say that the independence assumption here is met, because the groups and the drugs was randomly assigned,
#and we can see en the bloxplot that the grops are independent from each other. 

hist(dfanova$residuals) #The normal population assumption is met, because the histogram is nearly normal 

plot(dfanova) #The Residuals vs Fitted plot, the red line is relatively straight, that means that is
#relatively constant variance across the 3 groups, that met the similar variance assumption.
#The Q-Q plot for assessing normality, we can see that the points are relatively close to the
#diagonal line, if the points stay relatively close to the diagonal line I can again conclude, that
#the normality condition is satisfied. 




#f. Create a bar plot of the accuracy for each type of drug. Include axis labels, a title, 95% 
#error bars and colors from a chosen palette. From this plot, do any of the drugs seem to help
#participants become more accurate than others? 

library(ggplot2)

ggplot(df, aes(Group, Result)) + stat_summary(fun.y=mean, geom="bar", fill="Green", colour="Black")+stat_summary(fun.data=mean_cl_normal, geom="errorbar", position=position_dodge(width=0.9), width=0.2)+ labs(x="Group", y="Result", title="Results of Drug Test Groups") 


#Because the error bar of the drug of 100mg doesn't overlap with the other bars there is a 
#significant difference between the group drug 100mg and the other groups. And it seems that the 
#group that took the drug with 100mg had better results in the tests carried out.


#g.(2 marks) Perform Tukey's HSD test to confirm your results from the bar plot. Interpret your result to explain which mean(s) are different. 
TukeyHSD(dfanova, conf.level =0.95)

#The p value is less than the level of significance in the two groups of 50mg and 100mg (p.value = 0.07 <0.05), 
#this means that there is a difference in favor of the 100mg group, it means that it had better results in the 
#cluster. Similarly when the 100mg group was compared to the Placebo group, the same difference was found to exist 
#in favor of the 100gm group. The 100mg group has a higher mean than these groups.

#There is no difference between the Placebo and 50mg groups, their pvalue is greater than the level of significance. (p.value = 0.32 > .05).

#In conclusion, the 100mg drug has better results than the other two groups.




#############								   	                  
#Question #2							
#############


combined1 <-read.csv(file = file.choose(), header = TRUE)

#Creating the subsets
BAPG <- subset(combined, Program== "BAPG", Select=Study)
CAGC <- subset(combined, Program== "CAGC", Select=Study)
HAGC <- subset(combined, Program== "HAGC", Select=Study)

#Taking a random sample

install.packages("dplyr")                              
library("dplyr")  


BAPG50 <- sample(BAPG$Study,50)
CAGC50 <- sample(CAGC$Study,50)
HAGC50 <- sample(HAGC$Study,50)

df2 <- data.frame(BAPG50,CAGC50,HAGC50)



combined<- reshape(data=df2, idvar="Program",
                         varying = c("Study"),
                         v.name=c("Study"),
                         times=c("Study"),
                         new.row.names = 1:1000,
                         direction="long")



combinedanova <- aov(Study~Program, data= combined)
summary(combinedanova)

#the p. value  is 0.565 greater than 0.05 the level of significance, there is not a significant difference in the means
#which means no reject the Null, the students from the 3 programs in average studied the same hours.