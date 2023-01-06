

#a.	4 marks) Create three boxplots for biomass by salinity, biomass by temperature, 
#and biomass by both salinity and temperature. Based on these plots, explain if you 
#think higher or lower salinity and temperatures result in higher biomass for sprouts. 

sprouts<- read.csv(file = file.choose(), header = TRUE)


boxplot(Biomass~Salinity, sprouts, col=c("blue", "purple"), 
        ylab="grams per meter (gm) ", main="Grams per meter by Salinity")

boxplot(Biomass~Temperature, sprouts, col=c("blue", "purple"), 
        ylab="grams per meter (gm) ", main="Grams per meter by Temperature")

boxplot(Biomass~Salinity*Temperature, sprouts, col=c("blue", "purple"), 
        ylab="grams per meter (gm) ", main="Grams per meter by Salinity and Temperature")

#It seems that lower Salinity result in higher biomass for sprouts, and the medians of 
#0ppm and 4ppm appear to be the same, the other two medians of 12ppm and 8ppm are different 
#from the aforementioned means (0ppm and 4ppm) and different from each other .

#In other hand, the Temperatures of 34C and 36C are nearly have the same median, and the temperature
#of 32C is different from the others. 

#In the final boxlot we can find that the highest elevation of Biomass is between 34C and 36C 
#and Salinity od 4ppm.




#b.	(2 marks) State the null and alternative hypotheses for a two-way analysis of variance 
#to determine the effect the factors salinity and temperature have on biomass. 


#Two-way ANOVA Test
#H0: the means of Salinity are equal 
#H0: the means of Temperature are equal 
#H0: The effects of Salinity are constant across the levels of Temperature
#HA: At least one mean is different than the others



#c.(4 marks) Conduct the two-way analysis of variance at the ???? = 0.05 level. 
#Print a summary of the completed ANOVA table. State your decision regarding the 
#null hypotheses (using the p-values or critical values) and interpret your result in 
#the context of the problem. 


sprouts.anova<-aov(Biomass~Salinity*Temperature, data=sprouts)
summary(sprouts.anova)

#Reject the null, the Salinity and Temperature factor levels are significantly different, that 
#means at least a mean of Factors is different that the others, and at least a mean of Temperature
#is different that the others of the group. 



#d.	(2 marks) Are the similar variance and normality conditions satisfied? 
#Create at least 2 plots and interpret the plots to explain your answers.

#Boxplots to check equal variance
boxplot(Biomass~Salinity, sprouts, col=c("blue", "purple"), 
        ylab="grams per meter (gm)", main="Grams per meter by Salinity")


boxplot(Biomass~Temperature, sprouts, col=c("blue", "purple"), 
        ylab="grams per meter (gm) ", main="Grams per meter by Temperature")


#check other residual plots
plot(sprouts.anova) 
#Option 1: Fitted vs. Residuals - it seems that there is not indication of spread in variance
#Option 2: QQ plot - nearly normal

#The same variance condition is met



#Histogram of residuals for Nearly Normal Condition
hist(sprouts.anova$residuals, xlab="Residuals", 
     main="Histogram of Residuals") 
#The histogram shows a lightly left skewed, but see nearly normal. 



#e.(4 marks) Create an interaction plot (Hint: use temperature on the x-axis). 
#Explain how this plot supports the conclusion from the ANOVA test regarding the 
#significance of the main effects and interaction. 

#interaction plot
interaction.plot(sprouts$Salinity, sprouts$Temperature, sprouts$Biomass,
                 xlab="Salinity ", ylab="Biomass gm", 
                 main="Interaction Plot", trace.label="Temperature")


#The differences of more averages of the groups can be observed mainly in the group of 32C. 
#It is observed as an independent group of the two of 36 and 34C. With respect to salinity at 0ppm,
#the temperatures of 34 and 36 seem to have the same impact on gm, where the greatest impact is 
#found, the least is found at 12ppm. The temperatures react in the same way with respect to 
#salinity, the higher the salinity, the lower the biomass, the lower the salinity, the higher the
#biomass.



#f.	(3 marks) Perform Tukey's HSD Test. Interpret any significant values. 
#Which salinity/temperature combination would you recommend?

#Tukey's HSD - determine which means are different
TukeyHSD(sprouts.anova, conf.level=0.95)

#0ppm:36C and 0ppm:32C    are different, p.v = 0.0077689
#0ppm:36C and 12ppm:32C   are different, p.v = 0.0000360
#0ppm:36C and 4ppm:32C    are different, p.v = 0.0045343
#0ppm:36C and 8ppm:32C    are different, p.v = 0.0003685
#0ppm:34C and 12ppm:32C   are different, p.v = 0.0000656
#0ppm:34C and 4ppm:32C    are different, p.v = 0.0083177
#0ppm:34C and 8ppm:32C    are different, p.v = 0.0006827
#4ppm:36C and 12ppm:36C   are different, p.v = 0.0058384
#4ppm:36C and 0ppm:32C    are different, p.v = 0.0311529
#4ppm:36C and 12ppm:32C   are different, p.v = 0.0001495
#4ppm:36C and 8ppm:32C    are different, p.v = 0.0015793
#4ppm:34C and 12ppm:32C   are different, p.v = 0.0042805
#12ppm:36C and 0ppm:34C   are different, p.v = 0.0025434
#12ppm:36C and 0ppm:36C   are different, p.v = 0.0013733

#The significant values they are concentrated as expected at the 0pp outlet 
#and at temperatures of 36C and 34C. Followed by salinity of 4ppm. 
#At this point I would recommend a salinity of 0pp at 36C or 34C or 
#a biomass of 4ppm at a temperature of 34C or 36C to obtain greater biomass.




#g.	(3 marks) Create a grouped bar plot for the mean of biomass by salinity 
#and temperature. Use a chosen colour palette. Explain how this bar plot supports your
#recommendation for the best salinity/temperature combination from part f.

library(ggplot2)

ggplot(sprouts, aes(Salinity, Biomass))+
  stat_summary(fun="mean", geom="bar", fill="blue")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)+
  labs(x="Salinity", y="Biomass", title="Effect of Salinity on Biomass")


ggplot(sprouts, aes(Temperature,Biomass ))+
  stat_summary(fun="mean", geom="bar", fill="green")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)+
  labs(x="Temperature", y="Biomass", title="Effect of Temperature on Biomass")


ggplot(sprouts, aes(Temperature, Biomass, fill=Salinity))+
  stat_summary(fun="mean", geom="bar", position="dodge")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", position="dodge")+
  labs(x="Temperature", y="Biomass")+
  ggtitle("Effect of Temperature and Salinity of gm on Biomass")

#The barplot of salinity effects on Biomass shows that the best result is 0pp and the second 
#result is 4ppm, thus supporting the recommendation made.

#The barplot of temperature effects in Biomass shows that the best result is 
#at temperatures of 36C and 34C, in the same way it supports the recommendation

#And in the last barplot it shows where Salinity, temperature and Biomass effects are found, 
#reaffirming that the best combination to obtain biomass is temperature 36C or 34C, at a Salinity
#of 0pp and secondly 4ppm.

