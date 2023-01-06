

#a. Create a scatterplot of duration vs. heart rate. Add appropriate axis titles, a main
#title, and change the marker symbol to anything other than the default open
#circle. (5 marks)

penguins <- read.csv(file = file.choose(), header = TRUE) #import data base named Penguin

plot(penguins$Duration.min, penguins$Dive.Heart.Rate, main="Relationship between Duration vs Heart Rate", xlab="Minutes of Dive (min)",
     ylab="Heart Rate (bmp)", pch=11, col="blue", cex = 1, lwd = 1.5)


#b. Find the correlation coefficient between the two variables. State the direction
#and strength of the relationship

cor(penguins) #The correlation coefficient between these two variables is negative, and they are strongly correlated, 
#this can be seen in their coefficient of correlation, that is -0.84, close to -1.


#c. Find the equation of the regression line to predict a penguin's heart rate based
#on the duration of the dive 

penguins_line <- lm(Dive.Heart.Rate~Duration.min., penguins) #linear model between Heart Rate and Duration
penguins_line$coefficients  #the cofficents for penguins line

text(15, 100, "y = -5.468023x + 96.901980", col = "red") #equation of the line


#d. State and interpret the slope and y-intercept in the context of the problem.

#Slope and intercept define the linear relationship betweem two variables.The negative slope is derived from a negative correlation. Having a negative correlation indicates that the 
#two variables are associated in the opposite direction. The closer it gets to -1, the greater the strength 
#of the inverted correlation, this means by way of interpretation that the more minutes the penguin dives, 
#its heart rate decreases. This can be explained because the more minutes the pinguins dive, the greater is their apnea, 
#therefore, the oxygen in their blood must be conserved better by reducing their heart rate in order to have more diving time.
#Finally, the intercept indicates the value of the variable Y (heart rate), when X (minutes diving) is zero. 
#This means that when a penguin is not diving, the penguin's heart rate is 96.9 bpm.


#e. Plot the regression line and the equation of the line (in text) on your scatterplot.
#Change the colour of the line and text to any colour other than black.

abline(penguins_line, col = "red", lwd = 4) #find the line of regression
text(15, 100, "y = -5.468023x + 96.901980", col = "red") #equation of the line


#f. Using your regression line, what is the predicted heart rate of a penguin that
#dives for 14 minutes? Show the calculation using R.

new_minutes <- data.frame(Duration.min. = 14) #new values for dives
predict.lm(penguins_line,new_minutes) #to predict the new lm with the entered data


