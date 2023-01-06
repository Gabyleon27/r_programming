

#a. Create a vector that contains the outcomes for rolling a 9-sided die once. Use the 
# function to simulate 1000 rolls of the die. Create a plot that shows the 
#probability of rolling a 5 after each roll and add a line that shows the theoretical 
#probability of rolling a 5. HINT: follow the code completed in class for rolling a 
#die 1000 times and obtaining 3s. (4 marks) 

outcomes <- c(1,2,3,4,5,6,7,8,9)
die1000 <- sample(outcomes, 1000, replace = TRUE)
die1000
table (die1000)
barplot(table(die1000))
prop.table(table(die1000))


five <- ifelse(die1000 == 5, 1, 0)
plot(1:1000, cumsum(five)/(1:1000), xlab = "Number of rolls", ylab = "Percent of 5's rolled", 
     main = "Probability of rolling a 5 in 1000 rolls", type = "l")
abline(h=1/9, col ="red")




#b Repeat part a. but simulate rolling the die 10 000 times. What do you notice 
#about the relationship between the empirical and theoretical probability when 
#you roll the die more times? (2 marks)

outcomes <- c(1,2,3,4,5,6,7,8,9)
die10000 <- sample(outcomes, 10000, replace = TRUE)
die10000
table(die10000)
barplot(table(die10000))
prop.table(table(die10000))

five <- ifelse(die10000 == 5, 1, 0)
plot(1:10000, cumsum(five)/(1:10000), xlab = "Number of rolls", ylab = "Percent of 5's rolled", 
     main = "Probability of rolling a 5 in 10000 rolls", type = "l")
abline(h=1/9, col ="purple")


#The law of large numbers is a fundamental theorem of probability theory that indicates that if we repeat the same 
#experiment many times (tending to infinity), the frequency of the occurrence of a certain event tends to be a constant
#The more we throw the dice, the probability of obtaining 5 approaches the theoretical probability.

#c. Using the prob package, use the rolldie() function to create a probability space 
#for rolling a 9-sided die two times. (2 marks)

install.packages("prob")
library(prob)

die <- rolldie(2, 9, makespace = TRUE)


#d. Find the probability that the sum of two rolls of the 9-sided die is greater than 6
Prob(die, X1+X1>6)#0.6666



#e. Find the probability that the sum of two rolls of 9-sided die is less than 15.
Prob(die, X1+X1<15) #0.7777


#f. Find the probability that the sum of the rolls of two 9-sided die is less than 15 
#AND greater than 6
Prob(die, X1+X1<15 & X1+X1>6) #0.44444


#g. Find the probability that the sum of the rolls of two 9-sided die is greater than 15 
#AND less than 6. Explain this result
Prob(die, X1+X1>15 & X1+X1<6) #0.0   #The probability that the sum of both dice is greater than 15 and at the same time is less than 6 is improbable, 
#it cannot be under these conditions since being greater than 15 you are conditioning that it be greater than 6.

#h. Find the probability that the sum of the rolls of two 9-sided die is less than 15 OR 
#greater than 6. Explain this result.
Prob(die, X1+X1<15 | X1+X1>6) #1.0 #The probability that the sum is more than fifteen and at the same time greater than 6 is 1 since the condition
#greater than fifteen contains less than 6.




