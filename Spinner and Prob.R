
"
****************************************************************
Name: Gabriela Leon
Student Number: A00239383

QMM1001 Module 6 Applied Activity
****************************************************************
"
library(prob)


### QUESTION 1###

#a)
outcomes <- c(50,10,5,0) #the outcomes of the given spinner
probs <- c(1/5, 1/5, 1/5, 2/5)  #the probability of each result, the spaces are 5 and as they are of the same size one has a probability of 20%, however Game over is repeated 2 times
(spinner_pd <- cbind(outcomes, probs)) #probability distribution



#b) 
s5 <- sample(outcomes, 5, replace= TRUE, prob = probs) #distribution 5 times
barplot(table(s5)) #barplot
table(s5) #frequency table

s500 <- sample(outcomes, 500, replace= TRUE, prob = probs) #distribution 500 times
barplot(table(s500)) #barplot
table(s500) #frequency table

s50000 <- sample(outcomes, 50000, replace= TRUE, prob = probs) #distribution 50000 times
barplot(table(s50000)) #barplot
table(s50000) #frequency table


#c) 
mean(s5)#2
var(s5) #7.5

mean(s500)#13.38
var(s500) #370.51

mean(s50000)#13.08
var(s50000) #357.84



#d)
spinner_exp <- sum(outcomes*probs)#expected value
spinner_var <- sum((outcomes-spinner_exp)^2*probs) #variance


#e) 
#No, to begin with the probability of getting zero doubles any chance of winning any amount. 
#Now if the objective is to win 50 dollars, the probability of obtaining the 50 dollars in a single
#spin decreases. In the example of 5 spins, since there are few repetitions, there is even the 
#possibility that 50 will not come out as a winner, the more repetitions its probability is closer
#to the theoretical one, 40% to lose and win zero dollars, 40% if a number is obtained less and
#only 20% to get exactly $50. The mean and variance after many repetitions are 13 and 356, 
#respectively, which means that the mean is closer to the number 10 than to the number 50. 
#But when there are few repetitions, the mean, which is 2, is closer to 0 and to 5, and the variance
#is only 7.5, which means that the furthest number is 12.5, still distant from 50.



### QUESTION 2###

#a)
#Yes, it is a binomial experiment because:
#1. The first rule is fulfilled since Success = Don't get a "Game Over", and Fail = Get a "Game Over"
#2. The experiment has a total of 9 trails, n = 9
#3. The probability of success is the same, the variable probs does not change
#4. The trials are independent, because the result of the spinner is random.



#b)Probability distribution for the number of Game Overs spun in 9 spins
outcomes <- 0:9 
probs <- dbinom(outcomes, 9,0.4)
(cbind(outcomes,probs))


#c) Expected value and variance for the distribution
9*0.4  #Expected value = np   3.6
9*0.4*0.6 #Variance = npq     2.16

#d)Probability of spinning more than 3 Game Overs
pbinom(3,9,2/5, lower.tail = FALSE) #0.5173903

#e)Probability of spinning less than 2 Game Overs
pbinom(2,9,2/5, lower.tail = TRUE) #0.231787

#f)Probability of spinning at least 1 Game Over
pbinom(0,9,2/5, lower.tail = FALSE) #0.9899223

#g)Probability of spinning more than 1 but less than 6 Game Overs
sum(dbinom(2:6, 9, 2/5)) #0.9044214
pbinom(6,9,2/5, lower.tail = TRUE) - pbinom(1,9,2/5, lower.tail = TRUE) #0.9044214
pbinom(1,9, 2/5, lower.tail = FALSE) - pbinom(6,9,2/5, lower.tail = FALSE) #0.9044214

