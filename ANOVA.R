

#1.:8 marks) A sample of companies was selected randomly from each of three regions in Canada 
#(West, East, and Prairies), and annual salaries for marketing managers were collected.

# a.	(2 marks) A test is conducted to determine if the salaries of managers in the West is different 
#than $87,500. Identify the appropriate test and state the null and alternative hypotheses. 

#    One - Sample Test
#        Ho: ? = $87,500
#        H1: ? != $87,500

# b. (2 marks) A test is conducted to determine if the salaries of managers in the West is different 
#than in the Prairies. Identify the appropriate test and state the null and alternative hypotheses. 

#    Two sample t-test , Welch's Sample
#        Ho: ?1 - ?2 = 0
#        H1: ?1 - ?2  != 0


#c. (2 marks) A test is conducted to determine if the salaries of managers in the West, Prairies, 
#and East are the same. Identify the appropriate test and state the null and alternative hypotheses. 

#     ANOVA
#        Ho: ?1 = ?2 = ?3
#        H1: At least one mean is different


#d.	(2 marks) If companies were not randomly selected, and instead the salaries of marketing managers 
#over the last three years were collected from the Government of Canada, what kind of study would this be?
#Explain your reasoning. 

#     This is an observational study because it is conducted using pre-existing data from the Government
#     of Canada. In this case, the study would be of the retrospective type since it could examine 
#     historical records of the last 3 years.




#	(17 marks) A doctor is interested in how exercise affects sleep, so she conducts an experiment.
#30 healthy individuals are randomly assigned to three groups: light exercise, moderate exercise, 
#and heavy exercise. To collect her data, she has the light exercise group come into her facility 
#in the morning to perform the routine. Then she has the moderate exercise group perform the routine 
#after lunch. Finally, the heavy exercise group performs their routine in the late afternoon. 
#She records the number of hours each participant sleeps that night in the file exercise.csv. 



# a.	(1 mark) What is/are the factor(s) in this experiment? 
    # Factors are what is being manipulated, so the factors are: morning, after lunch and late afternoon


#b.	(3 marks) What are the levels of the factor(s)? 
#    The levels are randomly assigned into three groups: Light exercise, moderate exercise and heavy exercise


#c.	(1 mark) What are the subjects? 
#    The subjects are 30 individuals


#d. (1 mark) What is the response variable? 
#    As the doctor is interested in how exercise affects sleep, the responsable varible is hours sleep at night


#e.	(2 marks) What control issue(s) do you think is/are present in this design? Explain your thinking. 
#    The control factor that are no testing should be as similar as posible, the only difference between 
#    the groups should be what you are manipulating, so in this way I can identify control problems in this
#    design, for example gender. Gender is important in physical matters and athletic performance, 
#    men and women cannot be compared equally since the musculature is different, another control problem 
#    would be age and weight. Adding a block where the groups are defined by these given and non-randomly 
#    selected characteristics could help to have more accurate results. 



#f. (2 marks) What type of experimental design is used (factorial, completely randomized, or randomized 
#block)? Explain your thinking. 

#    It is a Completely Random Design, putting the 30 people randomly into 3 groups, and it does not 
#    have a block factor such as gender, and we are not analyzing different factors.


#g.	(2 marks) State the null and alternative hypothesis for this experiment. 
#     ANOVA
#        Ho: ?1 = ?2 = ?3
#        H1: At least one mean is different



#h.	(5 marks) Conduct an ANOVA to determine if different levels of exercise effect sleep at 
#???? = 0.05. Print out the ANOVA table (with the appropriate p-value), make a decision 
#(reject or do not reject), and interpret your result.


exercise <-read.csv(file = file.choose(), header = TRUE)  
exercise.aov <- aov(exercise$Sleep~excercise$Routine)
summary(exercise.aov)

# p = 0.0122 < 0.05 alpha
#Decision: Reject the null
#Interpretation: There are significant differences in the mean scores of the three groups, the amount
# of exercise and the time it is done, affects the hours of sleep. 





