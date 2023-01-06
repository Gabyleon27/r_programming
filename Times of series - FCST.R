
att<- read.csv(file = file.choose(), header = TRUE)

#a.	(1 mark) Create a ts() object for the AT&T Data.

att.ts<-ts(att[,-1], start=c(61, 01), end=c(67, 12), frequency = 12) #[,-1] means remove first column 




#b.	(3 marks) Plot the decomposition of the time series and comment on each of the four
#components of a time series.
plot(decompose(att.ts), col="blue")

#Trend Component : The overall pattern in the plot seems to be flat, 
#I can find fluctuations around years 61 to 62, the next two years seem to be stable,
#followed by a great depression between 64 (half a year) and 66, 
#finished with an increase in the return of one year.

#Seasonal Component : I can identify a 1 year seasonal component in all the 
# 7 years of the data set, it's around to may to may of each year. 
#The lowest peak appears every may, and the highest peak appears around november.
#Then I can say there are another significant lower peak around July, and an another higher peak around june.
# The last quarter of the year, the return comes downs until may from the nex year. 

#Cyclical component: I can say the ATT revenue, has a cyclical component, 
#we can find periods of prosperity, recession , depression and recovery over the years
#and under the same months. 

#Random / Irregular component: I can find a lot of spikes, that means
#I find a lot of irregular components that no fits into the model. 

#Since here we are talking about return that is correlated to the acquisition power
#of the population, we can care special attention to the seasonal component of the
#variables in relation of acquisition, like inflation, or external components that directly affects
#to the acquisition power like 'tie of vacations' or start to the next period of classes,
#Christmas, Thanks giving. When you analyze services that are not economically essentials (have a mobile plan is not essential), 
#the population just decide to quit this part of the consumption and satisfy the essential components in their life's.



#c.(3 marks) Fit an MA-5, MA-12, and another MA model that you choose to the time series.
install.packages("TTR")
library(TTR)

att.ma5<-SMA(att.ts, n=5) #n=5 is the length of the moving average
att.ma15<-SMA(att.ts, n=15) #n=15 is the length of the moving average
att.ma12<-SMA(att.ts, n=12) #n=12 is the length of the moving average



#d.	(2 marks) What is the forecast for the next month, January 1968, using all three moving
#average models? Do you predict that AT&T will exhibit a positive or negative return?

att.ma5[length(att.ma5)] #-0.00216
att.ma15[length(att.ma15)] #0.003338667
att.ma12[length(att.ma12)] #-0.003045
#MA 5 and 12 returns a negative value, the largest MA15 a positive. As I found the return
#is seasonal in 12 months, I infer th M15 are larger than the season, therefore over smoothed the value.
#So I predict a negative return between -0.003045  and -0.00216

#e. (4 marks) Plot the three smoothed models along with the original time series in one labelled
#plot. Give each series a different color and include a legend.

plot.ts(cbind(att.ts, att.ma5, att.ma15, att.ma12), 
        plot.type="single", 
        col=c("black", "red", "blue", "green"), 
        ylab="Return", 
        main="Return ATT")
legend("bottomright", legend=c("Data", "MA-5", "MA-15", "MA-12"), 
       col=c("black", "red", "blue", "green"), lty=1, cex=0.5)



#f. For all of the moving average models compute the MSE, MAD/MAE, and MAPE.
#Which model would you recommend?


ERRORS<-function(data, L){
  ma.data<-SMA(data, n=L)
  error<-NULL
  for (i in 1:length(data)-L){
    error[i]<-data[i+L]-ma.data[i+L-1]
  }
  error.p<-NULL
  for(i in 1:length(data)-L){
    error.p[i]<-abs(data[i+L]-ma.data[i+L-1])/abs(data[i+L])
  }
  MSE<-mean(error^2)
  MAD<-mean(abs(error))
  MAPE<-mean(error.p)*100
  error.df<-data.frame(errors=c(MSE, MAD, MAPE), row.names=c("MSE", "MAD", "MAPE"))
  return(error.df)
}


ERROR.MA5<-ERRORS(att.ts, 5)
ERROR.MA15<-ERRORS(att.ts, 15)
ERROR.MA12<-ERRORS(att.ts, 12)

#I would recommend the MA15 model , because has the lowest values in MSE, MAD and MAPE.


