
data <- "Satisfaction Survey.csv"
rawdata <- data.frame(read.csv(data))

#Understanding the Data  - Node 1


str(rawdata)
head(rawdata)

# This data has 28 variables  and 129889 observations
# Satisfaction is the dependent variable and all the other columns are independent variables
# The client is Southeast hence this project will have comparisons between Southeast 
# and other providers
# The date column will be converted into month and day of week to determine if time of
# year and time of week have any significant effect on the Satisfaction

# find unique values in each column to check for abnormal values
sapply(rawdata,function(y)unique(y))


#Cleaning the Data - Node 2

install.packages("caTools")

library(caTools)
clean_data<-rawdata

# find unique values in each column to check for abnormal values
sapply(clean_data,function(y)unique(y))

# the Satisfaction variable has 3 abnormal values which need to be replaced or removed

#conversion to numeric converts the abnprmal values to NA
clean_data$Satisfaction <- as.numeric(as.character(t(clean_data$Satisfaction)))

mean(clean_data$Satisfaction,na.rm=TRUE)
# mean = 3.37941
# replace the NA values with mean
# the NA values are replaces with 3.5 instead of the actual value of the mean because 
# satisfaction is a factor variable and 3.5 is the closest value
clean_data$Satisfaction[is.na(clean_data$Satisfaction)] <- 3.5
clean_data$southeast<-as.factor(trimws(clean_data$Airline.Name)=='Southeast Airlines Co.')
unique(clean_data$Satisfaction)
clean_data_full=clean_data
sample = sample.split(clean_data$Age, SplitRatio = .67)
train = subset(clean_data, sample == TRUE)
test  = subset(clean_data, sample == FALSE)
clean_data = train



#Categorizing the data - Node 3



data = clean_data_full


data$Flight.date=as.Date(data$Flight.date, format="%m/%d/%Y")
data$week_days=weekdays(data$Flight.date)
data$month<- strftime(data$Flight.date, "%m")

numbertoCategory<-function(vec){
  #getting the 40th and 60th percentile of the data
  q <- quantile(vec, c(0.4, 0.6),na.rm=TRUE)
  #creating a vector with 'Average' value of length of vec
  vBuckets <- replicate(length(vec), "Average")
  #values lower than 40th percentile are replaced by 'Low'
  vBuckets[vec < q[1]] <- "Low"
  #values greater than 60th percentile are replaced by 'high'
  vBuckets[vec > q[2]] <- "High"
  return(as.factor(vBuckets))
  
}
data$Age=numbertoCategory(data$Age)
data$No.of.Flights.p.a.=numbertoCategory(data$No.of.Flights.p.a.)
data$X..of.Flight.with.other.Airlines=numbertoCategory(data$X..of.Flight.with.other.Airlines)
data$Shopping.Amount.at.Airport=numbertoCategory(data$Shopping.Amount.at.Airport)
data$Departure.Delay.in.Minutes=numbertoCategory(data$Departure.Delay.in.Minutes)
data$Arrival.Delay.in.Minutes=numbertoCategory(data$Arrival.Delay.in.Minutes)
data$Flight.time.in.minutes=numbertoCategory(data$Flight.time.in.minutes)
data$Flight.Distance=numbertoCategory(data$Flight.Distance)
data[data$Flight.cancelled=='Yes',]$Arrival.Delay.in.Minutes='NotAvailable'
data[data$Flight.cancelled=='Yes',]$Departure.Delay.in.minutes='NotAvailable'
data[data$Flight.cancelled=='Yes',]$Flight.time.in.Minutes='NotAvailable'
data$Eating.and.Drinking.at.Airport=numbertoCategory(data$Eating.and.Drinking.at.Airport)


data$Satisfaction[data$Satisfaction>=4]="Satisfied"
data$Satisfaction[data$Satisfaction<4]="Not Satisfied"
data$Satisfaction=as.factor(data$Satisfaction)
data$Price.Sensitivity=as.factor(data$Price.Sensitivity)
data$No..of.other.Loyalty.Cards=as.factor(data$No..of.other.Loyalty.Cards)
data$Day.of.Month=as.factor(data$Day.of.Month)
data$Scheduled.Departure.Hour=as.factor(data$Scheduled.Departure.Hour)
data$week_days=as.factor(data$week_days)
data$month=as.factor(data$month)
data=data[,-15]

facna=addNA(data$Arrival.Delay.in.Minutes)
levels(facna) <- c(levels(data$Arrival.Delay.in.Minutes), 'Not Available')
data$Arrival.Delay.in.Minutes<-facna
facna=addNA(data$Departure.Delay.in.Minutes)
levels(facna) <- c(levels(data$Departure.Delay.in.Minutes), 'Not Available')
data$Departure.Delay.in.Minutes<-facna
facna=addNA(data$Flight.time.in.minutes)
levels(facna) <- c(levels(data$Flight.time.in.minutes), 'Not Available')
data$Arrival.Delay.in.Minutes<-facna

data$southeast=as.factor(data$southeast)
data$Year.of.First.Flight=as.factor(data$Year.of.First.Flight)
data<-data.frame(sapply(data,as.factor))
new_data<-data
str(new_data)


#Exploratory Data Analysis - Data Visualization -----------------------

install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
df<-new_data
#using this for Age
satisfactionAge<-ggplot(df)+geom_bar(mapping=aes(x=Age,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
satisfactionAge
# As observed from the above plots, We can interpret that initially with increase in age, people's satisfaction raatings increase and then It starts decreasing after certain age
# which says spproximately people in age range of 30 to 50 rate high on satisfaction
# similar trends when comparing south east and other airlines

#using this for Airline Status
satisfactionAirlineStatus<-ggplot(df)+geom_bar(mapping=aes(x=Airline.Status,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
satisfactionAirlineStatus
# More number of passengers are having Blue airline status and it increases from blue to platinum to gold to silver
#similar trends when comapring southeast and other airlines

#using this for gender
satisfactionGender<-ggplot(df)+geom_bar(mapping=aes(x=Gender,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
satisfactionGender
#similar trends when comparing south east and other airlines

#using this for price sensitivity
satisfactionPriceSensitivity<-ggplot(df)+geom_bar(mapping=aes(x=Price.Sensitivity,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
satisfactionPriceSensitivity
#decrease in satisfaction with increase and price sensitivity factor of the passenger
#similar trends when comparing southeast and other airlines

#using this for year of first flight
satisfactionYearOfFirstFlight<-ggplot(df)+geom_bar(mapping=aes(x=Year.of.First.Flight,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
satisfactionYearOfFirstFlight
#similar trend in southeast and other airlines



#using this for Age
p1<-ggplot(df)+geom_bar(mapping=aes(x=No.of.Flights.p.a.,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
p1
# As observed from the above plots, We can interpret that initially with increase in age, people's satisfaction raatings increase and then It starts decreasing after certain age
# which says spproximately people in age range of 30 to 50 rate high on satisfaction
# similar trends when comparing south east and other airlines

#using this for Airline Status
p2<-ggplot(df)+geom_bar(mapping=aes(x=X..of.Flight.with.other.Airlines,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
p2
# More number of passengers are having Blue airline status and it increases from blue to platinum to gold to silver
#similar trends when comapring southeast and other airlines

#using this for gender
p3<-ggplot(df)+geom_bar(mapping=aes(x=Type.of.Travel,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
p3

#similar trends when comparing south east and other airlines

#using this for price sensitivity
p4<-ggplot(df)+geom_bar(mapping=aes(x=No..of.other.Loyalty.Cards,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
p4

#decrease in satisfaction with increase and price sensitivity factor of the passenger
#similar trends when comparing southeast and other airlines

#using this for year of first flight
#satisfactionYearOfFirstFlight<-ggplot(df)+geom_bar(mapping=aes(x=Year.of.First.Flight,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()

#similar trend in southeast and other airlines


dataframe = new_data

SatisfactionClass <- ggplot(dataframe) +geom_bar(mapping=aes(x=Class, fill=Satisfaction),position="fill", width=0.4)+scale_fill_grey()+
  theme_classic()
SatisfactionClass

SatisfactionShopping <- ggplot(dataframe) +geom_bar(mapping=aes(x=Shopping.Amount.at.Airport, fill=Satisfaction),position="fill", width=0.4)+scale_fill_grey()+
  theme_classic()
SatisfactionShopping

SatisfactionEatingAndDrinking <- ggplot(dataframe) +geom_bar(mapping=aes(x=Eating.and.Drinking.at.Airport, fill=Satisfaction),position="fill", width=0.4)+scale_fill_grey()+
  theme_classic()
SatisfactionEatingAndDrinking

SatisfactionDayOfMonth <- ggplot(dataframe) +geom_bar(mapping=aes(x=Day.of.Month, fill=Satisfaction),position="fill", width=0.4)+scale_fill_grey()+
  theme_classic()
SatisfactionDayOfMonth


#Class variable
#SatisfactionClass <- ggplot(dataframe, aes(x = Class,y = Satisfaction))+facet_grid(southeast ~ .)+geom_count()+
#stat_summary(aes(y=Satisfaction),fun.y="mean",size=2,colour='red',geom="point") 

#For airlines other than Southeast, 
#Business class has highest rating (average) at 3.5, followed by Eco and then Eco plus
#The same trend is followed by Southeast airlines

#Day of month


#For airlines other than Southeast, 
#the satisfaction value averages around 3.3 for almost all days of month
#For Southeast airlines, the satisfaction value differs for different days.
#For the 4th, 7th, 10th days, it is 3.5 with values lesser than that for the other days


#Shopping amount at Airport
#SatisfactionShopping <- ggplot(dataframe) +geom_bar(mapping=aes(x=Shopping.Amount.at.Airport, fill=Satisfaction),position="fill", width=0.4)+scale_fill_grey()+
#theme_classic()
#cor(dataframe$Shopping.Amount.at.Airport, dataframe$Satisfaction)
#For airlines other than Southeast, 
#Shopping amount ranges from 0 to 900
#with customers who spend 700-800 units rating the Satisfaction at 4 (max) and 
#customers who spend 800-900 units rating the Satisfaction at 3 (min)
#For Southeast airlines, 
#Shopping amount ranges from 0 to 600
#with customers who spend 500-600 units rating the Satisfaction at 4 (max) and
#customers who spend 400-500 units rating the satisfaction at around 3.2 (min)

#This gives us an insight that customers do not spend more than 600 units on
#Shopping at airport when they are a Southeast customer

#The correlation between the two variables is 1.7% which is not very high

#Eating and drinking at airport
#SatisfactionEatingAndDrinking <- ggplot(dataframe) +geom_bar(x=Eating.and.Drinking.at.Airport,mapping=aes(fill=Satisfaction),position="fill", width=0.4)+scale_fill_grey()+
#theme_classic()
#cor(dataframe$Satisfaction, dataframe$Eating.and.Drinking.at.Airport)
#For airlines other than Southeast, 
#Eating and Drinking amount ranges from 0 to 900
#with customers who spend 700-800 units rating the Satisfaction at 3.8 (max) and 
#customers who spend 500-600 units rating the Satisfaction at 3 (min)
#For Southeast airlines, 
#Eating and Drinking amount ranges from 0 to 800
#with customers who spend 700-800 units rating the Satisfaction at 5 (max) and
#customers who spend 200-300 units rating the satisfaction at around 3.3 (min)

#The 750 value could be an outlier (since we are getting an average rating of 5)

#The correlation between the two variables is 0.01% which is not very high

#---------------------------------------------------------------------------

#convert the target variable to numeric
#to perfrom various EDA operations





#as plotting the whole date has no meaning,
#plot it with day off the week_days

#day of the month is already present as a variable
#the spread is more for southeast Airlines
#also few values fall near Satisfaction 3 and below

fD1<-ggplot(data)+geom_bar(mapping=aes(x=week_days,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
fD1
#more people seem to be not satisfied on thursdays
#the Satisfaction for southeast is more on tuesdays
#the remaining are almost same for all days



#the Satisfaction for different airlines is seen
#almost all of them have an average Satisfaction of 3.5

fD2<-ggplot(data)+geom_bar(mapping=aes(x=Airline.Name,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
fD2


#few of them lead to Satisfaction 3
fD3<-ggplot(data)+geom_bar(mapping=aes(x=Scheduled.Departure.Hour,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
fD3
#people are more not satisfied for scheduled departure hours 3,6 and 7

fD4<-ggplot(data)+geom_bar(mapping=aes(x=Departure.Delay.in.Minutes,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
fD4
#the more the delay depature, the less the people are satisfied


satisfactionArrivalDelay<-ggplot(df)+geom_bar(mapping=aes(x=Arrival.Delay.in.Minutes,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
satisfactionArrivalDelay
#The percentage of satisfied and not satisfied customers is pretty much the same considering the Arrival Delay.
#Even when it is High, Average or Low.

satisfactionCancelled<-ggplot(df)+geom_bar(mapping=aes(x=Flight.cancelled,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
satisfactionCancelled
#It can be seen that there are more non satisfied people where flights are cancelled. 
#This is explainable as cancelled flights could result in a negative review.

satisfactionFlightTime<-ggplot(df)+geom_bar(mapping=aes(x=Flight.time.in.minutes,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
satisfactionFlightTime
#Considering the flight time, there are almost equal number of satisfied and not satisfied customers.

satisfactionDistance<-ggplot(df)+geom_bar(mapping=aes(x=Flight.Distance,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
satisfactionDistance
#Same as flight time, the flight distance also has almost equal percentage of satisfied and non satisfied customers.

satisfactionArrivalGreater<-ggplot(df)+geom_bar(mapping=aes(x=Arrival.Delay.greater.5.Mins,fill=Satisfaction),position="fill",width=0.4)+scale_fill_grey() + theme_classic()
satisfactionArrivalGreater
#In this plot, it is shown that there are more non satisfied customers where the arrival delay is greater than
#5 minutes.
#This indicates that although there may not be a difference in just a regular arrival delay, if the delay is 
#greater than 5 minutes, then the satisfaction gets affected.


#Data Modelling -------------------


#Model 1 - Naive Bayes 

install.packages("e1071")
library(e1071)

clean_data <- train
clean_data <- na.omit(clean_data)

happyCust <- 1
med <- median(clean_data$Satisfaction)
happyCust[clean_data$Satisfaction>=med] <- 3
happyCust[clean_data$Satisfaction<med] <- 2

happyCust <- as.factor(happyCust)
clean_data1 <- cbind(clean_data,happyCust)
clean_data1 <- clean_data1[,-c(1)]

nb <- naiveBayes(happyCust ~ Airline.Status + Age + Gender +
                   Price.Sensitivity + Year.of.First.Flight + No.of.Flights.p.a. +
                   Type.of.Travel +
                   Shopping.Amount.at.Airport +
                   Class + Scheduled.Departure.Hour +Departure.Delay.in.Minutes +
                   Arrival.Delay.greater.5.Mins, data = clean_data1)


summary(nb)

#Output for Summary of model

#        Length Class  Mode     
#apriori  2     table  numeric  
#tables  12     -none- list     
#levels   2     -none- character
#call     4     -none- call     

nb

#Output for Naive Bayes 

#Naive Bayes Classifier for Discrete Predictors

#Call:
#naiveBayes.default(x = X, y = Y, laplace = laplace)

#A-priori probabilities:
#Y
#       2        3 
#0.485563 0.514437 

#Conditional probabilities:
#   Airline.Status
#Y         Blue       Gold   Platinum     Silver
#  2 0.81628756 0.05895719 0.02475283 0.10000242
#  3 0.55883091 0.10604851 0.03979101 0.29532958

#   Age
#Y       [,1]     [,2]
#  2 49.78390 20.30972
#  3 42.73826 12.87123

#   Gender
#Y      Female      Male
#  2 0.6276197 0.3723803
#  3 0.5064911 0.4935089

#   Price.Sensitivity
#Y       [,1]      [,2]
#  2 1.326138 0.5700696
#  3 1.230259 0.5180028

#   Year.of.First.Flight
#Y       [,1]     [,2]
#  2 2007.199 2.996252
#  3 2007.207 2.959490

#   No.of.Flights.p.a.
#Y       [,1]     [,2]
#  2 23.41321 15.27507
#  3 16.77218 12.57571

#   Type.of.Travel
#Y   Business travel Mileage tickets Personal Travel
#  2      0.36524934      0.06715173      0.56759893
#  3      0.85489060      0.08528600      0.05982340

#   Shopping.Amount.at.Airport
#Y       [,1]     [,2]
#  2 25.68725 52.67634
#  3 27.49250 53.46744

#   Class
#Y     Business        Eco   Eco Plus
#  2 0.06884382 0.82063864 0.11051754
#  3 0.09480025 0.80681740 0.09838235

#   Scheduled.Departure.Hour
#Y       [,1]     [,2]
#  2 12.92260 4.634437
#  3 13.02437 4.580868

#   Departure.Delay.in.Minutes
#Y       [,1]     [,2]
#  2 17.73618 42.04413
#  3 12.31951 34.72743

#   Arrival.Delay.greater.5.Mins
#Y          no       yes
#  2 0.5776064 0.4223936
#  3 0.7161697 0.2838303


#Evaluation of the model is done as follows:

predP <- predict(nb, newdata = clean_data1, type = "class")
matrixSat <- table(predP,clean_data1$happyCust)
sumMatrix <- matrixSat[1,1] + matrixSat[2,2]

percSat <- sumMatrix/sum(matrixSat)
percSat <- percSat*100 
percSat


#[1] 76.9396

#Using the predict command, the happy customers were matched and a confusion matrix was created
#called matrixSat.

#Then the percentage calculation was done to find how accurate the model was.
#The division shows 0.7693 and was multiplied with hundred to show the percentage.


#Model Generation 2 --- Association Rules Mining 

clean_data <- train

data<-clean_data
data$Flight.date=as.Date(data$Flight.date, format="%m/%d/%Y")
data$week_days=weekdays(data$Flight.date)
data$month<- strftime(data$Flight.date, "%m")

numbertoCategory<-function(vec){
  #getting the 40th and 60th percentile of the data
  q <- quantile(vec, c(0.4, 0.6),na.rm=TRUE)
  #creating a vector with 'Average' value of length of vec
  vBuckets <- replicate(length(vec), "Average")
  #values lower than 40th percentile are replaced by 'Low'
  vBuckets[vec < q[1]] <- "Low"
  #values greater than 60th percentile are replaced by 'high'
  vBuckets[vec > q[2]] <- "High"
  return(as.factor(vBuckets))
  
}
data$Age=numbertoCategory(data$Age)
data$No.of.Flights.p.a.=numbertoCategory(data$No.of.Flights.p.a.)
data$X..of.Flight.with.other.Airlines=numbertoCategory(data$X..of.Flight.with.other.Airlines)
data$Shopping.Amount.at.Airport=numbertoCategory(data$Shopping.Amount.at.Airport)
data$Departure.Delay.in.Minutes=numbertoCategory(data$Departure.Delay.in.Minutes)
data$Arrival.Delay.in.Minutes=numbertoCategory(data$Arrival.Delay.in.Minutes)
data$Flight.time.in.minutes=numbertoCategory(data$Flight.time.in.minutes)
data$Flight.Distance=numbertoCategory(data$Flight.Distance)
data[data$Flight.cancelled=='Yes',]$Arrival.Delay.in.Minutes='NotAvailable'
data[data$Flight.cancelled=='Yes',]$Departure.Delay.in.minutes='NotAvailable'
data[data$Flight.cancelled=='Yes',]$Flight.time.in.Minutes='NotAvailable'
data$Eating.and.Drinking.at.Airport=numbertoCategory(data$Eating.and.Drinking.at.Airport)


data$Satisfaction[data$Satisfaction>=4]="Satisfied"
data$Satisfaction[data$Satisfaction<4]="Not Satisfied"
data$Satisfaction=as.factor(data$Satisfaction)
data$Price.Sensitivity=as.factor(data$Price.Sensitivity)
data$No..of.other.Loyalty.Cards=as.factor(data$No..of.other.Loyalty.Cards)
data$Day.of.Month=as.factor(data$Day.of.Month)
data$Scheduled.Departure.Hour=as.factor(data$Scheduled.Departure.Hour)
data$week_days=as.factor(data$week_days)
data$month=as.factor(data$month)
data=data[,-15]

facna=addNA(data$Arrival.Delay.in.Minutes)
levels(facna) <- c(levels(data$Arrival.Delay.in.Minutes), 'Not Available')
data$Arrival.Delay.in.Minutes<-facna
facna=addNA(data$Departure.Delay.in.Minutes)
levels(facna) <- c(levels(data$Departure.Delay.in.Minutes), 'Not Available')
data$Departure.Delay.in.Minutes<-facna
facna=addNA(data$Flight.time.in.minutes)
levels(facna) <- c(levels(data$Flight.time.in.minutes), 'Not Available')
data$Arrival.Delay.in.Minutes<-facna

install.packages("arules")
install.packages("arulesViz")
install.packages("methods")
library(arules)
library(arulesViz)
library(methods)
data$southeast=as.factor(data$southeast)
data$Year.of.First.Flight=as.factor(data$Year.of.First.Flight)
data<-data.frame(sapply(data,as.factor))
str(data)
dataX <- as(data,"transactions")

ruleset <- apriori(dataX, parameter=list(support = 0.3,confidence = 0.3))

#subsetting to get only those rules that result into high overallCustSat
sub<-subset(ruleset, subset = rhs %in% "Satisfaction=Satisfied")
#inspect to see those rules
inspect(sub)


#Model Generation 3 - Linear Model 

clean_data <- train

happyCust <- 1
med <- median(clean_data$Satisfaction)
happyCust[clean_data$Satisfaction>=med] <- 3
happyCust[clean_data$Satisfaction<med] <- 2

happyCust <- as.factor(happyCust)
clean_data1 <- cbind(clean_data,happyCust)
clean_data1 <- clean_data1[,-c(1)]




clean_data1$Departure.Delay.in.Minutes[is.na(clean_data1$Departure.Delay.in.Minutes)] <- mean(clean_data1$Departure.Delay.in.Minutes,na.rm=T)
clean_data1$Arrival.Delay.in.Minutes[is.na(clean_data1$Arrival.Delay.in.Minutes)] <- mean(clean_data1$Arrival.Delay.in.Minutes,na.rm=T)
clean_data1$Flight.time.in.minutes[is.na(clean_data1$Flight.time.in.minutes)] <- mean(clean_data1$Flight.time.in.minutes,na.rm=T)


# origin city, destination, airline code, flights with other airlines,arrival and departure delay and day of month don't affect happiness significantly(high p values)
model3 <- glm(happyCust~Airline.Status+Age+Gender+Price.Sensitivity+No.of.Flights.p.a.+ Type.of.Travel+Shopping.Amount.at.Airport+Class+Arrival.Delay.greater.5.Mins+Eating.and.Drinking.at.Airport+southeast,family="binomial",data=clean_data1)
summary(model3)
#model3 <- glm(happyCust~.,family="binomial",data=clean_data1)
#summary(model3)

# Model Generation 4 --- Logistic Regression

clean_data <- train
model4 <- lm(Satisfaction ~ Airline.Status + Age + Gender +
               Price.Sensitivity + Year.of.First.Flight + No.of.Flights.p.a. +
               Type.of.Travel +
               Shopping.Amount.at.Airport +
               Class +
               Scheduled.Departure.Hour +
               Departure.Delay.in.Minutes +
               Arrival.Delay.greater.5.Mins, data = clean_data )

summary(model4)

#Evaluation of Logistic Regression

logtraindata <- clean_data1
logtestdata <- test
happyCust <- 1
med <- median(logtestdata$Satisfaction)
happyCust[logtestdata$Satisfaction>=med] <- 3
happyCust[logtestdata$Satisfaction<med] <- 2

happyCust <- as.factor(happyCust)

logtestdata$Departure.Delay.in.Minutes[is.na(logtestdata$Departure.Delay.in.Minutes)] <- mean(logtestdata$Departure.Delay.in.Minutes,na.rm=T)
logtestdata$Arrival.Delay.in.Minutes[is.na(logtestdata$Arrival.Delay.in.Minutes)] <- mean(logtestdata$Arrival.Delay.in.Minutes,na.rm=T)
logtestdata$Flight.time.in.minutes[is.na(logtestdata$Flight.time.in.minutes)] <- mean(logtestdata$Flight.time.in.minutes,na.rm=T)

logpred <- predict(model3,logtestdata,type="response")
View(logtestdata)
pos_or_neg <- ifelse(logpred > 0.5, 3, 2)

happyCusttest <- factor(pos_or_neg)
x <- table(happyCust,happyCusttest)
error <- (x[1,1]+x[2,2])/sum(x)
error
# 77% accuracy


