marchTexasData <- read.csv("filteredData1.csv")
juneTexasData <- read.csv("filteredData2.csv")
septemberTexasData <- read.csv("filteredData3.csv")
decemberTexasData <- read.csv("filteredData4.csv")


combinedTexasData <- rbind(marchTexasData, juneTexasData, septemberTexasData, decemberTexasData)
write.csv(combinedTexasData,file= 'combinedTexasData.csv')
combinedTexasData

#Lets look at highest hotel entries along with net promoter score for the hotels

#install.packages("sqldf")
library(sqldf)

avgHotelNetPromoterScore <- sqldf("select AVG(Likelihood_Recommend_H), Count(X), Brand_PL from combinedTexasData GROUP BY Brand_PL ")
avgHotelNetPromoterScore


#Now same calculation of AVG net promoter score for march month only
avgHotelNetPromoterScore3 <- sqldf("select AVG(Likelihood_Recommend_H), Count(X), Brand_PL from  marchTexasData GROUP BY Brand_PL ")
avgHotelNetPromoterScore3

#Now same calculation of AVG net promoter score for june month only
avgHotelNetPromoterScore4 <- sqldf("select AVG(Likelihood_Recommend_H), Count(X), Brand_PL from  juneTexasData GROUP BY Brand_PL ")
avgHotelNetPromoterScore4

#Now same calculation of AVG net promoter score for sep month only
avgHotelNetPromoterScore5 <- sqldf("select AVG(Likelihood_Recommend_H), Count(X), Brand_PL from  septemberTexasData GROUP BY Brand_PL ")
avgHotelNetPromoterScore5

#Now same calculation of AVG net promoter score for december month only
avgHotelNetPromoterScore2 <- sqldf("select AVG(Likelihood_Recommend_H), Count(X), Brand_PL from decemberTexasData GROUP BY Brand_PL ")
avgHotelNetPromoterScore2


#Comparison plot by tanay

#From the comparison we see that, Hyatt House and Hyatt Regency have weird behaviour throughout the 4 months with Hyatt regency having
#low promoter scores throughout data and especially in december with more customers coming, we will just pick Hyatt regency to do further analysis


#HyattRegency
hyattRegencyDataSet <- sqldf("select * from combinedTexasData WHERE Brand_PL ='Hyatt Regency' ")
hyattRegencyDataSet

#replacing null values with NA
hyattRegencyDataSet[hyattRegencyDataSet==''] <- NA

hyattRegencyDataSet$Clublounge_Used_H <- as.character(hyattRegencyDataSet$Clublounge_Used_H, stringsAsFactors=FALSE)
hyattRegencyDataSet$Spa_Used_H <- as.character(hyattRegencyDataSet$Spa_Used_H, stringsAsFactors=FALSE)
hyattRegencyDataSet$MEMBER_STATUS_R <- as.character(hyattRegencyDataSet$MEMBER_STATUS_R, stringsAsFactors=FALSE)

hyattRegencyDataSet[["Clublounge_Used_H"]][is.na(hyattRegencyDataSet[["Clublounge_Used_H"]])] <-"i don't know"
hyattRegencyDataSet[["Spa_Used_H"]][is.na(hyattRegencyDataSet[["Spa_Used_H"]])] <-"i don't know"
hyattRegencyDataSet[["MEMBER_STATUS_R"]][is.na(hyattRegencyDataSet[["MEMBER_STATUS_R"]])] <-"None"

#Replacing na Values in F.b_overall_h, Check_in_h, internet service and few other with mean values
for(col in 1:ncol(hyattRegencyDataSet)){
  hyattRegencyDataSet[is.na(hyattRegencyDataSet[,col]), col] <- round(mean(na.omit(hyattRegencyDataSet[,col])))
}


#Checking summary of fields to figure out which fields are not changing
#
summary(hyattRegencyDataSet$Mini.Bar_PL)
#
summary(hyattRegencyDataSet$All.Suites_PL)
summary(hyattRegencyDataSet$Bell.Staff_PL)
#
summary(hyattRegencyDataSet$Boutique_PL)
summary(hyattRegencyDataSet$Business.Center_PL)
#
summary(hyattRegencyDataSet$Casino_PL)
#
summary(hyattRegencyDataSet$Conference_PL)
summary(hyattRegencyDataSet$Convention_PL)
summary(hyattRegencyDataSet$Dry.Cleaning_PL)
#
summary(hyattRegencyDataSet$Elevators_PL)
#
summary(hyattRegencyDataSet$Fitness.Center_PL)
#
summary(hyattRegencyDataSet$Fitness.Trainer_PL)
summary(hyattRegencyDataSet$Golf_PL)
#
summary(hyattRegencyDataSet$Indoor.Corridors_PL)
summary(hyattRegencyDataSet$Laundry_PL)
summary(hyattRegencyDataSet$Limo.Service_PL)
#
summary(hyattRegencyDataSet$Pool.Indoor_PL)
#
summary(hyattRegencyDataSet$Pool.Outdoor_PL)
summary(hyattRegencyDataSet$Regency.Grand.Club_PL)
summary(hyattRegencyDataSet$Resort_PL)
#
summary(hyattRegencyDataSet$Restaurant_PL)
summary(hyattRegencyDataSet$Self.Parking_PL)
summary(hyattRegencyDataSet$Shuttle.Service_PL)
#
summary(hyattRegencyDataSet$Ski_PL)
summary(hyattRegencyDataSet$Spa_PL)
summary(hyattRegencyDataSet$Valet.Parking_PL)
summary(hyattRegencyDataSet$Spa_Used_H)
summary(hyattRegencyDataSet$Clublounge_Used_H)

#Dropping columns by names since these fields are not contributing here 
hyattRegencyDataSet = subset(hyattRegencyDataSet, select = -c(Spa.services.in.fitness.center_PL,Spa.online.booking_PL,Spa.F.B.offering_PL) )
hyattRegencyDataSetFiltered = subset(hyattRegencyDataSet, select = -c(Mini.Bar_PL,All.Suites_PL,Bell.Staff_PL, Boutique_PL,Casino_PL,Conference_PL,Elevators_PL,Fitness.Center_PL,
                                                              Fitness.Trainer_PL,Indoor.Corridors_PL,Pool.Indoor_PL,Pool.Outdoor_PL,Restaurant_PL,Ski_PL,ROOM_NUM_C,Club.Type_PL,MAJOR_MARKET_CODE_C,ROOM_TYPE_CODE_C,
                                                              PMS_ROOM_REV_USD_C,RESERVATION_STATUS_R,MAJOR_MARKET_CODE_R,PAST_VS_FUTURE_R,State_PL,Country_PL,Guest.NPS.Goal_PL,Brand_PL,Hotel.Inventory_PL,Floors_PL,Union_PL,Relationship_PL) )

#Cleaning Further by just keeping genders as male or females
hyattRegencyDataSetFiltered <- subset(hyattRegencyDataSetFiltered,Gender_H %in% c("Male","Female")) # only keep male and female gender
hyattRegencyDataSetFiltered$F.B_Overall_Experience_H <- round(hyattRegencyDataSetFiltered$F.B_Overall_Experience_H )
#Omitting few records where adult nos are not mentioned
hyattRegencyDataSetFiltered <-  hyattRegencyDataSetFiltered[!is.na(hyattRegencyDataSetFiltered$ADULT_NUM_C),]


hyattRegencyDataSetFiltered<- na.omit(hyattRegencyDataSetFiltered)

#write.csv(hyattRegencyDataSetFiltered, file='hyattRegencyDataSetFiltered.csv')

#Checkpoint after removing Na's and replacing relevant NA's with mean values


#Checking data is clean by going through the unique values


#Giving data types to each field properly
hyattRegencyDataSetFiltered$LENGTH_OF_STAY_C <- as.numeric(hyattRegencyDataSetFiltered$LENGTH_OF_STAY_C)
hyattRegencyDataSetFiltered$Gender_H <- as.factor(hyattRegencyDataSetFiltered$Gender_H)
hyattRegencyDataSetFiltered$Age_Range_H <- as.factor(hyattRegencyDataSetFiltered$Age_Range_H)
hyattRegencyDataSetFiltered$POV_CODE_C <- as.factor(hyattRegencyDataSetFiltered$POV_CODE_C)
hyattRegencyDataSetFiltered$ADULT_NUM_C <- as.numeric(hyattRegencyDataSetFiltered$ADULT_NUM_C) 
hyattRegencyDataSetFiltered$REVENUE_USD_R <- as.numeric(hyattRegencyDataSetFiltered$REVENUE_USD_R)
hyattRegencyDataSetFiltered$Spa_PL <- as.factor(hyattRegencyDataSetFiltered$Spa_PL)
hyattRegencyDataSetFiltered$Likelihood_Recommend_H <- as.numeric(hyattRegencyDataSetFiltered$Likelihood_Recommend_H)
hyattRegencyDataSetFiltered$Guest_Room_H <- as.numeric(hyattRegencyDataSetFiltered$Guest_Room_H)
hyattRegencyDataSetFiltered$Tranquility_H <- as.numeric(hyattRegencyDataSetFiltered$Tranquility_H)
hyattRegencyDataSetFiltered$Condition_Hotel_H <- as.numeric(hyattRegencyDataSetFiltered$Condition_Hotel_H)
hyattRegencyDataSetFiltered$Customer_SVC_H <- as.numeric(hyattRegencyDataSetFiltered$Customer_SVC_H)
hyattRegencyDataSetFiltered$Staff_Cared_H <- as.numeric(hyattRegencyDataSetFiltered$Staff_Cared_H)
hyattRegencyDataSetFiltered$Internet_Sat_H <- as.numeric(hyattRegencyDataSetFiltered$Internet_Sat_H)
hyattRegencyDataSetFiltered$Check_In_H <- as.numeric(hyattRegencyDataSetFiltered$Check_In_H)
hyattRegencyDataSetFiltered$Clublounge_Used_H <- as.factor(hyattRegencyDataSetFiltered$Clublounge_Used_H)
hyattRegencyDataSetFiltered$Spa_Used_H <- as.factor(hyattRegencyDataSetFiltered$Spa_Used_H)
hyattRegencyDataSetFiltered$MEMBER_STATUS_R <- as.factor(hyattRegencyDataSetFiltered$MEMBER_STATUS_R)

#Adding a new field to check whether people recommend
hyattRegencyDataSetFiltered$willRecommend <- as.factor(as.numeric(hyattRegencyDataSetFiltered$Likelihood_Recommend_H > 7))


#Calculating Avg revenue each customer giving us from Business and Leisure class
avgRevenue<- aggregate(hyattRegencyDataSetFiltered$REVENUE_USD_R, by= list(hyattRegencyDataSetFiltered$POV_CODE_C), mean)
avgRevenue

#Calculating Overall revenue from different groups of customers
netRevenue<- tapply(hyattRegencyDataSetFiltered$REVENUE_USD_R, hyattRegencyDataSetFiltered$POV_CODE_C, sum)
netRevenue

#Calculating Overall Customers from Leisure and Business
overallCustomers <- tapply(hyattRegencyDataSetFiltered$REVENUE_USD_R, hyattRegencyDataSetFiltered$POV_CODE_C, length)
overallCustomers

#Calculating Avg Length of stay for Business and Leisure Class
avgLengthStay<- tapply(hyattRegencyDataSetFiltered$LENGTH_OF_STAY_C, hyattRegencyDataSetFiltered$POV_CODE_C, mean)
avgLengthStay


comparisonRowNames <- c("Leisure", "Business")
#For Avg Revenue
pieSlices <- c(avgRevenue[2,2],avgRevenue[1,2]) 
pct <- round(pieSlices/sum(pieSlices)*100)
label <- paste(comparisonRowNames, pct) # add percents to labels 
lbls <- paste(label,"%",sep="") # ad % to labels 
pieChart1 <- pie(pieSlices,labels = lbls, col=rainbow(length(lbls)),main="Average Revenue for different Type of Guests in $")

#For total Revenue
pieSlices2 <- c(netRevenue[2],netRevenue[1])
pct2 <- round(pieSlices2/sum(pieSlices2)*100)
label2 <- paste(comparisonRowNames, pct2) # add percents to labels 
lbls <- paste(label2,"%",sep="")
pieChart2 <- pie(pieSlices2,labels = lbls, col=rainbow(length(lbls)),main="Net Revenue for different Type of Guests in $")

#For overall Customers
pieSlices3 <- c(overallCustomers[2],overallCustomers[1])
pct3 <- round(pieSlices3/sum(pieSlices3)*100)
label3 <- paste(comparisonRowNames, pct3) # add percents to labels 
lbls <- paste(label3,"%",sep="")
pieChart3 <- pie(pieSlices3,labels = lbls, col=rainbow(length(lbls)),main="Frequency of Guests")

#Conclusion - Avg $ spent by Business visitor is less than the Leisure Vistor but netrevenue from
#Business Vistor is 5 times the revenue from Leisure with proven fact both are staying for almost 2 days on an average
#Also, no of Business Vistors are 5 times the Leisure Visitors
#Main source of income for hyatt regency in Texas is Business Class

#Splitting data into training and test datasets
#2/3 training dataset and 1/3 test dataset
datasetFormation <- function(df){
  randIndex <- sample(1:nrow(df))
  cutPoint <- round(2 * nrow(df) / 3)
  trainingData <- df[randIndex[1:cutPoint],]
  testData <- df[randIndex[(cutPoint + 1): nrow(df)],]
  return(list(trainingData, testData))
}


#Calling datasetFormation for traindataSet and testDataSet
#All the importatn attributes taken here except X, and NPS_Type and Likelihood_Recommend_H
tempDataFrame <- datasetFormation(hyattRegencyDataSetFiltered[,c(2:13,15:36,38)])
#all the age, sex, date of checkin, adults no, class,guest country, membership, gender, length of stay
#tempDataFrame2 <- datasetFormation(hyattRegencyDataSetFiltered[,c(2:8,10:11,38)])

trainingData <- as.data.frame(tempDataFrame[1])
testingData <- as.data.frame(tempDataFrame[2])
rm(tempDataFrame)

install.packages("kernlab")
library("kernlab")

install.packages("e1071")
library("e1071")

install.packages("gridExtra")
library("gridExtra")

aim <- "willRecommend"

# This function compares the accuracy of a KSVM, an SVM and a Naive Bayes
# model and returns the nameof the appropriate model and its accuracy

  ############ KSVM Model
  ksvmModel <- ksvm(willRecommend~ ., data = trainingData, kernel = "rbfdot",
                    kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
  ksvmModel
  
  ksvmPrediction <- predict(ksvmModel, testingData, type = "response")
  ksvmPrediction
  
  newDf1 = data.frame(v1=testingData$willRecommend, v2=ksvmPrediction)
  newDf1
  
  ksvmError <- sum(testingData[,aim] != ksvmPrediction) * 100 /length(ksvmPrediction)
  ksvmError
  
  #Error is only 9.757695%
  
  ############ SVM Model
  svmModel <- svm(willRecommend ~ ., data = trainingData, kernel = "linear",
                  kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
  svmModel
  svmPrediction <- predict(svmModel, testingData, type = "response")
  svmPrediction
  
  newDf2 = data.frame(v1=testingData$willRecommend, v2=svmPrediction)
  newDf2
  
  svmError <- sum(testingData[,aim] != svmPrediction) * 100 /length(svmPrediction)
  svmError
  
  # Error is 9.823183% by SVM 
  
  ############ Naive Bayes Model
  nbModel <- naiveBayes(trainingData[,aim] ~ ., data = trainingData, kernel = "linear",
                        kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
  nbModel
  nbPrediction <- predict(nbModel, testingData, type = "class")
  nbPrediction
  
  newDf3 = data.frame(v1=testingData$willRecommend, v2=nbPrediction)
  newDf3
  
  nbError <- sum(testingData[,aim] != nbPrediction) * 100 /length(nbPrediction)
  nbError
  
  #Error is 4.453176% by Naive Bayes
  
  
  #Best algorithm to pick will be Naive Bayes Model here since it has just 4.453176% error  while other have
  #more percentage of error
  
  # We will use Naive Bayes to understand importance of all attributes in prediction by removing "willRecommend" in prediction
  predictors <- colnames(trainingData)[-35]
  # Let's get the weights of the different predictors
  valueOfAttributes <- nbModel$tables
  valueOfAttributes <- t(valueOfAttributes)
  write.csv(valueOfAttributes, file='NaiveBayesValues.csv')
  
  #Observed weights to be very scattered for values
  
