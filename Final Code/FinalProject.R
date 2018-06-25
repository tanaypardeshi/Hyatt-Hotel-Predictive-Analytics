#IST 687- Final Project 
#Group 1
#Alain Shema,Gayatri Kulkarni, Sameesh Bedi, Karan Sahai, Akshata Naronakar

# code inspired from this R-blog entry
# https://www.r-bloggers.com/merge-all-files-in-a-directory-using-r-into-a-single-dataframe/
# Data to be loaded contains suffix H columns
#ETL process
getCleanData <- function(dirPath){
  curPath <- getwd() # get the current directory so that we can return to it later
  setwd(dirPath) # change the current directory to where the location of the files
  file_list <- list.files() # get the list of all the files in the current directory
  for (file in file_list) {
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset <- read.table(file, header=TRUE, sep = ",", stringsAsFactors = FALSE,
                            na.strings = "", fill = TRUE)
      dataset <- na.omit(dataset)
    }
    
    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset <-read.table(file, header=TRUE, sep = ",", stringsAsFactors = FALSE,
                                na.strings = "", fill = TRUE)
      temp_dataset <- na.omit(temp_dataset)
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  rownames(dataset) <- NULL
  setwd(curPath) # return the previous directory
  return(dataset)
}
# Read the data into surveyData variable
surveyData <- getCleanData("/home/alain/Desktop/applied data science/")

# Data cleaning
surveyData <- subset(surveyData,Gender_H %in% c("Male","Female")) # only keep male and female gender
surveyData <- subset(surveyData,Clublounge_Used_H %in% c("Yes","No"))
# Creating a copy of survey data
surveyData1 <- surveyData

#Clean the ClubLoungeUsed_H values
# Check that data is clean (has expected values) by going through all the columns
unique(surveyData$POV_H) # "Leisure", "Business", "Combination of both business and leisure" and "Prefer not to answer"                    
unique(surveyData$Clublounge_Used_H) #"Yes", "No" and "I don't know"
unique(surveyData$Gender_H) # "Female" and "Male"  
unique(surveyData$Age_Range_H) # "36-45", "56-65", "26-35", "46-55", "66-75", "18-25" and "76+"  
unique(surveyData$Num_Adults_H) # "2", "1", "6", "3", "4" and "5"
unique(surveyData$Num_Kids_H) # "1", "2", "3" and "4"
unique(surveyData$Spa_Used_H) # "No" and "Yes"
unique(surveyData$Likelihood_Recommend_H) # "9", "4", "10", "8", "7", "5", "6", "3", "1" and "2" 
unique(surveyData$Overall_Sat_H) # "9", "1", "8", "10", "7", "5", "6", "4", "3" and "2" 
unique(surveyData$Tranquility_H) # 9, 8, 10, 6, 7, 4, 5, 1, 2 and 3
unique(surveyData$Condition_Hotel_H) # 7, 9, 8, 10, 5, 6, 1, 3, 4 and 2
unique(surveyData$Customer_SVC_H) # "9", "2", "10", "8", "7", "6", "3", "4", "5" and "1" 
unique(surveyData$Staff_Cared_H) # "9", "1", "2", "10", "8", "6", "7", "5", "4" and "3" 
unique(surveyData$Internet_Sat_H) # 6, 3, 2, 4, 9, 10, 8, 5, 1 and 7
unique(surveyData$Check_In_H) # 9, 8, 10, 5, 1, 2, 7, 4, 6 and 3

# Let's give each column the appropriate data type
surveyData$Survey_ID_H <- as.numeric(surveData$Survey_ID_H)
surveyData$Length_Stay_H <- as.numeric(surveyData$Length_Stay_H)
surveyData$Gender_H <- as.factor(surveyData$Gender_H)
surveyData$Age_Range_H <- as.factor(surveyData$Age_Range_H)
surveyData$POV_H <- as.factor(surveyData$POV_H)
surveyData$Num_Adults_H <- as.numeric(surveyData$Num_Adults_H) 
surveyData$Num_Kids_H <- as.numeric(surveyData$Num_Kids_H)
surveyData$Net_Rev_H <- as.numeric(surveyData$Net_Rev_H)
surveyData$Clublounge_Used_H <- as.factor(surveyData$Clublounge_Used_H)
surveyData$Spa_Used_H <- as.factor(surveyData$Spa_Used_H)
surveyData$Likelihood_Recommend_H <- as.numeric(surveyData$Likelihood_Recommend_H)
surveyData$Overall_Sat_H <- as.numeric(surveyData$Overall_Sat_H)
surveyData$Guest_Room_H <- as.numeric(surveyData$Guest_Room_H)
surveyData$Tranquility_H <- as.numeric(surveyData$Tranquility_H)
surveyData$Condition_Hotel_H <- as.numeric(surveyData$Condition_Hotel_H)
surveyData$Customer_SVC_H <- as.numeric(surveyData$Customer_SVC_H)
surveyData$Staff_Cared_H <- as.numeric(surveyData$Staff_Cared_H)
surveyData$Internet_Sat_H <- as.numeric(surveyData$Internet_Sat_H)
surveyData$Check_In_H <- as.numeric(surveyData$Check_In_H)

# Let's add a new column that indicates whether or not a guest will recommend
# the hotel based on their likelihood to recommend (and using NPS definition)
surveyData$willRecommend <- as.factor(as.numeric(surveyData$Likelihood_Recommend_H > 7))

# Let's rename the columns and give them shorter names
# This will help with SVM modeling
colnames(surveyData) <- c("surveyID", "lenStay", "guestCountry", "gender", "ageRange", "POV", 
                          "nAdults", "nKids", "netRevenue", "loungeUsed", "spaUsed",
                          "likelyRecommend", "overallSat", "gRoom", "tranquility",
                          "hCondition", "cService", "staffCare", "internet", "checkin",
                          "willRecommend")

# We define our guest profiles based on the purpose of visit.
# Let's look at how much each guest profile  brings in.
tapply(surveyData$netRevenue, surveyData$POV, mean)
# Business guests: USD 1,287.834
# Leisure guests: USD 1,303.354
# Guests who visit for both reasons: USD 1,198.863
# Guests who did not prefer to answer: USD 1,559.983
tapply(surveyData$netRevenue, surveyData$POV, sum)
# Business guests: USD 112,041.54
# Leisure guests: USD 3,227,103.66
# Guests who visit for both reasons: USD 335,681.53
# Guests who did not prefer to answer: USD 14,039.85 

# Let's visualize these values with pie charts
# Code inspired by this page
# http://www.statmethods.net/graphs/pie.html
# Pie Chart with Average Revenue Per Profile
slices <- c(1287.834, 1303.354, 1198.863, 1559.983) 
lbls <- c("Business", "Leisure", "Business and Leisure",
          "Unspecified")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie1 <- pie(slices,labels = lbls, col=rainbow(length(lbls)),
            main="Average Revenue Per Guest Profile")

# Pie Chart with Total Revenue Per Profile
slices <- c(112041.54, 3227103.66, 335681.53 ) 
lbls <- c("Business", "Leisure", "Business and Leisure")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie2<-pie(slices,labels = lbls, col=rainbow(length(lbls)),
          main="Total Revenue Per Guest Profile")


# This function splits the input data frame, dataset, into 2 randomly
# assigned data frames with 2/3 and 1/3 of the rows of dataset, respectively.
# It returns a list containing these data frames.
# This function can be used to divide a data frame into a training and testing
# data frames.
splitData <- function(dataset){
  randIndex <- sample(1:nrow(dataset))
  cutPoint <- floor(2 * nrow(dataset) / 3)
  trainingData <- dataset[randIndex[1:cutPoint],]
  testingData <- dataset[randIndex[(cutPoint + 1): nrow(dataset)],]
  return(list(trainingData, testingData))
}

# Let's use the above function to get the training and testing datasets
# Let's remove surveyID and guestCountry since we don't need them for our predictions
tmp <- splitData(surveyData[,c(-1,-3)])
trainingData <- as.data.frame(tmp[1])
testingData <- as.data.frame(tmp[2])
rm(tmp) # delete the temporary variable

# This function compares the accuracy of a KSVM, an SVM and a Naive Bayes
# model and returns the name  / type of the best model and its accuracy
compareModels <- function(training, testing, target){
  ############ KSVM Model ##########
  ksvmModel <- ksvm(training[,target] ~ ., data = training, kernel = "rbfdot",
                    kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
  ksvmPred <- predict(ksvmModel, testing, type = "response")
  ksvmErr <- sum(testing[,target] != ksvmPred) * 100 /
    length(ksvmPred)
  
  ############ SVM Model ###########
  svmModel <- svm(training[,target] ~ ., data = training, kernel = "linear",
                  kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
  svmPred <- predict(svmModel, testing, type = "response")
  svmErr <- sum(testing[,target] != svmPred) * 100 /
    length(svmPred)
  
  ############ Naive Bayes Model ###########
  nbModel <- naiveBayes(training[,target] ~ ., data = training, kernel = "linear",
                        kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
  nbPred <- predict(nbModel, testing, type = "class")
  nbErr <- sum(testing[,target] != nbPred) * 100 /
    length(nbPred)
  
  ########### Comparing the 3 models #########
  if((ksvmErr <= svmErr) & (ksvmErr <= nbErr)){
    return(paste("KSVM Model", toString(ksvmErr), sep = " "))
  }
  else if((svmErr <= ksvmErr) & (svmErr <= nbErr)){
    return(paste("SVM Model", toString(svmErr), sep = " "))
  }
  else{
    return(paste("Naive Bayes Model", toString(nbErr), sep = " "))
  }
}

# Let's use it to find the best model for our dataset
compareModels(trainingData, testingData, "willRecommend") # SVM Model with 100% accuracy
# Thus, we can use the SVM Model to understand which variables are important
# in predicting the "willRecommend" variable
predictors <- colnames(trainingData)[-19]
# Let's get the weights of the different predictors
t(svmModel$coefs) %*% svmModel$SV

####################################################################
############ LM Model ###########
lmModel <- lm(likelyRecommend ~ ., data = trainingData)
lmPred <- predict(lmModel, testingData, type = "response")
lmErr <- mean((testingData$likelyRecommend - lmPred) ^ 2)
# lmErr <- sum(testingData$willRecommend != svmPred) * 100 /
#  length(svmPred) # Ans. 36.5%

# create a map of countries colored using the likelihood to recommend
# install the rworldmap and ggplot packages
install.packages("rworldmap")
install.packages("ggplot2")

# load the 2 packages
library("rworldmap")
library("ggplot2")
worldMap <- map_data(map = "world")

# first compute the average for each country
countryRating <- as.data.frame(tapply(surveyData$likelyRecommend,
                                      surveyData$guestCountry, mean))
colnames(countryRating) <- c("rating")
countryRating$country <- row.names(countryRating)
rownames(countryRating) <- NULL
# change Iran (republic of) to Iran
# and United Kingdom to UK
countryRating$country[countryRating$country == "Iran (islamic Republic Of)"] <- "Iran"
countryRating$country[countryRating$country == "United Kingdom"] <- "UK"
worldMap$rating <- sapply(1:nrow(worldMap), function(i) 
  countryRating$rating[countryRating$country == worldMap$region[i]])
worldMap$rating <- as.numeric(worldMap$rating)

ggplot() + geom_map(data = worldMap, map = worldMap,
                    aes(map_id = region, x = long, y = lat, fill = rating)) +
  scale_fill_gradient(low = "yellow", high = "red", guide = "colorbar") +
  coord_equal() + xlab("") + ylab("")

# function to select the best linear regression model, i.e.,
# a model that has the highest R square value
selectBestLRModel < function(listOfIV){
  
}
#Sameeshs Code

str(surveyData)

age_range = sqldf('select Guest_Country_H from surveyData where Guest_Country_H = 1')
surveydatanew = surveyData[surveyData$Age_Range_H!='USA' & surveyData$Age_Range_H!='Male'& surveyData$Age_Range_H!='Canada'& surveyData$Age_Range_H!='Female', ]

str(surveydatanew)
table(surveydatanew$Age_Range_H)
surveydatanew = surveydatanew[surveydatanew$Survey_ID_H!='Guest Book BV' & surveydatanew$Survey_ID_H!='demographics'& surveydatanew$Survey_ID_H!='TripAdvisor Form'& surveydatanew$Survey_ID_H!='Page 27:', ]
table(surveydatanew$Survey_ID_H)
surveydatanew = surveydatanew[surveydatanew$Length_Stay_H!='2014-04-07' & surveydatanew$Length_Stay_H!='2014-04-18'& surveydatanew$Length_Stay_H!='2014-04-25'& surveydatanew$Length_Stay_H!='2014-07-18' & surveydatanew$Length_Stay_H!='2014-08-06'& surveydatanew$Length_Stay_H!='2014-11-16'& surveydatanew$Length_Stay_H!='2014-12-07', ]
table(surveydatanew$Length_Stay_H)
surveydatanew = surveydatanew[surveydatanew$Guest_Country_H!=1 & surveydatanew$Guest_Country_H!=2, ]
table(surveydatanew$Guest_Country_H)
surveydatanew = surveydatanew[surveydatanew$Gender_H!='IL' & surveydatanew$Gender_H!='SK'& surveydatanew$Gender_H!='USA', ]
table(surveydatanew$Gender_H)
surveydatanew = surveydatanew[surveydatanew$POV_H!='Female' & surveydatanew$POV_H!='56-65'& surveydatanew$POV_H!='46-55'& surveydatanew$POV_H!='36-45'& surveydatanew$POV_H!='Male', ]
table(surveydatanew$POV_H)
surveydatanew = surveydatanew[surveydatanew$Num_Adults_H!='2014-01-20' & surveydatanew$Num_Adults_H!='2014-04-03'& surveydatanew$Num_Adults_H!='OMGDS', ]
table(surveydatanew$Num_Adults_H)
surveydatanew = surveydatanew[surveydatanew$Num_Kids_H!='OMGDS', ]
table(surveydatanew$Num_Kids_H)
surveydatanew = surveydatanew[surveydatanew$Net_Rev_H!='RACKC1' & surveydatanew$Net_Rev_H!='RACKQ1', ]
table(surveydatanew$Net_Rev_H)
surveydatanew = surveydatanew[surveydatanew$Likelihood_Recommend_H!='No', ]
table(surveydatanew$Likelihood_Recommend_H)
surveydatanew = surveydatanew[surveydatanew$Overall_Sat_H!='No', ]
table(surveydatanew$Overall_Sat_H)
surveydatanew = surveydatanew[surveydatanew$Spa_Used_H!='USD', ]
table(surveydatanew$Spa_Used_H)


str(surveydatanew)

#Graphs and Charts
surveydatanew<-surveyData1
#plot1
#plot to get demographic information of the survey data
install.packages("ggplot")
library(ggplot2)
library(gridExtra)
gg1<- ggplot(data=surveydatanew, aes(x=Gender_H)) +geom_bar(stat="count",aes(fill=Gender_H),width=0.6) + labs(title = "Gender Distribution",y="Count",x="Gender") +  guides(fill=FALSE)
gg2<-ggplot(data=surveydatanew, aes(x=Age_Range_H)) +geom_bar(stat="count",aes(fill=Age_Range_H)) + labs(title = "Age Range Distribution ",y="Count",x="Age Range") + guides(fill=FALSE)
gg3<-ggplot(data=surveydatanew, aes(x=POV_H)) +geom_bar(stat="count",aes(fill=POV_H),width=0.6)+ labs(title = "Purpose of Visit Distribution ",y="Count",x="Purpose of Visit") + guides(fill=FALSE)
gg3<-gg3 + scale_x_discrete( labels=c("Business","Combination","Leisure","Unspecified"))
gg4<-ggplot(data=surveydatanew, aes(x=as.numeric(Length_Stay_H))) +geom_bar(stat="count",aes(fill=Length_Stay_H),width=0.6)+ labs(title = "Length of Stay Distribution",y="Count",x="Length of Stay") + guides(fill=FALSE)+ xlim(0,10)
grid.arrange(gg1,gg2,gg3,gg4,ncol=2)

#plot2
#Length of Stay vs Likelihood To Recommend
projscattr = ggplot(data = surveydatanew,aes(x=as.numeric(surveydatanew$Likelihood_Recommend_H), y=as.numeric(surveydatanew$Length_Stay_H))) + geom_point(data=surveydatanew, aes(size=Tranquility_H,color= Condition_Hotel_H)) + labs(title = "Factors Affecting Likelihood to Recommend",y="Length of Stay",x="Likelihood To Recommend")
projscattr


#plot3
#Facilities used vs Likelihood To Recommend
surveydatanew6 <- surveydatanew
surveydatanew6$Likelihood_Recommend_H <-as.numeric(surveydatanew6$Likelihood_Recommend_H)
surveydatanew7 <- subset(surveydatanew6,Likelihood_Recommend_H < 7)
p1 <-ggplot(data=surveydatanew7, aes(x=factor(Likelihood_Recommend_H))) +geom_bar(stat="count",aes(fill=as.factor(Likelihood_Recommend_H)),width=0.6) + labs(title = "Likelihood To Recommend if Spa Used",y="Count",x="Likelihood_Recommend_H")+
  facet_wrap( ~ Spa_Used_H, ncol = 2) + guides(fill=FALSE)
p1

surveydatanew6 <- subset(surveydatanew6,Clublounge_Used_H %in% c("Yes","No"))
p2 <-ggplot(data=surveydatanew7, aes(x=factor(Likelihood_Recommend_H))) +geom_bar(stat="count",aes(fill=as.factor(Likelihood_Recommend_H)),width=0.6) + labs(title = "Likelihood To Recommend if ClubLounge Used",y="Count",x="Likelihood_Recommend_H")+
  facet_wrap( ~ Clublounge_Used_H, ncol = 2) + guides(fill=FALSE)
p2
grid.arrange(p1,p2)

#plot4
#http://docs.ggplot2.org/current/geom_boxplot.html
p <- ggplot(surveydatanew6, aes(Guest_Country_H, Likelihood_Recommend_H))
p + geom_boxplot(outlier.colour = "red", outlier.shape = 1,fill = "white", colour = "green") +theme(axis.text.x = element_text(angle=90, hjust=1,vjust=0.3))

#PLOT6
library(stringr)
surveydatanew$POV_H <- str_replace(surveydatanew$POV_H,"Combination of both business and leisure","Combination")
gg9<-ggplot(data=subset(surveydatanew,!(POV_H %in% "Prefer not to answer")), aes(x=as.numeric(Length_Stay_H))) +geom_bar(stat="count",aes(fill=as.factor(as.numeric(Num_Kids_H)+as.numeric(Num_Adults_H))),position="stack") + labs(title = "Demographic Distribution ",x="Length of Stay")
gg9<-gg9+ facet_grid(~ POV_H) + xlim(0,10) + scale_fill_discrete(name="Number of People") 
gg9

#Calculation7
Percentage_Adults <- round(prop.table(table(as.numeric(surveydatanew$Num_Adults_H)))*100,digits=2)
Percentage_Adults
Percentage_Kids <- round(prop.table(table(as.numeric(surveydatanew$Num_Kids_H)))*100,digits=2)
Percentage_Kids

#Plot7
#Proof that Tranquility,Condition of Hotel are directly linked to Likelihood To Recommend
surveydatanew7$Net_Rev_H <-as.numeric(surveydatanew7$Net_Rev_H)
t1<- ggplot(data=surveydatanew6, aes(x=as.numeric(Tranquility_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se")  + 
  labs(y="LRH",x="Tranquility")
t1
t2<- ggplot(data=surveydatanew6, aes(x=as.numeric(Condition_Hotel_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se")  + 
  labs(y="LRH",x="Condition of Hotel")
t2
t3<- ggplot(data=surveydatanew6, aes(x=as.numeric(Internet_Sat_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se") + 
  labs(y="LRH",x="Internet Satisfaction")
t3
t4<- ggplot(data=surveydatanew6, aes(x=as.numeric(Check_In_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se")  + 
  labs(y="LRH",x="Check in Procedure")
t4
t5<- ggplot(data=surveydatanew6, aes(x=as.numeric(Customer_SVC_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se") +
  labs(y="LRH",x="Customer Service")
t5
t6<- ggplot(data=surveydatanew6, aes(x=as.numeric(Staff_Cared_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se") + 
  labs(y="LRH",x="Staff_Cared_H")
t6
grid.arrange(t1,t2,t3,t4,t5,t6)

#NPS - Promoter, Detractors, Passives
surveydatanewD<- subset(surveydatanew6,Likelihood_Recommend_H < 7)
surveydatanewP<- subset(surveydatanew6,Likelihood_Recommend_H > 8)
sumD<- (sum(table(surveydatanewD$Likelihood_Recommend_H)))
sumP <- (sum(table(surveydatanewP$Likelihood_Recommend_H)))
sumTotal <-nrow(surveydatanew7)
(sumP/sumTotal)*100 - (sumD/sumTotal)*100
percent(sumP/sumTotal - sumD/sumTotal)
lbls1 =  c("Detractors", "Passive", "Promoters")
slices1 <- c(sumD, sumTotal-(sumD+sumP),sumP) 
pct1 <- round(slices1/sum(slices1)*100,digits=2)
lbls1 <- paste(lbls1, pct1) # add percents to labels 
lbls1 <- paste(lbls1,"%",sep="") # ad % to labels 
pie(slices1,labels=lbls1,col= rainbow(length(lbls1)))

#Gayatris code
#Performing Linear Model to further our analysis
#Loading ggmap and ggplot2
library(ggmap)
library(ggplot2)

#Performing Linear Modelling to determine factors which influence the Likelihood_Recommend_H
#Taking various cases as below
#case1
m1 <- lm(formula = Likelihood_Recommend_H ~ surveydatanew$Staff_Cared_H, data = surveydatanew)
summary(m1) #r-square 0.44

#case2
m2 <- lm(formula = Likelihood_Recommend_H ~ surveydatanew$Customer_SVC_H, data = surveydatanew)
summary(m2) #r-square 0.51

#case3
m17 <- lm(formula = Likelihood_Recommend_H ~ surveydatanew$Condition_Hotel_H, data = surveydatanew)
summary(m3) #r-square 0.452

#case4
m18 <- lm(formula = Likelihood_Recommend_H ~ surveydatanew$Tranquility_H, data = surveydatanew)
summary(m4) #r-square 0.32

#case5
m19 <- lm(formula = Likelihood_Recommend_H ~ surveydatanew$Guest_Room_H, data = surveydatanew)
summary(m5) #r-square 0.4579

#case6
m20 <- lm(formula = Likelihood_Recommend_H ~ surveydatanew$Overall_Sat_H, data = surveydatanew)
summary(m6) #r-square 0.80

#Combining all the above cases to predict Likelihood_Recommend_H
#Likelihood_Recommend_H is predicted using the factors i.e Condition_Hotel_H,Staff_Cared_H,Customer_SVC_H,Guest_Room_H and Overall_Sat_H
#Storing in variable LRH_Linear
LRH_Linear <- lm(formula = Likelihood_Recommend_H ~ Condition_Hotel_H+Staff_Cared_H+Customer_SVC_H+Guest_Room_H+Overall_Sat_H, data= surveydatanew)

summary(LRH_Linear) # This gives a R-square value as 0.8215 which is close to 1
#Performing computations on the columns which has values 0-10 i.e. the one's which are rated by the customer as a feedback

mycol<- names(surveydatanew) %in% c(
  "Likelihood_Recommend_H",
  "Overall_Sat_H",
  "Guest_Room_H",
  "Tranquility_H",
  "Condition_Hotel_H",
  "Customer_SVC_H",
  "Staff_Cared_H",
  "Internet_Sat_H",
  "Check_In_H"
)

#Creating a subset survey_noFactor with the above columns only
survey_noFactor <- surveydatanew[mycol]

#Performing Data Association rules using apriori on the above created subset
library(arules)
library(arulesViz)
str(survey_noFactor)

#Converting Likelihood_Recommend_H as numeric since it was a character
survey_noFactor$Likelihood_Recommend_H <- as.numeric(survey_noFactor$Likelihood_Recommend_H)
#Performing analysis on the data which has Likelihood_Recommend_H less than 6
#This would help us understand what factors needs to be corrected inorder to increase its likelihood to recommend
#Creating a subset from the created subset with vaues for Likelihood_Recommend_H less than 6 and storing it in survey_lrh6
survey_lrh6<- survey_noFactor[which(survey_noFactor$Likelihood_Recommend_H < 6,arr.ind = TRUE),]
head(survey_lrh6)
#Since the columns selected were either characters or integers, it was necessary to convert them as factors in order to perform apriori
survey_lrh6$Likelihood_Recommend_H <- as.factor(survey_lrh6$Likelihood_Recommend_H)
survey_lrh6$Overall_Sat_H <- as.factor(survey_lrh6$Overall_Sat_H)
survey_lrh6$Guest_Room_H <- as.factor(survey_lrh6$Guest_Room_H)
survey_lrh6$Tranquility_H <- as.factor(survey_lrh6$Tranquility_H)
survey_lrh6$Condition_Hotel_H <- as.factor(survey_lrh6$Condition_Hotel_H)
survey_lrh6$Customer_SVC_H <- as.factor(survey_lrh6$Customer_SVC_H)
survey_lrh6$Staff_Cared_H <- as.factor(survey_lrh6$Staff_Cared_H)
survey_lrh6$Internet_Sat_H <- as.factor(survey_lrh6$Internet_Sat_H)
survey_lrh6$Check_In_H <- as.factor(survey_lrh6$Check_In_H)

#Performing Data association rules using apriori 
#Having the confidence as 0.8 and support as 0.05 gives the rules which are likely to predict one another
rule1 <- apriori(survey_lrh6,parameter=list(support=0.05, confidence=0.8))
#8 rules created

#Sorting it by confidence
rule1 <- sort(rule1, decreasing = TRUE, by= "confidence")
#inspecting the rules
inspect(rule1)
#Plotting the rule1
plot(rule1)