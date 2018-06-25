#Install relevant packages for visualizations
install.packages("ggplot2")
library(ggplot2)

install.packages("sqldf")
library(sqldf)

install.packages("reshape2")
library(reshape2)

#Reading the csv data for the March, June, September and December.
marchTexasData <- read.csv("/Users/tanay/Desktop/Hyatt Project/New Data/filteredData1.csv")
juneTexasData <- read.csv("/Users/tanay/Desktop/Hyatt Project/New Data/filteredData2.csv")
septemberTexasData <- read.csv("/Users/tanay/Desktop/Hyatt Project/New Data/filteredData3.csv")
decemberTexasData <- read.csv("/Users/tanay/Desktop/Hyatt Project/New Data/filteredData4.csv")

View(marchTexasData)

#Combined data set for March, June, September and December.
combinedTexasData <- rbind(marchTexasData, juneTexasData, septemberTexasData, decemberTexasData)
write.csv(combinedTexasData,file= 'combinedTexasData.csv')
View(combinedTexasData)

#Generating a line chart for comparing number of guests based on gender in the months of March, June, September and December-->

#Grouping the entire data set based on Gender
combined_data_gender <- sqldf("SELECT AVG(Likelihood_Recommend_H), Count(X), GENDER_H from combinedTexasData GROUP BY GENDER_H ")
combined_data_gender
combined_data_gender <- combined_data_gender[-c(1,4),]
View(combined_data_gender)


#Grouping the march data set based on Gender
march_data_gender <- sqldf("select AVG(Likelihood_Recommend_H),Count(X), GENDER_H from  marchTexasData GROUP BY GENDER_H ")
march_data_gender <- data.frame(march_data_gender)
colnames(march_data_gender) <- c("AvgLikelihoodRecommend","CountMarch","Gender")
march_data_gender <- march_data_gender[-c(1,4),]
View(march_data_gender)

#Grouping the june data set based on Gender
june_data_gender <- sqldf("select AVG(Likelihood_Recommend_H),Count(X), GENDER_H from  juneTexasData GROUP BY GENDER_H ")
june_data_gender
june_data_gender <- data.frame(june_data_gender)
colnames(june_data_gender) <- c("AvgLikelihoodRecommend","CountJune","Gender")
june_data_gender <- june_data_gender[-c(1,4),]
View(june_data_gender)

#Grouping the september data set based on Gender
sep_data_gender <- sqldf("select AVG(Likelihood_Recommend_H),Count(X), GENDER_H from  septemberTexasData GROUP BY GENDER_H ")
sep_data_gender
sep_data_gender <- data.frame(sep_data_gender)
colnames(sep_data_gender) <- c("AvgLikelihoodRecommend","CountSep","Gender")
sep_data_gender <- sep_data_gender[-c(1,4),]
View(sep_data_gender)

#Grouping the december data set based on Gender
dec_data_gender <- sqldf("select AVG(Likelihood_Recommend_H),Count(X), GENDER_H from decemberTexasData GROUP BY GENDER_H ")
dec_data_gender
dec_data_gender <- data.frame(dec_data_gender)
colnames(dec_data_gender) <- c("AvgLikelihoodRecommend","CountDec","Gender")
dec_data_gender <- dec_data_gender[-c(1,4),]
View(dec_data_gender)


#Storing month values in a data frame
Months1 <- c("March","March","June","June","September","September","December","December")
Months1 < data.frame(Months1)
View(Months1)

#Storing count values in a dataframe
Count_values <- c(march_data_gender[2],june_data_gender[2],sep_data_gender[2],dec_data_gender[2])
Count_values <- stack(Count_values)
View(Count_values)
Count_values <- Count_values[-2]
colnames(Count_values) <- "Count"
View(Count_values)

#Storing the gender values in a dataframe
Gender <- c("Female","Male","Female","Male","Female","Male","Female","Male")
Gender <- data.frame(Gender)
View(Gender)

#Merged dataframe
Newdf_gender <- data.frame(Gender,Months1,Count_values)
View(Newdf_gender)

Months1 <- factor(Months, levels = c('March', 'June', 'September','December'))

#Comparing customer count based on gender for March, June, September and December --->
g_line <- ggplot(Newdf_gender, aes(x=Months1,group=Gender, color=Gender)) + geom_line(aes(y=Count)) 
g_line <- g_line + geom_point(y=Count, colour="black", size=4, shape=21) 
g_line <- g_line + ylab("Number of Guests")+ xlab("Months")+ ggtitle("Comparing Customer Count based on Gender")
g_line
                    

#Generating a histogram for computing the number of guests visiting based on Age groups for all 4 months 

#Grouping the entire data set based on Age_Range_H
histogram_age <- sqldf("select AVG(Likelihood_Recommend_H),Count(X), Age_Range_H from combinedTexasData GROUP BY Age_Range_H ")
View(histogram_age)
histogram_age <- histogram_age[-1,]
colnames(histogram_age) <- c("AvgLikelihoodrecommend","CountAll","AgeGroup")

#Plotting histogram
g_histogram <- ggplot(histogram_age, aes(x=AgeGroup,y=CountAll)) + geom_histogram(stat ="identity",color="black", fill = c("red","yellow","gray","steelblue","brown","orange","blue")) +ylab("Number of Guests") + xlab("Age Groups") + ggtitle("Number of guests based on Age groups") 
g_histogram

















