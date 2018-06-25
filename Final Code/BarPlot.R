#Installing relevant packages for visualizations
install.packages("ggplot2")
library(ggplot2)

install.packages("sqldf")
library(sqldf)

install.packages("reshape2")
library(reshape2)

#Reading the csv data for the March, June, September and December.
marchTexasData <- read.csv("/Users/tanay/Desktop/Hyatt Project/filteredData1.csv")
juneTexasData <- read.csv("/Users/tanay/Desktop/Hyatt Project/filteredData2.csv")
septemberTexasData <- read.csv("/Users/tanay/Desktop/Hyatt Project/filteredData3.csv")
decemberTexasData <- read.csv("/Users/tanay/Desktop/Hyatt Project/filteredData4.csv")

View(marchTexasData)

#Combined data set for March, June, September and December.
combinedTexasData <- rbind(marchTexasData, juneTexasData, septemberTexasData, decemberTexasData)
write.csv(combinedTexasData,file= 'combinedTexasData.csv')
View(combinedTexasData)

#Generating a Bar plot for comparing number of guests visiting the different hotel chains in the months of March, June, September and December-->

#Grouping the entire data set based on Brand_PL
combined_data <- sqldf("select AVG(Likelihood_Recommend_H), Count(X), Brand_PL from combinedTexasData GROUP BY Brand_PL ")
combined_data
View(combined_data)

#Grouping the data set for march based on Brand_PL
march_data <- sqldf("select AVG(Likelihood_Recommend_H),Count(X), Brand_PL from  marchTexasData GROUP BY Brand_PL ")
march_data <- data.frame(march_data)
colnames(march_data) <- c("AvgLikelihoodRecommend","CountMarch","HotelChain")
View(march_data)

#Grouping the data set for june based on Brand_PL
june_data <- sqldf("select AVG(Likelihood_Recommend_H),Count(X), Brand_PL from  juneTexasData GROUP BY Brand_PL ")
june_data
june_data <- data.frame(june_data)
colnames(june_data) <- c("AvgLikelihoodRecommend","CountJune","HotelChain")
View(june_data)

#Grouping the data set for september based on Brand_PL
sep_data <- sqldf("select AVG(Likelihood_Recommend_H),Count(X), Brand_PL from  septemberTexasData GROUP BY Brand_PL ")
sep_data
sep_data <- data.frame(sep_data)
colnames(sep_data) <- c("AvgLikelihoodRecommend","CountSep","HotelChain")
View(sep_data)

#Grouping the data set for december based on Brand_PL
dec_data <- sqldf("select AVG(Likelihood_Recommend_H),Count(X), Brand_PL from decemberTexasData GROUP BY Brand_PL ")
dec_data
dec_data <- data.frame(dec_data)
colnames(dec_data) <- c("AvgLikelihoodRecommend","CountDec","HotelChain")
View(dec_data)

#Data frame to store month values
Months <- c("March","March","March","March","March","June","June","June","June","June", "September","September","September","September", "September","December","December","December", "December","December")
Months < data.frame(Months)
View(Months)

#Data frame to store the count values
CountValues <- c(march_data[2],june_data[2],sep_data[2],dec_data[2])
CountValues <- stack(CountValues)
View(CountValues)
CountValues <- CountValues[-2]
colnames(CountValues) <- "Count"
View(CountValues)

#Data frame to store the Brand_PL values
hotelchain <- c("Grand Hyatt","Hyatt","Hyatt House","Hyatt Place","Hyatt Regency","Grand Hyatt","Hyatt","Hyatt House","Hyatt Place","Hyatt Regency","Grand Hyatt","Hyatt","Hyatt House","Hyatt Place","Hyatt Regency","Grand Hyatt","Hyatt","Hyatt House","Hyatt Place","Hyatt Regency")
hotelchain <- data.frame(hotelchain)
View(hotelchain)

#Combined data frame
Newdf <- data.frame(hotelchain,Months,CountValues)
View(Newdf)

Months <- factor(Months, levels = c('March', 'June', 'September','December'))

#Comparing count data for all 4 months using bar chart:
g_bar <- ggplot(Newdf,aes(x=hotelchain,y=CountValues,fill=Months))+ geom_bar(stat="identity",position="dodge")+ scale_fill_manual(name="Months",values=c("grey", "yellow", "brown", "orange")) + xlab("Hotel Chain")+ylab("Number of guests")+ coord_cartesian(ylim=c(300,3000)) + ggtitle("Number of guests based on Hotel chain")
g_bar


#Generating a Bar plot for comparing the Avg of likelihood to recommend field of different hotel chains in the months of March, June, September and December-->
#Data frame to store the average likelihood to recommend values
AvgLikelihood <- c(march_data[1],june_data[1],sep_data[1],dec_data[1])
AvgLikelihood <- stack(AvgLikelihood)
View(AvgLikelihood)
AvgLikelihood <- AvgLikelihood[-2]
colnames(AvgLikelihood) <- "AvgLikelihood"
View(AvgLikelihood)

#New combined data frame
Newdf2 <- data.frame(hotelchain,Months,AvgLikelihood)
View(Newdf2)

#Comparing Avg likelihood to recommend data for all 4 months using bar chart:
g_avgLTR <- ggplot(Newdf2,aes(x=hotelchain,y=AvgLikelihood,fill=factor(Months)))+ geom_bar(stat="identity",position="dodge")+ scale_fill_manual(name="Months",values=c("grey", "yellow", "brown", "orange"))  + xlab("Hotel Chain")+ylab("Average Likelihood to recommend")+ coord_cartesian(ylim=c(8,9)) + ggtitle("Average Likelihood to recommend scores of different hotel chains")
g_avgLTR






