marchData <- read.csv("sampleMarchData.csv")
juneData <- read.csv("sampleJuneData.csv")
septemberData <- read.csv("sampleSeptemberData.csv")
decemberData <- read.csv("sampleDecemberData.csv")
fieldNames <- read.csv("majorAttributes.csv")
marchData <- marchData[,which(colnames(marchData) %in% colnames(fieldNames))]
juneData <- juneData[,which(colnames(juneData) %in% colnames(fieldNames))]
septemberData <- septemberData[,which(colnames(septemberData) %in% colnames(fieldNames))]
decemberData <- decemberData[,which(colnames(decemberData) %in% colnames(fieldNames))]
View(marchData)
View(juneData)
View(septemberData)
View(decemberData)

#write.csv(marchData,file= 'filteredDataMarch.csv')
#write.csv(juneData,file='filteredData2.csv')
#write.csv(septemberData,file='filteredData3.csv')
#write.csv(decemberData,file='filteredData4.csv')

#Comparison to shortlist Countries

#Find Highest visited Country
install.packages("ggplot2")
library(ggplot2)


#install.packages("reshape2")
#library(reshape2)

#For March, the top countries are:
marchdataTable <- table(marchData$Country_PL, marchData$State_PL, marchData$City_PL)
marchTableVector <- as.data.frame(marchdataTable)
class(marchTableVector$Var1)
marchTableVectorData <- c(marchTableVector$Var1)
headMarchData <- head(marchTableVector[order(-marchTableVector$Freq),],4)
headMarchData$month<-  rep("March",nrow(headMarchData)) # make new column 
headMarchData


#########Wait

#########Wait


dummyDF <- data.frame(state.name, stringsAsFactors=FALSE)
dummyDF$state <- tolower(dummyDF$state.name)


#United States
#States- Flordia, New York,Texas, California, DC, California
#City- Orlando, New York, San Antonio, San Diego

#For June, the top countries are:
juneDataTable <- table(juneData$Country_PL, juneData$State_PL, juneData$City_PL)
juneTableVector <- as.data.frame(juneDataTable)
headJuneData <- head(juneTableVector[order(-juneTableVector$Freq),],4)
headJuneData$month<-  rep("June",nrow(headJuneData)) # make new column
headJuneData
#United States
#States- New York, Florida, Texas, California, California
#City- New York, Orlando, San Antonio,San Diego

#For Sep, the top countries are:

sepDataTable <- table(septemberData$Country_PL,septemberData$State_PL, septemberData$City_PL)
sepTableVector <- as.data.frame(sepDataTable)
headSepData <- head(sepTableVector[order(-sepTableVector$Freq),],4)
headSepData$month<-  rep("Sep",nrow(headSepData)) # make new column
headSepData
#United States
#States- New York, Florida, Texas, Illinois
#City- New York, Orlando, San Antonio, Chicago
#For Dec, the top countries are:

decDataTable <- table(decemberData$Country_PL,decemberData$State_PL, decemberData$City_PL)
decTableVector <- as.data.frame(decDataTable)
headDecData<- head(decTableVector[order(-decTableVector$Freq),],4)
headDecData$month<-  rep("Dec",nrow(headDecData)) # make new column
headDecData
#United States
#State- Illinois, Flordia, New York, California, Texas
#City- Chicago, Orlando, New York

install.packages("reshape2")
library(reshape2)
combinedResults <- rbind(headMarchData, headJuneData, headSepData, headDecData)

colnames(combinedResults)[2]<- 'States'
combinedResults

#Merging months data and visualizing it


ggplot(combinedResults, aes(reorder(factor(month), -Freq), Freq, fill = States)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")+ylim(0, 170) + coord_cartesian(ylim=c(90,170))+labs(x="Months") +labs(y="No. of Customers")
#Since US is always leading, our main Aim is to consider majorly the records from United States
#Now for States, the states with highest booking considering all quarters are majorly Flordia, New York and Texas
#So, we will consider only Florida, Texas and New York bookings as they are the major source of revenue

#for March, June, Sep and Dec Data, cutting down the State to New York, Florida and Texas
marchDataUpdated<- marchData[which(marchData$State_PL =='New York' | marchData$State_PL =='Florida' | marchData$State_PL =='Texas' ),]
juneDataUpdated<- juneData[which(juneData$State_PL =='New York' | juneData$State_PL =='Florida' | juneData$State_PL =='Texas'),]
sepDataUpdated<- septemberData[which(septemberData$State_PL =='New York' | septemberData$State_PL =='Florida' | septemberData$State_PL =='Texas'),]
decDataUpdated<- decemberData[which(decemberData$State_PL =='New York' | decemberData$State_PL =='Florida'| decemberData$State_PL =='Texas'),]

write.csv(marchDataUpdated,file= 'StateDataMarch.csv')
write.csv(juneDataUpdated,file='StateDataJune.csv')
write.csv(sepDataUpdated,file='StateDataSep.csv')
write.csv(decDataUpdated,file='StateDataDec.csv')

#Merging Updated data into 1 File
combinedDataSet <- rbind(marchDataUpdated, juneDataUpdated, sepDataUpdated, decDataUpdated)
write.csv(combinedDataSet,file= 'combinedDataSet.csv')
combinedDataSet
#Removing NA's from major fields

#Apriori Association Mining Rules 
  #Pick Fields for mining Rules

#Linear Model for independent fields and likelihood_to_recommend
  #Pick independent variables

#SVM and KSVM 
 #Repeat linear Model fields here

#Geom_map plot for countries and states for hotel traffic - Spatial Analysis


#Descriptive Analysis

#Temporal Analysis- with 


