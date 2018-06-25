getwd()
Hyatt <- read.csv("filteredDataDec.csv")
View(Hyatt)
c <- sum(Hyatt$NPS_Type == 'Promoter')
c
d <- length(Hyatt$NPS_Type)
Nps = c/(d-c)
Nps
#The NPS is 0.04
myData <- Hyatt
which.max(table(myData$GUEST_COUNTRY_R))
#Hyatt has a lot of customers from United States
myData1 <- Hyatt
#Converting the Nps column to an numeric
myData1$NPS_Type <-gsub('Promoter','2',myData1$NPS_Type)
myData1$NPS_Type <-gsub('Passive','1',myData1$NPS_Type)
myData1$NPS_Type <-gsub('Detractor','0',myData1$NPS_Type)
View(myData1)
is.numeric(myData1$NPS_Type)
myData1$NPS_Type <-as.numeric(myData1$NPS_Type)
#Computing the effect of four major factors on the total NPS of the system
w1 <- lm(formula = myData1$NPS_Type~myData1$Condition_Hotel_H)
w1
summary(w1)
w2 <- lm(formula = myData1$NPS_Type =='2'~myData1$Condition_Hotel_H+myData1$Customer_SVC_H)
w2
summary(w2)
#When customer service is added it increases by a small margin
w3 <- lm(formula = myData1$NPS_Type~myData1$F.B_Overall_Experience_H)
w3
summary(w3)
w4<- lm(formula = myData1$NPS_Type~myData1$Condition_Hotel_H)
w4
#Overall experience affects nps type by 37%
summary(w4)
w5<- lm(formula = myData1$NPS_Type~myData1$Tranquility_H)
w5
summary(w5)
#Tranquility affects nps type by 46%
w6 <- lm(formula = myData1$NPS_Type~myData1$Customer_SVC_H)
w6
summary(w6)
#Customer service affects nps by 53%
w7 <- lm(formula = myData1$NPS_Type~myData1$Internet_Sat_H)
w7
summary(w7)
#Internet affects nps type by 15%
w8 <- lm(formula = myData1$NPS_Type~myData1$Staff_Cared_H)
w8
summary(w8)
#Staff cared affects nps by 48%
w9 <- lm(formula = myData1$NPS_Type~myData1$Staff_Cared_H+myData1$Customer_SVC_H+myData1$Tranquility_H+myData1$F.B_Overall_Experience_H)
w9
summary(w9)
#Customer service has a very low t value
#Hence remove customer service
 w10 <- lm(formula = myData1$NPS_Type~myData1$Staff_Cared_H+myData1$Tranquility_H+myData1$F.B_Overall_Experience_H)
 w10
 summary(w10)
 #Even after removing customer service its 69%
#Removing overall experience
 w11 <- lm(formula = myData1$NPS_Type~myData1$Staff_Cared_H+myData1$Tranquility_H)
 w11
 summary(w11)
 #The t values are close to 8 and these two factors significantly affect the nps of the chain(61%). So these are the two major factors that has to be concentrated on.
 
 