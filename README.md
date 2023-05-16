library(ggplot2)
library(lattice)
library(caret)
library(e1071)
library(Hmisc)
library(readxl)
library(survival)
library(OneR)
library(RWeka)
library(openxlsx)
library(factoextra)
library(ggcorrplot)
library(dplyr)
library(lares)
library(randomForest)
library(C50)
library(reshape2)
library(pROC)

data<-read.csv(file='C:/Users/dp00886/OneDrive - University of Surrey/ML Project/combined_data.csv')
#Data before random sampling
D <- data
#Random sampling
data <- data[sample(nrow(data), 10000, replace = F),]

#Finding distribution of target
prop.table(table(data$satisfaction)) * 100

#Removing first and second column
data<-data[,-1]
data<-data[,-1]

#Looking for missing values
naIndex<-is.na(data)
#Calculating total number of missing values
colSums(naIndex)
#Calculating percentage of of missing values
percent_null<-colSums(naIndex)/nrow(data)*100
percent_null

#Removing rows with missing values
data<-na.omit(data)

# pie chart with percentages
satisfaction_count <- table(data$satisfaction)
satisfaction_percent <- round(satisfaction_count / sum(satisfaction_count) * 100, 1)
pie(satisfaction_count, labels = paste0(satisfaction_percent, "%"), col = c("#FF4500", "#8FBC8F"), main = "Target Distribution")
legend("topright", c("Satisfied", "Neutral or Dissatisfied"), fill = c("#FF4500", "#8FBC8F"))

#Boxplot for all features 
boxdata<-data%>%
  select(c(7:20))
ggplot(stack(boxdata), aes(x = ind, y = values)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
        plot.margin=unit(c(0,0,0,1),"cm"))+labs(x="satisfaction", y="Rating")

#Histogram for age
age<-ggplot(data, aes(Age, fill=satisfaction)) +
  geom_histogram(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Age", y="Count", fill="Satisfaction")
age

#Histogram for Flight Distance
flightdistance<-ggplot(data, aes(Flight.Distance, fill=satisfaction)) +
  geom_histogram(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Flight Distance", y="Count", fill="Satisfaction")
flightdistance

#Barchart for classtype 
classtype<-ggplot(data, aes(Class, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Class Type", y="Count", fill="Satisfaction")
classtype

#Barchart for traveltype
traveltype<-ggplot(data, aes(Type.of.Travel, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Travel Type", y="Count", fill="Satisfaction")
traveltype

#Barchart for Gender
gender<-ggplot(data, aes(Gender, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Travel Type", y="Count", fill="Satisfaction")
gender

#Barchart for Inflight Wifi Service
inflightwifiservices<-ggplot(data, aes(Inflight.wifi.service, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Inflight Wifi Service Ratings", y="Count", fill="Satisfaction")
inflightwifiservices

#Barchart for Ease of online booking
easeofonlinebooking<-ggplot(data, aes(Ease.of.Online.booking, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Ease of Online Booking Ratings", y="Count", fill="Satisfaction")
easeofonlinebooking

#Barchart for Seatcomfort
seatcomfort<-ggplot(data, aes(Seat.comfort, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Seatcomfort Ratings", y="Count", fill="Satisfaction")
seatcomfort

#Barchart for Inflight Entertainment
inflightentertainment<-ggplot(data, aes(Inflight.entertainment, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Inflight Entertainment Ratings", y="Count", fill="Satisfaction")
inflightentertainment

#Barchart for Onboard Service
onboardservices<-ggplot(data, aes(On.board.service, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Onboard Service Ratings", y="Count", fill="Satisfaction")
onboardservices

#Barchart for Legroom Service
legroomservice<-ggplot(data, aes(Leg.room.service, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Legroom Service Ratings", y="Count", fill="Satisfaction")
legroomservice

#Barchart for Gate Location
gatelocation<-ggplot(data, aes(Gate.location, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Gate Location Ratings", y="Count", fill="Satisfaction")
gatelocation

#Barchart for Food and Drink
foodanddrink<-ggplot(data, aes(Food.and.drink, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Food and Drink Ratings", y="Count", fill="Satisfaction")
foodanddrink

#Barchart for Online Boarding
OnlineBoarding<-ggplot(data, aes(Online.boarding, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Online Boarding Ratings", y="Count", fill="Satisfaction")
OnlineBoarding

#Barchart for Baggage Handling
BaggageHandling<-ggplot(data, aes(Baggage.handling, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Baggage Handling Ratings", y="Count", fill="Satisfaction")
BaggageHandling

#Barchart for Check In Service
CheckInService<-ggplot(data, aes(Checkin.service, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Check In Service Ratings", y="Count", fill="Satisfaction")
CheckInService

#Barchart for Inflight Service
InflightService<-ggplot(data, aes(Inflight.service, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Inflight Service Ratings", y="Count", fill="Satisfaction")
InflightService

#Barchart for Cleanliness
Cleanliness<-ggplot(data, aes(Inflight.service, fill=satisfaction)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#FF4500", "#8FBC8F"),labels=c("neutral or dissatisfied", "satisfied")) +
  labs(x="Cleanliness Ratings", y="Count", fill="Satisfaction")
Cleanliness

#Scatterplot between Arrival Delay in minutes and Departure Delay in minutes
featurePlot(x=data$Arrival.Delay.in.Minutes,y=data$Departure.Delay.in.Minutes,labels = c("Departure Delay In Minutes", "Arrival Delay In Minutes"))

#Converting characters to ASCII
for (i in 1:(ncol(data)-1)) {
  if (is.character(data[, i])==TRUE){
    for(j in 1:nrow(data)){
      ascis <- as.numeric(charToRaw(data[j, i]))
      data[ j, i] <- sum(ascis)
    }
  }
  data[,i] <- as.numeric(data[,i])
}
#Converting target to binary
data$satisfaction<-ifelse(data$satisfaction=="satisfied",1,0)

#Converting target to factor
data[,ncol(data)] <- as.factor(data[,ncol(data)])

#Principal Component Analysis
pca <- prcomp(data[, -ncol(data)], center = TRUE, scale. = TRUE)
print(summary(pca))

#Selecting top 10 variables through PCA
fviz_pca_var(pca, select.var = list(contrib = 10), repel = TRUE)

#Correlation matrix
#corplot<-cor(data)
ggcorrplot(corplot, tl.cex = 8)
#all possible correlations
corr_cross(data, 
           max_pvalue = 0.05, 
           top = 10) 
#correlation with target
corr_features<-corr_var(data, 
         satisfaction, 
         top = 10 )
print(corr_features)
best_accuracy<-0
best_model<-NULL
bst_model<-NULL
cat("       ", file = "C:/Users/dp00886/OneDrive - University of Surrey/ML Project/Results.txt", sep = "\n", append =TRUE)
for(i in seq(1:4))
  {
# header line of on-screen performance metrics
  if(i==1)
  {
    cat("Random Forest\n", file = "C:/Users/dp00886/OneDrive - University of Surrey/ML Project/Results.txt", sep = "\n", append =TRUE)
    bst_model<-"Random Forest"
    }
  else if(i==2)
  {
    cat("Naive Bayes", file = "C:/Users/dp00886/OneDrive - University of Surrey/ML Project/Results.txt", sep = "\n", append =TRUE)
    bst_model<-"Naive Bayes"
    }
  else if(i==3)
  {
    cat("KNN", file = "C:/Users/dp00886/OneDrive - University of Surrey/ML Project/Results.txt", sep = "\n", append =TRUE)
    bst_model<-"KNN"
    }
  else
  {
    cat("C5.0", file = "C:/Users/dp00886/OneDrive - University of Surrey/ML Project/Results.txt", sep = "\n", append =TRUE)
    bst_model<-"C5.0"
    }

cat("---------------------------------------------------------------------------------------------------------------------------------", file = "C:/Users/dp00886/OneDrive - University of Surrey/ML Project/Results.txt", sep = "\n", append =TRUE)
cat("\t TR-Data% \t TS-Data% \t Accuracy% \t Kappa% \t Sensitivity% \t Specificity% \t Precision% \t Recall%", file = "C:/Users/dp00886/OneDrive - University of Surrey/ML Project/Results.txt", sep = "\n", append = TRUE) # Apply cat & append
cat("---------------------------------------------------------------------------------------------------------------------------------", file = "C:/Users/dp00886/OneDrive - University of Surrey/ML Project/Results.txt", sep = "\n", append =TRUE)
# creating a blank data frame to store performance metrics scores
pf = data.frame(matrix(
vector(), 9, 6, dimnames=list(c("TR-10", "TR-20", "TR-30", "TR-40", "TR-50", "TR-60", "TR-70", "TR-80", "TR-90"), c("Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall"))),
stringsAsFactors=F)
pfc <- 0 # pfc - performance frame counter

# creating sequence to represent training data ratio
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9) 

for (t in training_data_percentages){
pfc <- pfc+1
indx_Partition <- createDataPartition(data$satisfaction, p=t, list=FALSE) # index of training  data#
training_data <- data[indx_Partition,] # training dataset
testing_data <- data[-indx_Partition,] # testing dataset



 # Setting Seed value 
set.seed(120)
control<- trainControl(method="repeatedcv", repeats=10)

models <- list (TrainedClassifier_rf <- randomForest(satisfaction ~ Online.boarding+ Class+ Type.of.Travel+ Inflight.entertainment+ Seat.comfort+ On.board.service+ Leg.room.service+ Flight.Distance+ Cleanliness+ Inflight.wifi.service+ Food.and.drink+ Ease.of.Online.booking+ Departure.Arrival.time.convenient+ Gate.location , data = training_data,laplace=0),
                TrainedClassifier_nv <- naiveBayes(satisfaction ~ Online.boarding+ Class+ Type.of.Travel+ Inflight.entertainment+ Seat.comfort+ On.board.service+ Leg.room.service+ Flight.Distance+ Cleanliness+ Inflight.wifi.service+ Food.and.drink+ Ease.of.Online.booking+ Departure.Arrival.time.convenient+ Gate.location , data = training_data, laplace=0),
                TrainedClassifier_KNN <-train(satisfaction ~ Online.boarding+ Class+ Type.of.Travel+ Inflight.entertainment+ Seat.comfort+ On.board.service+ Leg.room.service+ Flight.Distance+ Cleanliness+ Inflight.wifi.service+ Food.and.drink+ Ease.of.Online.booking+ Departure.Arrival.time.convenient+ Gate.location , metric="Accuracy",data = training_data,method="knn",trControl=control),
                TrainedClassifier_C5 <- C5.0(satisfaction ~ Online.boarding+ Class+ Type.of.Travel+ Inflight.entertainment+ Seat.comfort+ On.board.service+ Leg.room.service+ Flight.Distance+ Cleanliness+ Inflight.wifi.service+ Food.and.drink+ Ease.of.Online.booking+ Departure.Arrival.time.convenient+ Gate.location , data = training_data))

#Predicting on test data
Predicted_outcomes <- predict(models[[i]], newdata =
                                    testing_data[,1:ncol(testing_data)-1])
# Confusion Matrix
cm<-confusionMatrix(Predicted_outcomes,as.factor(testing_data$satisfaction))
print(cm)
acc<-cm$overall[1]
if(acc>best_accuracy)
{
  best_accuracy<-acc
  best_model<-bst_model
}

# Printing performance metrics in txt file  
cat("\t ", t*100, "\t\t ", (1-t)*100, "\t\t ",
format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall = 2), "\t\t ",
format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2), "\t\t ",
format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), nsmall = 2), "\t\t ",
format(round(cm[["byClass"]][["Specificity"]]*100, 2), nsmall = 2), "\t\t ",
format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall = 2), "\t\t ",
format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2), "\n", file = "C:/Users/dp00886/OneDrive - University of Surrey/ML Project/Results.txt", sep = " ", append = TRUE)

# --------- assigning the performance metrics to the dataframe
pf[pfc,"Accuracy"] <- format(round(cm[["overall"]][["Accuracy"]]*100, 2), nsmall =2)
pf[pfc,"Kappa"] <- format(round(cm[["overall"]][["Kappa"]]*100, 2), nsmall = 2)
pf[pfc,"Sensitivity"] <- format(round(cm[["byClass"]][["Sensitivity"]]*100, 2), 
                                    nsmall = 2)
pf[pfc,"Specificity"] <- format(round(cm[["byClass"]][["Specificity"]]*100, 2), 
                                    nsmall = 2)
pf[pfc,"Precision"] <- format(round(cm[["byClass"]][["Precision"]]*100, 2), nsmall
                                  = 2)
pf[pfc,"Recall"] <- format(round(cm[["byClass"]][["Recall"]]*100, 2), nsmall = 2)


  
}

}
cat("The best model is", best_model,"with Accuracy: ",best_accuracy)



