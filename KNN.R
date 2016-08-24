#Course :  CS-513-A
#Name :  Prabha Bhat, Sapna Bhat
#Algorithm : KNN

#Remove all the Objects
rm(list=ls())

#Define max-min normalization function
norm <-function(x,minx,maxx) 
{
  z<-((x-minx)/(maxx-minx))
  return(z) 
}

#Load dataset
dataset <- read.csv("C:/Spring 2016/CS 513-A/Final Project/Expedia/30th April/expedia_train_30.csv") 

#View dataset
View(dataset)


newdate <- as.Date(dataset$date_time,format='%m/%d/%Y')
newdate1 <- as.POSIXlt(newdate)
newdate1$mon



date_category<- ifelse(newdate1$mon>=0 & newdate1$mon<=2,'WINTER', 
                                   ifelse(newdate1$mon>=3 & newdate1$mon<=5,'SPRING',
                                          ifelse(newdate1$mon>=6 & newdate1$mon<=8,'SUMMER',
                                             ifelse(newdate1$mon>=9 & newdate1$mon<=11,'FALL','WINTER'))))



#Replacing NA for orig_destination_distance
nm <- na.omit(dataset$orig_destination_distance)
dataset$orig_destination_distance <- ifelse(is.na(dataset$orig_destination_distance)==TRUE,
                                          mean(nm),dataset$orig_destination_distance)


#Create new dataset
new_dataset <- cbind(
   date_time=as.character(date_category)
  ,posa_continent=norm(dataset[,2],min(dataset[,2]),max(dataset[,2]))
  ,user_location_country=norm(dataset[,3],min(dataset[,3]),max(dataset[,3]))
  ,user_location_region=norm(dataset[,4],min(dataset[,4]),max(dataset[,4]))
  ,user_location_city=norm(dataset[,5],min(dataset[,5]),max(dataset[,5]))
  ,orig_dest_dist= dataset$orig_destination_distance
  ,is_mobile=norm(dataset[,7],min(dataset[,7]),max(dataset[,7]))
  ,is_package=norm(dataset[,8],min(dataset[,8]),max(dataset[,8]))
  ,channel=norm(dataset[,9],min(dataset[,9]),max(dataset[,9]))
  ,srch_adults_cnt=norm(dataset[,10],min(dataset[,10]),max(dataset[,10]))
  ,srch_children_cnt=norm(dataset[,11],min(dataset[,11]),max(dataset[,11]))
  ,srch_rm_cnt=norm(dataset[,12],min(dataset[,12]),max(dataset[,12]))
  ,srch_destination_id=norm(dataset[,13],min(dataset[,13]),max(dataset[,13]))
  ,srch_destination_type_id=norm(dataset[,13],min(dataset[,13]),max(dataset[,13]))
  ,hotel_continent=as.character(dataset$hotel_continent)
  ,hotel_country=norm(dataset[,16],min(dataset[,16]),max(dataset[,16]))
  ,hotel_cluster=norm(dataset[,18],min(dataset[,18]),max(dataset[,18]))
  ,is_booking=norm(dataset[,14],min(dataset[,14]),max(dataset[,14])))

View(new_dataset)

summary(new_dataset)


#Dividing training and test dataset
temp <- sample(nrow(new_dataset),as.integer(0.70 * nrow(new_dataset)))
Training <- new_dataset[temp,]
View(Training)
Test <- new_dataset[-temp,]
View(Test)


#KNN Analysis
library(class)
?knn()

#Classifying based on is_booking when k=1
Predict_book_1 <- knn(Training[,2:17], Test[,2:17], Training[,18], k=1)

#Error value for kNN when classifying with is_booking
e_result_1 <- cbind(Test, as.character(Predict_book_1))
View(e_result_1)
false_1 <- e_result_1[,18]!=e_result_1[,19]
err_rate_1 <- sum(false_1)/length(false_1)
err_rate_1

#Classifying based on is_booking when k=3
Predict_book_3 <- knn(Training[,2:17], Test[,2:17], Training[,18], k=3)

#Error value for kNN when classifying with is_booking
e_result_3 <- cbind(Test, as.character(Predict_book_3))
false_3 <- e_result_3[,18]!=e_result_3[,19]
err_rate_3 <- sum(false_3)/length(false_3)
err_rate_3

#Classifying based on is_booking when k=5
Predict_book_5 <- knn(Training[,2:17], Test[,2:17], Training[,18], k=5)

#Error value for kNN when classifying with is_booking
e_result_5 <- cbind(Test, as.character(Predict_book_5))
false_5 <- e_result_5[,18]!=e_result_5[,19]
err_rate_5 <- sum(false_5)/length(false_5)
err_rate_5

#Classifying based on is_booking when k=11
Predict_book_11 <- knn(Training[,2:17], Test[,2:17], Training[,18], k=11)

#Error value for kNN when classifying with is_booking
e_result_11 <- cbind(Test, as.character(Predict_book_11))
false_11 <- e_result_11[,18]!=e_result_11[,19]
err_rate_11 <- sum(false_11)/length(false_11)
err_rate_11

#Classifying based on is_booking when k=13
Predict_book_13 <- knn(Training[,2:17], Test[,2:17], Training[,18], k=13)

#Error value for kNN when classifying with is_booking
e_result_13 <- cbind(Test, as.character(Predict_book_13))
false_13 <- e_result_13[,18]!=e_result_13[,19]
err_rate_13 <- sum(false_13)/length(false_13)
err_rate_13

#Classifying based on is_booking when k=17
Predict_book_17 <- knn(Training[,2:17], Test[,2:17], Training[,18], k=17)

#Error value for kNN when classifying with is_booking
e_result_17 <- cbind(Test, as.character(Predict_book_17))
false_17 <- e_result_17[,18]!=e_result_17[,19]
err_rate_17 <- sum(false_17)/length(false_17)
err_rate_17

#Classifying based on is_booking when k=23
Predict_book_23 <- knn(Training[,2:17], Test[,2:17], Training[,18], k=23)

#Error value for kNN when classifying with is_booking
e_result_23 <- cbind(Test, as.character(Predict_book_23))
false_23 <- e_result_23[,18]!=e_result_23[,19]
err_rate_23 <- sum(false_23)/length(false_23)
err_rate_23

#Graph1: is_booking vs orig_dest_dist
plot(Predict_book_5,Test[,6]) 


#Graph2: is_booking vs hotel_continent
plot(Predict_book_5,Test[,15]) 


#Frequency Table is_booking vs orig_dest_dist
table(Predict_book_5, Test[,6]) 

#plotting knn_values vs knn_error_rate
knn_success_rate <-c(82.67,89.59,91,91.25,91.25,91.25,91.25)
knn_values <-c(1,3,5,11,13,17,23)
plot(knn_values,knn_success_rate,type='l')