#Course :  CS-513-A
#Name :  Prabha Bhat, Sapna Bhat
#Algorithm : C5.0



library(rpart)
library(rpart.plot)
library(rattle)
library(RWeka)
library(RColorBrewer)
library(C50)

#Remove all the Objects
rm(list=ls())

#Define max-min normalization function
norm <-function(x,minx,maxx) 
{
  z<-((x-minx)/(maxx-minx))
  return(z) 
}

#Read the datasets
dataset <- read.csv("C:/Users/Mahabaleshwar/Desktop/Datamining_Final/3rd May/30th April/expedia_train_30.csv") 
View(dataset)


#Formating Date
newdate <- as.Date(dataset$date_time,format='%m/%d/%Y')
newdate1 <- as.POSIXlt(newdate)
newdate1$mon

#Categorizing the variables
is_booking_cat<- ifelse(dataset$is_booking==0 ,'NO', 
                        ifelse(dataset$is_booking==1 ,'YES', 'NO'))


mobile_category<- ifelse(dataset$is_mobile==0 ,'NO', 
                         ifelse(dataset$is_mobile==1 ,'YES', 'NO'))



date_category<- ifelse(newdate1$mon>=0 & newdate1$mon<=2,'WINTER', 
                       ifelse(newdate1$mon>=3 & newdate1$mon<=5,'SPRING',
                              ifelse(newdate1$mon>=6 & newdate1$mon<=8,'SUMMER',
                                     ifelse(newdate1$mon>=9 & newdate1$mon<=11,'FALL','WINTER'))))


distance_group <- ifelse(dataset$orig_destination_distance>0 & dataset$orig_destination_distance<5000,'0-5000', 
                         ifelse(dataset$orig_destination_distance>5000 & dataset$orig_destination_distance<10000,'5000-10000', 
                                ifelse(dataset$orig_destination_distance>10000,'>10000','>12000')))
#       ifelse(dataset$orig_destination_distance>3000 & dataset$orig_destination_distance<4000,'3000-4000',
#        ifelse(dataset$orig_destination_distance>4000 & dataset$orig_destination_distance<5000,'4000-5000',
#        ifelse(dataset$orig_destination_distance>5000 & dataset$orig_destination_distance<6000,'5000-6000',
#   ifelse(dataset$orig_destination_distance>6000 & dataset$orig_destination_distance<7000,'6000-7000',
# ifelse(dataset$orig_destination_distance>7000 & dataset$orig_destination_distance<8000,'7000-8000',
#  ifelse(dataset$orig_destination_distance>8000 & dataset$orig_destination_distance<9000,'8000-9000',
#     ifelse(dataset$orig_destination_distance>9000 & dataset$orig_destination_distance<10000,'9000-10000',
#       ifelse(dataset$orig_destination_distance>10000 & dataset$orig_destination_distance<11000,'10000-11000',
#      ifelse(dataset$orig_destination_distance>11000 & dataset$orig_destination_distance<12000,'11000-12000','>12000'))))))))))))

hotel_group <- ifelse(dataset$hotel_cluster>0 & dataset$hotel_cluster<500,'2 STAR', 
                      ifelse(dataset$hotel_cluster>500 & dataset$hotel_cluster<1000,'3 STAR', 
                             ifelse(dataset$hotel_cluster>1000 & dataset$hotel_cluster<1500,'4 STAR',
                                    ifelse(dataset$hotel_cluster>1500 & dataset$hotel_cluster<2000,'5 STAR','6 STAR'))))

#Dividing user continents 
user_continent <- ifelse(dataset$posa_continent==0,'C1', 
                         ifelse(dataset$posa_continent==1,'C2', 
                                ifelse(dataset$posa_continent==2,'C3',
                                       ifelse(dataset$posa_continent==3,'C4',
                                              ifelse(dataset$posa_continent==4,'C5','C6')))))

#Dividing hotel continents 
hotel_continent_category <- ifelse(dataset$hotel_continent==0,'C1', 
                                   ifelse(dataset$hotel_continent==1,'C2', 
                                          ifelse(dataset$hotel_continent==2,'C3',
                                                 ifelse(dataset$hotel_continent==3,'C4',
                                                        ifelse(dataset$hotel_continent==4,'C5',
                                                               ifelse(dataset$hotel_continent==5,'C6',
                                                                      ifelse(dataset$hotel_continent==6,'C7','C8')))))))



#Create a new dataset
expedia <- cbind(
  date_time=as.character(date_category)
  ,posa_continent=as.character(user_continent)
  ,orig_destination_distance=as.character(distance_group)
  ,is_mobile=as.character(mobile_category)
  ,is_booking=as.character(is_booking_cat)
  ,hotel_cluster=as.character(hotel_group)
  ,hotel_continent=as.character(hotel_continent_category))


View(expedia)


write.csv(expedia, "C:/Users/Mahabaleshwar/Desktop/Datamining_Final/expedia_newtrain_30.csv",row.names=FALSE, na="")
expedia_new <- read.csv("C:/Users/Mahabaleshwar/Desktop/Datamining_Final/expedia_newtrain_30.csv") 

View(expedia_new)

#Divide the dataset into Training and Test dataset
temp <- sample(nrow(expedia_new),as.integer(0.50 * nrow(expedia_new)))
Training <- expedia_new[temp,]
View(Training)
Test <- expedia_new[-temp,]
View(Test)

#C5.0 on Training data
exp_C50_training <- C5.0(is_booking ~ ., data = Training)
summary(exp_C50_training)

# generate graph 
tree_train <- rpart(exp_C50_training,data=expedia_new,control=rpart.control(minsplit=20,cp=0,digits=6))
fancyRpartPlot(tree_train,palettes=c("Greens", "Reds"),cex=0.75, main="Expedia",sub="")

png("C5.0_4.png", width = 1200, height = 800)
post(tree_train, file = "", title. = "Classifying expedia Donation Size, 4 splits",
     bp = 18)
dev.off()


#Error value for C5.0 when classifying with is_booking
Predict_book <- predict(tree_train,Training,type = 'class')
e_result_1 <- cbind(Training, as.character(Predict_book))
View(e_result_1)
false_1 <- e_result_1[,5]!=e_result_1[,8]
err_rate_1 <- sum(false_1)/length(false_1)
err_rate_1
summary(exp_C50_training)



