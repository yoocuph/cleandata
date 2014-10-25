###This script  
#Open the connection to the features file
con1 <- file("~/Coursera/UCI HAR Dataset/features.txt",open="r")#Read the whole features file into a vector named features
features <- readLines(con1,skipNul=T)
#close the connection to features
close(con1)
#Open the connection to the test data set
con2 <- file("~/Coursera/UCI HAR Dataset/test/X_test.txt",open="r")
##read the test data set into a data frame named datax_test
datax_test <- read.table(con2)
##close the connection to the test data set
close(con2)
##open the connection to the train data set
con3 <- file("~/Coursera/UCI HAR Dataset/train/X_train.txt",open="r")
##read the data set into a data frame named datax_train
datax_train <- read.table(con3)
##close the conection to the train data set
close(con3)
##Combine the train data with the test data
combined_data1 <- rbind(datax_test,datax_train)
## name all variables based on the features vector
colnames(combined_data1) <- features
###Reading test subject and activity test label
con4_test <- file("~/Coursera/UCI HAR Dataset/test/subject_test.txt",open="r",encoding="UTF-8")
con5_test <- file("~/Coursera/UCI HAR Dataset/test/y_test.txt",open="r",encoding="UTF-8")
subject_test <- read.table(con4_test)
activitytest_label <- read.table(con5_test)
close(con4_test)
close(con5_test)
###Reading test subject and activity train label
con4_train <- file("~/Coursera/UCI HAR Dataset/train/subject_train.txt",open="r",encoding="UTF-8")
con5_train <- file("~/Coursera/UCI HAR Dataset/train/y_train.txt",open="r",encoding="UTF-8")
subject_train <- read.table(con4_train)
activitytrain_label <- read.table(con5_train)
close(con4_train)
close(con5_train)
###Joining the test and train subjects
subjects <- rbind(subject_test,subject_train)
###Joining the test and train activities
activity <- rbind(activitytest_label,activitytrain_label)
##create 2 factor variables to serve as factors for train and test data
x1 <- rep(1,2947)
##
y1 <- rep(2,7352)
z <- c(x1,y1)
##create a factor variable to indicate if a data is test or train
class <- factor(z,labels=c("test"," train"))
####Extracting the means and standard deviations
#create patterned vectors to identify the needed columns
x40 <- seq(from=1,to=200,by=40)
x41 <- seq(from=40,to=200,by=40)
x42 <- seq(from=201,to=265,by=13)
x43 <- seq(from=213,to=265,by=13)
#output data frame containing subject, activity and class variables
new_data3 <- data.frame(Subjects=subjects,Activities=activity,Class=class)
#
#new_data6 <- data.frame(class=class)
#
for(i in 1:5)
{
  a <- x41[i]
  l <- x40[i]
  gg <- seq(from=l, to=a, by=1)
  new_data <- combined_data1[,gg]
  new_data2 <- new_data[,1:6]
  #needed data
  new_data3 <- cbind(new_data3,new_data2)
}
for(i in 1:5)
{
  a <- x43[i]
  l <- x42[i]
  gg <- seq(from=l, to=a, by=1)
  new_data4 <- combined_data1[,gg]
  new_data5 <- new_data4[,1:2]
  #needed data
  new_data3 <- cbind(new_data3,new_data5)
}
#Creating patterned vector to walk through needed columns of the data frame
x44 <- seq(from=266,to=502,by=79)
x45 <- seq(from=344,to=502,by=79)
#
#new_data7 <- data.frame(class=class)
for(i in 1:3)
{
  a <- x45[i]
  l <- x44[i]
  gg <- seq(from=l, to=a, by=1)
  new_data8 <- combined_data1[,gg]
  new_data9 <- new_data8[,1:6]
  #needed data
  new_data3 <- cbind(new_data3,new_data9)
}
#Creating patterned vector to walk through needed columns of the data frame
x46 <- seq(from=503,to=554,by=13)
x47 <- seq(from=515,to=554,by=13)
#
#new_data10 <- data.frame(class=class)
for(i in 1:4)
{
  a <- x47[i]
  l <- x46[i]
  gg <- seq(from=l, to=a, by=1)
  new_data11 <- combined_data1[,gg]
  new_data12 <- new_data11[,1:2]
  #needed data
  new_data3 <- cbind(new_data3,new_data12)
}
####Removing all unwanted variables from the present work space
rm(new_data12,new_data11,new_data8,new_data9,new_data4,new_data5,
   new_data,new_data2,combined_data1,datax_test,datax_train,a,l,gg,x46,x47,
   x44,x45,x40,x41,x42,x43)
####Joining the different needed data
#clean_data<- cbind(new_data3,new_data6,new_data7,new_data10)
###Cleaning the column Names
untidy_names <- colnames(new_data3)
untidy_names2 <- gsub("[0-9\ ]", "",untidy_names)
untidy_names3 <- gsub("[()]","",untidy_names2)
untidy_names4 <- gsub("BodyBody","Body",untidy_names3)
tidy_names <- gsub("[-]","_",untidy_names4)
tidy_names[1] <- "Subject"
tidy_names[2] <- "Activity"
###Replace the untidy variable names with the tidy names
colnames(new_data3) <- tidy_names
####Removing unwanted variables
rm(untidy_names, untidy_names2,untidy_names3,tidy_names,untidy_names4)
##
con_actlabel <- file("~/Coursera/UCI HAR Dataset/activity_labels.txt",open="r")
act_labels <- readLines(con_actlabel,skipNul=T)
act_labels2 <- gsub("[0-9]\ ","",act_labels)
new_data3$Activity <- factor(new_data3$Activity,levels=1:6,labels=act_labels2)
close(con_actlabel)
##
rm(x1,y1,z,subjects,subject_train,subject_test,i,features,con5_test,con5_train
   ,con_actlabel,con1,con2,con3,class,activity,activitytest_label
  ,activitytrain_label,act_labels,con4_test,con4_train)
###Open connection to output file and write the output file
output_connection <- file("C:/Users/user/Desktop/Clean_data/clean_data.txt",open="w")
output_connection2 <- file("C:/Users/user/Desktop/Clean_data/clean_data.csv",open="w")
write.table(new_data3,output_connection)
write.csv(new_data3,output_connection2)
close(output_connection)
close(output_connection2)
rm(output_connection,output_connection2)
#################The Second Data Set
list1 <- split(new_data3,new_data3$Activity)
rep1 <- gl(6,30,labels=act_labels2)
out1 <- data.frame()
for(i in 1:6)
{
  frame <- list1[1]
  out2 <- data.frame(Subjects=1:30)
  for(j in 4:69)
  {
    out <-  tapply(frame[[1]][[4]],frame[[1]][[1]],mean)
    out2 <- cbind(out2,out)
  }
   out1 <- rbind(out1,out2)
}
names_befrore <- names(new_data3)
needed_names_before2 <- c(names_befrore[1],names_befrore[4:69])
colnames(out1) <- needed_names_before2
out1$Activity <- rep1
####Opening connection to output files
output_connection <- file("C:/Users/user/Desktop/Clean_data/clean_data2.txt",open="w")
output_connection2 <- file("C:/Users/user/Desktop/Clean_data/clean_data2.csv",open="w")
write.table(out1,output_connection)
write.csv(out1,output_connection2)
close(output_connection)
close(output_connection2)
rm(list=ls())




