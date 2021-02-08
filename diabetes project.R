#getting data
read.csv("C:/Users/Iwuoha Chimereze/Desktop/Data Analytics/R file/diabetesknn.csv")

#assigning data to value
sickness<-read.csv("C:/Users/Iwuoha Chimereze/Desktop/Data Analytics/R file/diabetesknn.csv")
str(sickness)
View(sickness)

#normalizing data
normalize<-function(x){
return((x-min(x))/(max(x)- min(x)))}
sickness.subset.n<-as.data.frame(lapply(sickness[,1:7],normalize))
head(sickness.subset.n)

set.seed(123)#to get random samples
sickness.d<-sample(1:nrow(sickness.subset.n),size = nrow(sickness.subset.n)*0.7,replace=FALSE)

train.sickness<-sickness[sickness.d,]
test.sickness<-sickness[-sickness.d,]

#creating separate data frame for "outcome" which is the target
train.sickness_labels<-sickness[sickness.d,8]
test.sickness_labels<-sickness[-sickness.d,8]

library(class)# knn package

NROW(train.sickness_labels)

#for k=37
knn.37<-knn(train=train.sickness,test=test.sickness,cl=train.sickness_labels,k=37)
#for k=38
knn.38<-knn(train=train.sickness,test=test.sickness,cl=train.sickness_labels,k=38)
#accuracy for 37
acc.37<-100* sum(test.sickness_labels==knn.37)/NROW(test.sickness_labels)
#accuracy for 38
acc.38<-100* sum(test.sickness_labels==knn.38)/NROW(test.sickness_labels)

acc.37
acc.38

confusionMatrix(table(knn.37,test.sickness_labels))
table(knn.38,test.sickness_labels)

#loop to generate multiple accuracy
i=1
k.optm=1
for (i in 1:38)
{
  knn.mod<- knn(train = train.sickness,test = test.sickness,cl=train.sickness_labels,k=i)
  k.optm[i]<-100 * sum(test.sickness_labels==knn.mod)/NROW(test.sickness_labels)
  k=i
  cat(k,'=',k.optm[i],'\n')
  
}
#graphical representation of accuracy
plot(k.optm,type = "b",xlab = "k - Value",ylab="Accuracy level") 
