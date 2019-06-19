library(mlbench)
library(dplyr)
library(Boruta)
data("BreastCancer")
?BreastCancer
View(head(BreastCancer))
dim(BreastCancer)

B_C1<-BreastCancer[,-1]

View(head(B_C1))

B_C1<-mutate(B_C1,Class1=ifelse(Class=="benign",2,4))
B_Can<-B_C1[,-10]
View(B_Can)
table(B_Can$Class1)

B_Can[] <- lapply( B_Can, factor) 

B_Can<-na.omit(B_Can)
B_C_Boruta<-Boruta(Class1~.,data=B_Can)
plot(B_C_Boruta,cex.axis=0.7,las=0.1)
#################################################################################################
set.seed(121)
B_Can_sam<-sample(2,nrow(B_Can),prob=c(0.9,0.1),replace=T)
B_train<-B_Can[B_Can_sam==1,]
B_test<-B_Can[B_Can_sam==2,]
##########################################randomforest############################################
library(randomForest)
B_Can_m3<-randomForest(Class1~.,data=B_train)
B_Can_p3<-predict(B_Can_m3,B_Can,type="response")
B_Can_df3<-data.frame(B_Can_p3,B_Can$Class1)
View(B_Can_df3)
colnames(B_Can_df3)<-c("predict","actual")
B_T3<-table(B_Can_df3$predict,B_Can_df3$actual)
B_T3
acc3<-sum(diag(B_T3))/sum(B_T3)
acc3


