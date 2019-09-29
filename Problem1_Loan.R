library(mlr)
library(ggplot2)
library(plyr)
library(rpart)
library(rattle)
library(car)




train <-read.csv("C:\\Users\\Anuj\\Desktop\\TMBS\\Loan Train.csv",na.strings = c(""," ",NA))
head(train)
test <-read.csv("C:\\Users\\Anuj\\Desktop\\TMBS\\Loan Test.csv",na.strings = c(""," ",NA))
head(test)

prop.table(table(train$Loan_Status))




prop.table(table(train$Married))
prop.table(table(test$Married))
par(mfrow=c(1,2))
barplot(table(train$Gender),main="Training Set")
barplot(table(test$Gender),main="Testing Set")


prop.table(table(train$Dependents))
prop.table(table(test$Dependents))
par(mfrow=c(1,2))
barplot(table(train$Dependents),main="Training Set")
barplot(table(test$Dependents),main="Testing Set")


prop.table(table(train$Education))
prop.table(table(test$Education))
par(mfrow=c(1,2))
barplot(table(train$Education),main="Training Set")
barplot(table(test$Education),main="Testing Set")


prop.table(table(train$Self_Employed))
prop.table(table(test$Self_Employed))
par(mfrow=c(1,2))
barplot(table(train$Self_Employed),main="Training Set")
barplot(table(test$Self_Employed),main="Testing Set")



par(mfrow=c(1,2))
boxplot(train$ApplicantIncome,train$CoapplicantIncome,names=c("Applicant_Income","Coapplicant_Income"),main="Training Set")
boxplot(test$ApplicantIncome,test$CoapplicantIncome,names=c("Applicant_Income","Coapplicant_Income"),main="Testing Set")



par(mfrow=c(1,2))
boxplot(train$LoanAmount,main="Training Set")
boxplot(test$LoanAmount,main="Testing Set")



par(mfrow=c(1,2))
hist(train$Loan_Amount_Term,breaks=500,main="Training Set")
hist(test$Loan_Amount_Term,breaks=500,main="Testing Set")



summary(train$Loan_Amount_Term)
summary(test$Loan_Amount_Term)


par(mfrow=c(1,2))
train$Credit_History <-as.factor(train$Credit_History)
test$Credit_History <- as.factor(test$Credit_History)
barplot(table(train$Credit_History),main="Training Set")
barplot(table(test$Credit_History),main="Testing Set")



prop.table(table(train$Credit_History))
prop.table(table(test$Credit_History)) 
par(mfrow=c(1,2))
barplot(table(train$Property_Area),main="Training Set")
barplot(table(test$Property_Area),main="Testing Set")


Loan_Status_Gender <- ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Gender)+ggtitle("Loan Status for Gender Applicants")


Loan_Status_Marital <- ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Married)+ggtitle("Loan Status for Marital Applicants")


Loan_Status_Eductation <- ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Education)+ggtitle("Loan Status for Education of Applicants")


Loan_Status_Dependent <- ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Dependents)+ggtitle("Loan Status or Dependents Applicants")


Loan_Status_Terms_Loan <- ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Loan_Amount_Term)+ggtitle("Loan Status for terms of loan")


Loan_Status_Employees <- ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Self_Employed)+ggtitle("Loan Status for Employment status of Applicants")


Loan_Status_Credit_History <- ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Credit_History)+ggtitle("Loan Status for credit history of Applicants")


Loan_Status_App_Income <- ggplot(train, aes(x=Loan_Status,y=ApplicantIncome))+geom_boxplot()+ggtitle("Loan Status for Applicants income")


Loan_Status_Coapp_Income <- ggplot(train, aes(x=Loan_Status,y=CoapplicantIncome))+geom_boxplot()+ggtitle("Loan Status for coapplicant income")


Loan_Status_Property <- ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Property_Area)+ggtitle("Loan Status for property area")



Loandata<-rbind(train[,2:12],test[,2:12])


Aplicant_Emp <- ggplot(data=Loandata[Loandata$ApplicantIncome<20000,],aes(ApplicantIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.)


Coapplicant_Emp <- ggplot(data=Loandata[Loandata$ApplicantIncome<20000,],aes(CoapplicantIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.)

Loandata2 <-mutate(Loandata,TotalIncome=ApplicantIncome+CoapplicantIncome)

Married_Gender <- ggplot(data=Loandata2,aes(TotalIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.)




Loandata2$Married[is.na(Loandata2$Married) & Loandata2$CoapplicantIncome==0]<-"No"
Loandata2$Married[is.na(Loandata2$Married)]<- "Yes"


Loandata2[is.na(Loandata2$Gender) & is.na(Loandata2$Dependents),]



Loandata2$Gender[is.na(Loandata2$Gender) & is.na(Loandata2$Dependents)] <- "Male"
Dependent_Gender <- ggplot(Loandata2,aes(x=Dependents, fill=Gender)) + geom_bar() + facet_grid(.~Married)

Loandata2$Dependents[is.na(Loandata2$Dependents) & Loandata2$Married=="No"]<- "0"

out <- Loandata2[(Loandata2$Gender=="Male" & Loandata2$Married=="Yes"),c(3,6:9,11)]
outtrain<-out[!is.na(out$Dependents),]
outtest<- out[is.na(out$Dependents),]




depFit <- rpart(data=outtrain,Dependents~.,xval=3)
rpart_plot_train <- fancyRpartPlot(depFit)


#accuracy_train
p <- predict(depFit,outtrain,type="class")
accuracy_train <- sum(p==outtrain[,1])/length(p)
accuracy_train



Loandata2$Dependents[is.na(Loandata2$Dependents) & Loandata2$Gender=="Male" & Loandata2$Married == "Yes"]<- predict(depFit,newdata=outtest,type="class")



gender_train <- Loandata2[!is.na(Loandata2$Gender),1:7]
gender_test <- Loandata2[is.na(Loandata2$Gender),1:7]
gender_Fit <-rpart(data=gender_train,Gender~.,xval=3)
gender_plot <- fancyRpartPlot(gender_Fit)



#accuracy_gender
p_gen <- predict(gender_Fit,gender_train,type="class")
accuracy_gen <- sum(p_gen==gender_train[,1])/length(p_gen)
accuracy_gen




Loandata2$Gender[is.na(Loandata2$Gender)]<-predict(gender_Fit,gender_test,type="class")



Loandata2$Self_Employed[is.na(Loandata$Self_Employed)] <- "No"

Loandata2$Credit_History<-recode(Loandata2$Credit_History,"NA=2")

loan_train<-Loandata2[!is.na(Loandata2$LoanAmount) & Loandata2$LoanAmount<500,c(1:8,10)]
loan_test <- Loandata2[is.na(Loandata2$LoanAmount),c(1:8,10)]
loan_Fit <- glm(data=loan_train,LoanAmount~.,na.action=na.exclude)
#impute
Loandata2$LoanAmount[is.na(Loandata2$LoanAmount)] <- predict(loan_Fit,newdata=loan_test)


Loandata2$Loan_Amount_Term <- as.factor(Loandata2$Loan_Amount_Term)
Loan_Amount_Term_Plot <- ggplot(data=Loandata2,aes(x=Loan_Amount_Term))+geom_bar()


Loandata2$Loan_Amount_Term[is.na(Loandata2$Loan_Amount_Term)]<-"360"
Loandata2$Loan_Amount_Term <- recode(Loandata2$Loan_Amount_Term,"'350'='360';'6'='60'")



numDependents <- recode(Loandata2$Dependents,"'3+'='3' ")
numDependents <- as.numeric(as.character(numDependents))
Loandata2$FamilySize <- ifelse((Loandata2$CoapplicantIncome>0 |Loandata2$Married=="Y"),numDependents+2,numDependents+1)
Loandata2$IncomePC <- Loandata2$TotalIncome/Loandata2$FamilySize




Loandata2$LoanAmountByTotInc <- Loandata2$LoanAmount/Loandata2$TotalIncome
Loandata2$LoanAmountPC <- Loandata2$LoanAmount/Loandata2$IncomePC

Loandata2$Loan_Amount_Term <- as.numeric(as.character(Loandata2$Loan_Amount_Term))
Loandata2$LoanPerMonth <- Loandata2$LoanAmount/Loandata2$Loan_Amount_Term

Loandata2$LoanPerMOnthByTotInc  <- Loandata2$LoanPerMonth/Loandata2$TotalIncome
Loandata2$LoanPerMonthPC <- Loandata2$LoanPerMonth/Loandata2$LoanAmountPC

#make loan term variable factor again
Loandata2$Loan_Amount_Term <- as.factor(Loandata2$Loan_Amount_Term)



bins<-cut(Loandata2$ApplicantIncome,breaks=20)
Applicant_bar_plot <- barplot(table(bins),main="Applicant Income")



logbins<-cut(ifelse(Loandata2$ApplicantIncome<2.72,0,log(Loandata2$ApplicantIncome)),breaks=20)
Log_Applicant_bar_plot <- barplot(table(logbins),main="Log of Applicant Income")



Loandata2$LogApplicantIncome <- ifelse(Loandata2$ApplicantIncome<2.72,0,log(Loandata2$ApplicantIncome))
Loandata2$LogCoapplicantIncome <- ifelse(Loandata2$CoapplicantIncome<2.72,0,log(Loandata2$CoapplicantIncome))

summary(Loandata2$LoanAmount)



Loandata2$LogLoanAmount <- log(Loandata2$LoanAmount)

summary(Loandata2$TotalIncome)


Loandata2$LogTotalIncome <- log(Loandata2$TotalIncome)

summary(Loandata2$IncomePC)



Loandata2$IncomePC <- log(Loandata2$IncomePC)

summary(Loandata2$LoanAmountByTotInc)



summary(Loandata2$LoanAmountPC)


Loandata2$LogLoanAmountPC <- log(1000*Loandata2$LoanAmountPC)

summary(Loandata2$LoanPerMonth)



Loandata2$LogLoanPerMOnth <- log(Loandata2$LoanPerMonth)

summary(Loandata2$LoanPerMOnthByTotInc)

summary(Loandata2$LoanPerMonthPC)



Loandata2$LogLoanPerMOnthPC <- log(Loandata2$LoanPerMonthPC)

nums <- sapply(Loandata2,class)=="numeric"
numvars <- Loandata2[,nums]
var<-cor(numvars)
vector_value<-as.vector(var) 
id1<- rep(rownames(var),17)
id2<-as.vector(sapply(rownames(var),function(x)rep(x,17)))
d<-data.frame(vector_value,id1,id2)
d<-d[d$v>0.8 & d$v<1,]
d


d<-d[c(1:5,8),]
d


Loandata2<-Loandata2[,!(names(Loandata2) %in% d$id1)]

newtrain <- cbind(Loan_Status=train$Loan_Status,Loandata2[1:614,])

#bogus Loan status for test set
Loan_Status <- as.factor(sample(c("N","Y"),replace=TRUE,size=dim(test)[1]))
newtest <- cbind(Loan_Status,Loandata2[615:981,])

#create task
trainTask <- makeClassifTask(data = newtrain,target = "Loan_Status")
testTask <- makeClassifTask(data = newtest, target = "Loan_Status")

#normalize the variables
trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")


tree <- makeLearner("classif.rpart", predict.type = "response")

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#Search for hyperparameters
treepars <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)

#try 100 different combinations of values
tpcontrol <- makeTuneControlRandom(maxit = 100L)

#hypertune the parameters
rm(acc)
set.seed(11)
treetune <- tuneParams(learner = tree, resampling = set_cv, 
                       task = trainTask, par.set = treepars, control = tpcontrol, measures = acc)
treetune



#using hyperparameters for modeling
tunedtree <- setHyperPars(tree, par.vals=treetune$x)

#train the model
treefit <- train(tunedtree, trainTask)
par(mfrow=c(1,1))
fancyRpartPlot(getLearnerModel(treefit))


#make predictions
treepred <- predict(treefit, testTask)

#create a submission file
submissionfile <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = treepred$data$response)
write.csv(submissionfile, "C:\\Users\\Anuj\\Desktop\\TMBS\\Loan_Submission.csv",row.names = T)




