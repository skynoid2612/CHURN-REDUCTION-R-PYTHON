####################CHURN REDUCTION####################
#Introduction
#The objective of this Case is to predict customer behaviour 
#and to develop an algorithm to predict the churn score based on usage pattern.

####################IMPORTING THE DATA SET####################
setwd('D:/Data Science/EDWISOR/2_PORTFOLIO/project 3')
Train_data = read.csv(file = 'Train_data.csv', header = TRUE)
Test_data = read.csv(file = 'Test_data.csv', header = TRUE)
data = rbind(Train_data, Test_data)
summary(data)
phone_numbers = data$phone.number
new_dataset  = subset(x = data, select = -phone.number)
DataCombine::rmExcept(c('new_dataset', 'phone_numbers'))
new_dataset = as.data.frame(new_dataset)
str(new_dataset)

new_dataset$Churn = as.factor(new_dataset$Churn)
new_dataset$Churn = as.numeric(new_dataset$Churn)
new_dataset$Churn[new_dataset$Churn == 1] = 0
new_dataset$Churn[new_dataset$Churn == 2] = 1 

categorical_columns = c('state','area.code',
                        'international.plan','voice.mail.plan', 'Churn')
numerical_columns = c('number.vmail.messages','total.day.minutes','total.day.calls','total.day.charge',
                      'total.eve.minutes','total.eve.calls','total.eve.charge','total.night.minutes',
                      'total.night.calls','total.night.charge','total.intl.minutes','total.intl.calls','total.intl.charge')
#CONVERTING THE DATASET
for (col in categorical_columns){new_dataset[,col] = as.factor(new_dataset[,col])}
str(new_dataset)
summary(new_dataset)
########################## EDA ##########################
############# MISSING VALUE CHECK #############
anyNA(new_dataset)
# RESULT IS FALSE NO MISSING VALUE PRESENT IN THE RESPECTIVE DATA SET


############# OUTLIER ANALYSIS #############
library(ggplot2) 

numeric_index = sapply(new_dataset,is.numeric)
numeric_data = new_dataset[,numeric_index] # creating the dataset with numerical varaible only
cnames = colnames(numeric_data) # fetching column names
for (i in 1:length(cnames)) # Ploting the boxplot
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(new_dataset))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Churn")+
           ggtitle(paste("Box plot of Churn for",cnames[i])))
}
rm(numeric_data,i)

#gridExtra::grid.arrange(gn1,gn2,gn3, ncol = 3)
#gridExtra::grid.arrange(gn4,gn5,gn6, ncol = 3)
#gridExtra::grid.arrange(gn7,gn8,gn9, ncol = 3)
#gridExtra::grid.arrange(gn10,gn11,gn12, ncol = 3)
#gridExtra::grid.arrange(gn13,gn14,gn15, ncol = 3)

rm(gn1,gn2,gn3,gn4,gn5,gn6,gn7,gn8,gn9,gn10,gn11,gn12,gn13,gn14,gn15)

#Mostly all the feature has outliers
# SO WE HAVE TWO CHOICE TO DEAL WITH THE OUTLIERS
# 1. DELETE THE OUTLIERS FROM THE DATA SET AND PERFORM THE FURTHER ANALYSIS
# 2. IMPUTE THE OUTLIER WITH IMPTATION TECHNIQUE OR ANY STATISTICAL TECHNIQUE

# Process1 will have the deletation technique 
Process1 =  new_dataset
for(i in cnames)
{
  val = Process1[,i][Process1[,i] %in% boxplot.stats(Process1[,i])$out]
  Process1 = Process1[which(!Process1[,i] %in% val),]
} # all the outliear got removed after applying this loop
#Around 17% of the outliear are removed


#Process2  will have the outlier imputation technique
Process2 = new_dataset
for(i in cnames)
{
  val = Process2[,i][Process2[,i] %in% boxplot.stats(Process2[,i])$out]
  Process2[,i][Process2[,i] %in% val] = NA
}

anyNA(Process2)
library(DMwR)
Process2 = knnImputation(Process2,k=5)

anyNA(Process2)


############# FEATURE SELECTION #############
library(corrplot)

#Feature selection for numerical type data set

corrplot(corr = cor(new_dataset[,numerical_columns]),method = 'number')

#Observation:
#As expected, with repect to the above figure following feature are highly correlated with each other :
  
# 1. total_day_charge and total_day_minutes (Correlation : 1)
# 2. total_eve_charge and total_eve_minutes (Correlation : 1)
# 3. total_night_charge and total_night_minutes (Correlation : 1)
# 4. total_intl_charge and total_intl_minutes (Correlation : 1)
#It's obvisous, as the amount charges by the operators are directly proportaional to total minutes

# Deleting the feature
Process1 = subset(Process1, select = -c(total.day.minutes,total.eve.minutes,total.night.minutes,total.intl.minutes))

Process2 = subset(Process2, select = -c(total.day.minutes,total.eve.minutes,total.night.minutes,total.intl.minutes))

# Feature Selection for categorical data
factor_index = sapply(Process1, is.factor)
factor_data = Process1[, factor_index]

for (i in 1:length(colnames(factor_data)))
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn,factor_data[,i])))
}


#1] "area.code"
#Pearson's Chi-squared test
#data:  table(factor_data$Churn, factor_data[, i])
#X-squared = 0.013014, df = 2, p-value = 0.9935
# As p-value is greater than 0.05, Feature should be remove

Process1 = subset(Process1, select = -area.code)
Process2 = subset(Process2, select = -area.code)

############# FEATURE SCALING #############
#Checking the distribution of the numerical feature
#hist(Process1$number.vmail.messages) # HISTOGRAM_number.vmail.messages
#hist(Process1$total.day.calls) # HISTOGRAM_total.day.calls
#hist(Process1$total.day.charge) # HISTOGRAM_total.day.charge
#hist(Process1$total.eve.calls) # HISTOGRAM_total.eve.calls
#hist(Process1$total.night.calls) # HISTOGRAM_total.night.calls
#hist(Process1$total.night.charge) # HISTOGRAM_total.night.charge
#hist(Process1$total.intl.calls) # HISTOGRAM_total.intl.calls
#hist(Process1$total.intl.charge) # HISTOGRAM_total.intl.charge
#hist(Process1$number.customer.service.calls) # HISTOGRAM_number.customer.service.calls

# INFERENCE:
# As THE DATA IS NOT NORMALIZED SO WE WILL USE Min-Max scaling METHOD


numerical_columns = c("account.length","number.vmail.messages" ,
                      "total.day.calls","total.day.charge","total.eve.calls","total.eve.charge",
                      "total.night.calls" ,"total.night.charge","total.intl.calls","total.intl.charge","number.customer.service.calls")
#Normalisation
for (i in numerical_columns)
{
    print(i)
    Process1[,i] = (Process1[,i] - min(Process1[,i]))/(max(Process1[,i] - min(Process1[,i])))
}

for (i in numerical_columns)
{
  print(i)
  Process2[,i] = (Process2[,i] - min(Process2[,i]))/(max(Process2[,i] - min(Process2[,i])))
}






########################## MODEL DEVELOPMENT ##########################

############# Spliting the dataset in test and train data #############
set.seed(123)
#Process1
train_index = caret::createDataPartition(Process1$Churn, p = 0.7, list = FALSE)
train = Process1[train_index,]
test = Process1[-train_index,]
#Process2
train_index2 = caret::createDataPartition(Process2$Churn, p = 0.7, list = FALSE)
train2 = Process2[train_index2,]
test2 = Process2[-train_index2,]


#MODEL DEVELOPMENT AND MODEL EVALUATION (CLASSIFICATION MODEL)
#As the dependent varaible is categorical varaibles, model selection will be done based on the accuracy of classification models. Following Machine Learning algorithm will be use to develop the classfication model:
  
#1 Logistic Regression Algorithm.
#2 Decision Tree Algorithm.
#3 Random Forest Algorithm.
#4 k-Nearest Neighbors Algorithm


############# 1 Logistic Regression Algorithm.#############
#Process1
logistic_model = glm(Churn~.,train, family = 'binomial')
summary(logistic_model)
logistic_predict1 = predict(logistic_model, test[,-15], type = 'response')
predicted_logit = as.factor(ifelse(logistic_predict1 > 0.5, 1,0))
caret::confusionMatrix(test$Churn, as.factor(predicted_logit))

#Accuracy : 0.9066

#Process2
logistic_model2 = glm(Churn~.,train2, family = 'binomial')
summary(logistic_model2)
logistic_predict2 = predict(logistic_model2, test2[,-15], type = 'response')
predicted_logit2 = as.factor(ifelse(logistic_predict2 > 0.5, 1,0))
caret::confusionMatrix(test2$Churn, as.factor(predicted_logit2))

#Accuracy : 0.8666

# With respect to the confusion matrix Process1 tent to perform far better as compare to process2

############# 2 Decision Tree Algorithm.############# 
library(C50)
#Process1
c50model = C5.0(Churn~., train, trails = 100, rules = TRUE)
summary(c50model)
c50_predict1 = predict(c50model, test[,-15])

caret::confusionMatrix(test$Churn, c50_predict1)

#Accuracy : 0.9565
#Process2
c50model2 = C5.0(Churn~., train2, trails = 100, rules = TRUE)
summary(c50model2)
c50_predict2 = predict(c50model2, test2[,-15])

caret::confusionMatrix(test2$Churn, c50_predict2)

#Accuracy : 0.9213

# Similar phenomena observed in Decision tree modeling
# Process1 produce better respect to Process2

############# 3 Random Forest Algorithm.############# 
#Process1
RF_model = randomForest::randomForest(x= train[,-15], y = train$Churn, importance = TRUE, ntree = 500)
summary(RF_model)
rf_predict1 = predict(RF_model, test[,-15])
caret::confusionMatrix(test$Churn, rf_predict1)

#Accuracy : 0.9597

#Process2
RF_model2 = randomForest::randomForest(x= train2[,-15], y = train2$Churn, importance = TRUE, ntree = 500)
summary(RF_model2)
rf_predict2 = predict(RF_model2, test2[,-15])
caret::confusionMatrix(test2$Churn, rf_predict2)

#Accuracy : 0.9213

# Similar phenomena observed in random forest modeling
# Process1 produce better respect to Process2

############# 4 k-Nearest Neighbors Algorithm#############
#Process1
for (i in c('state', 'international.plan','voice.mail.plan' ))
{
  train[,i] = as.factor(as.numeric(train[,i]))
  test[,i] = as.factor((as.numeric(test[,i])))
}
knn_predict1 = class::knn(train[,1:14], test[,1:14], train$Churn, k = 7)
caret::confusionMatrix(test$Churn, knn_predict1)

#Accuracy : 0.8969

#Process2
for (i in c('state', 'international.plan','voice.mail.plan' ))
{
  train2[,i] = as.factor(as.numeric(train2[,i]))
  test2[,i] = as.factor((as.numeric(test2[,i])))
}
knn_predict2 = class::knn(train2[,1:14], test2[,1:14], train2$Churn, k = 7)
caret::confusionMatrix(test2$Churn, knn_predict2)

#Accuracy : 0.8592

# Similar phenomena observed in KNN modeling
# Process1 produce better respect to Process2


#Final Inference:

#As per the above analysis and result genrated.
#Random Forest algorithm tend to provide the better result in respect to this data set
#Moreover, outlier imputed dataset preformance is quite low as compare to the outlier deleted dataset
#In this scenario We are going to select the Random Forest Algorithm and we will finalize the outlier deletation technique

