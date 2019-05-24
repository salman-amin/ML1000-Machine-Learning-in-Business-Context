###############################################################################################
####################   Assignment 1    #####################
###############################################################################################
#import packages;
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)
library(rpart)
library(e1071)
library(C5)

#################################################################################
##########################    Data Exploration     ##############################
#################################################################################
getwd();
data <- read.table("data.txt", sep =",", header = FALSE, dec =".")


#check distribution of target variable

colnames(data)[colnames(data)=="V1"] <- "age"
colnames(data)[colnames(data)=="V2"] <- "workclass"
colnames(data)[colnames(data)=="V3"] <- "fnlwgt"
colnames(data)[colnames(data)=="V4"] <- "education"
colnames(data)[colnames(data)=="V5"] <- "education-num"
colnames(data)[colnames(data)=="V6"] <- "marStat"
colnames(data)[colnames(data)=="V7"] <- "occupation"
colnames(data)[colnames(data)=="V8"] <- "relationship"
colnames(data)[colnames(data)=="V9"] <- "race"
colnames(data)[colnames(data)=="V10"] <- "sex"
colnames(data)[colnames(data)=="V11"] <- "capital-gain"
colnames(data)[colnames(data)=="V12"] <- "capital-loss"
colnames(data)[colnames(data)=="V13"] <- "hrs-per-week"
colnames(data)[colnames(data)=="V14"] <- "native-country"
colnames(data)[colnames(data)=="V15"] <- "income"
data$ID <- seq.int(nrow(data))

str(data)
summary(data)



#search for missing data
is.na(data) = data=='?'
is.na(data) = data==' ?'
sum(is.na(data))
mean(is.na(data))
#erase those rows
data = na.omit(data)

#outliers
boxplot(data, horizontal = T)
boxplot(data$`capital-gain`, horizontal = T)
boxplot(data$`capital-loss`, horizontal = T)


attach(data)

freq_tbl=table(workclass)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$workclass, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(education)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$education, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(`education-num`)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$education-`education-num`, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(marStat)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$marStat, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(occupation)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$occupation, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(relationship)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$relationship, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(race)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$race, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(sex)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$sex, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(data$`natice-country`)
head(freq_tbl)
ggplot(data) + aes(x=data$`native-country`, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

list_of_numcols = sapply(data, is.numeric)
numcols = data[ , list_of_numcols]
melt_data = melt(numcols, id.vars=c("ID"))
ggplot(data = melt_data, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')
summary(data$age)
summary(data$`education-num`)
summary(data$`capital-gain`)
summary(`capital-gain`)
summary(`capital-loss`)
summary(`hrs-per-week`)
ggplot(data) + aes(x=as.numeric(data$age), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')
ggplot(data, aes(x=data$age, y=data$income)) + 
  geom_point()
ggplot(data) + aes(x=as.numeric(data$`education-num`), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')
ggplot(data, aes(x=data$`education-num`, y=data$income)) + 
  geom_point()
ggplot(data) + aes(x=as.numeric(data$`capital-gain`), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')
ggplot(data, aes(x=data$`capital-gain`, y=data$income)) + 
  geom_point()
ggplot(data) + aes(x=as.numeric(data$`capital-loss`), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')
ggplot(data, aes(x=data$`capital-loss`, y=data$income)) + 
  geom_point()
ggplot(data) + aes(x=as.numeric(data$`hrs-per-week`), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')
ggplot(data, aes(x=data$`hrs-per-week`, y=data$income)) + 
  geom_point()


#################################################################################
 ##########################  Feature Engineering   #############################
#################################################################################
data <- read.table("data.txt", sep =",", header = FALSE, dec =".")

colnames(data)[colnames(data)=="V1"] <- "age"
colnames(data)[colnames(data)=="V2"] <- "workclass"
colnames(data)[colnames(data)=="V3"] <- "fnlwgt"
colnames(data)[colnames(data)=="V4"] <- "education"
colnames(data)[colnames(data)=="V5"] <- "education-num"
colnames(data)[colnames(data)=="V6"] <- "marStat"
colnames(data)[colnames(data)=="V7"] <- "occupation"
colnames(data)[colnames(data)=="V8"] <- "relationship"
colnames(data)[colnames(data)=="V9"] <- "race"
colnames(data)[colnames(data)=="V10"] <- "sex"
colnames(data)[colnames(data)=="V11"] <- "capital-gain"
colnames(data)[colnames(data)=="V12"] <- "capital-loss"
colnames(data)[colnames(data)=="V13"] <- "hrs-per-week"
colnames(data)[colnames(data)=="V14"] <- "native-country"
colnames(data)[colnames(data)=="V15"] <- "income"


#search for missing data
is.na(data) = data=='?'
is.na(data) = data==' ?'
sum(is.na(data))
mean(is.na(data))
#erase those rows
data = na.omit(data)
#need to determine the order of this
#anyDuplicated(data)
#data <- distinct(data)
#we decided not to remove duplicate data

#outliers
boxplot(data, horizontal = T)
boxplot(data$`capital-gain`, horizontal = T)
boxplot(data$`capital-loss`, horizontal = T)

data$fnlwgt <- NULL
data$'capital-gain' <- NULL
data$'capital-loss' <- NULL
data$'native-country' <- NULL


#logic regression

the_data <- data.frame(data)
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
lgreg<- train(income~., the_data, method="LMT", trControl=train_control)
predictions<- predict(lgreg,the_data)
the_data<- cbind(the_data,predictions)
table(the_data$predictions,the_data$income)
#          <=50K  >50K
#<=50K  20801  3039
#>50K    1853  4469
#(20801+4469)/(1853+3039+20801+4469) 83%


#C5.0
set.seed(123)
the_data <- data.frame(data)
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
c5<- train(income~., the_data, method="C5.0", trControl=train_control)
predictions<- predict(c5,the_data)
the_data<- cbind(the_data,predictions)
table(the_data$predictions,the_data$income)
#<=50K  >50K
#<=50K  21163  3205
#>50K    1491  4303
#(21163+4303)/(3205+1491+21163+4303) 84%

#logitboost model
set.seed(123)
the_data <- data.frame(data)
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
logmodel<- train(income~., the_data, method="LogitBoost", trControl=train_control)
predictions<- predict(logmodel,the_data)
the_data<- cbind(the_data,predictions)
table(the_data$predictions,the_data$income)
#<=50K  >50K
#<=50K  21533  4454
#>50K    1121  3054
#(21533+3054)/(4454+1121+21533+3054) 81%



#svm model

the_data <- data.frame(data)
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
svmModel<- train(income~., the_data, method="svmLinearWeights2", trControl=train_control)
predictions<- predict(svmModel,the_data)
the_data<- cbind(the_data,predictions)
table(the_data$predictions,the_data$income)
#<=50K  >50K
#<=50K  21031  3603
#>50K    1623  3905
#(21031+3905)/(1623+3603+21031+3905) 83%

#naive bayes model

the_data <- data.frame(data)
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
nbm<- train(income~., the_data, method="naive_bayes", trControl=train_control)
predictions<- predict(nbm,the_data)
the_data<- cbind(the_data,predictions)
table(the_data$predictions,the_data$income)
#<=50K  >50K
#<=50K  22647  7422
#>50K       7    86
#(22647+86)/(7422+7+22647+86) 75%

#nnet
the_data <- data.frame(data)
train_control<- trainControl(method="cv", number=10, verboseIter = TRUE)
nnet<- train(income~., the_data, method="nnet", trControl=train_control)
predictions<- predict(nnet,the_data)
the_data<- cbind(the_data,predictions)
table(the_data$predictions,the_data$income)
#<=50K  >50K
#<=50K  20811  3066
#>50K    1843  4442
#(20811+4442)/(3066+1843+20811+4442) 83%

#save model c5
saveRDS(c5, "c5model.rds")
test <- readRDS("c5model.rds")


test <- data.frame(age=c(43),
                   workclass=c(' Local-gov'),
                   education=c(' HS-grad'),
                   'education-num'=c(10),
                   marStat=c(' Widowed'),
                   occupation=(' Sales'),
                   relationship=(' Wife'),
                   race=c(' Black'),
                   sex=c(' Female'),
                   'hrs-per-week'=c(40))
predictions<- predict(lgreg,test)
predictions
