library("readxl")

#choose excel file to read
data <- read_xlsx(file.choose())

#gives number of objects and variable 
dim(data)

#gives first 10 rows of data
head(data,10)

#gives last 10 rows of data
tail(data,10)

#view printed data 
View(data)

#shows variable types
str(data)

#gives output of active employees vs non-active employees in table format
table(data$status)

#percentage of active employees in organization
(170/686)*100 

#gives summary of entire data set
summary(data)

#changing variables to factor types (more interested in their levels rather than mathematical means)
data$status <- as.factor(data$status)
data$job_level <- as.factor(data$job_level)
data$no_of_promotions <- as.factor(data$no_of_promotions)
data$risk_of_attrition <- as.factor(data$risk_of_attrition)
data$potential_rating <- as.factor(data$potential_rating)

#gives summary of changed data set
summary(data)

library(summarytools)

#more detailed summary table
dfSummary(data)

#gives number of missing values per column (variable)
colSums(is.na(data))

#new factor variable for difference of performances between 2019 and 2017
data$var_rating <- as.factor(data$performance_rating_2018 - data$performance_rating_2017)
data$var_rating

#new variable for percent salary change
data$percent_salary_change <- (data$salary_2018-data$salary_2017)/data$salary_2017*100
data$percent_salary_change

#new variable for age(present year was 2018)
data$age <- 2018 - data$year_of_birth
data$age

#summaries of new variables
dfSummary(data$percent_salary_change)

dfSummary(data$age)

dfSummary(data$var_rating)

#dropping variables that were used for new variables
data[ ,c('year_of_birth', 'salary_2017', 'salary_2018', 'performance_rating_2017', 'performance_rating_2018')] <- list(NULL)

#dropping unused/unimportant variables
data[, c('hire_date', 'e_code')] <- list(NULL)

library(vcd)

#create a coloured proportion chart of gender and status
mosaic(~ gender + status, data=data, gp=gpar(fill=matrix(c("red","blue"),2,2)))

#analysis of status
tab <- table(data$status) #table of status
tab #516 active employees, 170 inactive employees
prop.table(tab) #proportion table of status-25% inactive, 75% active employees

#analysis of status and gender
tab2 <- table(data$status,data$gender) #table of status and gender
tab2 #136 active female employees, 43 inactive females employees, 380 active male employees, 127 inactive male employees
round(prop.table(tab2,1)*100, digits=2) #rounded percentages of status and gender (status perspective)
#In active employees-26% female, 74% male. In inactive employees-25% female, 75% male.
round(prop.table(tab2,2)*100, digits=2) #rounded percentages of status and gender (gender perspective)
#For female employees-76% are active, 24% are inactive. For male employees-75% are active, 25% are inactive.
chisq.test(tab2)

#analysis of service agreement and status
mosaic(~ service_agreement + status, data=data, gp=gpar(fill=matrix(c("red","blue"),2,2)))
tab3 <- table(data$status,data$service_agreement)
tab3 #422 active no service agreement, 94 active yes service agreement, 154 inactive no service agreement,16 active yes service agreement
round(prop.table(tab3,1)*100, digits=2) #In active employees-82% no service agreement, 18% yes service agreement. In inactive employees-90% no service agreement, 10% yes service agreement 
round(prop.table(tab3,2)*100, digits=2) #For no service agreement-74% inactive employees, 26% active employees. For yes service agreement-85% active employees, 15% inactive employees
chisq.test(tab3) 

#analysis of job level and status
mosaic(~ job_level + status, data=data, gp=gpar(fill=matrix(c("red","blue", "green", "black", "yellow"),5,2)))
tab4 <- table(data$status,data$job_level) 
tab4 
round(prop.table(tab4,1)*100, digits=2)
round(prop.table(tab4,2)*100, digits=2)
chisq.test(tab4) 

#analysis of var rating and status
mosaic(~ var_rating + status, data=data, gp=gpar(fill=matrix(c("red","blue", "green", "black", "yellow"),5,2)))
tab5 <- table(data$status,data$var_rating)
round(prop.table(tab5,1)*100, digits=2)
round(prop.table(tab5,2)*100, digits=2)
chisq.test(tab5)

#analysis of no. of promotions and status
tab6 <- table(data$status,data$no_of_promotions)
round(prop.table(tab6,1)*100, digits=2)
round(prop.table(tab6,2)*100, digits=2)
chisq.test(tab6)

#analysis of attrition and status
mosaic(~ risk_of_attrition + status, data=data, gp=gpar(fill=matrix(c("red","blue", "green", "black"),4,2)))
tab7 <- table(data$status,data$risk_of_attrition)
round(prop.table(tab7,1)*100, digits=2)
round(prop.table(tab7,2)*100, digits=2)
chisq.test(tab7)

#analysis of potential rating and status
mosaic(~ potential_rating + status, data=data, gp=gpar(fill=matrix(c("red","blue", "green", "black", "yellow"),5,2)))
tab8 <- table(data$status,data$potential_rating)
round(prop.table(tab8,1)*100, digits=2)
round(prop.table(tab8,2)*100, digits=2)
chisq.test(tab8)

#analysis of awards and status
mosaic(~ awards + status, data=data, gp=gpar(fill=matrix(c("red","blue"),2,2)))
tab9 <- table(data$status,data$awards)
round(prop.table(tab9,1)*100, digits=2)
round(prop.table(tab9,2)*100, digits=2)
chisq.test(tab9)

#analysis of sign on bonus and status
mosaic(~ signon + status, data=data, gp=gpar(fill=matrix(c("red","blue"),2,2)))
tab10 <- table(data$status,data$signon)
round(prop.table(tab10,1)*100, digits=2)
round(prop.table(tab10,2)*100, digits=2)
chisq.test(tab10)

library(ggplot2)

#analysis of status and percent salary change
p1 <- ggplot(data, aes(x=status,y=percent_salary_change)) + geom_violin()
p1 + geom_violin(trim=T) + geom_violin(draw_quantiles = c(0.25,0.5,0.75)) #violin plot of percent salary change 
avg1 <- by(data$percent_salary_change, data$status, mean) #mean 
avg1
med1 <- by(data$percent_salary_change, data$status, median) #median
med1
mini1 <- by(data$percent_salary_change, data$status, min) #minimum
mini1
maxi1 <- by(data$percent_salary_change, data$status, max) #maximum
maxi1

#analysis of status and age
p2 <- ggplot(data, aes(x=status,y=age)) + geom_violin()
p2 + geom_violin(trim=T) + geom_violin(draw_quantiles = c(0.25,0.5,0.75)) 
avg2 <- by(data$age, data$status, mean)  
avg2
med2 <- by(data$age, data$status, median) 
med2
mini2 <- by(data$age, data$status, min) 
mini2
maxi2 <- by(data$age, data$status, max) 
maxi2

#analysis of status and distance from home
p3 <- ggplot(data, aes(x=status,y=distance_from_home) + geom_violin()
p3 + geom_violin(trim=T) + geom_violin(draw_quantiles = c(0.25,0.5,0.75)) 
avg3 <- by(data$distance_from_home, data$status, mean) 
avg3
med3 <- by(data$distance_from_home, data$status, median) 
med3
mini3 <- by(data$distance_from_home, data$status, min) 
mini3
maxi3 <- by(data$distance_from_home, data$status, max) 
maxi3

#analysis of status and manager sat
p4 <- ggplot(data, aes(x=status,y=manager_sat)) + geom_violin()
p4 + geom_violin(trim=T) + geom_violin(draw_quantiles = c(0.25,0.5,0.75)) 
avg4 <- by(data$manager_sat,data$status, mean)  
avg4
med4 <- by(data$manager_sat,data$status, median) 
med4
mini4 <- by(data$manager_sat, data$status, min) 
mini4
maxi4 <- by(data$manager_sat, data$status, max)
maxi4


#hypothesis t-testing
t.test(manager_changes ~ status, data=data)

t.test(manager_sat ~ status, data=data)

t.test(employee_sat ~ status, data=data)

t.test(bonus ~ status, data=data)

t.test(no_courses_taken ~ status, data=data)

t.test(time_in_position ~ status, data=data)

t.test(percent_salary_change ~ status, data=data)

#removing variables that were insignificant
data[ , c('gender', 'job_level', 'signon', 'age', 'manager_changes', 'bonus', 'time_in_position')] <- list(NULL)
names(data)

library(fastDummies)

#adding dummy variables
results_dummy <- dummy_cols(data, remove_most_frequent_dummy = T)
names(results_dummy)

#removing original variables
results_dummy[ ,c('status', 'service_agreement', 'no_of_promotions', 'risk_of_attrition', 'potential_rating', 'awards')] <- list(NULL)
names(results_dummy)
colnames(results_dummy)[25] <- "var_rating_minus1"
colnames(results_dummy)[26] <- "var_rating_minus3"
names(results_dummy)

library(caret)

#shuffle data to avoid bias
results_dummy <- results_dummy[sample(nrow(results_dummy)),]

#creating training data index, 70% of total rows in columns
trainIndex <- createDataPartition(results_dummy$status_1, p=0.7, list=FALSE)
trainIndex

#creating training data
X_train <- results_dummy[trainIndex,]

#creating testing data
X_test <- results_dummy[-trainIndex,]

#check training data
dim(X_train)

#check testing data
dim(X_test)

library(randomForest)

#change testing and training data of status to factor type
X_train$status_1 <- as.factor(X_train$status_1)
X_test$status_1 <- as.factor(X_test$status_1)

#random forest on training data
model_1 <- randomForest(status_1 ~., data=X_train)
model_1

str(X_train)

#output of tree
getTree(model_1,1)

library(randomForestExplainer)

#finding important variable in the tree
importance_frame <- measure_importance(model_1)
importance_frame

#present GINI plot
varImpPlot(model_1)

predications2 <- predict(model_1, X_train)

table(X_train$status_1, predications2)

#prediction percentage for train
(354+126)/(nrow(X_train))*100

predictions3 <- predict(model_1, X_test)

table(X_test$status_1, predictions3)

#prediction percentage for test
(155+39)/(nrow(X_test))*100

library(pROC)

#model prediction evaluation
result.roc <- roc(as.numeric(as.character(X_test$status_1)), as.numeric(as.character(predictions3)))
result.roc

plot(result.roc)

