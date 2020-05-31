library(caret)
library(caTools)
library(e1071)
df_bank = read.csv('BankCreditCard.csv')
# check the number of rows and columns
dim(df_bank)
#Checking NA values
sum(is.na(df_bank))
View(df_bank)
names(df_bank)
# describing the dataset
str(df_bank)
#Check Class Bias
q<-table(df_bank$Default_Payment)
plot(q,type="h",lend=2,lwd=80,col="darkred")
#Clearly, there is a class bias, a condition observed 
#when the proportion of events is much smaller than proportion of non-events. 
#Changing columns to factor
cols.to.factors <- c("Gender", "Academic_Qualification","Marital", 
                     "Repayment_Status_Jan","Repayment_Status_Feb","Repayment_Status_March","Repayment_Status_April","Repayment_Status_May","Repayment_Status_June",
                     "Default_Payment")

df_bank[cols.to.factors] <- lapply(df_bank[cols.to.factors], factor)

str(df_bank)

#Since there were males and females as 1 and 2, changing them to factor
# Renaming Male/Female levels for Gender variable
levels(df_bank$Gender)[levels(df_bank$Gender) == "1"] <- "Male"
levels(df_bank$Gender)[levels(df_bank$Gender) == "2"] <- "Female"
table(df_bank$Gender)

# Renaming levels for Academic Qualification
levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "1"] <- "Undergraduate"

levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "2"] <- "Graduate"

levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "3"] <- "Postgraduate"

levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "4"] <- "Professional"

levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "5"] <- "Others"

levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "6"] <- "Unknown"

str(df_bank$Academic_Qualification)
# randomly shuffling the dataset
grp = runif(nrow(df_bank))
df_bank = df_bank[order(grp),]
# Create training and test samples
train.rows<- createDataPartition(y= df_bank$Default_Payment, p=0.7, list = FALSE)
train.data<- df_bank[train.rows,] # 70% data goes in here
table(train.data$Default_Payment)

test.rows<-  createDataPartition(y= df_bank$Default_Payment, p=0.3, list = FALSE)
test.data<- df_bank[test.rows,] # 30% data goes in here
table(test.data$Default_Payment)
# number of records in train and test datasets
nrow(train.data)
nrow(test.data)
# build the logistic regression model
glm_full_model = glm(Default_Payment ~ ., family = "binomial", data=train.data) 
#we do binomial classification

# summarise model
summary(glm_full_model)
#Extracting p values of the variables
# Predicting probabilities obtain from the model

# predict the Y-values
predict_full_model = predict(glm_full_model,test.data,type="response") #response means it gives probabilities


predictions_full_model = ifelse(predict_full_model <=0.5, 0,1) 

# build the confusion matrix
confusionMatrix(table(predicted = predictions_full_model,actual = test.data$Default_Payment))
##Using SVM 
model2 = svm(formula = Default_Payment~., 
             data = train.data, 
             type = 'C-classification', 
             kernel = 'linear') 
pr.svm<-predict(model2, newdata=test.data)
confusionMatrix(table(actual=test.data$Default_Payment,predicted=pr.svm))
#I tried scaling the numerical variables but it turns out be ineffective for the model.
#I also tried including only significant variables in the model. However, there wasn't
#any significance change in the model.



