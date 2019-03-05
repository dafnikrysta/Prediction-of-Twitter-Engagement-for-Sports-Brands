#########################################################################
### Prediction of Online Engagement for Sports Apparel Industry  ########
###                         Regression                                ###
#########################################################################

#### TO BE RUN ONLY AFTER KEYWORDS_FINAL IS RUN #########################

# Set working directory to save intermediate tweet_basetable at the end
setwd("C:/Users/moetoKompiutarche/Documents/IESEG/Social Media Analytics/Group Project")

for (i in c('caret', 'pROC', 'randomForest', 'ROCR')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

# Read in basetable
tmls_complete <- read.csv("basetable_topic_SA_keyword.csv", stringsAsFactors = FALSE)

# Remove automatic and extraneous index value
tmls_complete <- tmls_complete[,-1]



### Split basetable into train and test; use stratified sample to ensure that brands are split evenly
# Set seed to ensure reproducibility
set.seed(1)
train.index <- createDataPartition(tmls_complete$X, p = .7, list = FALSE)
train <- tmls_complete[ train.index,]
test  <- tmls_complete[-train.index,]

# Benchmarks for number of targets
# Spoiler: the model works best on the normalized target variables as there are more evenly distributed
table(tmls_complete$target_fav)
table(tmls_complete$target_fav_norm)

table(tmls_complete$target_RT)
table(tmls_complete$target_RT_norm)

# Function to calculate AUC 
auc <- function(trueval, predval){
  df <- as.data.frame(cbind(trueval,predval))
  names(df) <- c("trueval","predval")
  auc <- roc(trueval~predval,data=df)$auc
  return(auc)
}

# Function to calculate F1 score
F1_score <- function(TP, FP, FN){
  Recall <- TP / (TP + FN)
  Precision <- TP / (TP + FP)
  F1_score <- 2 * (Precision*Recall) / (Precision + Recall)
  return(F1_score)
}


############################ Favorite Target #############################

####################################################################################
###### Regression on winsorized but not normalized binary target for favorite ######

### Method 1: Run forward stepwise logistic regression to pick the best predictors
variables <- names(train)[c(5:7, 10:12, 14:19, 22:32, 34:43, 50, 52:53, 55:66)]
variablesorder <- c()

# Initiate base model to add to
model <- glm(target_fav ~ 1,data=train,family=binomial)

# Create a formula (i.e. a list of the dependent and then independent variables) to use in the initial linear model
formula <- formula(paste("target_fav","~",paste(variables,collapse="+")))

# Order variables based on the size of their effect/predictive power
for(i in c(1:length(variables))){
  #calculate AIC of each model
  info <- add1(model,scope=formula,data=train)
  print(info)
  #get variable with lowest AIC
  orderedvariables <- rownames(info[order(info$AIC),])
  v <- orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  variablesorder <- append(variablesorder,v)
  formulanew <- formula(paste("target_fav","~",paste(variablesorder,collapse = "+")))
  model <- glm(formulanew,data=train,family=binomial)
  print(v)
}

# Initialize the variables to be used in the loop below
auctrain <- rep(0,length(variablesorder)-1)
auctest <- rep(0,length(variablesorder)-1)

# Determine the AUC of each model with an increasing number of variables to create the overall AUC chart
for(i in c(1:(length(variablesorder)-1))){
  vars <- variablesorder[0:i+1]
  print(vars)
  formula <- paste("target_fav","~",paste(vars,collapse="+"))
  model <- glm(formula,data=train,family="binomial")	
  predicttrain <- predict(model,newdata=train,type="response")
  predicttest <- predict(model,newdata=test,type="response")
  auctrain[i] <- auc(train$target_fav,predicttrain)
  auctest[i] <- auc(test$target_fav,predicttest)
} 

# Plot the AUC chart
plot(auctrain, main="AUC for Favorites", col="red", type='l')
lines(auctest,col="blue",type='l')
legend("bottom", legend=c("Train","Test"), ncol=2,bty="n",
       col=c("red", "blue"), lwd = 2)

# Based on the plot, determine the highest AUC with the lowest overtraining (i.e. gap)
finalvariables <- variablesorder[c(0:7)]
formula <- paste("target_fav","~",paste(finalvariables,collapse="+"))
model <- glm(formula,data=train,family="binomial")	
predicttrain <- predict(model,newdata=train,type="response")
predicttest <- predict(model,newdata=test,type="response")
auctrain <- auc(train$target_fav,predicttrain)
auctest <- auc(test$target_fav,predicttest)

# Discover the model created
print(model)
print(auctrain)
print(auctest)


### Model 1 evaluation ###
# Store AUC in dataframe to compare later
model_AUC <- c("train", "test")
AUC_fav <- as.data.frame(model_AUC)
AUC_fav$logistic_target_fav <- c(auctrain, auctest)

# Calculate the number of predicted high engagement based on default glm cutoff of 0.5
predict_target <- ifelse(predicttest > 0.5, 1, 0)
table(predict_target)

# Convert true and predicted targets to factors
true_target <- as.factor(test$target_RT_norm)
predict_target <- as.factor(predict_target)

# Calculate the confusion matrix
CM_target_fav_info <- confusionMatrix(true_target, predict_target)
CM_target_fav <- as.data.frame(CM_target_fav_info[["table"]])
CM_target_fav$Model <- "logistic_target_fav"

# Calculate the F1 score to determine classification accuracy
# Using first our function and then the one from the caret package to check consistency
F1_eval <- F1_score(CM_target_fav$Freq[4], CM_target_fav$Freq[2], CM_target_fav$Freq[3])
F_meas(predict_target, reference = true_target, relevant = "1")

F1_fav <- as.data.frame(c("logistic_target_fav", F1_eval))


### Method 2: Run random forest to compare to logistic regression
train_rf <- train[,c(5:7, 10:12, 14:19, 22:32, 34:43, 48, 50, 52:53, 55:66)]
test_rf <- test[c(5:7, 10:12, 14:19, 22:32, 34:43, 48, 50, 52:53, 55:66)]

# Train the model based on the selected numeric variables above
# modelrf gives a confusion matrix for the entire dataset (i.e. test and train)
train_rf$target_fav <- as.factor(train_rf$target_fav)
modelrf <- randomForest(target_fav ~., data=train_rf, importance=TRUE, ntree=100, maxnodes = 10, cutoff = c(0.1, 0.9))
predictions_train_rf <- predict(modelrf,newdata = train_rf, type = "prob")
predictions_test_rf <- predict(modelrf,newdata = test_rf, type = "prob")
auctrain <- auc(train_rf$target_fav,predictions_train_rf)
auctest <- auc(test_rf$target_fav,predictions_test_rf)

# Examined a histogram to see what an appropriate cutoff might be for this dataset
# Setting the cutoff so high leads to some overtraining and a very poor model
hist(predictions_test_rf[,"0"])
hist(predictions_test_rf[predictions_test_rf[,"0"] < .9,"0"])

### Model 2 evaluation ###
# Store AUC in dataframe to compare later
AUC_fav$randomForest_target_fav <- c(auctrain, auctest)

# Calculate the number of predicted high engagement based on default glm cutoff of 0.3
predict_target <- ifelse(predictions_test_rf[,"1"] > 0.1, 1, 0)
table(predict_target)

# Convert true and predicted targets to factors
true_target <- as.factor(test$target_fav_norm)
predict_target <- as.factor(predict_target)

# If no target is predicted, due to the small target incidence, there will only be one level
# The two lines below ensure that there are two levels even if the dataset only contains one
levels(predict_target) <- c("0", "1")

# Calculate the confusion matrix
CM_RF_target_fav_info <- confusionMatrix(true_target, predict_target)
CM_RF_target_fav <- as.data.frame(CM_RF_target_fav_info[["table"]])
CM_RF_target_fav$Model <- "RF_target_fav"

CM_fav <- rbind(CM_target_fav, CM_RF_target_fav)

# Calculate the F1 score to determine classification accuracy
# Using first our function and then the one from the caret package to check consistency
F1_eval <- F1_score(CM_RF_target_fav$Freq[4], CM_RF_target_fav$Freq[2], CM_RF_target_fav$Freq[3])
F_meas(predict_target, reference = true_target, relevant = "1")

F1_fav <- cbind(F1_fav, c("randomForest_target_fav", F1_eval))


################################################################
###### Regression on normalized binary target for favorite #####

### Method 1: Run forward stepwise logistic regression to pick the best predictors
variables <- names(train)[c(5:7, 10:12, 14:19, 22:32, 34:43, 50, 52:53, 55:66)]
variablesorder <- c()

# Initiate base model to add to
model <- glm(target_fav ~ 1,data=train,family=binomial)

# Create a formula (i.e. a list of the dependent and then independent variables) to use in the initial linear model
formula <- formula(paste("target_fav_norm","~",paste(variables,collapse="+")))

# Order variables based on the size of their effect/predictive power
for(i in c(1:length(variables))){
  #calculate AIC of each model
  info <- add1(model,scope=formula,data=train)
  print(info)
  #get variable with lowest AIC
  orderedvariables <- rownames(info[order(info$AIC),])
  v <- orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  variablesorder <- append(variablesorder,v)
  formulanew <- formula(paste("target_fav_norm","~",paste(variablesorder,collapse = "+")))
  model <- glm(formulanew,data=train,family=binomial)
  print(v)
}

# Initialize the variables to be used in the loop below
auctrain <- rep(0,length(variablesorder)-1)
auctest <- rep(0,length(variablesorder)-1)

# Determine the AUC of each model with an increasing number of variables to create the overall AUC chart
for(i in c(1:(length(variablesorder)-1))){
  vars <- variablesorder[0:i+1]
  print(vars)
  formula <- paste("target_fav_norm","~",paste(vars,collapse="+"))
  model <- glm(formula,data=train,family="binomial")	
  predicttrain <- predict(model,newdata=train,type="response")
  predicttest <- predict(model,newdata=test,type="response")
  auctrain[i] <- auc(train$target_fav_norm,predicttrain)
  auctest[i] <- auc(test$target_fav_norm,predicttest)
} 

# Plot the AUC chart
plot(auctrain, main="AUC for Normalized Favorites", col="red", type='l')
lines(auctest,col="blue",type='l')
legend("bottom", legend=c("Train","Test"), ncol=2,bty="n",
       col=c("red", "blue"), lwd = 2)

# Run the model with the final most significant variables
finalvariables <- variablesorder[c(0:7)]
formula <- paste("target_fav_norm","~",paste(finalvariables,collapse="+"))
model <- glm(formula,data=train,family="binomial")	
predicttrain <- predict(model,newdata=train,type="response")
predicttest <- predict(model,newdata=test,type="response")
auctrain <- auc(train$target_fav_norm,predicttrain)
auctest <- auc(test$target_fav_norm,predicttest)

# Export model coefficients for shiny
write.csv(model$coefficients, "model_fav_coefficients.csv")

# Discover the model created
print(model)
print(auctrain)
print(auctest)

### Model 3 evaluation ###
# Store AUC in dataframe to compare later
AUC_fav$logistic_target_fav_norm <- c(auctrain, auctest)

# Calculate the number of predicted high engagement based on default glm cutoff of 0.5
predict_target <- ifelse(predicttest > 0.5, 1, 0)
table(predict_target)

# Convert true and predicted targets to factors
true_target <- as.factor(test$target_fav_norm)
predict_target <- as.factor(predict_target)

# Calculate the confusion matrix
CM_target_fav_norm_info <- confusionMatrix(true_target, predict_target)
CM_target_fav_norm <- as.data.frame(CM_target_fav_norm_info[["table"]])
CM_target_fav_norm$Model <- "logistic_target_fav_norm"

CM_fav <- rbind(CM_fav, CM_target_fav_norm)

# Calculate the F1 score to determine classification accuracy
# Using first our function and then the one from the caret package to check consistency
F1_eval <- F1_score(CM_target_fav_norm$Freq[4], CM_target_fav_norm$Freq[2], CM_target_fav_norm$Freq[3])
F_meas(predict_target, reference = true_target, relevant = "1")

F1_fav <- cbind(F1_fav, c("logistic_target_fav_norm", F1_eval))


### Method 2: Run random forest to compare to logistic regression
train_rf <- train[c(5:7, 10:12, 14:19, 22:32, 34:43, 46, 50, 52:53, 55:66)]
test_rf <- test[c(5:7, 10:12, 14:19, 22:32, 34:43, 46, 50, 52:53, 55:66)]

# Train the model based on the selected numeric variables above
# modelrf gives a confusion matrix for the entire dataset (i.e. test and train)
train_rf$target_fav_norm <- as.factor(train_rf$target_fav_norm)
modelrf <- randomForest(target_fav_norm ~ ., data=train_rf, importance=TRUE, ntree=120, maxnodes = 15)
predictions_train_rf <- predict(modelrf,newdata = train_rf, type = "prob")
predictions_test_rf <- predict(modelrf,newdata = test_rf, type = "prob")
auctrain <- auc(train_rf$target_fav_norm,predictions_train_rf)
auctest <- auc(test_rf$target_fav_norm,predictions_test_rf)

### Model 4 evaluation ###
# Store AUC in dataframe to compare later
AUC_fav$RF_target_fav_norm <- c(auctrain, auctest)

# Calculate the number of predicted high engagement based on default glm cutoff of 0.5
predict_target <- ifelse(predictions_test_rf[,"1"] > 0.5, 1, 0)
table(predict_target)

# Convert true and predicted targets to factors
true_target <- as.factor(test$target_fav_norm)
predict_target <- as.factor(predict_target)

# Calculate the confusion matrix
CM_RF_target_fav_norm_info <- confusionMatrix(true_target, predict_target)
CM_RF_target_fav_norm <- as.data.frame(CM_RF_target_fav_norm_info[["table"]])
CM_RF_target_fav_norm$Model <- "randomForest_target_fav_norm"

CM_fav <- rbind(CM_fav, CM_RF_target_fav_norm)


# Calculate the F1 score to determine classification accuracy
# Using first our function and then the one from the caret package to check consistency
F1_eval <- F1_score(CM_RF_target_fav_norm$Freq[4], CM_RF_target_fav_norm$Freq[2], CM_RF_target_fav_norm$Freq[3])
F_meas(predict_target, reference = true_target, relevant = "1")

F1_fav <- cbind(F1_fav, c("randomForest_target_fav_norm", F1_eval))


### Deterine which model is best
# Print model evaluations for four models
AUC_fav
CM_fav
F1_fav

### Export evalutions to csv for use in Shiny
write.csv(AUC_fav, "AUC_fav.csv")
write.csv(CM_fav, "CM_fav.csv")
write.csv(F1_fav, "F1_fav.csv")






############################ Retweet Target #############################

####################################################################################
###### Regression on winsorized but not normalized binary target for retweet ######

### Method 1: Run forward stepwise logistic regression to pick the best predictors
variables <- names(train)[c(5:7, 10:12, 14:19, 22:32, 34:43, 50, 52:53, 55:66)]
variablesorder <- c()

# Initiate base model to add to
model <- glm(target_RT ~ 1,data=train,family=binomial)

# Create a formula (i.e. a list of the dependent and then independent variables) to use in the initial linear model
formula <- formula(paste("target_RT","~",paste(variables,collapse="+")))

# Order variables based on the size of their effect/predictive power
for(i in c(1:length(variables))){
  #calculate AIC of each model
  info <- add1(model,scope=formula,data=train)
  print(info)
  #get variable with lowest AIC
  orderedvariables <- rownames(info[order(info$AIC),])
  v <- orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  variablesorder <- append(variablesorder,v)
  formulanew <- formula(paste("target_RT","~",paste(variablesorder,collapse = "+")))
  model <- glm(formulanew,data=train,family=binomial)
  print(v)
}

# Initialize the variables to be used in the loop below
auctrain <- rep(0,length(variablesorder)-1)
auctest <- rep(0,length(variablesorder)-1)

# Determine the AUC of each model with an increasing number of variables to create the overall AUC chart
for(i in c(1:(length(variablesorder)-1))){
  vars <- variablesorder[0:i+1]
  print(vars)
  formula <- paste("target_RT","~",paste(vars,collapse="+"))
  model <- glm(formula,data=train,family="binomial")	
  predicttrain <- predict(model,newdata=train,type="response")
  predicttest <- predict(model,newdata=test,type="response")
  auctrain[i] <- auc(train$target_RT,predicttrain)
  auctest[i] <- auc(test$target_RT,predicttest)
} 

# Plot the AUC chart
plot(auctrain, main="AUC for Retweets", col="red", type='l')
lines(auctest,col="blue",type='l')
legend("bottom", legend=c("Train","Test"), ncol=2,bty="n",
       col=c("red", "blue"), lwd = 2)

# Based on the plot, determine the highest AUC with the lowest overtraining (i.e. gap)
finalvariables <- variablesorder[c(0:7)]
formula <- paste("target_RT","~",paste(finalvariables,collapse="+"))
model <- glm(formula,data=train,family="binomial")	
predicttrain <- predict(model,newdata=train,type="response")
predicttest <- predict(model,newdata=test,type="response")
auctrain <- auc(train$target_RT,predicttrain)
auctest <- auc(test$target_RT,predicttest)

# Discover the model created
print(model)
print(auctrain)
print(auctest)


### Model 1 evaluation ###
# Store AUC in dataframe to compare later
model_AUC <- c("train", "test")
AUC_RT <- as.data.frame(model_AUC)
AUC_RT$logistic_target_RT <- c(auctrain, auctest)

# Calculate the number of predicted high engagement based on default glm cutoff of 0.5
predict_target <- ifelse(predicttest > 0.5, 1, 0)
table(predict_target)

# Convert true and predicted targets to factors
true_target <- as.factor(test$target_RT_norm)
predict_target <- as.factor(predict_target)

# Calculate the confusion matrix
CM_target_RT_info <- confusionMatrix(true_target, predict_target)
CM_target_RT <- as.data.frame(CM_target_RT_info[["table"]])
CM_target_RT$Model <- "logistic_target_RT"

# Calculate the F1 score to determine classification accuracy
# Using first our function and then the one from the caret package to check consistency
F1_eval <- F1_score(CM_target_RT$Freq[4], CM_target_RT$Freq[2], CM_target_RT$Freq[3])
F_meas(predict_target, reference = true_target, relevant = "1")

F1_RT <- as.data.frame(c("logistic_target_RT", F1_eval))


### Method 2: Run random forest to compare to logistic regression
train_rf <- train[,c(5:7, 10:12, 14:19, 22:32, 34:43, 49, 50, 52:53, 55:66)]
test_rf <- test[c(5:7, 10:12, 14:19, 22:32, 34:43, 49, 50, 52:53, 55:66)]

# Train the model based on the selected numeric variables above
# modelrf gives a confusion matrix for the entire dataset (i.e. test and train)
train_rf$target_RT <- as.factor(train_rf$target_RT)
modelrf <- randomForest(target_RT ~., data=train_rf, importance=TRUE, ntree=100, maxnodes = 10, cutoff = c(0.1, 0.9))
predictions_train_rf <- predict(modelrf,newdata = train_rf, type = "prob")
predictions_test_rf <- predict(modelrf,newdata = test_rf, type = "prob")
auctrain <- auc(train_rf$target_RT,predictions_train_rf)
auctest <- auc(test_rf$target_RT,predictions_test_rf)

# Examined a histogram to see what an appropriate cutoff might be for this dataset
# Setting the cutoff so high leads to some overtraining and a very poor model
hist(predictions_test_rf[,"0"])
hist(predictions_test_rf[predictions_test_rf[,"0"] < .9,"0"])

### Model 2 evaluation ###
# Store AUC in dataframe to compare later
AUC_RT$randomForest_target_RT <- c(auctrain, auctest)

# Calculate the number of predicted high engagement based on default glm cutoff of 0.3
predict_target <- ifelse(predictions_test_rf[,"1"] > 0.1, 1, 0)
table(predict_target)

# Convert true and predicted targets to factors
true_target <- as.factor(test$target_RT_norm)
predict_target <- as.factor(predict_target)

# If no target is predicted, due to the small target incidence, there will only be one level
# The two lines below ensure that there are two levels even if the dataset only contains one
levels(predict_target) <- c("0", "1")

# Calculate the confusion matrix
CM_RF_target_RT_info <- confusionMatrix(true_target, predict_target)
CM_RF_target_RT <- as.data.frame(CM_RF_target_RT_info[["table"]])
CM_RF_target_RT$Model <- "RF_target_RT"

CM_RT <- rbind(CM_target_RT, CM_RF_target_RT)

# Calculate the F1 score to determine classification accuracy
# Using first our function and then the one from the caret package to check consistency
F1_eval <- F1_score(CM_RF_target_RT$Freq[4], CM_RF_target_RT$Freq[2], CM_RF_target_RT$Freq[3])
F_meas(predict_target, reference = true_target, relevant = "1")

F1_RT <- cbind(F1_RT, c("randomForest_target_RT", F1_eval))


################################################################
###### Regression on normalized binary target for retweet #####

### Method 1: Run forward stepwise logistic regression to pick the best predictors
variables <- names(train)[c(5:7, 10:12, 14:19, 22:32, 34:43, 50, 52:53, 55:66)]
variablesorder <- c()

# Initiate base model to add to
model <- glm(target_RT ~ 1,data=train,family=binomial)

# Create a formula (i.e. a list of the dependent and then independent variables) to use in the initial linear model
formula <- formula(paste("target_RT_norm","~",paste(variables,collapse="+")))

# Order variables based on the size of their effect/predictive power
for(i in c(1:length(variables))){
  #calculate AIC of each model
  info <- add1(model,scope=formula,data=train)
  print(info)
  #get variable with lowest AIC
  orderedvariables <- rownames(info[order(info$AIC),])
  v <- orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  variablesorder <- append(variablesorder,v)
  formulanew <- formula(paste("target_RT_norm","~",paste(variablesorder,collapse = "+")))
  model <- glm(formulanew,data=train,family=binomial)
  print(v)
}

# Initialize the variables to be used in the loop below
auctrain <- rep(0,length(variablesorder)-1)
auctest <- rep(0,length(variablesorder)-1)

# Determine the AUC of each model with an increasing number of variables to create the overall AUC chart
for(i in c(1:(length(variablesorder)-1))){
  vars <- variablesorder[0:i+1]
  print(vars)
  formula <- paste("target_RT_norm","~",paste(vars,collapse="+"))
  model <- glm(formula,data=train,family="binomial")	
  predicttrain <- predict(model,newdata=train,type="response")
  predicttest <- predict(model,newdata=test,type="response")
  auctrain[i] <- auc(train$target_RT_norm,predicttrain)
  auctest[i] <- auc(test$target_RT_norm,predicttest)
} 

# Plot the AUC chart
plot(auctrain, main="AUC for Normalized Retweets", col="red", type='l')
lines(auctest,col="blue",type='l')
legend("bottom", legend=c("Train","Test"), ncol=2,bty="n",
       col=c("red", "blue"), lwd = 2)

# Run the model with the final most significant variables
finalvariables <- variablesorder[c(0:6)]
formula <- paste("target_RT_norm","~",paste(finalvariables,collapse="+"))
model <- glm(formula,data=train,family="binomial")	
predicttrain <- predict(model,newdata=train,type="response")
predicttest <- predict(model,newdata=test,type="response")
auctrain <- auc(train$target_RT_norm,predicttrain)
auctest <- auc(test$target_RT_norm,predicttest)

# Export model coefficients
write.csv(model$coefficients, "model_RT_coefficients.csv")

# Discover the model created
print(model)
print(auctrain)
print(auctest)

### Model 3 evaluation ###
# Store AUC in dataframe to compare later
AUC_RT$logistic_target_RT_norm <- c(auctrain, auctest)

# Calculate the number of predicted high engagement based on default glm cutoff of 0.5
predict_target <- ifelse(predicttest > 0.5, 1, 0)
table(predict_target)

# Convert true and predicted targets to factors
true_target <- as.factor(test$target_RT_norm)
predict_target <- as.factor(predict_target)

# Calculate the confusion matrix
CM_target_RT_norm_info <- confusionMatrix(true_target, predict_target)
CM_target_RT_norm <- as.data.frame(CM_target_RT_norm_info[["table"]])
CM_target_RT_norm$Model <- "logistic_target_RT_norm"

CM_RT <- rbind(CM_RT, CM_target_RT_norm)

# Calculate the F1 score to determine classification accuracy
# Using first our function and then the one from the caret package to check consistency
F1_eval <- F1_score(CM_target_RT_norm$Freq[4], CM_target_RT_norm$Freq[2], CM_target_RT_norm$Freq[3])
F_meas(predict_target, reference = true_target, relevant = "1")

F1_RT <- cbind(F1_RT, c("logistic_target_RT_norm", F1_eval))


### Method 2: Run random forest to compare to logistic regression
train_rf <- train[c(5:7, 10:12, 14:19, 22:32, 34:43, 47, 50, 52:53, 55:66)]
test_rf <- test[c(5:7, 10:12, 14:19, 22:32, 34:43, 47, 50, 52:53, 55:66)]

# Train the model based on the selected numeric variables above
# modelrf gives a confusion matrix for the entire dataset (i.e. test and train)
train_rf$target_RT_norm <- as.factor(train_rf$target_RT_norm)
modelrf <- randomForest(target_RT_norm ~ ., data=train_rf, importance=TRUE, ntree=100, maxnodes = 10)
predictions_train_rf <- predict(modelrf,newdata = train_rf, type = "prob")
predictions_test_rf <- predict(modelrf,newdata = test_rf, type = "prob")
auctrain <- auc(train_rf$target_RT_norm,predictions_train_rf)
auctest <- auc(test_rf$target_RT_norm,predictions_test_rf)


### Model 4 evaluation ###
# Store AUC in dataframe to compare later
AUC_RT$RF_target_RT_norm <- c(auctrain, auctest)

# Calculate the number of predicted high engagement based on default glm cutoff of 0.5
predict_target <- ifelse(predictions_test_rf[,"1"] > 0.5, 1, 0)
table(predict_target)

# Convert true and predicted targets to factors
true_target <- as.factor(test$target_RT_norm)
predict_target <- as.factor(predict_target)

# Calculate the confusion matrix
CM_RF_target_RT_norm_info <- confusionMatrix(true_target, predict_target)
CM_RF_target_RT_norm <- as.data.frame(CM_RF_target_RT_norm_info[["table"]])
CM_RF_target_RT_norm$Model <- "randomForest_target_RT_norm"

CM_RT <- rbind(CM_RT, CM_RF_target_RT_norm)


# Calculate the F1 score to determine classification accuracy
# Using first our function and then the one from the caret package to check consistency
F1_eval <- F1_score(CM_RF_target_RT_norm$Freq[4], CM_RF_target_RT_norm$Freq[2], CM_RF_target_RT_norm$Freq[3])
F_meas(predict_target, reference = true_target, relevant = "1")

F1_RT <- cbind(F1_RT, c("randomForest_target_RT_norm", F1_eval))


### Deterine which model is best
# Print model evaluations for four models
AUC_RT
CM_RT
F1_RT

### Export evalutions to csv for use in Shiny
write.csv(AUC_RT, "AUC_RT.csv")
write.csv(CM_RT, "CM_RT.csv")
write.csv(F1_RT, "F1_RT.csv")



