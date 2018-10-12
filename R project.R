d <- read.csv(file = "insurance.csv",header = TRUE)
source("DataQualityReport.R")
DataQualityReport(d)
names(d)[7] <- "y"
d <- d[,c(7,1:6)]

#create dummy variables
library(caret)
dummies <- dummyVars(y ~ ., data = d)            
ex <- data.frame(predict(dummies, newdata = d)) 
names(ex) <- gsub("\\.", "", names(ex))          
d <- cbind(d$y, ex)                              
names(d)[1] <- "y"                             
rm(dummies, ex) 

# Identify Correlated Predictors and remove them
################################################################################
# If a model that has highly correlated independent variables it can
# lead to unstable models because it will tend to weight those more even though
# they might not be that important

# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(d[,2:ncol(d)])                          
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85)
summary(descrCor[upper.tri(descrCor)])
# which columns in correlation matrix have a correlation greater than some
# specified absolute cutoff?
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
# remove those specific columns from your dataset
filteredDescr <- d[,2:ncol(d)][,-highlyCorDescr] 
descrCor2 <- cor(filteredDescr) # calculate a new correlation matrix
# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])
# update our d dataset by removing those filtered variables that were highly correlated
d <- cbind(d$y, filteredDescr)
names(d)[1] <- "y"
rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)


library(caret)
y <- d$y
d <- cbind(rep(1, nrow(d)), d[2:ncol(d)])
names(d)[1] <- "ones"
comboInfo <- findLinearCombos(d)
comboInfo
d <- d[, -comboInfo$remove]
d <- d[, c(2:ncol(d))]
d <- cbind(y, d)
rm(y, comboInfo)

numcols <- apply(X=d, MARGIN=2, function(c) sum(c==0 | c==1)) != nrow(d)
catcols <- apply(X=d, MARGIN=2, function(c) sum(c==0 | c==1)) == nrow(d)
dNums <- d[,numcols]
dCats <- d[,catcols]
preProcValues <- preProcess(dNums[,2:ncol(dNums)], method = c("center","scale"))
dNums <- predict(preProcValues, dNums)
d <- cbind(dNums, dCats)
rm(preProcValues, numcols, catcols, dNums, dCats) 

# identify records that will be used in the training set. Here we are doing a
# 85% train/ 15% test split.
set.seed(1234)
library(caret)
inTrain <- createDataPartition(y = d$y,   
                               p = .85,   
                               list = F)
train <- d[inTrain,]
test <- d[-inTrain,] 

library(caret)
ctrl <- trainControl(method="cv",     
                     number=5,        
                     classProbs = F,  
                     summaryFunction = defaultSummary, 
                     allowParallel=T)

# train a Lasso model
lassofit <- train(y ~ .,
                  data = train,
                  method = "lars",
                  trControl = ctrl,
                  tuneLength = 15,                
                  metric = "RMSE"                  
)

# train a linear regression (ols) model
ols <- train(y ~ .,
             data = train,
             method = "lm",
             trControl = ctrl,
             metric = "RMSE"
)

# train a random forest (rf) model
rf <- train(y ~ .,
             data = train,
             method = "rf",
             trControl = ctrl,
             metric = "RMSE"
)

lasso_pred1 <- predict(lassofit, train)
lasso_pred2 <- predict(lassofit, test)
ols_pred1 <- predict(ols, train)
ols_pred2 <- predict(ols, test)
rf_pred1 <- predict(rf, train)
rf_pred2 <- predict(rf, test)

tr_results <- rbind(
  postResample(pred = lasso_pred1, obs = train$y),
  postResample(pred = ols_pred1, obs = train$y),
  postResample(pred = rf_pred1, obs = train$y)
)
te_results <- rbind(
  postResample(pred = lasso_pred2, obs = test$y),
  postResample(pred = ols_pred2, obs = test$y),
  postResample(pred = rf_pred2, obs = test$y)
)

Models <- c("Lasso","Linear Regression","Random Forest")
Set <- c(rep("Train",3))
(tr_results <- data.frame(Models, Set, tr_results))
Set <- c(rep("Test",3))
(te_results <- data.frame(Models, Set, te_results))

library(reshape)
results1 <- melt(tr_results, id=c("Models","Set"))
results2 <- melt(te_results, id=c("Models","Set"))
(results <- rbind(results1, results2))

library(ggplot2)
theme_set(theme_classic())
g <- ggplot(results[results$variable=="Rsquared",], aes(fill=Set, y=value, x=Models)) 
g <- g + geom_bar(position="dodge", colour="black", stat="identity")
g <- g + labs(title="Statistical Performance by Model", y="R-Squared")
g <- g + theme(plot.title = element_text(color="black", face="bold", size=14, hjust=0.5))
g <- g + theme(axis.text.x=element_text(colour="black"),
               axis.text.y=element_text(colour="black"))
g <- g + scale_y_continuous(labels = scales::percent)
g <- g + scale_fill_manual("Partition", values=c("Train"="orange","Test"="#0072B2"))
g
rm(g, inTrain, lassofit, missing, ols, rf, results1, results2, test, lasso_pred1
   , lasso_pred2, Models, ols_pred1, ols_pred2, rf_pred1, rf_pred2, Set, Mode)

insurance <- read.csv(file = "insurance.csv",header = TRUE)
smokernochildhealthy <- insurance %>%
  filter(smoker == "yes" & children == 0 & bmi < 30)
summary(lm(charges ~ age + bmi, data = smokernochildhealthy))

smokernochildunhealthy <- insurance %>%
  filter(smoker == "yes" & children == 0 & bmi >= 30)
summary(lm(charges ~ age + bmi, data = smokernochildunhealthy))

nonsmokernochildhealthy <- insurance %>%
  filter(smoker == "no" & children == 0 & bmi < 30)
summary(lm(charges ~ age + bmi, data = nonsmokernochildhealthy))

nonsmokernochildunhealthy <- insurance %>%
  filter(smoker == "no" & children == 0 & bmi >= 30)
summary(lm(charges ~ age + bmi, data = nonsmokernochildunhealthy))

smokerchildhealthy <- insurance %>%
  filter(smoker == "yes" & children > 0 & bmi < 30)
summary(lm(charges ~ age + bmi, data = smokerchildhealthy))

smokerchildunhealthy <- insurance %>%
  filter(smoker == "yes" & children > 0 & bmi >= 30)
summary(lm(charges ~ age + bmi, data = smokerchildunhealthy))

nonsmokerchildhealthy <- insurance %>%
  filter(smoker == "no" & children > 0 & bmi < 30)
summary(lm(charges ~ age + bmi, data = nonsmokerchildhealthy))

nonsmokerchildunhealthy <- insurance %>%
  filter(smoker == "no" & children > 0 & bmi >= 30)
summary(lm(charges ~ age + bmi, data = nonsmokerchildunhealthy))


predict <- function(x){
  for(i in 1:nrow(x)){
    if(x[i,"smoker"] == "yes" && x[i,"children"] == 0 && x[i,"bmi"] < 30){
      x[i,"result"] = -956.74 + (251.20*x[i,"age"]) + (505.18*x[i,"bmi"])
    } else if(x[i,"smoker"] == "yes" && x[i,"children"] == 0 && x[i,"bmi"] >= 30) {
      x[i,"result"] = 8120.10 + (292.16*x[i,"age"]) + (614.01*x[i,"bmi"])
    } else if(x[i,"smoker"] == "no" && x[i,"children"] == 0 && x[i,"bmi"] < 30){
      x[i,"result"] = -3791.02 + (276.56*x[i,"age"]) + (22.4*x[i,"bmi"])
    } else if(x[i,"smoker"] == "no" && x[i,"children"] == 0 && x[i,"bmi"] >= 30){
      x[i,"result"] = -465.01 + (254.86*x[i,"age"]) + (-48.9*x[i,"bmi"])
    } else if(x[i,"smoker"] == "yes" && x[i,"children"] > 0 && x[i,"bmi"] < 30){
      x[i,"result"] = 2428.48 + (259.48*x[i,"age"]) + (359.27*x[i,"bmi"])
    } else if(x[i,"smoker"] == "yes" && x[i,"children"] > 0 && x[i,"bmi"] >= 30){
      x[i,"result"] = 16021.03 + (253.72*x[i,"age"]) + (447.91*x[i,"bmi"])
    } else if(x[i,"smoker"] == "no" && x[i,"children"] > 0 && x[i,"bmi"] < 30){
      x[i,"result"] = -2835.49 + (247.27*x[i,"age"]) + (79.79*x[i,"bmi"])
    } else {
      x[i,"result"] = -923.87 + (283.07*x[i,"age"]) + (-35.83*x[i,"bmi"])
    }    
  }
  return(x)
}
predcharges <- predict(insurance)

predictfunction <- function(smoker,children,bmi,age){
  if(smoker == "yes" && children == 0 && bmi < 30){
      result = -956.74 + (251.20*age) + (505.18*bmi)
  } else if(smoker == "yes" && children == 0 && bmi >= 30) {
      result = 8120.10 + (292.16*age) + (614.01*bmi)
  } else if(smoker == "no" && children == 0 && bmi < 30){
      result = -3791.02 + (276.56*age) + (22.4*bmi)
  } else if(smoker == "no" && children == 0 && bmi >= 30){
      result = -465.01 + (254.86*age) + (-48.9*bmi)
  } else if(smoker == "yes" && children > 0 && bmi < 30){
      result = 2428.48 + (259.48*age) + (359.27*bmi)
  } else if(smoker == "yes" && children > 0 && bmi >= 30){
      result = 16021.03 + (253.72*age) + (447.91*bmi)
  } else if(smoker == "no" && children > 0 && bmi < 30){
      result = -2835.49 + (247.27*age) + (79.79*bmi)
  } else {
      result = -923.87 + (283.07*age) + (-35.83*bmi)
  }    
  return(result)
  }
predictfunction("no",1,33,46)
