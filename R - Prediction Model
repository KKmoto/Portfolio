---
title: "Predictive Model Sample"
output: html_document
---

```{r setup, include=FALSE}
library("dplyr")
library("car")
library("forcats")
library("rpart")
library("rpart.plot")
library("nnet")
library("foreign")
library("corrplot")
library("randomForest")
library("pdp")
library("ggplot2")
library("effects")
library("gmodels")
library("gplots")
```

```{r}
source("BCA_functions_source_file.R")

QK <- read.csv(file="Assignment 2-QK2021.csv", stringsAsFactors = TRUE) 
```

```{r}
#QUESTION 3 - SPLIT
QK$Sample <- create.samples(QK,
                                 est = 0.60, # allocate 60% to estimation sample
                                 val = 0.40, # 40% to validation sample
                                 rand.seed = 100) # for reproducibility
```

```{r}
#QUESTION 4 - Cleaning

variable.summary(QK)

#Week3Meals is trivially related


#Change NA for Disc into None
QK$Disc <- fct_explicit_na(QK$Disc, # Factor of interest
                                  na_level = "None") # replacement value

#Remove Title entirely due to non-response
QK$Title <- NULL

#Remove postal code due to too many factor levels
QK$Pcode <- NULL

#custid is an identifier
row.names(QK) <- QK$custid
QK$custid <- NULL

#Adjusting date
QK$LastOrderDays <-difftime(QK$LastOrder, "2018-03-05", units = c("days"))
QK$LastOrderDays.round <-round(QK$LastOrderDays, digits=0)
QK$LastOrderDays.round <-(QK$LastOrderDays.round) * -1

# Remove all missing values from dataset


QK$Weeks3Meals <- NULL
QK2 <- na.omit(QK)

```

```{r}
#Check for correlation
# Select numeric columns only, then calculate and print correlation coefficients
corrMatrix <- cor(select_if(QK2, is.numeric)) # see ?dplyr::select_if

options(digits = 7) 

# Visualize correlation
corrplot(corrMatrix,method="number",type="lower",
         diag = FALSE,number.cex = 0.7)

#Remove TotPurch due to correlation
QK2$TotPurch <- NULL

#Remove NumMeals due to correlation
QK2$NumMeals <- NULL

#Check for correlation again
corrMatrix <- cor(select_if(QK2, is.numeric))

options(digits = 7) 

corrplot(corrMatrix,method="number",type="lower",
         diag = FALSE,number.cex = 0.7)

paste(names(QK2), collapse = " + ")


#Remove Old Dates
QK$LastOrder <- NULL
QK2$LastOrder <- NULL

QK$LastOrderDays <- NULL
QK2$LastOrderDays <- NULL

```




```

```{r}
#MODEL DEVELOPMENT

#Note that target variable is SUBSCRIBE

#First explore variables with random forest

# Copy & paste given variable names into the predictor list
paste(names(QK2), collapse = " + ")


#New regression
QK3 <-na.omit(QK2)
variable.summary(QK3)




# all variables in random forest
QK2ForestAllv <- randomForest(formula = SUBSCRIBE ~  Disc + DA_Income + 
                                DA_Under20 + DA_Over60 + DA_Single + NumDeliv + 
                                MealsPerDeliv + Healthy + Veggie + Meaty + 
                                Special + LastOrderDays.round,
                              data = filter(QK2, Sample =="Estimation"),
                              importance = TRUE,
                              ntree = 500, mtry = 4)

#
#Contingency Table
QK2ForestAllv[["confusion"]]

#
#Variable importance
varImpPlot(QK2ForestAllv,type = 2,
           main="QK2ForestAllv", # title
           cex =0.7) # font size

#
#partial dependence plots
# NumDeliv
partial(QK2ForestAllv, pred.var = "NumDeliv", # target and predictor
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

# LastOrderDays.round - DOESN'T WORK FOR SOME REASON
partial(QK2ForestAllv, pred.var = "LastOrderDays.round", # target and predictor
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

#Categorical version - Disc
partial(QK2ForestAllv, pred.var = "Disc",
        which.class = 2,
        plot = TRUE,
        rug = TRUE, plot.engine = "ggplot2",
        prob = TRUE)


```

```{r}
# Checking that base level of the one categorical variable is correct
levels(QK2$Disc)

#set the base levels
QK2$Disc <- relevel(QK2$Disc, "None")

# Create a logistic regression model
QK2Logis <- glm(formula = SUBSCRIBE ~  Disc + DA_Income + 
                  DA_Under20 + DA_Over60 + DA_Single + NumDeliv + 
                  MealsPerDeliv + Healthy + Veggie + Meaty + 
                  Special + LastOrderDays.round,
                data = filter(QK2, Sample =="Estimation"),
                family = binomial(logit))
# Print
summary(QK2Logis)

``` 

```{r}
# Calculate and print McFadden R square (See Logistic Regression Chapter)
MR2 <- 1 - (QK2Logis$deviance / QK2Logis$null.deviance)
MR2.3 <- round(MR2,digits = 3)
print(paste("McFadden Rsquared: ",MR2.3))

#Run a stepwise regression
QK2Step <- step(QK2Logis, direction = "both")

#
summary(QK2Step)

#
#McFadden R2
MR2.step <- 1 - (QK2Step$deviance / QK2Step$null.deviance)
MR2.step.3 <- round(MR2.step,digits = 3)
print(paste("McFadden Rsquared: ",MR2.step.3))
```

```{r}
# Compare both models using cumulative Lift Chart
lift.chart(modelList = c("QK2Logis","QK2Step"),
           data = filter(QK2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.01, type = "cumulative",
           sub = "Validation")

```{r}
# 4 node nnet (Only stepwise variables)
QK2Nnet4 <- Nnet(formula = SUBSCRIBE ~  DA_Income + 
                   DA_Under20 + NumDeliv + 
                   MealsPerDeliv + Healthy + Meaty + 
                   Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 decay = 0.15, size = 6)
```{r}
# Compare stepwise, tree, and neural network model
lift.chart(modelList = c("QK2Step", "QK2Nnet4","QK2ForestAllv"),
           data = filter(QK2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.01,
           type = "cumulative", sub = "Validation")
```{r}
## All variables in nnet
QK2NetAllv <- Nnet(formula = SUBSCRIBE ~  Disc + DA_Income + 
                     DA_Under20 + DA_Over60 + DA_Single + NumDeliv + 
                     MealsPerDeliv + Healthy + Veggie + Meaty + 
                     Special + LastOrderDays.round,
                   data = filter(QK2, Sample =="Estimation"),
                   decay = 0.15, size = 4)

```{r}
#Compare on Validation Sample
lift.chart(modelList = c("QK2Step", "QK2Nnet4",
                         "QK2NetAllv", "QK2ForestAllv"),
           data = filter(QK2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.01,
           type = "cumulative", sub = "Validation")
```
#Neural Network with all variabels started to overfit
#Therefore, original neural network is fine

#Next step is trying to find non-linear variables and transform them
#This way, we can get logistic model closer to neural network strength


# Create plot of means with equal proportions (equal counts in each bin)

#First, create Num version of target variable for easier visualizations

```{r}
QK2$SUBSCRIBE.Num <- if_else(QK2$SUBSCRIBE == "Y",1,0)

# Starting with DA_Income
QK2$DA_Income.Cat <- binVariable(QK2$DA_Income, bins = 4,
                                 method = "proportions",
                                 labels = NULL) # bin borders as x-labels

plotmeans(SUBSCRIBE.Num ~ DA_Income.Cat, data = QK2)

#It's a concave decreasing returns structure, so LOG it

QK2$Log.DA_Income <- log(QK2$DA_Income)

glimpse(QK2)

#Try plotting means with new log version
QK2$Log.DA_Income.Cat <- binVariable(QK2$Log.DA_Income, bins = 4,
                                 method = "proportions",
                                 labels = NULL) # bin borders as x-labels

plotmeans(SUBSCRIBE.Num ~ Log.DA_Income.Cat, data = QK2)
```

```{r}
#Since it didn't change anything, try square root instead

QK2$sqrt.DA_Income <- sqrt(QK2$DA_Income)

glimpse(QK2)

QK2$sqrt.DA_Income.Cat <- binVariable(QK2$sqrt.DA_Income, bins = 4,
                                     method = "proportions",
                                     labels = NULL) # bin borders as x-labels

plotmeans(SUBSCRIBE.Num ~ sqrt.DA_Income.Cat, data = QK2)

#Okay maybe I should square it instead oops
``` 

```{r}
QK2$square.DA_Income <- (QK2$DA_Income)^2

glimpse(QK2)

QK2$square.DA_Income.Cat <- binVariable(QK2$square.DA_Income, bins = 4,
                                      method = "proportions",
                                      labels = NULL) # bin borders as x-labels

plotmeans(SUBSCRIBE.Num ~ square.DA_Income.Cat, data = QK2)



QK2Logis2 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + NumDeliv + 
                   MealsPerDeliv + Healthy + Meaty + 
                   Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis2)

#AIC Improved!

# Next is with DA_Under20
QK2$DA_Under20.Cat <- binVariable(QK2$DA_Under20, bins = 6,
                                 method = "proportions",
                                 labels = NULL) # bin borders as x-labels

plotmeans(SUBSCRIBE.Num ~ DA_Under20.Cat, data = QK2)

#seems pretty linear so ignore

# Next is with NumDeliv
QK2$NumDeliv.Cat <- binVariable(QK2$NumDeliv, bins = 5,
                                  method = "proportions",
                                  labels = NULL) # bin borders as x-labels

plotmeans(SUBSCRIBE.Num ~ NumDeliv.Cat, data = QK2)
```{r}
#seems VERY curved, so try logging ot fix
QK2$Log.NumDeliv <- log(QK2$NumDeliv)

glimpse(QK2)

QK2Logis3 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + Log.NumDeliv + 
                   MealsPerDeliv + Healthy + Meaty + 
                   Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis3)

```

#Try Square rooting

```{r}
QK2$sqrt.NumDeliv <- sqrt(QK2$NumDeliv)

glimpse(QK2)

QK2Logis3 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + sqrt.NumDeliv + 
                   MealsPerDeliv + Healthy + Meaty + 
                   Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis3)

#Okay still worse than just leaving it alone
#Maybe try squaring it?
```

```{r}
QK2$square.NumDeliv <- (QK2$NumDeliv)^2

glimpse(QK2)

QK2Logis3 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + square.NumDeliv + 
                   MealsPerDeliv + Healthy + Meaty + 
                   Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis3)

```{r}

#IT WORKED!!! AIC BECAME BETTER!!!!

# Next is with MealsPerDeliv
QK2$MealsPerDeliv.Cat <- binVariable(QK2$MealsPerDeliv, bins = 2,
                                method = "proportions",
                                labels = NULL) # bin borders as x-labels

plotmeans(SUBSCRIBE.Num ~ MealsPerDeliv.Cat, data = QK2)
```
#Not enough range to understand relationship, so left it alone

# Next is with Healthy

```{r}
QK2$Healthy.Cat <- binVariable(QK2$Healthy, bins = 4,
                                     method = "proportions",
                                     labels = NULL) # bin borders as x-labels

plotmeans(SUBSCRIBE.Num ~ Healthy.Cat, data = QK2)
```
#Seems a bit u-shaped again, so try squaring it like previous one to fix


```{r}
QK2$square.Healthy <- (QK2$Healthy)^2

glimpse(QK2)

QK2Logis4 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + square.NumDeliv + 
                   MealsPerDeliv + square.Healthy + Meaty + 
                   Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis4)
```
#AIC became worse, so maybe fix with log?


```{r}
QK2$Log.Healthy <- log(QK2$Healthy + 1)

glimpse(QK2)

QK2Logis4 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + square.NumDeliv + 
                   MealsPerDeliv + Log.Healthy + Meaty + 
                   Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis4)
```


```{r}
QK2$sqrt.Healthy <- sqrt(QK2$Healthy)

glimpse(QK2)

QK2Logis4 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + square.NumDeliv + 
                   MealsPerDeliv + sqrt.Healthy + Meaty + 
                   Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis4)
```

```{r}
#This improved AIC even more, so stick with this

# Next is with Meaty
QK2$Meaty.Cat <- binVariable(QK2$Meaty, bins = 4,
                                     method = "proportions",
                                     labels = NULL) # bin borders as x-labels

plotmeans(SUBSCRIBE.Num ~ Meaty.Cat, data = QK2)

#Exponential curve, so try to fix with square
QK2$square.Meaty <- (QK2$Meaty)^2

glimpse(QK2)

QK2Logis5 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + square.NumDeliv + 
                   MealsPerDeliv + sqrt.Healthy + square.Meaty + 
                   Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis5)

#AIC got worse, maybe try fixing with LOG
QK2$Log.Meaty <- log(QK2$Meaty + 1)

glimpse(QK2)

QK2Logis5 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + square.NumDeliv + 
                   MealsPerDeliv + sqrt.Healthy + Log.Meaty + 
                   Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis5)

#AIC still bad, maybe try square root?
QK2$sqrt.Meaty <- sqrt(QK2$Meaty)

glimpse(QK2)

QK2Logis5 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + square.NumDeliv + 
                   MealsPerDeliv + sqrt.Healthy + sqrt.Meaty + 
                   Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis5)


# Next is with Special
QK2$Special.Cat <- binVariable(QK2$Special, bins = 6,
                             method = "proportions",
                             labels = NULL) # bin borders as x-labels

plotmeans(SUBSCRIBE.Num ~ Special.Cat, data = QK2)

#u-shaped, so try square again
QK2$square.Special <- (QK2$Special)^2

glimpse(QK2)

QK2Logis5 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + square.NumDeliv + 
                   MealsPerDeliv + sqrt.Healthy + Meaty + 
                   square.Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis5)


QK2$Log.Special <- log(QK2$Special + 1)

QK2Logis5 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + square.NumDeliv + 
                   MealsPerDeliv + sqrt.Healthy + Meaty + 
                   Log.Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis5)

#TEST SQRT

QK2$sqrt.Special <- sqrt(QK2$Special)

QK2Logis5 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + square.NumDeliv + 
                   MealsPerDeliv + sqrt.Healthy + Meaty + 
                   sqrt.Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis5)

#AIC Improved

# Last potential transformation is LastOrderDays.round

#Convert to numeric first
variable.summary(QK2)

QK2$LastOrderDays.num <- as.numeric(QK2$LastOrderDays.round, units="days")

variable.summary(QK2)

#Now that that's fixed, try again
QK2$LastOrderDays.num.Cat <- binVariable(QK2$LastOrderDays.num, bins = 6,
                               method = "proportions",
                               labels = NULL) # bin borders as x-labels

plotmeans(SUBSCRIBE.Num ~ LastOrderDays.num.Cat, data = QK2)

#Maybe try square to see what happens
QK2$square.LastOrderDays.num <- (QK2$LastOrderDays.num)^2

QK2Logis6 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + square.NumDeliv + 
                   MealsPerDeliv + sqrt.Healthy + Meaty + 
                   sqrt.Special + square.LastOrderDays.num,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis6)

#AIC got worse, so try again with Log

QK2$Log.LastOrderDays.num <- log(QK2$LastOrderDays.num)

QK2Logis6 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + square.NumDeliv + 
                   MealsPerDeliv + sqrt.Healthy + Meaty + 
                   sqrt.Special + Log.LastOrderDays.num,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis6)

#AIC IMPROVED but still want to double check if square root could work

QK2$sqrt.LastOrderDays.num <- sqrt(QK2$LastOrderDays.num)

QK2Logis6 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                   DA_Under20 + square.NumDeliv +
                   MealsPerDeliv + sqrt.Healthy + Meaty + 
                   sqrt.Special + sqrt.LastOrderDays.num,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis6)

#AIC became tiny bit better, so use square root as final transformation for ifnal model

#Now that we have a new logistic model with transformed variables, test lift chart to compare logistic models
lift.chart(modelList = c("QK2Logis","QK2Step","QK2Logis6"),
           data = filter(QK2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.01, type = "cumulative",
           sub = "Validation")

#The variable transformations helped! now test against neural network
lift.chart(modelList = c("QK2Logis6", "QK2Nnet4","QK2ForestAllv"),
           data = filter(QK2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.01,
           type = "cumulative", sub = "Validation")

#Neural network still strongest, but there's a significant improvement compared to the first logistic model,
#And the logistic model is close enough to the neural network for us not to worry!

#Testing logistic model but ONLY squaring the numdeliv and nothing else
QK2Logis7.0 <- glm(formula = SUBSCRIBE ~  Log.DA_Income + 
                         DA_Under20 + square.NumDeliv + NumDeliv +
                         MealsPerDeliv + Healthy + Meaty + 
                         Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 family = binomial(logit))

summary(QK2Logis7.0)

```{r}

### ^ BEST AIC
###ABOVE FINAL MODEL

```

```{r}
lift.chart(modelList = c("QK2Nnet4","QK2Logis7.0"),
           data = filter(QK2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.01,
           type = "cumulative", sub = "Validation")

#Below, we removed the log transformation for DA_Income to test AIC
QK2Logis7.1 <- glm(formula = SUBSCRIBE ~  DA_Income + 
                           DA_Under20 + square.NumDeliv + NumDeliv +
                           MealsPerDeliv + Healthy + Meaty + 
                           Special + LastOrderDays.round,
                   data = filter(QK2, Sample =="Estimation"),
                   family = binomial(logit))

summary(QK2Logis7.1)
```{r}
#AIC was better than logging DA, but the lift chart was worse, so stick with Logis7.0

# 4 node nnet (Only stepwise variables)
QK2Nnet4 <- Nnet(formula = SUBSCRIBE ~  DA_Income + 
                         DA_Under20 + NumDeliv + 
                         MealsPerDeliv + Healthy + Meaty + 
                         Special + LastOrderDays.round,
                 data = filter(QK2, Sample =="Estimation"),
                 decay = 0.5, size = 15)
summary(QK2Logis7.0)
```{r}
#Liftchart testing below
lift.chart(modelList = c("QK2Nnet4","QK2Logis7.0"),
           data = filter(QK2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.165,
           type = "cumulative", sub = "Validation")

```
