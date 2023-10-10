# Pro1
### Set working directory ##
setwd("/Volumes/GoogleDrive/My Drive/Documents/MGT_7177/ST-Assignment 1/dataset")

### load in libraries
library(tidyverse)
library(readxl) 
library(psych)
library(gridExtra)
library(factoextra)
library(dplyr)

### load in data
musei <- read_excel("ames.xlsx")


                              ### UNDERSTANDING AMES DATASET

### Understanding data, looking for outliers, missing values
glimpse(musei)

### looking at the first 10 rows of the dataset as well as the last 7 rows of the observations
head(musei, 10)
tail(musei, 7)
names(musei)


### summarise data
summary(musei)

## carrying out descriptive statistics on selected variables that are likely to predict Sale Price for building hypotheses
SH <- c('Bedroom.AbvGr','Sale.Price', 'Lot.Area','Gr.Liv.Area','Overall.Qual','BsmtFin.Type.1','Foundation', 'Condition.1','Total.Bsmt.SF','Exter.Qual','Exter.Cond')
summary(musei[SH])
                       
                        ### DATA QUALITY ISSUES  ##
### Checking for outliers in Sale.Price using ggplot  ###
mean(musei$Sale.Price)
summary(musei$Sale.Price)

histo1<- ggplot(musei) +
  geom_histogram(aes(Sale.Price), bins = 100, colour= "red") +
  labs(title = "Sale Price Outlier  Fig. 1       -  Histogram")

boxplt1 <- ggplot(musei) +
  geom_boxplot(aes(Sale.Price), colour= "red") +
  labs(title = "Sale Price Outlier  Fig. 2    -     boxplot")

### combining the above visualisation histo1 and boxplt1
grid.arrange(histo1, boxplt1)

### subset to remove outliers in Sale.Price  ###
histo2 <- ggplot(musei[musei$Sale.Price < 750000,]) +
  geom_histogram(aes(Sale.Price), bins = 100, colour="Green") +
  labs(title = "Sale Price Clean  Fig. 3           -    Histogram")

boxplt2 <- ggplot(musei[musei$Sale.Price < 750000,]) +
  geom_boxplot(aes(Sale.Price), colour="Green") +
  labs(title = "Sale Price Clean  Fig. 4           -    Boxplot")
### combining the above visualisation histo1 and boxplt1
grid.arrange(histo2, boxplt2)
### Assigning Sale.Price outlier as NA
musei$Sale.Price[musei$Sale.Price > 750000] <- NA

### Remove 2 NAs from Sale.Price outlier and replace with mean value
musei$Sale.Price[is.na(musei$Sale.Price)] <- mean(musei$Sale.Price, na.rm=TRUE)
summary(musei$Sale.Price)


### Checking for outliers in Lot.Area using ggplot  ###
histo3 <- ggplot(musei) +
  geom_histogram(aes(Lot.Area), bins = 100, colour= "Blue")+
  labs(title = "Lot Area Outlier  Fig. 5       -  Histogram")

boxplt3 <- ggplot(musei) +
  geom_boxplot(aes(Lot.Area), colour= "Blue") +
  labs(title = "Lot Area Outlier  Fig. 6     -    Boxplot")

### ggplot combining Lot Area outliers  and Lot Area clean data
grid.arrange(histo3, boxplt3)


### subset to remove outliers in Lot.Area ###
histo4 <- ggplot(musei[musei$Lot.Area < 750000,]) +
  geom_histogram(aes(Lot.Area), bins = 100, colour="Green")+
  labs(title = "Lot Area Clean  Fig. 7     -    Histogram")

boxplt4 <- ggplot(musei[musei$Lot.Area < 750000,]) +
  geom_boxplot(aes(Lot.Area), colour="Green") +
  labs(title = "Lot Area Clean  Fig. 8       -    Boxplot")

### ggplot combining Lot Area outliers  and Lot Area clean data
grid.arrange(histo4, boxplt4)
### Assigning Lot.Shape outlier as NA
musei$Lot.Area[musei$Lot.Area > 750000] <- NA

### Remove 1 NA from Lot.Area outlier and replace with mean value
musei$Lot.Area[is.na(musei$Lot.Area)] <- mean(musei$Lot.Area, na.rm=TRUE)
summary(musei$Lot.Area)


### Checking for data issues in Overall Quality using ggplot  ###
boxplot5 <- ggplot(musei,aes(x=as.factor(Overall.Qual), y= Sale.Price))+
  geom_boxplot()+
  geom_point(colour='red')+
  labs(title = "Overall Quality Dirty  Fig. 9       -    Boxplot")

### Fixing data quality issue in Overall.Qual. It is supposed to have 10 levels not 11 levels according to the ames data dictionary
musei<-musei %>% 
  filter(Overall.Qual<=10)

boxplot6 <- ggplot(musei,aes(x=as.factor(Overall.Qual), y= Sale.Price))+
  geom_boxplot()+
  geom_point(colour='green')+
  labs(title = "Overall Quality Clean  Fig. 10       -    Boxplot")

### ggplot combining Initial error and clean Overall.Quality variable
grid.arrange(boxplot5, boxplot6)


### Checking for data issues in Ground Living Area using ggplot  ###
histo6<- ggplot(musei) +
  geom_histogram(aes(Gr.Liv.Area), bins = 100, colour= "red") +
  labs(title = "Ground Living Area  Fig. 11       -  Histogram")

boxplt7 <- ggplot(musei) +
  geom_boxplot(aes(Gr.Liv.Area), colour= "red") +
  labs(title = " Ground Living Area  Fig. 12    -     boxplot")

### combining the two visualisations above using ggplot2
grid.arrange(histo6, boxplt7)


### Summarising Gr.Liv.Area
summary(musei$Gr.Liv.Area)

### Removing 3 NAs and assigning the mean value to them
musei$Gr.Liv.Area[is.na(musei$Gr.Liv.Area)] <- mean(musei$Gr.Liv.Area, na.rm=TRUE)
summary(musei$Gr.Liv.Area)


### Checking for data issues in Total.Bsmt.SF using ggplot  ###
histo9 <- ggplot(musei) +
  geom_histogram(aes(Total.Bsmt.SF), bins = 100, colour= "Blue")+
  labs(title = "Total.Bsmt.SF Outlier  Fig. 13       -  Histogram")


boxplt9 <- ggplot(musei) +
  geom_boxplot(aes(Total.Bsmt.SF), colour= "Blue") +
  labs(title = "Total.Bsmt.SF Outlier  Fig. 14     -    Boxplot")

### ggplot combined plot of histogram and boxplot showing NA in Total.Bsmt.SF
grid.arrange(histo9, boxplt9)

### summarising Total.Bsmt.SF
summary(musei$Total.Bsmt.SF)

### Removing 1 NA and assigning the mean value to them
musei$Total.Bsmt.SF[is.na(musei$Total.Bsmt.SF)] <- mean(musei$Total.Bsmt.SF, na.rm=TRUE)
summary(musei$Total.Bsmt.SF)


### Converting character variables to factor in preparation for correlation test

### Setting musei$BsmtFin.Type.1 to factor
musei$BsmtFin.Type.1 <- factor(replace_na(musei$BsmtFin.Type.1, '0'), levels = c('0', 'Unf', 'LwQ', 'Rec','BLQ', 'ALQ','GLQ'))

### Setting musei$BsmtFin.Type.2 to factor
musei$BsmtFin.Type.2 <- factor(replace_na(musei$BsmtFin.Type.2, '0'), levels = c('0', 'Unf', 'LwQ', 'Rec','BLQ', 'ALQ', 'GLQ'))


### Setting External condition to factor
musei$Exter.Cond <- factor(musei$Exter.Cond)
      
### Setting Roof Style to factor
musei$Roof.Style <- as.factor(musei$Roof.Style)
levels(musei$Roof.Style)

### Setting Lot.shape to factor
musei$Lot.Shape <- factor(musei$Lot.Shape)

                              ## HYPOTHESES ##

## h1 lot area is positively related to Sale Price
## h2 bedroom is positively related to Sale Price 
## h3 Ground living area is positively related to Sale Price
## h4 overall quality is positively related to Sale Price
## h5 BsmtFin.Type.1 is positively related to Sale Price

                             ## VISUALISATION ##

## sale price by lot area showing scatter plot amd lm
ggplot(data=musei) +
  geom_point(mapping=aes(x=Lot.Area, y=Sale.Price,colour="black")) +
  geom_smooth(method = "lm", formula = y~x, mapping = aes(x=Lot.Area, y= Sale.Price)) +
  labs(title="Sale Price by Lot Area",x="Lot.Area(sqft)", y="Sale.Price($)")

## sale price by Bedrooms
ggplot(data=musei) +
  geom_boxplot(mapping=aes(x=as.factor(Bedroom.AbvGr), y=Sale.Price)) +
  labs(title="Sale Price by Bedroom", x="No of Bedrooms (Features)", y="Sale.Price($)")

## sale price by Ground Living area showing scatter plot amd lm
ggplot(data=musei) +
  geom_point(aes(x=Gr.Liv.Area, y=Sale.Price, colour= 'green')) +
  geom_smooth(method = "lm", formula = y~x, mapping = aes(x=Gr.Liv.Area, y= Sale.Price)) +
  labs(title="Sale Price by Ground Living Area",x="Gr.Liv.Area (sqft)", y="Sale.Price($)")

## sale price by lot Overal Quality
ggplot(data=musei) +
  geom_boxplot(mapping=aes(x=as.factor(Overall.Qual), y=Sale.Price)) +
  geom_point(aes(x=Overall.Qual, y=Sale.Price, colour= 'green')) +
  labs(title="Sale Price by Overall Quality", x="Overall Quality", y="Sale.Price($)")

## sale price by Basement Finish Type 1
ggplot(data=musei) +
  geom_boxplot(mapping=aes(x=as.factor(BsmtFin.Type.1), y=Sale.Price)) +
  geom_point(aes(x=BsmtFin.Type.1, y=Sale.Price, colour= 'red')) +
  labs(title="Sale Price by Basement Finish Type 1", x="Basement Finish 1 (Features)", y="Sale.Price($)")


## sale price by lot Foundation
ggplot(data=musei) +
  geom_boxplot(mapping=aes(x=Foundation, y=Sale.Price)) +
  geom_point(aes(x=Foundation, y=Sale.Price, colour= 'green')) +
  labs(title="Sale Price by Foundation", x="Foundation (sqft)", y="Sale.Price($)")

## sale price by Building Type
ggplot(data=musei) +
  geom_boxplot(mapping=aes(x=as.factor(Bldg.Type), y=Sale.Price)) +
  geom_point(aes(x=Bldg.Type, y=Sale.Price)) +
  labs(title="Sale Price by Building Type", x="Building Type (Features)", y="Sale.Price($)")

## sale price by Basement Quality
ggplot(data=musei) +
  geom_boxplot(mapping=aes(x=as.factor(Bsmt.Qual), y=Sale.Price)) +
  geom_point(aes(x=Bsmt.Qual, y=Sale.Price, colour= 'green')) +
  labs(title="Sale Price by Basement Quality", x="Basement Quality (Features)", y="Sale.Price($)")


## sale price by Basement Finish Type 2
ggplot(data=musei) +
  geom_boxplot(mapping=aes(x=as.factor(BsmtFin.Type.2), y=Sale.Price)) +
  geom_point(aes(x=BsmtFin.Type.1, y=Sale.Price)) +
  labs(title="Sale Price by Basement Finish Type 2", x="Basement Finish 2 (Features)", y="Sale.Price($)")

## sale price by External Condition
ggplot(data=musei) +
  geom_boxplot(mapping=aes(x=as.factor(Exter.Cond), y=Sale.Price)) +
  geom_point(aes(x=Exter.Cond, y=Sale.Price)) +
  labs(title="Sale Price by External Condition", x="External Condition (Features)", y="Sale.Price($)")

                                  ## CORRELLATION

## correlation 1
cor(musei$Lot.Area, musei$Sale.Price)
cor.test(musei$Lot.Area, musei$Sale.Price)

## correlation 2
cor(musei$Bedroom.AbvGr, musei$Sale.Price, method = "spearman")

## correlation 3
cor(musei$Gr.Liv.Area, musei$Sale.Price)

## correlation 4
cor(as.numeric(musei$Overall.Qual), musei$Sale.Price)

## correlation 5
bsmt_type <- as.numeric(musei$BsmtFin.Type.1)
cor(bsmt_type, musei$Sale.Price, method = "spearman")


## correlation 6
musei$Foundation <- as.factor(musei$Foundation)
Fdn <- as.numeric(musei$Foundation)
cor(Fdn, musei$Sale.Price,method="spearman")

## correlation 7
musei$Condition.1 <- as.factor(musei$Condition.1)
Cd1 <- as.numeric(musei$Condition.1)
cor(Cd1, musei$Sale.Price, method = "spearman")

## correlation 8
cor(musei$Total.Bsmt.SF, musei$Sale.Price)

## correlation 9
musei$Exter.Qual <- as.factor(musei$Exter.Qual)  ## negative correlation
Extd <- as.numeric(musei$Exter.Qual)
cor(Extd, musei$Sale.Price, method = "spearman")

## correlation 10
musei$Exter.Cond <- as.factor(musei$Exter.Cond)
Ext <- as.numeric(musei$Exter.Cond)
cor(Ext, musei$Sale.Price, method = "spearman")

## correlation 11
lahPE <- as.numeric(musei$Lot.Shape)            ## negative correlation
cor(lahPE,musei$Sale.Price,method="spearman")

## correlation 12
bsmt_type2 <- as.numeric(musei$BsmtFin.Type.2)
cor(bsmt_type2, musei$Sale.Price, method = "spearman")


                  ## MULTIPLE REGRESSION
### Load in caret library in preparation for regression
library(caret)

### Set seed to keep values of regression constant
set.seed(1845)
index <- createDataPartition(musei$Sale.Price, times =1, p =0.8, list= FALSE)

train <- musei[index,]
test  <- musei[-index,]

### build the model on train data
### model 1
formula <- Sale.Price ~ Lot.Area + Gr.Liv.Area + Overall.Qual + Bedroom.AbvGr + BsmtFin.Type.1
model1 <- lm(formula, train)
summary(model1)


### model 2
formula <- Sale.Price ~ Lot.Area + Gr.Liv.Area + Overall.Qual + Bedroom.AbvGr + BsmtFin.Type.1 + Foundation
model2 <- lm(formula, train)
summary(model2)


### model 3
formula <- Sale.Price ~ Lot.Area + Gr.Liv.Area + Overall.Qual + Bedroom.AbvGr + BsmtFin.Type.1 + Foundation + Condition.1 
model3 <- lm(formula, train)
summary(model3)


### model 4
formula <- Sale.Price ~ Lot.Area + Gr.Liv.Area + Overall.Qual + Bedroom.AbvGr + BsmtFin.Type.1 + Foundation + Condition.1 + Total.Bsmt.SF
model4 <- lm(formula, train)
summary(model4)


### model 5
formula <- Sale.Price ~ Lot.Area + Gr.Liv.Area + Overall.Qual + Bedroom.AbvGr + BsmtFin.Type.1 + Foundation + Condition.1 + Total.Bsmt.SF + Exter.Qual
model5 <- lm(formula, train)
summary(model5)
### model 6
formula <- Sale.Price ~ Lot.Area + Gr.Liv.Area + Overall.Qual + Bedroom.AbvGr + BsmtFin.Type.1 + Foundation + Condition.1 + Total.Bsmt.SF + Exter.Qual+ Exter.Cond 
model6 <- lm(formula, train)
summary(model6)


### model 7
formula <- Sale.Price ~ Lot.Area + Gr.Liv.Area + Overall.Qual + Bedroom.AbvGr + BsmtFin.Type.1 + Foundation + Condition.1 + Total.Bsmt.SF + Exter.Qual+ Exter.Cond + Lot.Shape
model7 <- lm(formula, train)
summary(model7)   ### No significant increase in the model looking at the Adjusted R squared result reason for dropping the model         

### check model1 accuracy of the test data (20%)
predictions <- predict(model1, test)
postResample(predictions, test$Sale.Price)

### check model2 accuracy of the test data (20%)
predictions <- predict(model2, test)
postResample(predictions, test$Sale.Price)

### check model3 accuracy of the test data (20%)
predictions <- predict(model3, test)
postResample(predictions, test$Sale.Price)


### check model4 accuracy of the test data (20%)
predictions <- predict(model4, test)
postResample(predictions, test$Sale.Price)

### check model5 accuracy of the test data (20%)
predictions <- predict(model5, test)
postResample(predictions, test$Sale.Price)


### check model6 accuracy of the test data (20%)
predictions <- predict(model6, test)
postResample(predictions, test$Sale.Price)
test$pred <- predictions




### CHECKING FOR ASSUMPTIONS

### Checking for Multicollinearity to see if VIF > 3
library(car)
vif(model6)

### Checking for Homoscedasticity
plot(model6) ### 1st plot showing Residuals Vs Fitted values

### Normally distributed residuals- Check Normal Q-Q residual plot
plot(model6) ### 2nd plot showing Residuals Vs Normal distribution

### Inflenntial cases: Check cook distance > 1
cooks <- cooks.distance(model6)
sum(cooks >1)


### Independent residuals : run dwtest . value between 1.5 and 2.5 is good
library(lmtest)
dwtest(model6)

train$residuals <- resid(model6)
train$predictions <- fitted(model6)

hist(train$residuals, breaks= 100, col = "red") ### plot histogram to see if it is normally distributed

plot(train$residuals, train$Sale.Price)


summary(model6)
![image](https://github.com/Paduski/Pro1/assets/22614731/0aff4ebe-c448-4f14-b715-a7121dd9667d)
