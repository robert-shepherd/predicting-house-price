#install.packages("moments")

library(tidyverse) #data manipulation
library(moments) #measure skew and kurtosis

#Set the below to the directory where the file is saved
setwd("C:/Users/rshepherd/Desktop/MSc/Predictive Modelling/Project")

#Reading in data
housing <- read.csv("housing.csv")


#######################
#Initial data analysis#
#######################

summary(housing)
#Bath likely has outlier (63 baths - removed later in script)
#No NA
#Precip has minus figure and 0 (removed later in script)

###Looking at distribution of variables

#Elevation
plot(density(housing$elevation),main="")
#Looks normally distributed

#Dist_AM1
plot(density(housing$dist_am1),main="")
#Looks normally distributed

#Dist_AM2
plot(density(housing$dist_am2),main="")
#Looks normally distributed

#Dist_AM3
plot(density(housing$dist_am3),main="")
#Looks normally distributed

#Bath
plot(density(housing$bath),main="")
#Skewed due to outlier
#Checking without outlier
plot(density(housing$bath[housing$bath != 63]))
#Only 4 integer values
barplot(table(housing$bath[housing$bath != 63]))

#SQFT
plot(density(housing$sqft),main="")
#Outlier present at 12000+

#Checking and removing outlier
View(arrange(housing,desc(price)))
#Looking at the data, one record stands out as having a higher price, square footage
#and number of baths compared to the other records
#Excluding record from future analysis
housing_cleaned <- housing %>%
  filter(bath != 63)

#SQFT (repeated)
#repeating density plot above
plot(density(housing_cleaned$sqft))
#Data looks like it may have a negative kurtosis
kurtosis(housing_cleaned$sqft)
#Test suggests data is mesokurtic 

#Parking
barplot(table(housing_cleaned$parking))
#Factor with significant volume across each level

#Precipitation
plot(density(housing_cleaned$precip))
#Two peaks, negative kurtosis
skewness(housing_cleaned$precip)
#No significant skew
kurtosis(housing_cleaned$precip)
#Test suggests data is mesokurtic

#Price
plot(density(housing_cleaned$price))
#Two peaks, negative kurtosis
skewness(housing_cleaned$price)
#Skew to the right
kurtosis(housing_cleaned$price)
#Test suggests data is mesokurtic

###Checking distribution of predictors vs. price

#Elevation
qplot(elevation, price, data=housing_cleaned) + geom_smooth(method='lm', se = FALSE)
#No clear trend

#Amenity 1
qplot(dist_am1, price, data=housing_cleaned) + geom_smooth(method='lm', se = FALSE)
#Slight positive trend

#Amenity 2
qplot(dist_am2, price, data=housing_cleaned) + geom_smooth(method='lm', se = FALSE)
#Slight positive trend

#Amenity 3
qplot(dist_am3, price, data=housing_cleaned) + geom_smooth(method='lm', se = FALSE)
#Slight positive trend

#Bath
qplot(bath, price, data=housing_cleaned) + geom_smooth(method='lm', se = FALSE)
#Clear positive trend

#SQFT
qplot(sqft, price, data=housing_cleaned) + geom_smooth(method='lm', se = FALSE)
#Slight positive trend

#Parking
boxplot(price~parking, data=housing_cleaned)
ggplot(housing_cleaned, aes(x=parking, y=price)) + stat_summary(fun.y="mean", geom="bar")
#Some small differences - is it significant?
category_model <- lm(price~parking,data=housing_cleaned)
summary(category_model)
anova(category_model)
#Not significant in isolation but evidence of some impact, will check when
#continuous predictors are selected close to final model

#Precipitation
qplot(sqft, precip, data=housing_cleaned) + geom_smooth(method='lm', se = FALSE)
#No clear trend
#Need to investigate minus figure and 0
erroneous_precip <- housing_cleaned %>%
  filter(precip <= 0)
View(erroneous_precip)
#All other data points for records look normal so making them NA
housing_cleaned$precip[housing_cleaned$precip <= 0] <- NA
#Checking if this has changed fit
qplot(sqft, precip, data=housing_cleaned) + geom_smooth(method='lm', se = FALSE)
#No clear trend

###Checking for colinearity
pairs(housing_cleaned)
#Colinearity evident between distance from amenities, particularly 1 and 3
#Zooming in on just amenities
amenities_only <- housing_cleaned[2:4]
pairs(amenities_only)
#Possible amenities 1 and 3 are around the same place?
cor.test(amenities_only$dist_am1, amenities_only$dist_am3)
#Very strong correlation (0.7954)
#Checking amenity 2 vs. others
cor.test(amenities_only$dist_am1, amenities_only$dist_am2)
cor.test(amenities_only$dist_am3, amenities_only$dist_am2)
#Still strong correlation, particularly between 3 and 2
#Will not include all amenity variables when building model

###Selecting the amenity value with the biggest correlation with 

###Building preliminary model
lm(price~data=housing_cleaned)


#############
#Diagnostics#
#############

###Checking for non-constant variance
#Residual plots
var.test()

###Checking for normality
#QQPLOTs
shapiro.test()

###Checking for correlated errors

###Checking for unusual observations

###Checking the structure of the model

#Checking error assumptions
test <- lm(price~.,data=housing_cleaned)

test <- lm(price~dist_am3+precip+elevation+sqft, data = housing_cleaned)
summary(test)

plot(fitted(test),residuals(test),xlab="Fitted",ylab="Residuals",abline(h=0))

###Introducing categorical variable

category <- lm(price~.,data=housing_cleaned)
no_category <- lm(price~elevation+dist_am1+dist_am2+dist_am3+bath+sqft+precip,data=housing_cleaned)

summary(no_category)

summary(category)
summary(no_category)price
anova(category,no_category)

test.separate <- lm(price~sqft*parking,data=housing)
test.parallel <- lm(price~sqft+parking,data=housing)
test.single <- lm(price~sqft,data=housing)

anova(test.separate,test.parallel,test.single)


example <- lm(price~sqft*bath,data = housing_cleaned)