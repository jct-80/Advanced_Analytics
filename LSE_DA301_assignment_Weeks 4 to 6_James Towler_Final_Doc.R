## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library(tidyverse)

# Import the data set.
df1 <- read.csv(file.choose(), header=T)

# Print the data frame.
df1

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
df2 = subset(df1, select = c('Product','Platform','EU_Sales','Global_Sales', 'NA_Sales'))


# View the data frame.
df2

# View the descriptive statistics.
summary(df2)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(EU_Sales, NA_Sales, xlab='EU Sales (£M)', ylab='NA Sales (£M)', main='Plot of EU vs NA and type of Platform', color=Platform, data=df2, geom=c('point', 'jitter'))
qplot(NA_Sales, Global_Sales, data=df2)

## 2b) Histograms
# Create histograms.
qplot(Global_Sales, data=df2, bins=20, xlab='Global Sales', ylab='Frequency', main='Histogram of Global Sales')
qplot(Platform, data=df2, bins=20, xlab='Platform', ylab='Frequency', main='Histogram of Platform Sales')
qplot(EU_Sales, data=df2, bins=20, xlab='EU Sales', ylab='Frequency', main='Histogram of EU Sales')
qplot(NA_Sales, data=df2, bins=20, xlab='NA Sales', ylab='Frequency', main='Histogram of NA Sales')

## 2c) Boxplots
# Create boxplots.
qplot(EU_Sales, data=df2, geom='boxplot', main='boxplot of EU Sales')
qplot(NA_Sales, data=df2, geom='boxplot', main='boxplot of NA Sales')
qplot(Global_Sales, data=df2, geom='boxplot', main='boxplot of Global Sales')
qplot(EU_Sales, Platform, data=df2, geom='boxplot', main='boxplot of EU Sales against type of Platform')
qplot(NA_Sales, Platform, data=df2, geom='boxplot', main='boxplot of NA Sales against type of Platform')
qplot(Global_Sales, Platform, data=df2, geom='boxplot', main='boxplot of Global Sales against type of Platform')

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
#Correlation between EU_Sales and NA_Sales looks to be fairly correlated as looks to have positive correlation.
#Taking into the platform type is difficult via scatterplot.
#The histograms of sales are not very insightful, however, the histogram of platform is a nice chart to easily most populat gaming platforms.
#Boxplots of sales again not very informative, however, these make nice charts to visually see link between EU/NA or Global Sales and and association with platform.



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
df2

# View the descriptive statistics.
summary(df2)

###############################################################################

# 2. Determine the impact on sales per product_id.
library(dplyr)

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
df3 <- aggregate(Product~EU_Sales+Global_Sales+NA_Sales, df2, FUN=sum)

#View the results
df3

# Aggregate function in R with mean summary statistics
df4 <- aggregate(Product~EU_Sales+Global_Sales+NA_Sales, df2, FUN=mean)
# View the results.
dim(df4)

# View the data frame.
df4

# Explore the data frame.
dim(df4)
summary(df4)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(Product, Global_Sales, data=df4)

# Create histograms.
qplot(Product, data=df4, bins=50)


# Create boxplots.
qplot(Product, data=df4, geom='boxplot')
qplot(Global_Sales, data=df4, geom='boxplot')

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(df2$Global_Sales)
qqline(df2$Global_Sales) 


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(df2$Global_Sales) # resul is very low suggesting data not normally distributed


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(df2$Global_Sales) #The result of 4.04 is positive skewness, which suggests that the distribution is heavily right-skewed
kurtosis(df2$Global_Sales) #The result shows the data is not platykurtic


## 3d) Determine correlation
# Determine correlation.
cor(df2$NA_Sales, df2$EU_Sales) # positive correlation with EU/NA Sales
cor(df2$Global_Sales, df2$Product) # negative correlation
cor(df3)


###############################################################################

# 4. Plot the data

# Create plots to gaincore insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


###############################################################################

# 5. Observations and insights
# Your observations and insights here...
#The tests for normality show that the dataset for all sales does not follow a noramly distribution. 
#The lack of normality can be seen in scatterplots and boxplots with curved scatters and mediam lines in box plots skewed.
#it was not a surprise the statistical tests gave such low p-values. 


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
df3

# Determine a summary of the data frame.
summary(df3)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model1 <- lm(EU_Sales,NA_Sales,
             data=df3)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
        plot(model1$residuals)

summary(model1)
        

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
model2 = lm(Global_Sales~EU_Sales+NA_Sales, data=df3)

# Multiple linear regression model.
summary(model2)

str(df3)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
predictTest = predict(model2, newdata=df2,
                      interval='confidence')

# Print the object.
predictTest 


###############################################################################

# 5. Observations and insights
# Your observations and insights here...
#The adjusted R-squared of the linear model is 0.9865
#The adjusted R-squared of the multiple linear model is 0.1919
#This shows that the multiple linear model taking into account Global/NA/EU Sales is a better predictor for future sales


###############################################################################
###############################################################################




