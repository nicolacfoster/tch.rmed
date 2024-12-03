
# Author:   Dr Nicola Foster
# Project:  Teaching materials for UCL respiratory medicine PhD programme
# Date:     September 2024
###############################################################################

# set the working directory
setwd("/Users/ucl/Documents/")

library(readxl)

# imports data
data1 <- read_excel("~/Desktop/session1_data.xlsx")
View(data1)

ages <- as.numeric(data1$ages)
gender <- as.factor(data1$gender)
happiness <- as.numeric(data1$happiness)


#summarising data
head(data1) #first 6 observations
mean(ages) # average age
sd(ages) # standard deviation of age
range(ages) #range of ages
summary(data1) #statistics for all numeric variables in dataset
table(gender)
by(data1, gender, summary) #statistics by group


#plotting
hist(happiness)
boxplot(happiness~gender)

# same graph plotted using ggplot2
ggplot(data1) +
  aes(x=gender, y=happiness) +
  geom_boxplot()

#linear regression analysis
#response variable ~ explanatory variable(s)
ggplot(mapping = aes(x=ages, y=happiness), data=data1) +
  geom_point()

lm1 <- lm(happiness ~ gender+ages, data=data1)
lm1
summary(lm1)

ggplot(mapping = aes(x=ages, y=happiness), data=data1) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE)

