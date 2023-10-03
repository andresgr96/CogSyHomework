# Imports
library(lme4) 


# Set Path and Load File
setwd('C:\\Users\\andre\\Desktop\\CogSyHomework\\Ass2')
data <- read.csv('data.txt')


# Clean data
data <- data[data$error == 0,]
data <- data[data$final_stimulus == 'target',]


# Remove outliers:
outlier1 <- mean(data$RT) + 2.5 * sd(data$RT)
outlier2 <- mean(data$RT) - 2.5 * sd(data$RT)
data <- data[data$RT > outlier2,]
data <- data[data$RT < outlier1,]

# Declare Factors
data$target_background <- as.factor(data$target_background)


#Intercepts, Slopes and Models
model1 <- lmer(
  pupil_size ~ target_background + (1|subject)+(1|RT)+(1|target_side),
  data=data,)

model2 <- lmer(
  pupil_size ~ target_background * target_side + (1+target_side|subject)+(1|RT),
  data=data,)

model3 <- lmer(
  pupil_size ~ target_background * RT + (1+target_side|subject)+(1|target_side),
  data=data,)

model4 <- lmer(
  pupil_size ~ target_background + (1+ target_side|subject)+(1|RT)+(1|target_side),
  data=data,)


# Showcase results
summary(model1)
summary(model2)
summary(model3)
summary(model4)




