library(lme4) # This is the package that we need for using LMMs


# First, specify the path to the datafile:
setwd('C://Users//Josh//Desktop//Josh work//Courses//Gastcolleges//LMM workshop')
data <- read.csv('workshop_data.txt')


# For the analysis of RTs, we want to exclude incorrectly answered trials:
data <- data[data$error == 0,]


# And we want to remove outliers:
outlier1 <- mean(data$RT) + 2.5 * sd(data$RT)
outlier2 <- mean(data$RT) - 2.5 * sd(data$RT)
data <- data[data$RT > outlier2,]
data <- data[data$RT < outlier1,]

# We may have factors that we want to specify as such. In the matrix, our IV
# 'distractor' has values 0 and 1, for the no-distraction and distraction conditions,
# respectively. Let's just give labels "no" and "yes", respectively:

data$session <- as.factor(data$session)
data$distractor <- factor(data$distractor,
labels=c("no","yes"))

# We can choose reference levels for our factors of interest. 
# We can then interpret all effects as differences from the reference

data$distractor <- relevel(data$distractor, ref="no")
data$session <- relevel(data$session, ref="control")

# Here is the code for running a very simple LMM with distractor as fixed effect,
# and by-subject and by-item intercepts as random effect

model1 <- lmer(
    RT ~ distractor + (1|subject)+(1|item),
    data=data, )




# To read out the results, request a summary of the model
summary(model1)




