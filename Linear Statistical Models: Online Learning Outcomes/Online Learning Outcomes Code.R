###############################################################
## Title: LSM Online Learning Outcomes
## Author: Ralph Parlin
## Date Created: 09 SEP 2020
###############################################################


# Clear the environment
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment


# Set working directory 
setwd("C:/Users/katie/Desktop/Syracuse University/MAS766 -Linear Statistical Models_Regression I/Project Data")
getwd()

# Load Packages

library(data.table)
library(tidyverse)
library(psych)
library(readxl)
library(car)
library(ggplot2)


#Get project data

stdata <- read_excel("DS1_Unique Student_For Research Question 1.xlsx", sheet = 1)
stdata <- data.frame(stdata)

View(stdata)

colnames(stdata)

str(stdata)

#Data Cleaning########################################################################################################


stdata$gender <- as.factor(stdata$gender)
stdata$highest_eduation_us_equivalent <- as.factor(stdata$highest_eduation_us_equivalent)
stdata$highest_education <- as.factor(stdata$highest_education)
stdata$imd_band <- as.factor(stdata$imd_band)
stdata$age_band <- as.factor(stdata$age_band)
stdata$disability <- as.factor(stdata$disability)
stdata$final_result <- as.factor(stdata$final_result)

#check for missing Values
#install.packages("mice")
library(mice)
md.pattern(stdata, rotate.names = TRUE)

#visualize the missing data

#install.packages("VIM")
library(VIM)
mice_plot <- aggr(stdata, col=c('grey','orange'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(stdata), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# We have a lot of data with missing sum clicks, nearly 85% of the data
# To effectively analyize what factors impact online student 
# outcomes, we decided to condition the data to include only students that were actually involved.
# We determind involvement required a student to make at least 1 click in a course. As such, we removed
# all observations that had lass than 1 click.

stdata <- stdata %>% filter(sum_clicks_percourse > 0)


#Next up is date of registration.  Just a few NA's so we deleted the observations with missing values

stdata <- stdata %>% filter(date_registration !="NA")





# Data Partitioning #########################################################################################

#The first data partition should exclude students that withdrew.  It does no good
# model what leads to an academic outcome (score) if the student withdrew form the course.

stdataInv <- stdata %>% filter(final_result == "Distinction" | final_result == "Pass" | final_result == "Fail")
table(stdataInv$final_result)

# Now we should make a data table of just students that passed.  The values can be 
# "Distinction" or "Pass" 

stdataPass <- stdata %>% filter(final_result == "Distinction" | final_result == "Pass")
table(stdataPass$final_result)


# And maybe a data table of students that withdrew
stdataWdn <- stdata %>% filter(final_result == "Withdrawn")
table(stdataWdn$final_result)


# And maybe a data table of students that failed
stdataFail <- stdataInv %>% filter(final_result == "Fail")
table(stdataFail$final_result)


table(stdataInv$imd_band)


# EDA  ########################################################################################################
###### Histograms to check the distributions

su

### Average Scores
scoreHist <- ggplot(stdataInv, aes(x = avg_score_all_assessments)) +
  geom_histogram(binwidth = 1, col="black", fill="orange", alpha = .5) +
  labs(x="Avg Score", y="Count") +
  ggtitle(label = "Histogram of Scores (bin = 1)")
scoreHist

# We see a big problem here, there is s asignificant potion of our data with zeros
# We will conduct outlier analysis and see if any exist.
# However, we are reluctant to remove any data but at a minimum, 
# to keep with our rationale for modeling an "involved student" we will remove any data
# with a score of 0. It's unsound to model students that were not enganged as determined by the
# amount of interaction and the lack of basic effort it wouuld take to get a 0 score.  
# A 1 is ok, but a zero is not



### Population
popHist <- ggplot(stdataInv, aes(x = population_millions)) +
  geom_histogram(breaks=seq(2, 10, by = 1), col="black", fill="orange", alpha = .5) +
  labs(x="Population in millions", y="Count") +
  ggtitle(label = "Population (bin = 1)")
popHist


### Population Density
densHist <- ggplot(stdataInv, aes(x = region_density)) +
  geom_histogram(breaks=seq(160, 17000, by = 500), col="black", fill="orange", alpha = .5) +
  labs(x="Population density)", y="Count") +
  ggtitle(label = "Region Density (in millions) (bin = 500)")
densHist


### Broadband Median Download
medDownloadHist <- ggplot(stdataInv, aes(x = region_broadband_median_download_speed_mbitpers)) +
  geom_histogram(breaks=seq(15, 55, by = 1), col="black", fill="orange", alpha = .5) +
  labs(x="Median Download Speed (Mbps)", y="Count") +
  ggtitle(label = "Median Download Speed (Mbps) (bin = 1)")
medDownloadHist


### Broadband Average Download
avgDownloadHist <- ggplot(stdataInv, aes(x = region_broadband_average_download_speed_Mbitspers)) +
  geom_histogram(breaks=seq(20, 70, by = 1), col="black", fill="orange", alpha = .5) +
  labs(x="Average Download Speed (Mbps)", y="Count") +
  ggtitle(label = "Average Download Speed (Mbps) (bin = 1)")
avgDownloadHist


### Broadband Average Usage
avgUsageHist <- ggplot(stdataInv, aes(x = region_broadband_avg_data_usage_gbs)) +
  geom_histogram(breaks=seq(140, 270, by = 10), col="black", fill="orange", alpha = .5) +
  labs(x="Average Usage Speed (GBs)", y="Count") +
  ggtitle(label = "Average Usage Speed (GBs) (bin = 10)")
avgUsageHist


### Number of Previous Attempts
attemptsHist <- ggplot(stdataInv, aes(x = num_of_prev_attempts)) +
  geom_histogram(breaks=seq(0, 5, by = 1), col="black", fill="orange", alpha = .5) +
  labs(x="Previous Attempts", y="Count") +
  ggtitle(label = "Previous Attempts (bin = 1)")
attemptsHist


### Studied Credits
creditsHist <- ggplot(stdataInv, aes(x = studied_credits)) +
  geom_histogram(breaks=seq(50, 420, by = 10), col="black", fill="orange", alpha = .5) +
  labs(x="Studied Credits", y="Count") +
  ggtitle(label = "Studied Credits (bin = 10)")
creditsHist


### Number of Courses
courseHist <- ggplot(stdataInv, aes(x = num_courses)) +
  geom_histogram(breaks=seq(0, 4, by = 1), col="black", fill="orange", alpha = .5) +
  labs(x="Number of Courses", y="Count") +
  ggtitle(label = "Number of Courses (bin = 1)")
courseHist


table(stdataInv$num_courses)
#   1    2    3 
#2908   29    1


### Date Registration
regHist <- ggplot(stdataInv, aes(x = date_registration)) +
  geom_histogram(breaks=seq(-275, 70, by = 7), col="black", fill="orange", alpha = .5) +
  labs(x="Days (bin = 7)", y="Count") +
  ggtitle(label = "Days Registered Early (-) and Late (+)")
regHist


### Sum Clicks
clickHist <- ggplot(stdataInv, aes(x = sum_clicks_percourse)) +
  geom_histogram(breaks=seq(0, 3000, by = 100), col="black", fill="orange", alpha = .5) +
  labs(x="Sum Clicks (bin = 100)", y="Count") +
  ggtitle(label = "Student Interaction with Course")
clickHist

#This plot shows there are several observations with excessive clicks.  We'll explore this
# more in the Data Transformation section





####### Hist for Categorical Features



###Gender

genderHist <- ggplot(stdataInv, aes(x = gender )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Gender", y="Count") +
  ggtitle(label = "Gender") #+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
genderHist


###Region

regionHist <- ggplot(stdataInv, aes(x = region )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Region", y="Count") +
  ggtitle(label = "Region") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
regionHist


###HighEd

highedHist <- ggplot(stdataInv, aes(x = highest_eduation_us_equivalent )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Highest Education Level", y="Count") +
  ggtitle(label = "Highest Education Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
highedHist


### IMD Band

imdHist <- ggplot(stdataInv, aes(x = imd_band )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="IMD Band", y="Count") +
  ggtitle(label = "IMD Band") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
imdHist


### Age

ageHist <- ggplot(stdataInv, aes(x = age_band )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Age Band", y="Count") +
  ggtitle(label = "Age Band") #+
#theme(axis.text.x = element_text(angle = 45, hjust = 1))
ageHist


### Disability

disabilityHist <- ggplot(stdataInv, aes(x = disability )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Disability", y="Count") +
  ggtitle(label = "Disability") #+
#theme(axis.text.x = element_text(angle = 45, hjust = 1))
disabilityHist


### Final Results

resultHist <- ggplot(stdataInv, aes(x = final_result )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Final Results", y="Count") +
  ggtitle(label = "Final Results") #+
#theme(axis.text.x = element_text(angle = 45, hjust = 1))
resultHist


#### Figures ############################

#install.packages("ggpubr")
library(ggpubr)

figure1 <- ggarrange(popHist, densHist, medDownloadHist, avgDownloadHist, 
                     avgUsageHist, attemptsHist,creditsHist, courseHist, regHist, 
                     labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
                     ncol = 3, nrow = 3)
figure1



figure2 <- ggarrange(genderHist, highedHist, imdHist, ageHist, disabilityHist, resultHist,
                    labels = c("J", "K", "L", "M", "N", "O"),
                    ncol = 3, nrow = 2)
figure2




 
##### Basic Scatterplots #########

## Well begin by conducting an initial look, and then dive deeper in any that appear questionable

library(gpairs)
gpairs(stdataInv[ ,c(4,5,6,8,12,13,16,17,18,19)], diag.pars = list(fontsize = 7, show.hist = FALSE, hist.color = 'white'))



#All of the relationships look linear, but we'll do more investigation here:

clickscat <- ggplot(stdataInv, aes(x= sum_clicks_percourse , y = avg_score_all_assessments)) + 
  geom_point() + 
  ggtitle(label = "Student Interaction (clicks) vs. AVG Score")
clickscat

# Yup, that was ok

# but to look deeper, let's look at the correlation values

library(corrplot)
corrplot.mixed(cor(stdataInv[ ,c(4,5,6,7,8,12,13,16,17,18,19)]), upper = "ellipse", tl.pos = "n", lower.col = "black", number.cex = .7 )



# corrplot(stdataInv[ ,c(4,5,6,7,8,12,13,16,17,18,19)], type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# round(cor(stdataInv[,c(4,5,6,7,8,12,13,16,17,18,19) ]), digits = 2)

# What we see is that the median adn average downsload speed are very highly correlated.
# As such, we will only use median as it explains the smae thing, but has a more normal distibution.



# colnames(stdataInv[c(4,5,6,7,8,12,13,16,17,18,19)])



# Data Transformations that need to occur based on EDA #####################################################################################################

#During EDA, it was determined that avg_score_all_assessments had a significant portion of 0 vfor values.
# The boxplot below shows this.

boxplot(stdataInv$avg_score_all_assessments,
        main = "Student Average Score Overall",
        xlab = "AVG Score",
        col = "orange",
        border = "black",
        horizontal = TRUE,
        notch = TRUE)

# Now we will use the Fence method to identify outliers

summary(stdataInv$avg_score_all_assessments)
# I will use a lower fence identify outliers and remove them from them data.
# #IQR = 84.55-69 = 15.55
# Lower Fence 69 - (1.5 * 15.55) = 45.675
# Upper Fence 84.55 + (1.5 * 15.55) = 107,875 so MAX of 100

# so it appears there are no upper outliers but any alue below 45 is an outlier.
# However this accounts for approximately 5% of the data.  As such we are unwilling to simply
# remove the values.  That said, in keeping with our definition of an "Involved Stident", we are going to remove 
# students that scored a 0.  It's unsound to model students that were not enganged as determined by the
# amount of interaction (sum_clicks) and the lack of basic effort (0 avg score) it wouuld take to get a 0 score.

#With this decison made, we must re-establish the dataframe to meet this criteria
stdataInv <- stdataInv %>% filter(avg_score_all_assessments > 0)






# During EDA, it was aso determined that sum_clicks_percourse may have a significant amount of outliers. 
# We'll plot them using a blox plot to investigate

boxplot(stdataInv$sum_clicks_percourse,
        main = "Student Interaction (clicks)",
        xlab = "Clicks per course",
        col = "orange",
        border = "black",
        horizontal = TRUE,
        notch = TRUE)

# This plot shows there are some sognificant upper fence outliers.

summary(stdataInv$sum_clicks_percourse)
# I will use an upper fence identify outliers
#IQR = 1107 - 224.2 = 882.8
# Upper Fence = (1.5 * 882.8) + 1107.2 = 2431.4

# it appears any click value above 2431 is considered an outlier.
quantile(stdataInv$sum_clicks_percourse, probs = .91)
# In fact, they constitute nearly 9% of the data.
# As such, we will leave them in for now.



# Also our scatterplots showed us there is a significant correlation between median and avg 
# dowload speeds of a given region.  This is to be expected as they explain the same thing.
# Given Median Download speed has a more normal distibution
# so we will use this feature in favor of the Average.




# Create Train and Test Split ############################################################################

#A.	Generate a set of random indices that will allow you to choose cases for your training and data sets and assign it to a new variable name. The range of your new indices should span from 1 to the final element index of the diamond subset data (35,342 if previous steps have been done correctly). 

#Lets randomize each row by assigning random index numbers to each row

set.seed(101)

randIndex <- sample(1:dim(stdataInv)[1])
head(randIndex)

# Build a training dataset and test dataset. The training datatset should be 4/5 of the data, and the test dataset should be one fifth of the data.

cutPoint4_5 <- floor(4 * dim(stdataInv) [1]/5)

cutPoint4_5

stdataInvTrain <- stdataInv[randIndex[1:cutPoint4_5],]

stdataInvTest <- stdataInv[randIndex[(cutPoint4_5+1):dim(stdataInv)[1]],]



# Models ########################################################################################################

# Features not to be modeled:

# id_student - This variable provides no inforamtion and is simply a unique identifier for each student

# region - since Region is such a localized value, we wil remove it 
# and use pop density in its place so the finding are easier to 
# apply to areas outside these regions.

# final_result - Final result is nearly the same as our dependent variable but in a 
# categorical form.  As such, we will exclude it.

# region_broadband_average_download_speed_Mbitspers - Average broadband download speed.  
# Given Median Download speed has a more normal distibution we wil use that instead.


# Null Model
# The lowest model is the Null model, it is ascally teh average of avg_score_all_assessments 
nullModel <- lm(avg_score_all_assessments ~ 1, data = stdataInvTrain)
summary(nullModel)
#Check # mean(stdataInvTrain$avg_score_all_assessments)



# MODEL 1 
# Full minus those variables listed above
m1Full <- lm(avg_score_all_assessments ~  .- population_millions -id_student -region - final_result -region_broadband_average_download_speed_Mbitspers + disability*gender, 
             data = stdataInvTrain) 

summary(m1Full)



## MODEL 1 Diagnostics
#QQplot
ggplot(m1Full, aes(sample= .stdresid)) +
  stat_qq() +
  geom_abline() +
  ggtitle(label = "Normal Q-Q Plot")

FullHist <- ggplot(m1Full, aes(x = m1Full$residuals)) +
  geom_histogram(binwidth = 1, col="black", fill="orange", alpha = .5) +
  labs(x="Residuals", y="Count") +
  ggtitle(label = "Hist of Residuals")
FullHist




plot(m1Full, which = 2)

m1Full
hist(m1Full$residuals)

par(mfrow=c(1,1))
par(mfrow=c(2,2))
plot(m1Full, which = c(1,3,4,5))


###Additional Tests to execute
#test for constant error across all fitted values - Heteroscedasticity
library(lmtest)
bptest(m1Full)

# The results from teh test suggest we should reject teh Null in favor 
# of the Alternative Hypothesis aand conclude there is Heteroscedasticity.
# howeever, the Scale-Location plot shows a generally horizontal line 
# with the residulas appearing randomly spread for the most part. 


# Next is to test for Multicolinearity.  If the independent variables
# included in the model are correlated, the estimates of the individual
# parameter estimates will be unstable and thier errors inflated.

# To test for multicolinearity will we asesess variance inflation factors.
vif(m1Full)

# We can see that most of the coefficients are fine as the VIF is 1, and none exceed 10.
# However, as we determined before, we can see that populaiton_millions and region_density
# both score just over 2.  This is to be expected as populaiton density is derived from the populaiton.
# as a result, we should consider only modeling population density.

# We'll skip this one for now
#library(MASS)
#boxcox(m1Full)



###################################################
##Stepwise technique
# STEP MODEL
# Stepwise in both directions between Null and Full Mddel
stepModel <- step(nullModel,
                   scope=list(lower=nullModel, upper=m1Full),
                   direction = "both")

# Reveal the chosen model
stepModel

#Sumamrize the chosen Model
summary(stepModel)

##Step MODEL Diagnostics 
#QQplot
ggplot(stepModel, aes(sample= .stdresid)) +
  stat_qq() +
  geom_abline() +
  ggtitle(label = "Normal Q-Q Plot")


###############################################################

# MODEL 2

m2 <- lm(avg_score_all_assessments ~  imd_band 
         + region_broadband_median_download_speed_mbitpers + region_broadband_avg_data_usage_gbs
         + region_density + highest_eduation_us_equivalent + age_band + disability + disability*gender
         + gender  + sum_clicks_percourse
         + date_registration,  data = stdataInvTrain) 
summary(m2)

m2$coefficients

#MODEL 2 Diagnostics 
#QQplot
ggplot(m2, aes(sample= .stdresid)) +
  stat_qq() +
  geom_abline() +
  ggtitle(label = "Normal Q-Q Plot")


## Lets compare the three model coefficients

#install.packages("coefplot")
library(coefplot)
multiplot(m1Full, stepModel, m2)

#This plot helps show that with each of the thee models, the Intercept, Gender, 
# Highest Education Level, IMD Band, Age Band, NUmber of Course matter.


###Predict and Calculate PMSE###########################################################################################
#From coefplot package

#m1Full Prediction

m1FullPred <- predict(m1Full, newdata = stdataInvTest, se.fit = TRUE,
                      interval = "prediction", level = .95)
head(m1FullPred$fit)

m1FullPred <- data.frame(m1FullPred)

m1FullPred$y <- stdataInvTest$avg_score_all_assessments

m1FullPred$error <- m1FullPred$y - m1FullPred$fit.fit

m1FullPred$PredSSE <- m1FullPred$error^2

m1FUllPMSE <- (sum(m1FullPred$PredSSE)/dim(m1FullPred)[1])

head(m1FullPred)


#stepModel Prediction

stepModelPred <- predict(stepModel, newdata = stdataInvTest, se.fit = TRUE,
                      interval = "prediction", level = .95)
head(stepModelPred$fit)

stepModelPred <- data.frame(stepModelPred)

stepModelPred$y <- stdataInvTest$avg_score_all_assessments

stepModelPred$error <- stepModelPred$y - stepModelPred$fit.fit

stepModelPred$PredSSE <- stepModelPred$error^2

stepModelPredPMSE <- (sum(stepModelPred$PredSSE)/dim(stepModelPred)[1])

head(stepModelPred)



#MODEL 2 Prediction

m2Pred <- predict(m2, newdata = stdataInvTest, se.fit = TRUE,
                         interval = "prediction", level = .95)
head(m2Pred$fit)

m2Pred <- data.frame(m2Pred)

m2Pred$y <- stdataInvTest$avg_score_all_assessments

m2Pred$error <- m2Pred$y - m2Pred$fit.fit

m2Pred$PredSSE <- m2Pred$error^2

m2PredPMSE <- (sum(m2Pred$PredSSE)/dim(m2Pred)[1])

head(m2Pred)



### MSE/PMES Comparison

anova(m1Full)
m1FullMSE <- 115.4
#ratio = 1.17

anova(stepModel)
stepModelMSE <- 115.7
#ratio = 1.17

anova(m2)
m2MSE <- 115.6
#ratio = 1.15

# Each model passes the General Rule check where PMSE/MSE < 2, model is fine.

#### Condidence LEvels
confint(m1, conf.level = 0.95)



##########  LOGIT Model
# Could ask: How does the probability that a student will drop out of online study (Withdraw) depend
# on the x, y, and z, of the student (put in a few features for X, Y, Z)









######notes########################

stdataInv %>% filter(avg_score_all_assessments == 0 & final_result == "Distinction")%>% 
  summarise(count=n())

table(stdataInv$final_result)


scorebyreg <- stdataInv %>% group_by(date_registration) %>% summarize(mean(avg_score_all_assessments))
max(stdataInv$region_broadband_median_download_speed_mbitpers)
