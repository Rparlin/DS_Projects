# Ralph Parlin - Group 1 Project



dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

setwd("C:/Users/katie/Desktop/Syracuse University/IST 719 - Information Visualization/R Files")
getwd()

#Read in the data
df <- read.csv("C:/Users/katie/Desktop/Syracuse University/IST 719 - Information Visualization/R Files/USCG Consolidated DataFile.csv")

library(tidyverse)

#install.packages("remotes")
#remotes::install_github("ramnathv/tidymetrics")
library(tidymetrics)

head(df)
str(df)
class(df)
dim(df)
View(df)
## Notes boat sales are up a result of COVID
## With more people boating, it's important to understand more about it
## Accidents by State, Injuries by state, deaths by state. Perhaps report as a percent of boating to normalize the effects

#### Data Cleaning ###########################################

#Rename Columns
colnames(df)

df <- df %>% rename(
  Year = ï..ReleasableAccidents_Year, #this name sometimes need to be recopied since it has a wierd character at the begining
  Day1y = Day..1...yes.,
  Death = Death..1...Yes.,
  Injury = Injury..1...Yes.)

#Change data type
library(lubridate)

#transform date column to date rather than factor
df$Year <- as.factor(df$Year)
df$Death <- as.factor(df$Death)
df$Injury <- as.factor(df$Injury)
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")



#Address NAs
#install.packages("mice")
library(mice)
md.pattern(df, rotate.names = TRUE)

#visualize the missing data

#install.packages("VIM")
library(VIM)
mice_plot <- aggr(df, col=c('grey','orange'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(df), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
# Columns with missing data and percent missing:
# DeceasedAge 0.88959044
# InjuredAge 0.62320819
# OperatorAge 0.18071672
# Yearbuilt 0.10145051
# Length 0.05017065
# NumberPeopleOnboard 0.04027304

# Deceased Age, Injured Age are conditional on if those events occurred
#Deceased Age
#Let's first look at the distibution of deceased age to determine if we replace with mean or median
hist(df$DeceasedAge)
sum(is.na(df$DeceasedAge))
# It's left skewed however there are way too many to apply a single number.
# Doing so will result in very distorted distribution.  Instead, I'll replace NAs with random numbers conatained in the 
# distribution.
#install.packages("imputeTS")
library(imputeTS)
# It's relatively normal so we'll use mean
df$DeceasedAge <- round(ifelse(df$Death == "1" & is.na(df$DeceasedAge), na_random(x = df$DeceasedAge, lower_bound = min(df$DeceasedAge, na.rm = TRUE), upper_bound = max(df$DeceasedAge, na.rm = TRUE)), df$DeceasedAge), digits = 0)


# Injured Age
#Let's first look at the distibution of injured age to determine if we replace with mean or median
hist(df$InjuredAge)
# too many to use a single number, will use random
df$InjuredAge <- round(ifelse(df$Injury == "1" & is.na(df$InjuredAge), na_random(x = df$InjuredAge, lower_bound = min(df$InjuredAge, na.rm = TRUE), upper_bound = max(df$InjuredAge, na.rm = TRUE)), df$InjuredAge), digits = 0)
#df$InjuredAge <- ifelse(df$Injury == "1" & is.na(df$InjuredAge), median(df$InjuredAge, na.rm = TRUE), df$InjuredAge)
# Replaced NAs for records that inlcuded Injury but had no age with median of 33



# Operator Age
#Let's first look at the distibution of injured age to determine if we replace with mean or median or random
hist(df$OperatorAge)
abline(v=quantile(df$OperatorAge, 0.01), col = "red")
abline(v=quantile(df$OperatorAge, 0.05), col = "red")
abline(v=quantile(df$OperatorAge, 0.50), col = "blue")
abline(v=quantile(df$OperatorAge, 0.95), col = "red")
abline(v=quantile(df$OperatorAge, 0.99), col = "red")

df$OperatorAge <- round(ifelse(is.na(df$OperatorAge), na_random(x = df$OperatorAge, lower_bound = min(df$OperatorAge, na.rm = TRUE), upper_bound = max(df$OperatorAge, na.rm = TRUE)), df$OperatorAge), digits = 0)


# Yearbuilt 
#Let's first look at the distribution of year the boat was built to determine if we replace with mean or median
hist(df$Yearbuilt)
sum(is.na(df$Yearbuilt)) #there are too many to use mean or median, I'll use random based on the distribution
# Left skewed so we'll use median
df$Yearbuilt <- round(ifelse(is.na(df$Yearbuilt), na_random(x = df$Yearbuilt, lower_bound = min(df$Yearbuilt, na.rm = TRUE), upper_bound = max(df$Yearbuilt, na.rm = TRUE)), df$Yearbuilt), digits = 0)


# Length
#Let's first look at the distribution of boat length to determine if we replace with mean or median
hist(df$Length)
sum(is.na(df$Length))
# Too many, we'll use random
df$Length <- round(ifelse(is.na(df$Length), na_random(x = df$Length, lower_bound = min(df$Length, na.rm = TRUE), upper_bound = max(df$Length, na.rm = TRUE)), df$Length), digits = 0)


# NumberPeopleOnboard
#Let's first look at the distribution of number of people on board to determine if we replace with mean or median
hist(df$NumberPeopleOnboard)
sum(is.na(df$NumberPeopleOnboard))
# Right Skewed so we'll use median
df$NumberPeopleOnboard <- round(ifelse(is.na(df$NumberPeopleOnboard), na_random(x = df$NumberPeopleOnboard, lower_bound = min(df$NumberPeopleOnboard, na.rm = TRUE), upper_bound = max(df$NumberPeopleOnboard, na.rm = TRUE)), df$NumberPeopleOnboard), digits = 0)


#Address Duplicate Records
# Find and eliminate duplicate rows
df <- df %>% distinct()
# reduced the df from 11720 observations to 11626 observations

###Factor Inspection#############################################################

#Inspect Factor Levels and fix typos and levels

# Covert to Factor
df$OperatorUsingAlcohol <- as.factor(df$OperatorUsingAlcohol)


levels(df$WaterConditions)
df$WaterConditions[df$WaterConditions=="choppy"] <-"Choppy"
df$WaterConditions[df$WaterConditions=="rough"] <-"Rough"
df$WaterConditions[df$WaterConditions=="unknown"] <-"Unknown"
df$WaterConditions[df$WaterConditions=="Very rough"] <-"Very Rough"
df$WaterConditions <- droplevels(df$WaterConditions)
df$WaterConditions <- factor(df$WaterConditions, c("Very Rough", "Rough", "Choppy", "Calm", "Unknown" ))


levels(df$Visibility)
df$Visibility[df$Visibility=="unknown"] <-"Unknown"
df$Visibility <- droplevels(df$Visibility)
df$Visibility <- factor(df$Visibility, c("Poor", "Fair", "Good", "Unknown"))

levels(df$Wind)
df$Wind <- factor(df$Wind, c("Storm", "Strong", "Moderate", "Light", "None", "Unknown"))

levels(df$DayofWeek)
df$DayofWeek <- factor(df$DayofWeek, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

levels(df$AccidentCause)

levels(df$OperatorGender)
#make a new level
levels(df$OperatorGender) <- c(levels(df$OperatorGender),"Unknown")
df$OperatorGender[df$OperatorGender=="m"] <-"M"
df$OperatorGender[df$OperatorGender=="U"] <-"Unknown"
df$OperatorGender <- droplevels(df$OperatorGender)
df$OperatorGender <- factor(df$OperatorGender, c("F", "M", "Unknown"))



# These new replacements are easier if the column is a charter
df$DeceasedGender <- as.character(df$DeceasedGender)
df$DeceasedGender <- ifelse(df$DeceasedGender == "m", "M", df$DeceasedGender)
df$DeceasedGender <- ifelse(df$DeceasedGender == "f", "F", df$DeceasedGender)
df$DeceasedGender <- ifelse(df$DeceasedGender == "U", "Unknown", df$DeceasedGender)
df$DeceasedGender <- ifelse(df$Death == 1 & df$DeceasedGender == "", "Unknown", df$DeceasedGender)
df$DeceasedGender <- ifelse(df$DeceasedGender == "", "", df$DeceasedGender)
# Convert back to factor
df$DeceasedGender <- as.factor(df$DeceasedGender)

#Inspect the work
table(df$DeceasedGender)
levels(df$DeceasedGender)
#Inspect
df %>% filter(Death == 1) %>% group_by(DeceasedGender) %>% count() #use tally/count for factors



# These new replacements are easier if the column is a charter
df$InjuredGender <- as.character(df$InjuredGender)
df$InjuredGender <- ifelse(df$InjuredGender == "m", "M", df$InjuredGender)
df$InjuredGender <- ifelse(df$InjuredGender == "f", "F", df$InjuredGender)
df$InjuredGender <- ifelse(df$InjuredGender == "U", "Unknown", df$InjuredGender)
df$InjuredGender <- ifelse(df$Injury == 1 & df$InjuredGender == "", "Unknown", df$InjuredGender)
df$InjuredGender <- ifelse(df$InjuredGender == "", "", df$InjuredGender)
df$InjuredGender <- as.factor(df$InjuredGender)
# inspect the work
levels(df$InjuredGender) 
df %>% filter(Injury == 1) %>% group_by(InjuredGender) %>% count() #use tally/count for factors




# These new replacements are easier if the column is a charter
df$InjuryType <- as.character(df$InjuryType)
df$InjuryType <- ifelse(df$Injury == 1 & df$InjuryType == "", "Unknown", df$InjuryType)
df$InjuryType <- as.factor(df$InjuryType)
# Inspect the work
levels(df$InjuryType)
df %>% filter(Injury == 1) %>% group_by(InjuryType) %>% count() #use tally/count for factors





levels(df$AccidentEvent)
df$AccidentEvent[df$AccidentEvent=="falls overboard"] <-"Falls overboard"
df$AccidentEvent[df$AccidentEvent=="Fall in Vessel"] <-"Fall in vessel"
#add a new level to  combine other levels into
levels(df$AccidentEvent) <- c(levels(df$AccidentEvent),"Collision with another vessel")
#now combine levels
df$AccidentEvent[df$AccidentEvent=="Collision with recreational Vessel"] <-"Collision with another vessel"
df$AccidentEvent[df$AccidentEvent=="Collision with recreational vessel"] <-"Collision with another vessel"
df$AccidentEvent[df$AccidentEvent=="Collision with commercial vessel"] <-"Collision with another vessel"
df$AccidentEvent[df$AccidentEvent=="Collision with government vessel"] <-"Collision with another vessel"
df$AccidentEvent <- droplevels(df$AccidentEvent)
df$AccidentEvent <- factor(df$AccidentEvent,c("Capsizing", "Carbon monoxide exposure", "Collision with another vessel", "Collision with fixed object", "Collision with floating object", "Collision with submerged object",
"Electrocution", "Fall in vessel","Falls overboard", "Fire/explosion (fuel)", "Fire/explosion (non-fuel)",
"Fire/explosion (unknown origin)", "Flooding/swamping", "Grounding", "Person departed vessel",
"Person ejected from vessel", "Person struck by propeller", "Person struck by vessel", "Skier mishap",
"Sudden medical condition", "Other"))         



levels(df$Operation)
df$Operation[df$Operation=="Being towed"] <-"Towing"
df$Operation[df$Operation=="towing"] <-"Towing"
df$Operation <- droplevels(df$Operation)


# This will be easier to clean if converted to char first
df$CauseofDeath <- as.character(df$CauseofDeath)
df$CauseofDeath <- ifelse(df$Death == 1 & df$CauseofDeath == "", "Unknown", df$CauseofDeath)
df$CauseofDeath <- as.factor(df$CauseofDeath)
levels(df$CauseofDeath)
#Inspect work
df %>% filter(Death == 1) %>% group_by(CauseofDeath) %>% count() #use tally/count for factors


# This will be easier to clean if converted to char first
df$DeceasedRole <- as.character(df$DeceasedRole)
df$DeceasedRole <- ifelse(df$Death == 1 & df$DeceasedRole == "", "Unknown", df$DeceasedRole)
df$DeceasedRole <- ifelse(df$Death == 1 & df$DeceasedRole == "other", "Other", df$DeceasedRole)
df$DeceasedRole <- as.factor(df$DeceasedRole)
levels(df$DeceasedRole)
#Inspect the work
df %>% filter(Death == 1) %>% group_by(DeceasedRole) %>% count() #use tally/count for factors


# This will be easier to clean if converted to char first
df$InjuredRole <- as.character(df$InjuredRole)
df$InjuredRole <- ifelse(df$Injury == 1 & df$InjuredRole == "", "Unknown", df$InjuredRole)
df$InjuredRole <- as.factor(df$InjuredRole)
levels(df$InjuredRole)
#Inspect the work
df %>% filter(Injury == 1) %>% group_by(InjuredRole) %>% count() #use tally/count for factors


#Create a new column for accident outcome
# set all to Accident Only
df$Outcome <- "AccidentOnly"
# Set to Death if Death = 1
df$Outcome<- ifelse(df$Death == 1 & df$Injury == 0 , "DeathOnly", df$Outcome)
#Set to Injury if Injury = 1
df$Outcome<- ifelse(df$Injury == 1 & df$Death == 0 , "InjuryOnly", df$Outcome)
#set Injury to Injury and Death if both Injury adn Death = 1
df$Outcome<- ifelse(df$Injury == 1 & df$Death == 1, "Injury and Death", df$Outcome)
#Convert the column to factor
df$Outcome <- as.factor(df$Outcome)
df$Outcome <- factor(df$Outcome, levels = c("AccidentOnly", "InjuryOnly", "DeathOnly", "Injury and Death"))
#View(df)

#Using the logic above, we need to partition Datasets to create one for Accidents ONly, INjury Only, Death Only, 
# Death and injury and accidents only, and then the union of some of these.

dfAccidentOnly <- df %>% filter(df$Death == 0 & df$Injury == 0)
dfAccidentwithDeath <- df %>% filter(df$Death == 1)
dfAccidentwithInjury <- df %>% filter(df$Injury == 1)

dfDeathOnly <- df %>% filter(df$Death == 1 & df$Injury == 0)
dfInjuryOnly <- df %>% filter(Injury ==1 & Death == 0)

dfInjuryandDeath <- df %>% filter(df$Death == 1 & df$Injury == 1)
dfInjuryORDeath <- df %>% filter(df$Death == 1 | df$Injury == 1)



##### EDA ########################

# Numeric Columns
'''  Numeric
NumberDeaths         
NumberInjured       
NumberVesselsInvolved
TotalDamage          
OperatorAge
Length 
Yearbuilt  
NumberPeopleOnboard
DeceasedAge
InjuredAge'''



#Plots
numDeathsHist <- df %>% filter(Death == 1) %>% 
  ggplot(aes(x = NumberDeaths))+
  geom_histogram(breaks=seq(0, 20, by = 1), color="black", fill="Orange", alpha = .5)  +
  labs(x="Number of Deaths", y="Count") +
  ggtitle(label = "Number of Deaths per Accident", subtitle = "For accidents that involve death, below is the distubution of how many people die.\nAs we can see, most often only one person dies." ) 
  # geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.5) , size=4)
#theme(axis.text.x = element_text(angle = 45, hjust = 1), 
numDeathsHist

table(dfAccidentwithDeath$NumberDeaths)


numInjured <- df %>% filter(Injury ==  1) %>% 
  ggplot(aes(x = NumberInjured))+
  geom_histogram(breaks=seq(0, 20, by = 1), color="black", fill="Orange", alpha = .5)  +
  labs(x="Number of Injuries", y="Count") +
  ggtitle(label = "Number of Injuries per Accident", subtitle = "For accidents that involve injury, below is the distubution of how many people get injured per accident.\nAs we can see, most often only one person gets injured." )
#theme(axis.text.x = element_text(angle = 45, hjust = 1), 
numInjured

table(dfAccidentwithInjury$NumberInjured)


numVesselsInvolved <- ggplot(df, aes(x = NumberVesselsInvolved))+
  geom_histogram(breaks=seq(0, 25, by = 1), color="black", fill="Orange", alpha = .5)  +
  labs(x="Number of Vessels Involved", y="Count") +
  ggtitle(label = "Number of Vessels Involved per Accident")
#theme(axis.text.x = element_text(angle = 45, hjust = 1), 
numVesselsInvolved

mean(df$TotalDamage)



TotalDamage <- ggplot(df, aes(x = TotalDamage)) +
  geom_histogram(breaks=seq(0, 100000, by = 1000), color="black", fill="Orange", alpha = .5)  +
  labs(x="Total Damage (by $1000)", y="Count") +
  ggtitle(label = "Total Damage per Accident: All reports in ($1000)")
#theme(axis.text.x = element_text(angle = 45, hjust = 1), 
TotalDamage

TotalDamageInj <- df %>% filter(Injury == 1) %>% 
  ggplot(aes(x = TotalDamage))+
  geom_histogram(breaks=seq(0, 100000, by = 1000), color="black", fill="Orange", alpha = .5)  +
  labs(x="Total Damage (by $1000)", y="Count") +
  ggtitle(label = "Total Damage per Accident: When an injury occurs in ($1000)")
#theme(axis.text.x = element_text(angle = 45, hjust = 1), 
TotalDamageInj



TotalDamageDeath <- df %>% filter(Death == 1) %>% 
  ggplot(aes(x = TotalDamage))+
  geom_histogram(breaks=seq(0, 100000, by = 1000), color="black", fill="Orange", alpha = .5)  +
  labs(x="Total Damage (by $1000)", y="Count") +
  ggtitle(label = "Total Damage per Accident: When a death occurs in ($1000)")
#theme(axis.text.x = element_text(angle = 45, hjust = 1), 
TotalDamageDeath



OperatorAge <- ggplot(df, aes(x = OperatorAge))+
  geom_histogram(breaks=seq(0, 100, by = 1), color="black", fill="Orange", alpha = .5)  +
  labs(x="Operator Age", y="Count") +
  ggtitle(label = "Operator Age per Accident")
#theme(axis.text.x = element_text(angle = 45, hjust = 1), 
OperatorAge


Length <- ggplot(df, aes(x = Length))+
  geom_histogram(breaks=seq(0, 100, by = 1), color="black", fill="Orange", alpha = .5)  +
  labs(x="Boat Length", y="Count") +
  ggtitle(label = "Boat Length per Accident")
#theme(axis.text.x = element_text(angle = 45, hjust = 1), 
Length


Yearbuilt <- ggplot(df, aes(x = Yearbuilt))+
  geom_histogram(breaks=seq(1940, 2020, by = 1), color="black", fill="Orange", alpha = .5)  +
  labs(x="Year of Boat", y="Count") +
  ggtitle(label = "Year of Boat per Accident")
#theme(axis.text.x = element_text(angle = 45, hjust = 1), 
Yearbuilt


NumberPeopleOnboard <- ggplot(df, aes(x = NumberPeopleOnboard))+
  geom_histogram(breaks=seq(0, 20, by = 1), color="black", fill="Orange", alpha = .5)  +
  labs(x="Number of People Onboard", y="Count") +
  geom_vline(aes(xintercept=mean(df$NumberPeopleOnboard)), color="red", linetype="dashed", size=1) +
  ggtitle(label = "Number of People Onboard per Accident (red line is mean)")
#theme(axis.text.x = element_text(angle = 45, hjust = 1), 
NumberPeopleOnboard

# Does the number of people on board lead to more accidents
plot(df$NumberPeopleOnboard, df$Outcome)

DeceasedAge <- df %>% filter(Death == 1) %>% 
  ggplot(aes(x = DeceasedAge))+
  geom_histogram(breaks=seq(0, 100, by = 1), color="black", fill="Orange", alpha = .5)  +
  labs(x="Deceased Age", y="Count") +
  ggtitle(label = "Deceased Age per Accident Involving Death")
#theme(axis.text.x = element_text(angle = 45, hjust = 1), 
DeceasedAge


InjuredAge <- df %>% filter(Injury == 1) %>% 
  ggplot(aes(x = InjuredAge))+
  geom_histogram(breaks=seq(0, 100, by = 1), color="black", fill="Orange", alpha = .5)  +
  labs(x="Injured Age", y="Count") +
  ggtitle(label = "Injured Age per Accident Involving Injury")
#theme(axis.text.x = element_text(angle = 45, hjust = 1), 
InjuredAge


# Categorical Columns
#State
'''WaterConditions
Wind
Visibility
DayofWeek
AccidentCause
AccidentEvent
OperatorGender
VesselType
Operation
Activity
DeceasedGender
CauseoDeath
DeceasedPFDWorn
DeceasedRole
InjuredGender
InjuredAge
InjuryType
InjuredRole
Outcome
'''

#Outcome
Outcome <- ggplot(df, aes(x = Outcome )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Outcome", y="Count") +
  ggtitle(label = "Outcomes of Boating Accidents") 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
Outcome

table(df$Outcome)
round(prop.table(table(df$Outcome)),digits = 3)

#State
State<- ggplot(df, aes(x = State )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="State", y="Count") +
  ggtitle(label = "State of Boating Accidents") +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))
State



#WaterConditions
WaterConditions <- ggplot(df, aes(x = WaterConditions )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="WaterConditions", y="Count") +
  ggtitle(label = "Waterconditions of Boating Accidents") 
#theme(axis.text.x = element_text(angle = 45, hjust = 1))
WaterConditions

#Wind
Wind <- ggplot(df, aes(x = Wind )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Wind", y="Count") +
  ggtitle(label = "Wind of Boating Accidents") 
#theme(axis.text.x = element_text(angle = 45, hjust = 1))
Wind


#Visibility
Visibility <- ggplot(df, aes(x = Visibility )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Visibility", y="Count") +
  ggtitle(label = "Visibility of Boating Accidents") 
#theme(axis.text.x = element_text(angle = 45, hjust = 1))
Visibility


#DayofWeek
DayofWeek <- ggplot(df, aes(x = DayofWeek )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="DayofWeek", y="Count") +
  ggtitle(label = "DayofWeek of Boating Accidents") 
#theme(axis.text.x = element_text(angle = 45, hjust = 1))
DayofWeek


#AccidentCause
AccidentCause <- ggplot(df, aes(x = AccidentCause )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Accident Cause", y="Count") +
  ggtitle(label = "Accident causes of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
AccidentCause


#AccidentEvent
AccidentEvent <- ggplot(df, aes(x = AccidentEvent )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Accident Event", y="Count") +
  ggtitle(label = "Accident Event of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
AccidentEvent


#operatorGender
OperatorGender <- df %>% filter(OperatorGender == "M" | OperatorGender == "F") %>% 
  ggplot(aes(x = OperatorGender)) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Operator Gender", y="Count") +
  ggtitle(label = "Operator Gender of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
OperatorGender

#VesselType
VesselType <- ggplot(df, aes(x = VesselType )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="VesselType", y="Count") +
  ggtitle(label = "VesselType of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
VesselType

#Operation
Operation <- ggplot(df, aes(x = Operation )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Operation", y="Count") +
  ggtitle(label = "Operation of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Operation

#Activity
Activity<-ggplot(df, aes(x = Activity )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Activity", y="Count") +
  ggtitle(label = "Activity of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Activity



#DeceasedGender
DeceasedGender <- df %>% filter(Death == 1) %>% 
  ggplot(aes(x = DeceasedGender )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="DeceasedGender", y="Count") +
  ggtitle(label = "DeceasedGender of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
DeceasedGender

#CauseofDeath
CauseofDeath <- df %>% filter(Death ==1) %>% 
  ggplot(aes(x = CauseofDeath )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="CauseofDeath", y="Count") +
  ggtitle(label = "Cause of Death of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
CauseofDeath

#DeceasedRole
DeceasedRole <- df %>% filter(Death == 1) %>% 
  ggplot(aes(x = DeceasedRole )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="DeceasedRole", y="Count") +
  ggtitle(label = "Deceased Role of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
DeceasedRole

table(df$DeceasedRole)




#InjuredGender
InjuredGender <- df %>% filter(Injury == 1) %>% 
  ggplot(aes(x = InjuredGender )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Injured Gender", y="Count") +
  ggtitle(label = "Injured Gender of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
InjuredGender

#InjuryType
InjuryType <- df %>% filter(Injury == 1) %>% 
  ggplot(aes(x = InjuryType )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Injury Type", y="Count") +
  ggtitle(label = "Injury type of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
InjuryType


#InjuredRole
InjuredRole <- df %>% filter(Injury == 1) %>% 
  ggplot(aes(x = InjuredRole )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Injured Role", y="Count") +
  ggtitle(label = "Injured Role of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
InjuredRole





#USmap for the total number of death, injured, and vessels involved in the boating accident.
#NumberDeath
TotalDeath <- aggregate(df$NumberDeaths,list(df$State,df$Year),FUN=sum)
TotalDeath  
TotalDeath<-TotalDeath%>%rename(
  state=Group.1,
  Year=Group.2,
  TotalNumberDeath=x
)
TotalDeath<-as.data.frame(TotalDeath)
TotalDeath
str(TotalDeath)
#install.packages("usmap")
library(usmap)
TotalNumberDeath<-plot_usmap(data=TotalDeath,values="TotalNumberDeath",color="red")+
  scale_fill_continuous(
    low="white",high="red",name="Total Number of Death", label=scales::comma
  )+theme(legend.position = "right")
TotalNumberDeath

#NumberInjured
TotalInjured<-aggregate(df$NumberInjured,list(df$State,df$Year),FUN=sum)
TotalInjured  
TotalInjured<-TotalInjured%>%rename(
  state=Group.1,
  Year=Group.2,
  TotalNumberInjured=x
)
TotalInjured<-as.data.frame(TotalInjured)
TotalInjured
str(TotalInjured)

TotalNumberInjured<-plot_usmap(data=TotalInjured,values="TotalNumberInjured",color="dark blue")+
  scale_fill_continuous(
    low="white",high="blue",name="Total Number of Injured", label=scales::comma
  )+theme(legend.position = "right")
TotalNumberInjured

#NUmberVesselsInvolved
TotalVessels<-aggregate(df$NumberVesselsInvolved,list(df$State,df$Year),FUN=sum)
TotalVessels
TotalVessels<-TotalVessels%>%rename(
  state=Group.1,
  Year=Group.2,
  TotalNumberVessels=x
)
TotalVessels<-as.data.frame(TotalVessels)
TotalVessels
str(TotalVessels)

TotalNumberVessels<-plot_usmap(data=TotalVessels,values="TotalNumberVessels",color="purple")+
  scale_fill_continuous(
    low="white",high="purple",name="Total Number of Vessels involved", label=scales::comma
  )+theme(legend.position = "right")
TotalNumberVessels


#From the barplot for "Accident causes of boating accidents", most of the accidents are caused by humans.
#For example, operator inattention,improper lookout, operator inexperience, alcohol use, and so on.
#It is worth to look closely for the operators situations at this point. 

#Stacked Bar plot for outcome and accident cuases
OutcomeAccidentCause<-table(df$Outcome,df$AccidentCause)
OutcomeAccidentCause
barplot(as.matrix(OutcomeAccidentCause))

barplot(OutcomeAccidentCause,main = "Stacked Bar Plot for Outcome",xlab = "AccidentCause",ylab="Outcome",
        col = c("red","yellow","orange","blue"),pch=2,
        legend.text = c("AccidentOnly", "InjuryOnly","DeathOnly","Injury and Death"),las=2,
        xpd=T,bty="n")



#Another way to do it with GGplot
stackOutcomeCause <- df %>%  filter(AccidentCause == "Operator inattention" | AccidentCause == "Improper lookout" | AccidentCause == "Operator inexperience" | AccidentCause == "Excessive speed" | AccidentCause == "Alcohol use") %>% 
  ggplot(aes(fill= Outcome, x = AccidentCause)) +
  geom_bar(position = "stack", stat = "count") +
  labs(x="Accident Cause", y="Count") +
  ggtitle(label = "The Outcome of Accidents Given Top 5 Causes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
stackOutcomeCause


TopCause <- df %>%  filter(AccidentCause == "Operator inattention" | AccidentCause == "Improper lookout" | AccidentCause == "Operator inexperience" | AccidentCause == "Excessive speed" | AccidentCause == "Alcohol use") %>% 
  select(AccidentCause, Outcome) %>% 

 
table(TopCause)



Top5StackedOutcome <- df %>% cross_by_dimensions(Outcome, AccidentCause) %>% 
  summarize(Countme = n()) %>% 
  arrange(desc(Countme)) %>% 
  top_n(5, Countme) %>%
  ungroup() %>%
  ggplot(aes(fill= Outcome, x = AccidentCause, y = Countme))+
  geom_bar(position = "stack", stat = "identity") +
  labs(x="Accident Cause", y="Count") +
  ggtitle(label = "The Outcome of Accidents Given their Cause") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Top5StackedOutcome

Top5StackedOutcome <- df %>% group_by(Outcome, AccidentCause) %>% 
  summarize(Countme = n()) %>% 
  arrange(desc(Countme)) %>% 
  top_n(5, Countme) %>%
  ungroup() %>%
  ggplot(aes(fill= Outcome, x = AccidentCause, y = Countme))+
  geom_bar(position = "stack", stat = "identity") +
  labs(x="Accident Cause", y="Count") +
  ggtitle(label = "The Outcome of Accidents Given their Cause") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Top5StackedOutcome





#use of alcohol for Outcomes
IDalcohol<-table(df$OperatorUsingAlcohol,df$Outcome)
IDalcohol
barplot(as.matrix(IDalcohol))

barplot(IDalcohol,main = "Stacked Bar Plot for outcomes and Alcohol using",
        xlab = "Outcome",ylab="Alcohol",
        col = c("brown","yellow"),pch=2,
        legend.text = c("No", "Yes"),las=2)

#Another way to do it with GGplot
stackOutcomeAlchohol <- ggplot(df, aes(fill= OperatorUsingAlcohol, x = Outcome))+
  geom_bar(position = "stack", stat = "count") +
  labs(x="Accident Outcome", y="Count") +
  ggtitle(label = "The Outcome of Accidents Given Alcohol USe") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
stackOutcomeAlchohol


######## Primary Charts ########################################################################################

####Build a donut chart TOTAL
# Convert outcome to a percent
dfPercent <- df %>% group_by(Year, Outcome) %>%
 count() %>% mutate(Percent = n/nrow(df)*100)

#compute cumulative % (top of each rectangle)
dfPercent$ymax <- cumsum(dfPercent$Percent)

# Compute the bottom of each rectangle
dfPercent$ymin <- c(0, head(dfPercent$ymax, n=-1))

# Compute label position
dfPercent$labelPosition <- (dfPercent$ymax + dfPercent$ymin) / 2

# Compute a good label
dfPercent$label <- paste0(dfPercent$Outcome, "\n Percent: ", round(dfPercent$Percent, digits = 1))

# Make the plot
ggplot(dfPercent, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Outcome)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

# Make the plot
ggplot(dfPercent, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Outcome)) +
  geom_rect() +
  geom_text( x=2.2, aes(y=labelPosition, label=label, color=Outcome), size=3) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=7) +
  scale_color_brewer(palette=7) +
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none")



### Year versus year count

Yearbarplot <- df %>% group_by(Year) %>% 
  count() %>% 
  ggplot(aes(x = Year, y = n)) +
  geom_bar(col="black", fill="orange", alpha = .5, stat = "identity") +
  labs(x="Accident Event", y="Count") +
  ggtitle(label = "Accident Event of Boating Accidents")
Yearbarplot
  



####Summary Stats

#Gender Splilt
df %>% filter(OperatorGender != "Unknown") %>% 
  count(OperatorGender) %>% 
  group_by(OperatorGender)




#AccidentEvent Top 5
AccidentEvent <- ggplot(df, aes(x = AccidentEvent )) +
  geom_bar(col="black", fill="orange", alpha = .5) +
  labs(x="Accident Event", y="Count") +
  ggtitle(label = "Accident Event of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
AccidentEvent



#Activities Top 5
TopActivities <- df %>% 
  group_by(Activity) %>% 
  summarize(Countme = n()) %>% 
  arrange(desc(Countme)) %>% 
  top_n(5, Countme) %>% 
  ggplot(aes(x = Activity, y = Countme )) +
  geom_bar(col="black", fill="orange", alpha = .5, stat = "identity") +
  labs(x="Activity", y="Count") +
  ggtitle(label = "Top 5 Activity of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
TopActivities



#Age Boxplots
injuredAgeBox <- df %>% filter(Injury == 1) %>% 
  ggplot(aes(x = InjuredAge)) +
  geom_boxplot(col="black", fill="orange", alpha = .5 ,outlier.colour="orange", outlier.shape=8,
               outlier.size=4)+
  coord_flip()
injuredAgeBox 

mean(df$InjuredAge, na.rm = TRUE)


deathAgeBox <- df %>% filter(Death== 1) %>% 
  ggplot(aes(x = DeceasedAge)) +
  geom_boxplot(col="black", fill="orange", alpha = .5 ,outlier.colour="orange", outlier.shape=8,
               outlier.size=4)+
  coord_flip()
deathAgeBox 

mean(df$DeceasedAge, na.rm = TRUE)



#### Role Plots
TopDeceasedRole <- df %>% filter(Death == 1 & DeceasedRole != "Unknown" & DeceasedRole != "Other") %>% 
  group_by(DeceasedRole) %>% 
  summarize(Countme = n()) %>% 
  arrange(desc(Countme)) %>% 
  top_n(5, Countme) %>% 
  ggplot(aes(x = reorder(DeceasedRole, -Countme), y = Countme)) + #if we add a y, we need to put stat = identity below)
  geom_bar(col="black", fill="orange", alpha = .5, stat = "identity") +
  labs(x="Deceased Role", y="Count") +
  ggtitle(label = "Top 5 Dceased Role of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
TopDeceasedRole


TopInjuredRole <- df %>% filter(Injury == 1 & InjuredRole != "Unknown") %>% 
  group_by(InjuredRole) %>% 
  summarize(Countme = n()) %>% 
  arrange(desc(Countme)) %>% 
  top_n(5, Countme) %>% 
  ggplot(aes(x = InjuredRole, y = Countme)) + #if we add a y, we need to put stat = identity below)
  geom_bar(col="black", fill="orange", alpha = .5, stat = "identity") +
  labs(x="Injured Role", y="Count") +
  ggtitle(label = "Top 5 Injured Role of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
TopInjuredRole


#### Injury / Death Type Plots

TopInjury <- df %>% filter(Injury == 1 & InjuryType != "Unknown") %>%
  group_by(InjuryType) %>% 
  summarize(Countme = n()) %>% 
  arrange(desc(Countme)) %>% 
  top_n(5, Countme) %>% 
  ggplot(aes(reorder(x = InjuryType, - Countme), y = Countme)) + #if we add a y, we need to put stat = identity below)
  geom_bar(col="black", fill="orange", alpha = .5, stat = "identity") +
  labs(x="Injury Type", y="Count") +
  ggtitle(label = "Top 5 Injury Types of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
TopInjury

TopDeath <- df %>% filter(Death == 1 & CauseofDeath != "Unknown") %>%
  group_by(CauseofDeath) %>% 
  summarize(Countme = n()) %>% 
  arrange(desc(Countme)) %>% 
  top_n(5, Countme) %>% 
  ggplot(aes(reorder(x = CauseofDeath, - Countme), y = Countme)) + #if we add a y, we need to put stat = identity below)
  geom_bar(col="black", fill="orange", alpha = .5, stat = "identity") +
  labs(x="Cause of Death", y="Count") +
  ggtitle(label = "Top 5 Death of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
TopDeath


# Cause of Accident


#AccidentCause
TopAccidentCause <- df %>% filter(AccidentCause != "Unknown") %>%
  group_by(AccidentCause) %>% 
  summarize(Countme = n()) %>% 
  arrange(desc(Countme)) %>% 
  top_n(5, Countme) %>% 
  ggplot(aes(reorder(x = AccidentCause, - Countme), y = Countme)) +  
  geom_bar(col="black", fill="orange", alpha = .5, stat = "identity") +
  labs(x="Accident Cause", y="Count") +
  ggtitle(label = "Accident Cause") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
TopAccidentCause

TopAccidentCauseINJ <- df %>% filter(Injury == 1 & AccidentCause != "Unknown") %>%
  group_by(AccidentCause) %>% 
  summarize(Countme = n()) %>% 
  arrange(desc(Countme)) %>% 
  top_n(5, Countme) %>% 
  ggplot(aes(reorder(x = AccidentCause, - Countme), y = Countme)) +
  geom_bar(col="black", fill="orange", alpha = .5, stat = "identity") +
  labs(x="Accident Cause When Injured", y="Count") +
  ggtitle(label = "Accident causes of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
TopAccidentCauseINJ


TopAccidentCauseDEATH <- df %>% filter(Death == 1 & AccidentCause != "Unknown") %>%
  group_by(AccidentCause) %>% 
  summarize(Countme = n()) %>% 
  arrange(desc(Countme)) %>% 
  top_n(5, Countme) %>% 
  ggplot(aes(reorder(x = AccidentCause,- Countme), y = Countme)) +
  geom_bar(col="black", fill="orange", alpha = .5, stat = "identity") +
  labs(x="Accident Cause When Death", y="Count") +
  ggtitle(label = "Accident causes of Boating Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
TopAccidentCauseDEATH




# Accident Event
TopAccidentEvent <- df %>%
  group_by(AccidentEvent) %>% 
  summarize(Countme = n()) %>% 
  arrange(desc(Countme)) %>% 
  top_n(5, Countme) %>% 
  ggplot(aes(reorder(x = AccidentEvent,- Countme), y = Countme)) +
  geom_bar(col="black", fill="orange", alpha = .5, stat = "identity") +
  labs(x="Accident Event", y="Count") +
  ggtitle(label = "Accident Events") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
TopAccidentEvent



#Chord Diagram
OutcomeEvent <- df %>%
  filter(AccidentEvent == "Collision with another vessel" | AccidentEvent == "Collision with fixed object" | AccidentEvent == "Flooding/swamping" | AccidentEvent == "Grounding") %>%
  group_by(Outcome, AccidentEvent) %>% 
  summarize(Countme = n()) %>% 
  arrange(desc(Outcome))

library(circlize)
# color_array <- c("#FF000080", "#00FF0080", "#0000FF80","#FF000080", "#00FF0080", "#0000FF80")
# grid.col <-  c(AccidentOnly = "steelblue1", InjuryOnly = "blue", DeathOnly = "orange", Injury and Death = "red")
chordDiagram(OutcomeEvent, directional = TRUE, transparency = 0.5)  

#Scratch Work
df %>% group_by(AccidentEvent) %>%
  summarize(Countme = n()) %>% 
  arrange(desc(Countme))
  




# Great code to get percentages of sub-groups in a column of data
dfPercentNew <- df %>%  group_by(Year,Outcome) %>% 
  summarise(PercentageByYear=n()) %>% 
  group_by(Year) %>% 
  mutate(PercentageByYear=PercentageByYear/sum(PercentageByYear)*100)


#compute cumulative % (top of each rectangle)
dfPercentNew$ymax <- cumsum(dfPercentNew$PercentageByYear)

DonutPie <- ggplot(dfPercentNew , aes(x = Year, y = PercentageByYear, fill = Outcome)) +
  geom_col() +
  scale_x_discrete(limits = c(" ", "2017","2018")) +
  scale_fill_viridis_d() +
  coord_polar("y") +
  theme_void()
DonutPie

