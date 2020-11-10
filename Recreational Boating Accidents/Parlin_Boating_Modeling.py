# -*- coding: utf-8 -*-
"""
Created on Tue Jun 30 13:51:36 2020

@author: Ralph Parlin
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns 

#################################################################################
# Data import and wrangling

df = pd.read_csv('USCG Consolidated DataFile.csv') # read the data into a dataframe
df.head()# inspect the data
df.tail()# inspect the data
df.shape # review the shape of the dataframe
df.dtypes # review the types of data in the datafra
totalNAs = str(df.isnull().sum().sum()) # this helps us determine the amount of NAs, record a sstring, assing them to totalNAs 
print("This dataframe contains a total of",totalNAs,"missing values.")

# Records with NAs
totalRecordNAs = str(df.isnull().any(axis=1).sum()) # sum the number of rows with missing values, record as string, assing to totalRecordNAs
print("This dataframe contains a total of",totalRecordNAs,"records missing values.")

# Columns with NAs
totalColumnNAs = str(df.isnull().any(axis=0).sum()) # sum the number of rows with missing values
print("This dataframe contains a total of",totalColumnNAs,"columns missing values.")

# List of columns missing data
colsNA = df.columns[df.isnull().any()].tolist()  # to get a list instead of an Index object
colsNA

''' The columns missing data include: OperatorAge, Length, Yearbuilt, 
NumberPeopleOnboard, DeceasedAge, DeceasedGender, CauseofDeath, DeceasedPFDWorn, 
DeceasedRole, InjuredGender, InjuredAge, InjuryType, and InjuredRole. However, this 
is to be expected after moving the database to second normal form.  For example, 
not all accidents resulted in injury or death so for each of those observations, 
a great deal of data was missing.  I began by addressing the columns that are 
unique to all accidents: OperatorAge, Length, Yearbuilt, NumberPeopleOnboard.
'''

# NAs for Operator Age
df["OperatorAge"].isnull().sum()
df["OperatorAge"].describe() # and here we get a better understanding of summary stats
ageHist = df["OperatorAge"].hist(bins = 100) # Ok so we see a right skewed distribtion
# The distibution is roughly normal so I'll use mean replacement for missing values
df["OperatorAge"].fillna(round(df["OperatorAge"].mean()), inplace = True) #Using the fillna method I replaced all the missing values with a rounded version of the mean of the column and did this swap inplace to change the dataframe

# NAs for Length
df["Length"].isnull().sum()
df["Length"].describe() # and here we get a better understanding of summary stats
legthHist = df["Length"].hist(bins = 100) # Ok so we see a right skewed distribtion
# The distibution is right skewed so I'll use median replacement for missing values
df["Length"].median() 
df["Length"].fillna(round(df["Length"].median()), inplace = True) #Using the fillna method I replaced all the missing values with a rounded version of the median of the column and did this swap inplace to change the dataframe

# NAs for YearBuilt
df["Yearbuilt"].isnull().sum()
df["Yearbuilt"].describe() # and here we get a better understanding of summary stats
YearbuiltHist = df["Yearbuilt"].hist(bins = 100) # Ok so we see a left skewed distribtion so I'll use median for replacement
df["Yearbuilt"].median() 
df["Yearbuilt"].fillna(round(df["Yearbuilt"].median()), inplace = True) #Using the fillna method I replaced all the missing values with a rounded version of the median of the column and did this swap inplace to change the dataframe

# NAs for NumberPeopleOnboard
df["NumberPeopleOnboard"].isnull().sum()
df["NumberPeopleOnboard"].describe() # and here we get a better understanding of summary stats
NumberPeopleOnboard = df["NumberPeopleOnboard"].hist(bins = 50) # Ok so we see a right skewed distribtion so I'll use median for replacement
df["NumberPeopleOnboard"].median() 
df["NumberPeopleOnboard"].fillna(round(df["NumberPeopleOnboard"].median()), inplace = True) #Using the fillna method I replaced all the missing values with a rounded version of the median of the column and did this swap inplace to change the dataframe



# The following NA replacements requiring conditioning the replacment on the status of the accident: Death or no Death and Injury or No Injury
# Deceased Age
df["DeceasedAge"].isnull().sum()
df["DeceasedAge"].describe() # and here we get a better understanding of summary stats
DecAgeHist = df["DeceasedAge"].hist(bins = 100) # Ok so we see a right skewed distribtion
# The distibution is roughly normal so I'll use mean replacement for missing values
df["DeceasedAge"].mean() 
# Now that I know the mean, for records where there was a death adn teh age is null, I'll replace with the mean.
df["DeceasedAge"] = np.where((df["Death (1 = Yes)"] == 1) & (df["DeceasedAge"].isnull()) , 46, df["DeceasedAge"])

#Deceased Gender
df["DeceasedGender"].isnull().sum()
# If the Gender is unknown, I'll replace with Unknown
df["DeceasedGender"] = np.where((df["Death (1 = Yes)"] == 1) & (df["DeceasedGender"].isnull()) , "U", df["DeceasedGender"])
# Now I need to fix the formating
df["DeceasedGender"] = np.where((df["Death (1 = Yes)"] == 1) & (df["DeceasedGender"]=="f") , "F", df["DeceasedGender"])
df["DeceasedGender"] = np.where((df["Death (1 = Yes)"] == 1) & (df["DeceasedGender"]=="m") , "M", df["DeceasedGender"])
#Let's check
df["DeceasedGender"].value_counts()

#CauseofDeath
df["CauseofDeath"].isnull().sum()
df["CauseofDeath"].value_counts()
df["CauseofDeath"] = np.where((df["Death (1 = Yes)"] == 1) & (df["CauseofDeath"].isnull()) , "Unknown", df["CauseofDeath"])

#DeceasedPFDWorn
df["DeceasedPFDWorn"].isnull().sum()
df["DeceasedPFDWorn"].value_counts()
df["DeceasedPFDWorn"] = np.where((df["Death (1 = Yes)"] == 1) & (df["DeceasedPFDWorn"].isnull()) , "Unknown", df["DeceasedPFDWorn"])

#DeceasedRole
df["DeceasedRole"].isnull().sum()
df["DeceasedRole"].value_counts()
df["DeceasedRole"] = np.where((df["Death (1 = Yes)"] == 1) & (df["DeceasedRole"].isnull()) , "Unknown", df["DeceasedRole"])
df["DeceasedRole"] = np.where((df["Death (1 = Yes)"] == 1) & (df["DeceasedRole"]=="other") , "Other", df["DeceasedRole"])

#InjuredGender
df["InjuredGender"].isnull().sum()
df["InjuredGender"].value_counts()
df["InjuredGender"] = np.where((df["Injury (1 = Yes)"] == 1) & (df["InjuredGender"].isnull()) , "U", df["InjuredGender"])
df["InjuredGender"] = np.where((df["Injury (1 = Yes)"] == 1) & (df["InjuredGender"] == "m"), "M", df["InjuredGender"])
df["InjuredGender"] = np.where((df["Injury (1 = Yes)"] == 1) & (df["InjuredGender"] == "f"), "F", df["InjuredGender"])

#InjuredAge
df["InjuredAge"].isnull().sum()
df["InjuredAge"].describe() # and here we get a better understanding of summary stats
InjAgeHist = df["InjuredAge"].hist(bins = 100) # Ok so we see a right skewed distribtion
# The distibution is slightly right skewed so I'll use median replacement for missing values
df["InjuredAge"].median() 
df["InjuredAge"] = np.where((df["Injury (1 = Yes)"] == 1) & (df["InjuredAge"].isnull()) , 33, df["InjuredAge"])

#InjuryType
df["InjuryType"].isnull().sum()
df["InjuryType"].value_counts()
df["InjuryType"] = np.where((df["Injury (1 = Yes)"] == 1) & (df["InjuryType"].isnull()) , "Unknown", df["InjuryType"])

#InjuredRole
df["InjuredRole"].isnull().sum()
df["InjuredRole"].value_counts()
df["InjuredRole"] = np.where((df["Injury (1 = Yes)"] == 1) & (df["InjuredRole"].isnull()) , "Unknown", df["InjuredRole"])


# Check for duplicate
# Check and report total number of duplicated records
dups = len(df)-len(df.drop_duplicates()) # determine the number of duplicats in the entire datframe and assign to dups
print("There were a total of", dups, "duplicate records but they have now been deleted.")
# Delete those duplicated record
df = df.drop_duplicates() # drop the duplicates and assign the view back to the orginal dataframe
# inpsect the new shape
df.shape



#Check for and addressing of outliers
#NumberDeaths
df["NumberDeaths"].describe()
boxplot = df.boxplot(column="NumberDeaths") # also review via boxplot
# four deaths seems reasonable so I'll leae this
           
#NumberInjured
df["NumberInjured"].describe()
boxplot = df.boxplot(column="NumberInjured")
# I'll use the IQR fence method to identify outliers and use mean to replace them.
numInjQ1 = df["NumberInjured"].quantile(q = .25) # calculate Q1
numInjQ3 = df["NumberInjured"].quantile(q = .75) # calcualte Q3
numInjIQR = numInjQ3-numInjQ1 #calculate IQR (inter quratile region)
numInjUpperFence = (1.5 * numInjIQR) + numInjQ3 # Caluculate upper fence, less than max, anything beyond is outlier
numInjLowerFence = (1.5 * numInjIQR) - numInjQ1 # Calculate the lower fence, anything less than this value is an outlier
# Given this is a right skewed distibution, I will use the median to replace outlying values with the median
df.loc[df["NumberInjured"] > numInjUpperFence, "NumberInjured"] = round(df["NumberInjured"].median())

#NumberVesselsInvolved
df["NumberVesselsInvolved"].describe()
boxplot = df.boxplot(column="NumberVesselsInvolved")
numVesQ1 = df["NumberVesselsInvolved"].quantile(q = .25) # calculate Q1
numVesQ3 = df["NumberVesselsInvolved"].quantile(q = .75) # calcualte Q3
numVesIQR = numVesQ3-numVesQ1 #calculate IQR (inter quratile region)
numVesUpperFence = (1.5 * numVesIQR) + numVesQ3 # Caluculate upper fence, less than max, anything beyond is outlier
numVesLowerFence = (1.5 * numVesIQR) - numVesQ1 # Calculate the lower fence, anything less than this value is an outlier
# Given this is a right skewed distibution, I will use the median to replace outlying values with the median
df.loc[df["NumberVesselsInvolved"] > numVesUpperFence, "NumberVesselsInvolved"] = round(df["NumberVesselsInvolved"].median())


#TotalDamage
df["TotalDamage"].describe()
boxplot = df.boxplot(column="TotalDamage")
damageQ1 = df["TotalDamage"].quantile(q = .25) # calculate Q1
damageQ3 = df["TotalDamage"].quantile(q = .75) # calcualte Q3
damageIQR = damageQ3-damageQ1 #calculate IQR (inter quratile region)
damageUpperFence = (1.5 * damageIQR) + damageQ3 # Caluculate upper fence, less than max, anything beyond is outlier
damageLowerFence = (1.5 * damageIQR) - damageQ1 # Calculate the lower fence, anything less than this value is an outlier
# Given this is a right skewed distibution, I will use the median to replace outlying values with the median
df.loc[df["TotalDamage"] > damageUpperFence, "TotalDamage"] = round(df["TotalDamage"].median())

#OperatorAge
df["OperatorAge"].describe()

#Length   
df["Length"].describe()
boxplot = df.boxplot(column="Length")
lenQ1 = df["Length"].quantile(q = .25) # calculate Q1
lenQ3 = df["Length"].quantile(q = .75) # calcualte Q3
lenIQR = lenQ3-lenQ1 #calculate IQR (inter quratile region)
lenUpperFence = (1.5 * lenIQR) + lenQ3 # Caluculate upper fence, less than max, anything beyond is outlier
lenLowerFence = (1.5 * lenIQR) - lenQ1 # Calculate the lower fence, anything less than this value is an outlier
# Given this is a right skewed distibution, I will use the median to replace outlying values with the median
df.loc[df["Length"] > lenUpperFence, "Length"] = round(df["Length"].median())
                     
#Yearbuilt  
df["Yearbuilt"].describe()
             
#NumberPeopleOnboard 
df["NumberPeopleOnboard"].describe()
boxplot = df.boxplot(column="NumberPeopleOnboard")
numboardQ1 = df["NumberPeopleOnboard"].quantile(q = .25) # calculate Q1
numboardQ3 = df["NumberPeopleOnboard"].quantile(q = .75) # calcualte Q3
numboardIQR = numboardQ3-numboardQ1 #calculate IQR (inter quratile region)
numboardUpperFence = (1.5 * numboardIQR) + numboardQ3 # Caluculate upper fence, less than max, anything beyond is outlier
numboardLowerFence = (1.5 * numboardIQR) - numboardQ1 # Calculate the lower fence, anything less than this value is an outlier
# Given this is a right skewed distibution, I will use the median to replace outlying values with the median
df.loc[df["NumberPeopleOnboard"] > numboardUpperFence, "NumberPeopleOnboard"] = round(df["NumberPeopleOnboard"].median())
             
#DeceasedAge
df["DeceasedAge"].describe()

#InjuredAge
df["InjuredAge"].describe()



#Transformations from float to int
df["OperatorAge"] = df["OperatorAge"].astype(int)
df["Length"] = df["Length"].astype(int)
df["Yearbuilt"] = df["Yearbuilt"].astype(int)
df["NumberPeopleOnboard"] = df["NumberPeopleOnboard"].astype(int)




#Other Cleaning 
# Label Consistency
df["WaterConditions"] = np.where(df["WaterConditions"] == "choppy", "Choppy", df["WaterConditions"])
df["WaterConditions"] = np.where(df["WaterConditions"] == "rough", "Rough", df["WaterConditions"])
df["WaterConditions"] = np.where(df["WaterConditions"] == "Very rough", "Very Rough", df["WaterConditions"])
df["WaterConditions"] = np.where(df["WaterConditions"] == "unknown", "Unknown", df["WaterConditions"])
df["Visibility"] = np.where(df["Visibility"] == "unknown", "Unknown", df["Visibility"])
df["OperatorGender"] = np.where(df["OperatorGender"] == "m", "M", df["OperatorGender"])
df["DeceasedRole"] = np.where(df["DeceasedRole"] == "other", "Other", df["DeceasedRole"])
###################################################################################
# EDA
# I'll start with lookinig at several of the categorical features and the damage numeric
dfcat = df[[
    "Date",
    "Death (1 = Yes)",
    "Injury (1 = Yes)",
    "Time",
    "State",
    "WaterConditions",
    "Wind",
    "Visibility",
    "DayofWeek",
    "AccidentCause",
    "AccidentEvent",
    "OperatorGender",
    "VesselType",
    "Operation",
    "Activity",
    "DeceasedGender",
    "CauseofDeath",
    "DeceasedPFDWorn",
    "DeceasedRole",
    "InjuredGender",
    "InjuryType",
    "InjuredRole",
    "TotalDamage"]] # creating as smaller dataframe to work catagorical features
# The intent here is to uncover any intersting insights with repect to day of month a person was contacted, the amount of times a person was contacted, the duration of the contact, if the person previously participated in campaings, the balance of the clients account, and how these
# interact with the target feature.
dfcat.head() # inspect dfplot
dfcat.shape # inspect dfplot
dfcat.columns


#Accident Event (Decending)
sns.set(style='white')
sns.countplot(x="AccidentEvent", data=dfcat, order = dfcat["AccidentEvent"].value_counts().index).set(
    xlabel="AccidentEvent", 
    ylabel="Count")
plt.xticks(rotation=90, size = 7)

#Accident Cause (Decending)
sns.set(style='white')
sns.countplot(x="AccidentCause", data=dfcat, order = dfcat["AccidentCause"].value_counts().index).set(
    xlabel="Accident Cause", 
    ylabel="Count")
plt.xticks(rotation=90, size = 7)


#Day of week
sns.set(style='white')
order = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
sns.countplot(x="DayofWeek", order = order, data=dfcat).set(
    xlabel="Day of Week", 
    ylabel="Count")
plt.xticks(rotation=45)

# Day of  week and damages
sns.set(style='white')
order = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
sns.catplot(x="DayofWeek", y="TotalDamage", kind="box", order = order, data=dfcat).set(
    xlabel="Day of Week", 
    ylabel="Total Damages ($)")
plt.xticks(rotation=45)


#Accidents by State
sns.set(style='white')
sns.countplot(x="State", data=dfcat).set(
    xlabel="State", 
    ylabel="Count")
plt.xticks(rotation=90, size = 7)


#Accident Event, Damages and Death
sns.set(style='white')
sns.barplot(x="AccidentEvent", y = "TotalDamage", hue = "Death (1 = Yes)", data=dfcat, ci=None, order = dfcat["AccidentEvent"].value_counts().index).set(
    xlabel="Accident Event", 
    ylabel="Total Damage ($)")
plt.xticks(rotation=90, size = 7)
plt.legend(title = 'Involved Death (1 = Yes)', bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)


#Accident Event, Damgages and Injnury
sns.set(style='white')
sns.barplot(x="AccidentEvent", y = "TotalDamage", hue = "Injury (1 = Yes)", data=dfcat, ci=None, order = dfcat["AccidentEvent"].value_counts().index).set(
    xlabel="Accident Event", 
    ylabel="Total Damage ($)")
plt.xticks(rotation=90, size = 7)
plt.legend(title = 'Involved Injury (1 = Yes)', bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)


# Ok in both cases there sees to be a lot of collisions. Maybe  this isbecuaseof weather conditions or visiability.
# Let's look

#Visasbility
sns.set(style='white')
sns.countplot(x="Visibility", data=dfcat, order = dfcat["Visibility"].value_counts().index).set(
    xlabel="Visibility", 
    ylabel="Count")
plt.xticks(rotation=90, size = 7)

#Visibility Conditions with Total Damage
sns.set(style='white')
sns.barplot(x="Visibility", y = "TotalDamage", data=dfcat, order = dfcat["Visibility"].value_counts().index).set(
    xlabel="Visibility", 
    ylabel="Total Damages")
plt.xticks(rotation=90, size = 7)


#Water Conditions (Decending)
sns.set(style='white')
sns.countplot(x="WaterConditions", data=dfcat, order = dfcat["WaterConditions"].value_counts().index).set(
    xlabel="Water Conditions", 
    ylabel="Count")
plt.xticks(rotation=90, size = 7)

#Water Conditions with Total Damage (Decending)
sns.set(style='white')
orderwater = ["Calm", "Choppy", "Rough", "Very Rough","Unknown"]
sns.barplot(x="WaterConditions", y = "TotalDamage", data=dfcat, order = orderwater).set(
    xlabel="Water Conditions", 
    ylabel="Total Damages")
plt.xticks(rotation=90, size = 7)

#Water Conditions with Total Damge and Injury 
sns.set(style='white')
orderwater = ["Calm", "Choppy", "Rough", "Very Rough","Unknown"]
sns.barplot(x="WaterConditions", y = "TotalDamage", hue = "Injury (1 = Yes)", data=dfcat, order = orderwater).set(
    xlabel="WaterConditions", 
    ylabel="Total Damages")
plt.xticks(rotation=90, size = 7)
plt.legend(title = 'Involved Injury (1 = Yes)', bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)


#########################################
#Summary Stats for numerics

dfnumeric = df[[
    "NumberDeaths",
    "NumberInjured", 
    "NumberVesselsInvolved", 
    "TotalDamage", 
    "OperatorAge",
    "Length",
    "Yearbuilt",
    "NumberPeopleOnboard",
    "DeceasedAge",
    "InjuredAge"]]
    # creating as smaller dataframe to work catagorical features
# The intent here is to uncover any intersting insights with repect to day of month a person was contacted, the amount of times a person was contacted, the duration of the contact, if the person previously participated in campaings, the balance of the clients account, and how these
# interact with the target feature.
dfnumeric.head() # inspect dfplot
dfnumeric.shape # inspect dfplot
dfnumeric.columns

round(dfnumeric[["NumberDeaths","NumberInjured", "NumberVesselsInvolved","TotalDamage"]].describe())
round(dfnumeric[["OperatorAge","Length","Yearbuilt","NumberPeopleOnboard"]].describe())
round(dfnumeric[["DeceasedAge","InjuredAge"]].describe())

#Quick tables to review some aggregated distibutions/summaries

round(df.groupby('CauseofDeath')['NumberDeaths'].count())
round(df.groupby('CauseofDeath')['DeceasedAge'].mean())
round(df.groupby('InjuryType')['InjuredAge'].mean())
round(df.groupby('InjuryType')['NumberInjured'].count())

df.groupby('DeceasedGender')['CauseofDeath'].count()
df.groupby('AccidentEvent')['OperatorAge'].mean()
df.groupby('AccidentCause')['OperatorAge'].mean()


df.groupby('AccidentEvent')['NumberInjured'].count()

### Correlation Matirx 
import seaborn as sn
import matplotlib.pyplot as plt

corrMatrix = round(dfnumeric.corr(),2)
sn.heatmap(corrMatrix, annot=True)
plt.show()


#####################################################################################
#Cluster Analysis
clustdf = df.copy() # copy the DF

#drop the columns I don't want to consider in clustering
clustdf.drop(["Date",
    "ReleasableAccidents_Year",
    "Time",
    "State",
    "Wind",
    "AccidentCause",
    "AccidentEvent",
    "VesselType",
    "Operation",
    "Activity",
    "CauseofDeath",
    "DeceasedPFDWorn",
    "DeceasedRole",
    "InjuredGender",
    "InjuryType",
    'DeceasedAge',
    'DeceasedGender',
    "InjuredRole",
    'InjuredAge',
    "WaterConditions",
    "Visibility",
    "DayofWeek",
    "OperatorGender",
    "TotalDamage"], axis = 1, inplace = True)
clustdf.columns

#now I will normalize the data
# Normalize the data
from scipy.stats import zscore
clustdfZ = clustdf.apply(zscore)
# Create the dummies from the original df
df_Dummy = pd.get_dummies(df[["WaterConditions", "Visibility", "DayofWeek", "OperatorGender"]])
# join the dummies to the clustdf df
clustdf_dummies = pd.concat([clustdfZ, df_Dummy], axis=1, sort = False)

#inspect df
clustdf_dummies.head()
clustdf_dummies.tail()
clustdf_dummies.dtypes
clustdf_dummies.columns
clustdf_dummies.shape
clustdf_dummies.isnull().sum().sum()
clustdf_dummies.reset_index(drop=True, inplace=True)

# clustdf_dummiesZ = clustdf_dummies[['NumberDeaths', 'NumberInjured','NumberVesselsInvolved', 'OperatorAge', 'Length', 'Yearbuilt', 'NumberPeopleOnboard']].apply(zscore)
# I'll reset the index to prevent potential problems
# clustdf_dummiesZ.reset_index



#Kmean Clustering
from sklearn.cluster import KMeans

# I'll run through 10 clusters and use the elbow technique to find the number of clusters to use
sse = {}
for k in range(1, 10):
    kmeans = KMeans(n_clusters=k, max_iter=1000).fit(clustdf_dummies)
    clustdf_dummies["clusters"] = kmeans.labels_
    print(clustdf_dummies["clusters"])
    sse[k] = kmeans.inertia_ # Inertia: Sum of distances of samples to their closest cluster center
plt.figure()
plt.plot(list(sse.keys()), list(sse.values()))
plt.xlabel("Number of cluster")
plt.ylabel("SSE")
plt.show()

#Based on the elbow graph above, it looks like I could go with 3 

# Now I'll reset the df and conduct the K means with 3 clusters
# I'm resetting the DF now that the priliminary cluster number identificaiton is complete
clustdfZ = clustdf.apply(zscore)
# Create the dummies from the original df
df_Dummy = pd.get_dummies(df[["WaterConditions", "Visibility", "DayofWeek", "OperatorGender"]])
# join the dummies to the clustdf df
clustdf_dummies = pd.concat([clustdfZ, df_Dummy], axis=1, sort = False)

#inspect df
clustdf_dummies.head()
clustdf_dummies.tail()
clustdf_dummies.dtypes
clustdf_dummies.columns
clustdf_dummies.shape
clustdf_dummies.isnull().sum().sum()
clustdf_dummies.reset_index(drop=True, inplace=True)

# running k-means cluster analysis with 3 clusters

newKmeans = KMeans(n_clusters=3).fit(clustdf_dummies)
clustnum = newKmeans.labels_
print(clustnum)

clustnum = pd.DataFrame(clustnum, columns=["Cluster"])
clustnum.shape

#I'll reset the indexes to prevent errors
clustnum.reset_index(drop=True, inplace=True)

# attaching each observation its cluster number in the orginal df:
clustdf_dummiesClust = pd.concat([clustdf_dummies, clustnum], axis=1, sort=False)
clustdf_dummiesClust.shape
#check for NAs
clustdf_dummiesClust.isnull().sum().sum()

#inspect the df
clustdf_dummiesClust.head()
clustdf_dummiesClust.tail()
clustdf_dummiesClust.columns

#Let's inpsect a few crosstabs to get a better underststanding of the clusters
pd.crosstab(clustdf_dummiesClust["Cluster"], clustdf_dummiesClust["Death (1 = Yes)"], rownames=['Cluster'], colnames=["Death (1 = Yes)"])
pd.crosstab(clustdf_dummiesClust["Cluster"], clustdf_dummiesClust["Injury (1 = Yes)"], rownames=['Cluster'], colnames=["Injury (1 = Yes)"])
pd.crosstab(clustdf_dummiesClust["Cluster"], clustdf_dummiesClust["Day (1 = yes)"], rownames=['Cluster'], colnames=["Day (1 = yes)"])
pd.crosstab(clustdf_dummiesClust["Cluster"], clustdf_dummiesClust["WaterConditions_Rough"], rownames=['Cluster'], colnames=["WaterConditions_Rough"])
pd.crosstab(clustdf_dummiesClust["Cluster"], clustdf_dummiesClust["WaterConditions_Calm"], rownames=['Cluster'], colnames=["WaterConditions_Calm"])
pd.crosstab(clustdf_dummiesClust["Cluster"], clustdf_dummiesClust["Visibility_Good"], rownames=['Cluster'], colnames=["Visibility_Good"])
pd.crosstab(clustdf_dummiesClust["Cluster"], clustdf_dummiesClust["Visibility_Poor"], rownames=['Cluster'], colnames=["Visibility_Poor"])


"WaterConditions", "Visibility", "DayofWeek", "OperatorGender"

#########################################################################################
# Regression Analysis
#create new df
regdf = df.copy()

regdf.drop(["Date",
    "ReleasableAccidents_Year",
    "Time",
    "State",
    "Wind",
    "AccidentCause",
    "Operation",
    "Activity",
    "CauseofDeath",
    "DeceasedPFDWorn",
    "DeceasedRole",
    "InjuredGender",
    "InjuryType",
    'DeceasedAge',
    'DeceasedGender',
    "InjuredRole",
    'InjuredAge',
    "WaterConditions",
    "Visibility",
    "OperatorGender"], axis = 1, inplace = True)
regdf.columns

#split the data to train/test
from sklearn.model_selection import train_test_split
trainreg, testreg = train_test_split(regdf, test_size=0.3, random_state=42)

import patsy
import statsmodels.api as sm
import statsmodels.formula.api as smf

# I'll start with the full model, and use the P-value approach to backwards eliminate features in the model
mod1 = smf.ols(formula = "TotalDamage ~ NumberDeaths + NumberInjured + NumberVesselsInvolved  + DayofWeek + AccidentEvent + OperatorAge + OperatorUsingAlcohol + VesselType + Length + Yearbuilt + NumberPeopleOnboard" , data = trainreg)
res1 = mod1.fit()
print(res1.summary())

# I'll remove OperatorAge
mod2 = smf.ols(formula = "TotalDamage ~ NumberDeaths + NumberInjured + NumberVesselsInvolved  + DayofWeek + AccidentEvent + OperatorUsingAlcohol + VesselType + Length + Yearbuilt + NumberPeopleOnboard" , data = trainreg)
res2 = mod2.fit()
print(res2.summary())

# I'll remove Number of People onboard
mod3 = smf.ols(formula = "TotalDamage ~ NumberDeaths + NumberInjured + NumberVesselsInvolved  + DayofWeek + AccidentEvent + OperatorUsingAlcohol + VesselType + Length + Yearbuilt" , data = trainreg)
res3 = mod3.fit()
print(res3.summary())

# I'll remove Length
mod4 = smf.ols(formula = "TotalDamage ~ NumberDeaths + NumberInjured + NumberVesselsInvolved  + DayofWeek + AccidentEvent + OperatorUsingAlcohol + VesselType + Yearbuilt" , data = trainreg)
res4 = mod4.fit()
print(res4.summary())

# quick look at the distibution of the residuals
import statsmodels.api as sm
from matplotlib import pyplot as plt
res = res4.resid # residuals
fig = sm.qqplot(res)
plt.show()


# With model 4 as my best model, I'll now run it on the test data and calcualte the model's Adjusted R^2 and it's Prediction MSE



## Summary:
from sklearn.metrics import r2_score
from sklearn.metrics import mean_squared_error
print("The OlS regression model 4's r^2 value is", round(res4.rsquared, 2),"the MSE is", round(res4.mse_model, 2))



########################################################################################


# make dummies 
df_Dummy = pd.get_dummies(df[["DayofWeek", "AccidentEvent", "VesselType"]])
# join the dummies to the regdf df
regdf_dummies = pd.concat([regdf, df_Dummy], axis=1, sort = False)
#Drop orginal features for which I made dummies, and I have to drop number of deaths/number injured, and injured since having this value will inform the prediction
regdf_dummies.drop(["DayofWeek", "AccidentEvent", "VesselType", 'NumberDeaths', 'NumberInjured', 'Injury (1 = Yes)'], axis=1, inplace = True)
#inpsect the df
regdf_dummies.head()
regdf_dummies.tail()
regdf_dummies.dtypes
regdf_dummies.columns
regdf_dummies.shape
regdf_dummies.isnull().sum().sum()
regdf_dummies.reset_index(drop=True, inplace=True)

from sklearn.model_selection import train_test_split

train, test = train_test_split(regdf_dummies, test_size=0.3, random_state=42)

features = train.copy()
features.drop("Death (1 = Yes)", axis = 1, inplace = True)
col_list = features.columns.tolist()


features = col_list
y_test = test["Death (1 = Yes)"]
X_test = test[features]
y_train = train["Death (1 = Yes)"]
X_train = train[features]

# Ok, now we'll try the classification tree
from sklearn.tree import DecisionTreeClassifier
from sklearn import tree

dtc = DecisionTreeClassifier()
dtc.fit(X_train, y_train)
dtc_predict = dtc.predict(X_test)

# Confusion Matrix put into a datframe with labels
from sklearn.metrics import confusion_matrix
conmat2 = pd.DataFrame(confusion_matrix(y_test, dtc_predict), columns = ["No Death", "Death"], index = ["True No Death", "True Death"])

# Precision
from sklearn.metrics import precision_score
precision_score(y_test, dtc_predict, average=None)

from sklearn.metrics import accuracy_score
print("Decision tree classifier accuracy: ", round(accuracy_score(y_test, dtc_predict), 2))

# Classification Report - more detailed
from sklearn.metrics import classification_report
print('\nClasification report:\n', classification_report(y_test, dtc_predict))


