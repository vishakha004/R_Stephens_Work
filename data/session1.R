### R Session 1 ###
### 21 October 2023 ###

### By: Ananya Iyengar ###

### Objectives for this lecture: Data Manipulation

#Step 1: Setting the Working Directory

setwd("/Users/vishakhasingla/RWork/R1")

##############################################################################

#Step 2: Installing and Loading the Required Packages

library(readxl)
library(readr)
library(haven)
library(dplyr)
library(fastDummies)

###############################################################################

#Step 3: Importing Data

#To import .dta files, we use the read_dta command from the haven package. here, my file is within the R1 folder 
#in my working directory, so I use the "/" to mention my path correctly! 

practice_dta <- read_dta("/Users/vishakhasingla/RWork/R1/practice_dta.dta")

#To import .txt files is slightly more involved! We need to know the documentation of the data! 

#Pit Stop Data in F1 races: 
# - 1-3: raceID
# - 4-5: driverID
# - 6: stop_no
# - 7-8: lap_no
# - 9-13: milliseconds

pit_stop <- read.fwf(file = "/Users/vishakhasingla/RWork/R1/pit_stop.txt"), 
                     fwf_cols(raceID = c(1,3), driverID = c(2,5),
                              stop_no = c(6), lap_no = c(7,8),
                              milliseconds = c(9,13))

#To import .xlsx files, we use the read_excel command from the readxl package! 

tn_sc_pop <- read_excel("/Users/vishakhasingla/RWork/R1/tn_sc_pop.xlsx")
tn_sc_edu <- read_excel("/Users/vishakhasingla/RWork/R1/tn_sc_edu.xlsx")

####################################################################################

#Data Cleaning

#Example 1: Finding NA Observations and Removing them 

print(sum(complete.cases(practice_dta)))

print(sum(is.na(practice_dta$high2011))) 

practice_dta[which(is.na(practice_dta$high2011)), ]

edu <- practice_dta[complete.cases(practice_dta$high2011), ]

# Example 2: Creating and Removing Columns

# Selecting data on the basis of entries in a row! here, we select a certain part of the data we want to work with!

#We will continue to use the edu data set! 

#First, we will choose a state of our choice! I will choose Bihar 

#We want: edu -> bihar -> scheduled castes -> choose top 10 scheduled castes in terms of population ->
# choose relevant variables for analysis

#The above exercise will use a wide range of dplyr functions! 

#For you: Region Codes: CHOOSE YOUR OWN STATE!

#Andhra Pradesh 1
#Punjab 231
#Madhya Pradesh 113
#Orissa 217
#Uttar Pradesh 289
#West Bengal 344
#Kerala 102
#Rajasthan 243
#Bihar 34

state <- edu %>% filter(region7111 == 34) %>% 
  filter(scst2011 == "sc") %>%
  select("region7111", "regionname7111", "castegroup_2011_code", "totalp" ,, "totalf", "primaryp","primaryf" ,"edu_primary_fracp",
         "primary2011", "primary1971", "primary_frac2011", "primary_frac1971")

#Creating New variables using the Mutate

#I want to know what proportion of those who have attained primary education are women 

#I have the variables primaryp: total pop of those with primary education in the caste and primaryf: total pop of women with primary education in the caste

state <- state %>% mutate(prop_female_primary = primaryf/primaryp)

#Finally, I want to select the top 6 castes by size! 

state <- state %>% arrange(desc(totalp)) %>% slice(1:6)

################################################################################

#Merging data frames 

#For this, we use the tn_sc_edu and tn_sc_pop data sets

#Cleaning!! 

#I want total population only, at the state level. Try to write code yourself!! 

tn_sc_pop <- tn_sc_pop %>% filter(region == "Total") %>%
  filter(dist_code == "00") #take care of variable type! numeric or character? important in filtering! 

tn_sc_edu <- tn_sc_edu %>% filter(region == "Total") %>%
  filter(dist_code == "00")

#To merge, we need a common ID!

tn <- merge(tn_sc_edu, tn_sc_pop, by = "sc_code", all = T)

## 'all=T' means the larger dataframe (with more rows will be considered) will be considered and NAs will be put wherever values aren't available 

#In general to create unique IDs, we use the paste0 command! 

##################################################################################

#Reshaping data frames: We will look at this more tomorrow in the context of graphing! 


##################################################################################

#creating dummy variables: useful in econometrics! 

#let's say i want to know what belonging to certain castes in a state does for people's primary education levels?

#for this, i want dummy variables for the top 6 caste categories I chose! 

#I use the fastDummies package for this! 

state_dummy <- dummy_cols(state, select_columns = c("castegroup_2011_code"))

####################################################################################







