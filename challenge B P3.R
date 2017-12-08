library(MASS)
library(dplyr)
# load all the needed data

#import the data
CNIL <- read.csv('OpenCNIL_Organismes_avec_CIL_VD_20171204.csv',sep = ';')
unique(CNIL$Portee)
# We first convert the variable Code_Postal to character in order to use the substr function
tbl_df(CNIL)
CNIL$Code_Postal <- as.character(CNIL$Code_Postal)
depart <- substr(CNIL$Code_Postal,1,2)
# We use the substr function to represent each observation with the first two digits of their postcode. 
# Then we find the number of "types" of the first two digits, since all the firms in the list have CIL.
depart
unique(depart)
CILperdepart <- table(depart)

sir1 <- read.csv("sirc-17804_9075_14211_2017340_E_Q_20171207_022339046.csv", sep = ";")
sir1
