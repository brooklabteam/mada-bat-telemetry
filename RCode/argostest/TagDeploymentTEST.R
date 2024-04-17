## This script takes in Argos raw data and subsets it based on 
## deployments, to only include relevant data.
# Run this before generating plots

rm(list=ls())

# set wd
setwd("/Users/sophiahorigan/Documents/GitHub/mada-bat-telemetry/RCode/Argostest")

## subset to each deployment  

## ANA139
## Tag ID : 58557
## Argos ID : 245091
## Deployment Start: 2023-08-23 
## Deployment End : 

# id subset
ana139 <- read.table('DSA_Con_245091_231018124044.txt', skip=2, sep=",")
colnames(ana139) = c("CRC", "Date", "Time", "Latitude", "Longitude", "Fix", "Alt(m)")
ana139 = ana139[,1:7]
# add id column
ana139$Tag.ID. <- '58557'
ana139$Argos.ID. <- '245091'
ana139$Bat.ID. <- 'ANA139'


# ANA140
# Tag ID : 58556
# Argos ID : 245090
# Deployment Start: 2023-08-23 
# Deployment End : 

# id subset
ana140 <- read.table('DSA_Con_245090_231018124044.txt', skip=2, sep=",")
colnames(ana140) = c("CRC", "Date", "Time", "Latitude", "Longitude", "Fix", "Alt(m)")
ana140 = ana140[,1:7]
ana140$Date <- as.Date(ana140$Date, format = "%d/%m/%Y") 
# add id column
ana140$Tag.ID. <- '58556'
ana140$Argos.ID. <- '245090'
ana140$Bat.ID. <- 'ANA140'


# subset to each deployment
# ANA154
# Tag ID : 58563
# Argos ID : 245097
# Deployment Start: 2023-10-01
# Deployment End : 

ana154 <- read.table('DSA_Con_245097_231018124044.txt', skip=2, sep=",")
colnames(ana154) = c("CRC", "Date", "Time", "Latitude", "Longitude", "Fix", "Alt(m)")
ana154 = ana154[,1:7]
ana154$Date <- as.Date(ana154$Date, format = "%d/%m/%Y") 
# add id column
ana154$Tag.ID. <- '58563'
ana154$Argos.ID. <- '245097'
ana154$Bat.ID. <- 'ANA154'

# add deployment (this tag used at both Ambositra and Analambotaka)
ana154 <- ana154[which(ana154$Date >= "2023-10-01"),]

# subset to each deployment
# MARO113
# Tag ID : 58564
# Argos ID : 245098
# Deployment Start: 2023-09-11
# Deployment End : 

maro113 <- read.table('DSA_Con_245098_231018124044.txt', skip=2, sep=",")
colnames(maro113) = c("CRC", "Date", "Time", "Latitude", "Longitude", "Fix", "Alt(m)")
maro113 = maro113[,1:7]
maro113$Date <- as.Date(maro113$Date, format = "%d/%m/%Y") 
# add id column
ana154$Tag.ID. <- '58564'
ana154$Argos.ID. <- '245098'
ana154$Bat.ID. <- 'MARO113'

# subset to each deployment
# MARO114
# Tag ID : 58558
# Argos ID : 245092
# Deployment Start: 2023-09-11
# Deployment End : 

# id subset
maro114 <- points.df[which(points.df$Platform.ID.No. == '245092'), ]

# add id column
maro114$Bat.ID. <- 'MARO114'


# subset to each deployment
# MARO115
# Tag ID : 58565
# Argos ID : 245099
# Deployment Start: 2023-09-11
# Deployment End :

# id subset
maro115 <- points.df[which(points.df$Platform.ID.No. == '245099'), ]

# add id column
maro115$Bat.ID. <- 'MARO115'


# subset to each deployment
# TSI092
# Tag ID : 58559
# Argos ID : 245093
# Deployment Start: 2023-09-07
# Deployment End : 

# id subset
tsi092 <- points.df[which(points.df$Platform.ID.No. == '245093'), ]

# add id column
tsi092$Bat.ID. <- 'TSI092'


# subset to each deployment
# TSI093
# Tag ID : 58562
# Argos ID : 245096
# Deployment Start: 2023-09-09
# Deployment End : 

# id subset
tsi093 <- points.df[which(points.df$Platform.ID.No. == '245096'), ]

# add id column
tsi093$Bat.ID. <- 'TSI093'

## bind together
points.df <- rbind(ana139, ana140, ana154, maro113, maro114, maro115, tsi092, tsi093)

## export cleaned data
write.csv(points.df, "/Users/sophiahorigan/Documents/GitHub/mada-bat-telemetry/RCode/metapop_telem_data.csv")


