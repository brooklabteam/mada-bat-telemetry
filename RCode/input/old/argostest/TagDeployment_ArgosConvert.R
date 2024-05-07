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
ana139$Date <- as.Date(ana139$Date, format = "%d/%m/%Y") 
# add id column
ana139$Tag.ID. <- '58557'
ana139$Argos.ID. <- '245091'
ana139$Bat.ID. <- 'ANA139'
#deployment (occasionally date errors in Argos data)
ana139 <- ana139[which(ana139$Date >= "2023-08-23"),]



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
#deployment
ana140 <- ana140[which(ana140$Date >= "2023-08-23"),]


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
# add deployment 
ana154 <- ana154[which(ana154$Date >= "2023-10-01"),]

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
maro113$Tag.ID. <- '58564'
maro113$Argos.ID. <- '245098'
maro113$Bat.ID. <- 'MARO113'
#deployment
maro113 <- maro113[which(maro113$Date >= "2023-09-11"),]


# MARO114
# Tag ID : 58558
# Argos ID : 245092
# Deployment Start: 2023-09-11
# Deployment End : 

maro114 <- read.table('DSA_Con_245092_231018124044.txt', skip=2, sep=",")
colnames(maro114) = c("CRC", "Date", "Time", "Latitude", "Longitude", "Fix", "Alt(m)")
maro114 = maro114[,1:7]
maro114$Date <- as.Date(maro114$Date, format = "%d/%m/%Y") 
# add id column
maro114$Tag.ID. <- '58558'
maro114$Argos.ID. <- '245092'
maro114$Bat.ID. <- 'MARO114'
#deployment
maro114 <- maro114[which(maro114$Date >= "2023-09-11"),]


# subset to each deployment
# MARO115
# Tag ID : 58565
# Argos ID : 245099
# Deployment Start: 2023-09-11
# Deployment End :

maro115 <- read.table('DSA_Con_245099_231018124044.txt', skip=2, sep=",")
colnames(maro115) = c("CRC", "Date", "Time", "Latitude", "Longitude", "Fix", "Alt(m)")
maro115 = maro115[,1:7]
maro115$Date <- as.Date(maro115$Date, format = "%d/%m/%Y") 
# add id column
maro115$Tag.ID. <- '58565'
maro115$Argos.ID. <- '245099'
maro115$Bat.ID. <- 'MARO115'
# deployment
maro115 <- maro115[which(maro115$Date >= "2023-09-11"),]

# subset to each deployment
# TSI092
# Tag ID : 58559
# Argos ID : 245093
# Deployment Start: 2023-09-07
# Deployment End : 

tsi092 <- read.table('DSA_Con_245092_231018124044.txt', skip=2, sep=",")
colnames(tsi092) = c("CRC", "Date", "Time", "Latitude", "Longitude", "Fix", "Alt(m)")
tsi092 = tsi092[,1:7]
tsi092$Date <- as.Date(tsi092$Date, format = "%d/%m/%Y") 
# add id column
tsi092$Tag.ID. <- '58559'
tsi092$Argos.ID. <- '245093'
tsi092$Bat.ID. <- 'TSI092'
# deployment
tsi092 <- tsi092[which(tsi092$Date >= "2023-09-07"),]


# subset to each deployment
# TSI093
# Tag ID : 58562
# Argos ID : 245096
# Deployment Start: 2023-09-09
# Deployment End : 

tsi093 <- read.table('DSA_Con_245096_231018124044.txt', skip=2, sep=",")
colnames(tsi093) = c("CRC", "Date", "Time", "Latitude", "Longitude", "Fix", "Alt(m)")
tsi093 = tsi093[,1:7]
tsi093$Date <- as.Date(tsi093$Date, format = "%d/%m/%Y") 
# add id column
tsi093$Tag.ID. <- '58562'
tsi093$Argos.ID. <- '245096'
tsi093$Bat.ID. <- 'TSI093'
# deployment
tsi093 <- tsi093[which(tsi093$Date >= "2023-09-09"),]



## bind together
points.df.all <- rbind(ana139, ana140, ana154, maro113, maro114, maro115, tsi092, tsi093)
points.df.OK <- points.df.all[which(points.df.all$CRC == '           OK'),]
## export cleaned data
write.csv(points.df.OK, "/Users/sophiahorigan/Documents/GitHub/mada-bat-telemetry/RCode/pruf_morsub_argos_all.csv")


