
#################################
## CLEAN DATA _ 1
################################
# Created by: Sophia Horigan
# Contact: shorigan@uchicago.edu
# Last updated: May 7, 2024

# This script takes in Movebank telemetry downloads to clean and prep for figures


rm(list=ls())

# set wd
setwd("/Users/sophiahorigan/Documents/GitHub/mada-bat-telemetry/RCode")

# load data
# update to most recent
points.df <- read.csv('ArgosData_2023_10_16_10_48_59.csv', header = TRUE, sep = ';')

## subset to each deployment

## ANA139
## Tag ID : 
## Argos ID : 245091
## Deployment Start: 2023-08-23 
## Deployment End : 

# id subset
ana139 <- points.df[which(points.df$Platform.ID.No. == '245091'), ]

# deployment subset
# ana139 <- ana139[which(ana139$Loc..date > '2023-08-22' && ana139$Loc..date > '')]

# add id column
ana139$Bat.ID. <- 'ANA139'

# subset to each deployment
# ANA140
# Tag ID : 58556
# Argos ID : 245090
# Deployment Start: 2023-08-23 
# Deployment End : 

# id subset
ana140 <- points.df[which(points.df$Platform.ID.No. == '245090'), ]

# add id column
ana140$Bat.ID. <- 'ANA140'


# subset to each deployment
# ANA154
# Tag ID : 58563
# Argos ID : 245097
# Deployment Start: 2023-10-01
# Deployment End : 

# id subset
ana154 <- points.df[which(points.df$Platform.ID.No. == '245097'), ]

# add deployment (this tag used at both Ambositra and Analambotaka)
ana154 <- ana154[which(ana154$Loc..date > '10-01-2023 00:00:00'),]

# add id column
ana154$Bat.ID. <- 'ANA154'


# subset to each deployment
# MARO113
# Tag ID : 58564
# Argos ID : 245098
# Deployment Start: 2023-09-11
# Deployment End : 

# id subset
maro113 <- points.df[which(points.df$Platform.ID.No. == '245098'), ]

# add id column
maro113$Bat.ID. <- 'MARO113'


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


