########Survey dredge comaprison#########3

#revamp with code joined correcly

#libraries go here
library(ggplot2)
library(tidyverse)

#read in data
logbook <- read.csv("./data_folder/DredgeSurvey_FishLogData_CatchComparisonHauls.csv") 

## catch data
catch_raw <- read.csv("./data_folder/DredgeSurvey_CatchData_CatchComparisonHauls.csv") 

## specimen data
specimen <- read.csv("./data_folder/DredgeSurvey_ScallopBioData_CatchComparisonHauls.csv")



#join the logbook data to the catch data by _____ and then calculate the CPUE - see notes from conversation with tyler
View(logbook)
View(catch_raw)
length(logbook$cruise_year)
length(catch_raw$cruise_year)

by <- join_by("cruise_year", "cruise", "tow", "haul", "stn_id", "bed_code", "bed_name", "reg_area", "district", "depth_avg")
joined <- left_join(logbook, catch_raw, by)
joined2 <- right_join(logbook, catch_raw, by) no
joined3 <-left_join(logbook, catch_raw)
joined4 <- inner_join(logbook, catch_raw)
joined5 <- inner_join(catch_raw, logbook)
joined <- left_join(logbook, catch_raw, relationship="one-to-one")


#alex explores
unique(joined$comname)
joined_clean <- joined %>% filter(comname == "weathervane scallop")
##looks good, BUT we want to remove hauls where both comparison pulls have 0 catches
##and I'm going to get a new datset from Ryan at some popint to account for a data entry error
##see Tyler email

##randomized block ANOVA

##other statistical methods-


#calculaute SHAD (shell height and .... uh... density?) to get the density distributions between the dredges