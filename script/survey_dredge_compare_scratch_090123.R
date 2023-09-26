########Survey dredge comaprison#########3

#revamp with code joined correcly

#libraries go here
library(ggplot2)
library(tidyverse)

#read in data
logbook <- read.csv("./data_folder/new_data_dump/DredgeSurvey_FishLogData_CatchComparisonHauls.csv") 

## catch data
catch_raw <- read.csv("./data_folder/new_data_dump/DredgeSurvey_CatchData_CatchComparisonHauls.csv") 

## specimen data
specimen <- read.csv("./data_folder/new_data_dump/DredgeSurvey_ScallopBioData_CatchComparisonHauls.csv")



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
joined_full <- full_join(logbook, catch_raw, by)
##line 62 is

#09/15/23 so the above is right but not entierly right . We need to add the zero catches
##testing joined_full
###line 62 in this is a zero. There are others.
##check tyler email

#alex explores
unique(joined$comname)
joined_clean <- joined %>% filter(comname == "weathervane scallop")
##looks good, BUT we want to remove hauls where both comparison pulls have 0 catches
##and I'm going to get a new datset from Ryan at some popint to account for a data entry error
##see Tyler email


#calculate CPUE
joined_clean_cpue <- joined_clean %>% 
    mutate(cpue_cnt = samp_cnt/area_swept_sqnm,
           cpue_wt = samp_wt/area_swept_sqnm)



###END TYLER BETA

#separate large and small scallops
large_scallops <- joined_clean_cpue %>% filter(samp_grp==1) 


small_scallops <- joined_clean_cpue %>% filter(samp_grp==2)

#EDA
ggplot(large_scallops) + aes(x=cpue_wt) + geom_density()
ggplot(small_scallops) + aes(x=cpue_wt) + geom_density()
hist(large_scallops$cpue_wt)
hist(large_scallops$cpue_cnt)
summary(large_scallops$cpue_wt)

#log the CPUE
hist(log(large_scallops$cpue_wt))
hist(log(large_scallops$cpue_cnt)) #not normal but will pass

hist(log(small_scallops$cpue_wt))
hist(log(small_scallops$cpue_cnt)) #not normal but will pass


########################################
###TYLER BETA
expand_grid(logbook,
            distinct(transmute(catch_raw, samp_grp, rcode, comname, whole_haul, sample_type))) %>%
    left_join(catch_raw) %>%
    # fill in zero catches
    replace_na(list(samp_cnt = 0, samp_wt = 0)) %>%
    # filter for scallops
    filter(rcode == 74120) -> catch # now you have records of scallop catch per size class (samp_grp) per gear type

#########################################


##randomized block ANOVA

##other statistical methods-


#calculaute SHAD (shell height and .... uh... density?) to get the density distributions between the dredges
##small and large as one. Don't separate for this one. see tyler notes and email
#also see alyssa emails to check you're doing it right