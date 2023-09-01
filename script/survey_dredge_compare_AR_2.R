#######################################
####Dredge comparison -AR #####
##initiated: 08/22/23
##last edits: 08/22/23
########################################



##SET THE STAGE
#load in packages
library(ggplot2)
library(tidyverse)
#library(ggpmisc)
#library(patchwork)
#library(scales)





#read in data
logbook <- read.csv("./Scallop_project_data/DredgeSurvey_FishLogData_CatchComparisonHauls.csv") 

## catch data
catch_raw <- read.csv("./Scallop_project_data/DredgeSurvey_CatchData_CatchComparisonHauls.csv") 

## specimen data
specimen <- read.csv("./Scallop_project_data/DredgeSurvey_ScallopBioData_CatchComparisonHauls.csv")



#functions?
##edit this to work for me
f_clean_log<- function(x, drop = T){ 
    x %>%
        as_tibble() %>%
        filter(haul_type == 10,
               tow != 19010054,
               perform %in% 1:4) %>%
        mutate(lat = (lat_start + lat_end) / 2,
               lon = (lon_start + lon_end) / 2,
               area_swept = distance_nm * 0.00131663,
               depth_avg = ifelse(is.na(depth_avg), (depth_start + depth_end) / 2, depth_avg)) %>%
        
        rename(year = cruise_year) -> tmp
    if(drop == T){
        tmp %>%
            dplyr::select(year, cruise, tow, haul, perform, district, bed_code, bed_name, lat, lon, depth_avg, area_swept)
    } else{tmp}
}

f_clean_log_2<- function(x, drop = T){ 
    x %>%
        as_tibble() %>%
        filter(haul_type == 11,
               tow != 19010054,
               perform %in% 1:4) %>%
        mutate(lat = (lat_start + lat_end) / 2,
               lon = (lon_start + lon_end) / 2,
               area_swept = distance_nm * 0.00131663,
               depth_avg = ifelse(is.na(depth_avg), (depth_start + depth_end) / 2, depth_avg)) %>%
        
        rename(year = cruise_year) -> tmp
    if(drop == T){
        tmp %>%
            dplyr::select(year, cruise, tow, haul, perform, district, bed_code, bed_name, lat, lon, depth_avg, area_swept, gear_code)
    } else{tmp}
}


## compute total catch and cpue by tow 
## args
### x - raw catch data (2019 - present format)
### y - cleaned logbook data (see f_clean_log)
f_catch_by_tow <- function(x, y){
    # remove clappers and empty shells
    x %>%
        filter(!(comname %in% c("Empty shells", "Clapper", "Clappers", "Empty_Shells, Scallop", "Empty Scallop Shells"))) %>%
        mutate(comname = ifelse(grepl("eathervane", comname), "weathervane scallop", comname)) %>%
        
        # temporarily give small scallops a different rcode for ease of data summary
        
        mutate(rcode = ifelse((rcode == 74120 & samp_grp == 2), rcode + 9999999, rcode)) -> x
    
    # deal with all whole hauled samples
    x %>%
        filter(whole_haul == "Y") %>%
        group_by(tow, rcode, samp_grp) %>%
        summarise(samp_cnt = sum(samp_cnt, na.rm = T),
                  samp_wt = sum(samp_wt, na.rm = T)) -> tmp
    
    # deal with non-whole hauled samples
    # join and include non-catch hauls
    # compute cpue
    x %>%
        filter(whole_haul == "N") %>%
        dplyr::select(tow, rcode, samp_cnt, samp_wt) %>%
        group_by(tow) %>%
        mutate(sub_wt = sum(samp_wt, na.rm = T)) %>%
        left_join(tmp %>%
                      ungroup() %>%
                      filter(rcode == 99997) %>%
                      dplyr::select(tow, samp_wt) %>%
                      rename(bulk_wt = samp_wt), by = "tow") %>%
        mutate(bulk_wt = bulk_wt + sub_wt) %>%
        mutate(samp_wt = bulk_wt / sub_wt * samp_wt,
               samp_cnt = bulk_wt / sub_wt * samp_cnt) %>%
        dplyr::select(-sub_wt, -bulk_wt) %>%
        bind_rows(tmp) %>%
        right_join(expand_grid(rcode = unique(x$rcode), 
                               tow = unique(y$tow)) %>%
                       mutate(samp_grp = case_when(rcode == 74120 ~ 1,
                                                   rcode == (74120 + 9999999) ~ 2)), 
                   by = c("rcode", "tow", "samp_grp"))  %>%
        filter(rcode != 99997) %>%
        replace_na(list(samp_wt = 0)) %>%
        mutate(samp_cnt = ifelse(samp_wt == 0, 0, samp_cnt)) %>%
        right_join(y, by = "tow") %>%
        mutate(cpue_cnt = ifelse(rcode %in% c(74120, 74120 + 9999999), samp_cnt / (0.83*area_swept), samp_cnt / area_swept),
               cpue_wt = ifelse(rcode %in% c(74120, 74120 + 9999999), samp_wt / (0.83*area_swept), samp_wt / area_swept)) %>%
        ungroup %>%
        dplyr::select(6, 7, 1, 8:16, 2, 5, 3:4, 17:18) %>%
        # change rcode of small scallops back
        mutate(rcode = ifelse(rcode == (74120 + 9999999), 74120, rcode)) -> out
    
    return(out)
    
}


#make sure we select right columns for scallop gear conmparison purposes
f_catch_by_tow_2 <- function(x, y){
    # remove clappers and empty shells
    x %>%
        filter(!(comname %in% c("Empty shells", "Clapper", "Clappers", "Empty_Shells, Scallop", "Empty Scallop Shells"))) %>%
        mutate(comname = ifelse(grepl("eathervane", comname), "weathervane scallop", comname)) %>%
        
        # temporarily give small scallops a different rcode for ease of data summary
        
        mutate(rcode = ifelse((rcode == 74120 & samp_grp == 2), rcode + 9999999, rcode)) -> x
    
    # deal with all whole hauled samples
    x %>%
        filter(whole_haul == "Y") %>%
        group_by(tow, rcode, samp_grp) %>%
        summarise(samp_cnt = sum(samp_cnt, na.rm = T),
                  samp_wt = sum(samp_wt, na.rm = T)) -> tmp
    
    # deal with non-whole hauled samples
    # join and include non-catch hauls
    # compute cpue
    x %>%
        filter(whole_haul == "N") %>%
        dplyr::select(tow, rcode, samp_cnt, samp_wt) %>%
        group_by(tow) %>%
        mutate(sub_wt = sum(samp_wt, na.rm = T)) %>%
        left_join(tmp %>%
                      ungroup() %>%
                      filter(rcode == 99997) %>%
                      dplyr::select(tow, samp_wt) %>%
                      rename(bulk_wt = samp_wt), by = "tow") %>%
        mutate(bulk_wt = bulk_wt + sub_wt) %>%
        mutate(samp_wt = bulk_wt / sub_wt * samp_wt,
               samp_cnt = bulk_wt / sub_wt * samp_cnt) %>%
        dplyr::select(-sub_wt, -bulk_wt) %>%
        bind_rows(tmp) %>%
        right_join(expand_grid(rcode = unique(x$rcode), 
                               tow = unique(y$tow)) %>%
                       mutate(samp_grp = case_when(rcode == 74120 ~ 1,
                                                   rcode == (74120 + 9999999) ~ 2)), 
                   by = c("rcode", "tow", "samp_grp"))  %>%
        filter(rcode != 99997) %>%
        replace_na(list(samp_wt = 0)) %>%
        mutate(samp_cnt = ifelse(samp_wt == 0, 0, samp_cnt)) %>%
        right_join(y, by = "tow") %>%
        mutate(cpue_cnt = ifelse(rcode %in% c(74120, 74120 + 9999999), samp_cnt / (0.83*area_swept), samp_cnt / area_swept),
               cpue_wt = ifelse(rcode %in% c(74120, 74120 + 9999999), samp_wt / (0.83*area_swept), samp_wt / area_swept)) %>%
        ungroup %>%
        dplyr::select(6, 7, 1, 8:16, 2, 5, 3:4, 17:19) %>% #was 17:18, changed to 17:19. Does it work? YES!
        # change rcode of small scallops back
        mutate(rcode = ifelse(rcode == (74120 + 9999999), 74120, rcode)) -> out
    
    return(out)
    
}

## separate shell height and damage data from all specimen data, compute sample factor
## args
### x - raw specimen data (2019 - present format)
### y - cleaned catch by tow data (see output of f_catch_by_tow)
f_get_shad <- function(x, y){ #Shell High And Damage = shad
    x %>%
        filter(tow %in% y$tow) %>%
        as_tibble() %>%
        filter(samp_grp %in% c(1, 2),
               !is.na(shell_height)) %>%
        dplyr::select(-whole_wt, -sex, -shell_num, -gonad, -meat_condition, -mud_blister,
                      -shell_worm, -shell_retained, -meat_wt) %>%
        rename(year = cruise_year) %>%
        group_by(year, tow, samp_grp, shell_height, damage) %>%
        summarise(count = n()) %>%
        group_by(tow, samp_grp) %>%
        mutate(n_measured = sum(count)) %>% ungroup %>%
        # join to catch data to comput sample_fraction 
        # no data that are not in standard, good perfomance hauls
        left_join(y %>%
                      filter(rcode == 74120) %>%
                      dplyr::select(year, cruise, tow, haul, district, bed_code, bed_name, lat, lon, depth_avg,
                                    area_swept, rcode, samp_grp, samp_cnt),
                  by = c("tow", "samp_grp", "year")) %>%
        mutate(sample_factor = samp_cnt * (count / n_measured)) %>%
        dplyr::select(year, cruise, tow, haul, district, bed_code, bed_name, lat, lon, depth_avg, area_swept,
                      rcode, samp_grp, shell_height, damage, sample_factor) -> out 
    return(out)
}

#EDIT SO GEAR CODE STAYS IN OUTPUT
f_get_shad_2 <- function(x, y){ #Shell High And Damage = shad
    x %>%
        filter(tow %in% y$tow) %>%
        as_tibble() %>%
        filter(samp_grp %in% c(1, 2),
               !is.na(shell_height)) %>%
        dplyr::select(-whole_wt, -sex, -shell_num, -gonad, -meat_condition, -mud_blister,
                      -shell_worm, -shell_retained, -meat_wt) %>%
        rename(year = cruise_year) %>%
        group_by(year, tow, samp_grp, shell_height, damage) %>%
        summarise(count = n()) %>%
        group_by(tow, samp_grp) %>%
        mutate(n_measured = sum(count)) %>% ungroup %>%
        # join to catch data to comput sample_fraction 
        # no data that are not in standard, good perfomance hauls
        left_join(y %>%
                      filter(rcode == 74120) %>% #why this rcode? Is this right? #seems to filter for the right survey data?
                      dplyr::select(year, cruise, tow, haul, district, bed_code, bed_name, lat, lon, depth_avg,
                                    area_swept, rcode, samp_grp, samp_cnt, gear_code),
                  by = c("tow", "samp_grp", "year")) %>%
        mutate(sample_factor = samp_cnt * (count / n_measured)) %>%
        dplyr::select(year, cruise, tow, haul, district, bed_code, bed_name, lat, lon, depth_avg, area_swept,
                      rcode, samp_grp, shell_height, damage, sample_factor, gear_code) -> out #tried adding gear_code here and it did not work
    return(out)
}

#data wrangle
###Q: we just need data from 2022 for the comparison?
tows <-  f_clean_log(logbook) 
#ok this gives me regular survey hauls. is this what I want? I think I want dredge comparison hauls?
##filter for comaprison hauls, then compare hauls 1 (old dredge) to hauls 4 and 5 (new dredge)

tows_compare <- f_clean_log_2(logbook)
unique(tows_compare$gear_code) #ok, this is the comparison. This is what I want, I think

#catch
catch <- f_catch_by_tow(catch_raw, tows) #this is from tylers code

catch_compare <- f_catch_by_tow_2(catch_raw, tows_compare) #tyler's function, tows_compare (the function to create it) was edited to use comparison hauls instead of regular hauls
##NOTE, I may want to include regular hauls in the catch comparison, esp for Kappenman's method and maybe for GLM/GLMM
##but let's stick with the direct comparison for now.

shad <- f_get_shad(specimen %>%
                       mutate(whole_wt = NA, sex = NA, gonad = NA, meat_condition = NA,
                              mud_blister = NA, shell_worm = NA, shell_retained = NA, meat_wt = NA), catch)

# I did not make any edits to this one from Tyler's code. I may need to make edits. Be aware of this.
#Q: for the specimen and shad datasets how do I know which ones are from experimental hauls and which ones are from regular hauls? Maybe looking at the function will tell me.

shad_compare <- f_get_shad_2(specimen %>%
                               mutate(whole_wt = NA, sex = NA, gonad = NA, meat_condition = NA,
                                      mud_blister = NA, shell_worm = NA, shell_retained = NA, meat_wt = NA), catch_compare)
##this one uses the comparison data (haul type 11) and I edited the function to put gear_code in the output. 
##ok, that seemed to work. My data is good if we're cool with using just haul type 11 in analysis and including gear_code in the output datasets.


#############################################
#EXPLORATORY DATA ANALYSIS
##CPUE. graph
ggplot(catch_compare) + aes(x=cpue_cnt) + geom_density() #wow that's a lot of 0's. Poisson dist? Delta dist?
ggplot(catch_compare) + aes(x=cpue_wt)+geom_density()

#going to need a model that can handle lots of zeros. zero-inflated poisson? hurdle model? delta distribution
##maybe kappenman's can work here too
##a kind of ANOVA that does not need gaussian?

#what if we log cpue?
ggplot(catch_compare) + aes(x=log(cpue_cnt+1)) + geom_density() #now we have a bimodal distribution. Fantastic/
ggplot(catch_compare) + aes(x=log(cpue_wt+1))+geom_density() #bimondal cpue distribution
hist(log(catch_compare$cpue_cnt+1))
hist(log(catch_compare$cpue_wt+1))

hist(catch_compare$cpue_wt)

ggplot(catch_compare) + aes(x=log(cpue_cnt+1)) + geom_density() +facet_wrap(~gear_code)
ggplot(catch_compare) + aes(x=log(cpue_wt+1))+geom_density()+ facet_wrap(~gear_code)

ggplot(catch_compare) + aes(x=factor(gear_code), y=log(cpue_cnt+1)) + geom_violin() + geom_jitter()
ggplot(catch_compare) + aes(x=factor(gear_code), y=log(cpue_wt+1)) + geom_violin() + geom_jitter()


ggplot(catch_compare) + aes(x=factor(gear_code), y=cpue_cnt) + geom_violin() + geom_jitter()
ggplot(catch_compare) + aes(x=factor(gear_code), y=cpue_wt) + geom_violin() + geom_jitter()

ggplot(catch_compare) + aes(x=haul, y=cpue_wt, color=gear_code) + geom_point() + geom_smooth()
ggplot(catch_compare) + aes(x=factor(haul), y=log(cpue_wt+1), color=factor(gear_code)) + geom_point() + geom_smooth()

#one paper I read.... which one?...had a binomial model to see if there was a catch at all and then...
###Hesler et al 2004
##Bernoulli distribution used to model the probability of a non-zero haul
###then gamma distibution to model catch rate (can also try delta, over-dispersed poisson)
##maybe the log-normal distribution? I already logge dthe datatho?

#########################################
##ANALYSIS
#randomized block ANOVA -- (is the data set up ok for this method)
#note- see Tyler code if possible, and see what has already been done with scallops
#see the paper of this design where Tyler was an author
#shoot, which dataset do I use?
##I have three of them. Maybe shad_compare because that uses all of them?
##catch_compare because that has the cpue?
##I IDENTIFIED A PROBLEM. CATCH_COMPARE DOES NOT HAVE CPUE. cATCH DOES HAVE CPUE. GO INTO THAT FUNCTION AND FIGURE OUT WHY. CATCH_COMPARE SHOULD HAVE CPUE. FIGURE OUT IF IT IS A SELECTION THING OR IF IT IS A LACKING DATA COLUMNS THING.
##problem resolved
##now fiugre out how to set up the ANOVA- see Tyler paper and other examples

#do MSE simulations too? see tyler paper

#other analysis methods
#Kapennmans (is there a function for this?)

#GLM, GLMM - see paper

#GAM, GAMM - see Matt Cheng paper

#hurdle model
library(pscl)
catch_compare_2 <- catch_compare %>%
    mutate(log_cpue_cnt = log(cpue_cnt + 1) ) %>%
        mutate(log_cpue_wt = log(cpue_wt +1)) 

#adding in the grand mean as the vector
u <- mean(catch_compare_2$log_cpue_wt)
vec_u <- rep(u, length(catch_compare_2$log_cpue_wt) )
catch_compare_2$vec_u <- vec_u


    
mod_hurdle_1 <- hurdle(log_cpue_wt ~ as.factor(haul), data = catch_compare_2, dist= "poisson") #try this
mod_hurdle_2 <- hurdle(as.numeric(cpue_wt) ~ as.factor(haul), data = catch_compare_2, dist= "poisson") #neither work


library(gamlss)

###new attempt
#join logbook to catch, compute cpue. (this data is provided)
##we should not have lots of zeros
