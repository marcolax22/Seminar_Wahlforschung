#loading all packages--------------------------------------------------------------

library(tidyverse)
library(haven)
library(naniar)
library(lubridate)
library(zoo)

###################################################################################

load("data/ZA5770_full.RData")

#dataset for RDD-------------------------------------------------------------------

ZA5770_RDD <- ZA5770_full %>% filter(month_year >= "1992-01-01")
ZA5770_RDD2 <- ZA5770_full %>% filter(month_year >= "1990-01-01" & month_year <= "1993-01-01")

#preparing the dataset for RDD and RDD2--------------------------------------------

class(ZA5770_RDD$jpid_a)
ZA5770_RDD$jpid_a <- as.numeric(ZA5770_RDD$jpid_a)
ZA5770_RDD$PID_2013 <- as.numeric(ZA5770_RDD$PID_2013)

na_strings <- c("-97")

ZA5770_RDD <- ZA5770_RDD %>% replace_with_na_all(condition = ~ .x %in% na_strings |(as.integer(.x) < 0))

ZA5770_RDD$PID_2013[is.na(ZA5770_RDD$PID_2013)] = 0    

#-----------------------

ZA5770_RDD2$PID_2013 <- as.numeric(ZA5770_RDD2$PID_2013)

na_strings <- c("-97")

ZA5770_RDD2 <- ZA5770_RDD2 %>% replace_with_na_all(condition = ~ .x %in% na_strings |(as.integer(.x) < 0))

ZA5770_RDD2$PID_2013[is.na(ZA5770_RDD2$PID_2013)] = 0  

#Change date into figures----------------------------------------------------------

ZA5770_RDD <- ZA5770_RDD %>% select(lfdn, month_year, PID_2013)

ZA5770_RDD <- ZA5770_RDD %>%  if(ZA5770_RDD$month_year = "1995-08-01")
  
  
  ifelse(ZA5322_birth$fmonat < 
                                       13, ZA5322_birth$fmonat,
                                     ifelse(ZA5322_birth$gmonat < 
                                              13, ZA5322_birth$gmonat,
                                            ifelse(ZA5322_birth$jmonat < 
                                                     13, ZA5322_birth$jmonat, NA)))

###################################################################################




###################################################################################

load("data/ZA5320_full.RData")

#dataset for RDD PID_2005----------------------------------------------------------

ZA5320_RDD <- ZA5320_full %>% filter(month_year >= "1984-01-01" & month_year <= "1988-01-01")

#preparing the dataset for RDD PID_2005--------------------------------------------

#class(ZA5320_RDD$jpid_a)
#ZA5320_RDD$jpid_a <- as.numeric(ZA5320_RDD$jpid_a)
ZA5320_RDD$PID_2005 <- as.numeric(ZA5320_RDD$PID_2005)

na_strings <- c("-97")

ZA5320_RDD <- ZA5320_RDD %>% replace_with_na_all(condition = ~ .x %in% na_strings |(as.integer(.x) > 5))

ZA5320_RDD$PID_2005[is.na(ZA5320_RDD$PID_2005)] = 0    


#dataset for RDD PID_2009----------------------------------------------------------

ZA55320_RDD <- ZA5320_full %>% filter(month_year >= "1989-01-01" & month_year <= "1995-01-01")

#preparing the dataset for RDD PID_2009--------------------------------------------

#class(ZA5320_RDD$jpid_a)
#ZA5320_RDD$jpid_a <- as.numeric(ZA5320_RDD$jpid_a)
ZA5320_RDD$PID_2009 <- as.numeric(ZA5320_RDD$PID_2009)

na_strings <- c("-97")

ZA5320_RDD <- ZA5320_RDD %>% replace_with_na_all(condition = ~ .x %in% na_strings |(as.integer(.x) < 0))

ZA5320_RDD$PID_2009[is.na(ZA5320_RDD$PID_2009)] = 0    

###################################################################################




###################################################################################

load("data/ZA5321_full.RData")

ZA5321_RDD <- ZA5321_full %>% filter(month_year >= "1992-01-01")
ZA5321_RDD2 <- ZA5321_full %>% filter(month_year >= "1990-01-01" & month_year <= "1993-01-01")

#preparing the dataset for RDD and RDD2--------------------------------------------

#class(ZA5321_RDD$jpid_a)
#ZA5321_RDD$jpid_a <- as.numeric(ZA5321_RDD$jpid_a)
ZA5321_RDD$PID_2013 <- as.numeric(ZA5321_RDD$PID_2013)

na_strings <- c("-97")

ZA5321_RDD <- ZA5321_RDD %>% replace_with_na_all(condition = ~ .x %in% na_strings |(as.integer(.x) < 0))

ZA5321_RDD$PID_2013[is.na(ZA5321_RDD$PID_2013)] = 0  

###################################################################################




###################################################################################

load("data/ZA5322_full.RData")

ZA5322_RDD <- ZA5322_full %>% filter(month_year >= "1992-01-01")
ZA322_RDD2 <- ZA5322_full %>% filter(month_year >= "1990-01-01" & month_year <= "1993-01-01")

#preparing the dataset for RDD and RDD2--------------------------------------------

class(ZA5322_RDD$jpid_a)
ZA5322_RDD$jpid_a <- as.numeric(ZA5322_RDD$jpid_a)
ZA5322_RDD$PID_2013 <- as.numeric(ZA5322_RDD$PID_2013)

na_strings <- c("-97")

ZA5322_RDD <- ZA55322_RDD %>% replace_with_na_all(condition = ~ .x %in% na_strings |(as.integer(.x) < 0))

ZA5322_RDD$PID_2013[is.na(ZA5322_RDD$PID_2013)] = 0  

###################################################################################




###################################################################################

#Saving the data set

save(ZA5770_RDD, file = "data/ZA5770_RDD.RData")

save(ZA5770_RDD2, file= "data/ZA5770_RDD2.RData")

save(ZA5320_RDD, file = "data/ZA5320_RDD.RData")

save(ZA5321_RDD, file = "data/ZA53201_RDD.RData")

save(ZA5322_RDD, file = "data/ZA5322_RDD.RData")

###################################################################################




###################################################################################

#Joining the different datasets





###################################################################################