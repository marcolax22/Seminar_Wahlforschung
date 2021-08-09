#loading all packages--------------------------------------------------------------

library(tidyverse)
library(haven)
library(naniar)
library(lubridate)
library(zoo)

#read in the data------------------------------------------------------------------

ZA5770 <- read_dta(file.path("data", "ZA5770.dta"))
ZA5770_birth <- read_dta(file.path("data", "ZA5770_Birthmonth.dta"))

#preparing the data set: ZA5770.dta------------------------------------------------

ZA5770.prepare <- ZA5770 %>% select(lfdn, j, k, l, m, n, jahr, jhhchild18, j60_2, n60_2,
                            j65a, j65b, j64a, j64b, m65a, m65b, m64a, m64b, j69a, n69a,
                            j69b, n69b, j70a, j70b, n70a, n70b, 
                            jpidstrk, npidstrk, jpid_a, jpid_b, lfdn_lfp09)

ZA5770.prepare <-  ZA5770.prepare %>% rename(PID_2013 = jpidstrk,
                                             PID_2017 = npidstrk,
                                             Teilnahme_2013 = j,
                                             Teilnahme_2014 = k,
                                             Teilnahme_2015 = l,
                                             Teilnahme_2016 = m,
                                             Teilnahme_2017 = n,
                                             Unter_18 = jhhchild18,
                                             Wahlbeteiligung_2013 = j60_2,
                                             Wahlbeteiligung_2017 = n60_2)

#preparing the data set: ZA5770_Birthmonth.dta-------------------------------------

ZA5770_birth.prepare <- ZA5770_birth %>% 
  select(lfdn, jmonat, kmonat, lmonat,mmonat, nmonat)

ZA5770_birth.prepare$monat <- ifelse(ZA5770_birth.prepare$jmonat > 
                                       -95, ZA5770_birth.prepare$jmonat,
                              ifelse(ZA5770_birth.prepare$kmonat > 
                                       -95, ZA5770_birth.prepare$kmonat,
                              ifelse(ZA5770_birth.prepare$lmonat > 
                                       -95, ZA5770_birth.prepare$lmonat,
                              ifelse(ZA5770_birth.prepare$mmonat > 
                                       -95, ZA5770_birth.prepare$mmonat,
                              ifelse(ZA5770_birth.prepare$nmonat > 
                                       -95, ZA5770_birth.prepare$nmonat, NA)))))

ZA5770_birth.prepare <- ZA5770_birth.prepare %>% select(lfdn,monat)

#joining the datasets--------------------------------------------------------------

ZA5770_full <- full_join(ZA5770.prepare, ZA5770_birth.prepare, by = "lfdn", na_matches = "never")

#joining month and year------------------------------------------------------------

ZA5770_full$jahr <- as_factor(ZA5770_full$jahr)

ZA5770_full <- ZA5770_full %>% unite(month_year, monat, jahr, sep = "-") %>% 
  mutate(month_year = as.yearmon(month_year, "%m-%Y")) %>% 
  mutate(month_year = as.Date(month_year))

#Randomization for the day of birth------------------------------------------------

#ZA5770_full$tag <- sample(1:28, 5456, replace=T)

#Saving the data set with merged data----------------------------------------------

save(ZA5770_full, file = "data/ZA5770_full.RData")

#dataset for RDD-------------------------------------------------------------------

load("data/ZA5770_full.RData")

ZA_RDD <- ZA5770_full %>% filter(month_year >= "1992-01-01")
ZA_RDD2 <- ZA5770_full %>% filter(month_year >= "1990-01-01" & month_year <= "1993-01-01")

#preparing the dataset for RDD and RDD2--------------------------------------------

class(ZA_RDD$jpid_a)
ZA_RDD$jpid_a <- as.numeric(ZA_RDD$jpid_a)
ZA_RDD$PID_2013 <- as.numeric(ZA_RDD$PID_2013)

na_strings <- c("-97")

ZA_RDD <- ZA_RDD %>% replace_with_na_all(condition = ~ .x %in% na_strings |(as.integer(.x) < 0))

ZA_RDD$PID_2013[is.na(ZA_RDD$PID_2013)] = 0    

#-----------------------

ZA_RDD2$PID_2013 <- as.numeric(ZA_RDD2$PID_2013)

na_strings <- c("-97")

ZA_RDD2 <- ZA_RDD2 %>% replace_with_na_all(condition = ~ .x %in% na_strings |(as.integer(.x) < 0))

ZA_RDD2$PID_2013[is.na(ZA_RDD2$PID_2013)] = 0  

#---------------------------------------------------------------------------------

ZA_RDD <- ZA_RDD %>% 




#Saving the data set---------------------------------------------------------------

save(ZA_RDD, file = "data/ZA_RDD.RData")
save(ZA_RDD2, file= "data/ZA_RDD2.RData")
