#loading all packages--------------------------------------------------------------

library(tidyverse)
library(haven)
library(naniar)
library(lubridate)
library(zoo)

###################################################################################

#read in the data------------------------------------------------------------------

ZA5770 <- read_dta(file.path("data", "ZA5770.dta"))
ZA5320 <- read_dta(file.path("data","ZA5320.dta"))
ZA5321 <- read_dta(file.path("data","ZA5321.dta"))
ZA5322 <- read_dta(file.path("data","ZA5322.dta"))

#read in the data of birthmonths---------------------------------------------------

ZA5770_birth <- read_dta(file.path("data", "ZA5770_Birthmonth.dta"))
ZA5320_birth <- read_dta(file.path("data","20210830_Shikano_ZA5320_v2-0-0.dta"))
ZA5321_birth <- read_dta(file.path("data","20210830_Shikano_ZA5321_v2-1-0.dta"))
ZA5322_birth <- read_dta(file.path("data","20210830_Shikano_ZA5322_v1-1-0.dta"))

###################################################################################




###################################################################################

#preparing the data set: ZA5770.dta------------------------------------------------

ZA5770 <- ZA5770 %>% select(lfdn, jahr, j, k, l, m, n, jhhchild18, j60_2, n60_2,
                            j65a, j65b, j64a, j64b, m65a, m65b, m64a, m64b, j69a, n69a,
                            j69b, n69b, j70a, j70b, n70a, n70b, 
                            jpidstrk, npidstrk, jpid_a, jpid_b, lfdn_lfp09)


ZA5770 <- ZA5770 %>% rename(PID_2013 = jpidstrk,
                            PID_2017 = npidstrk,
                            Teilnahme_2013 = j,
                            Teilnahme_2014 = k,
                            Teilnahme_2015 = l,
                            Teilnahme_2016 = m,
                            Teilnahme_2017 = n,
                            Unter_18 = jhhchild18,
                            Wahlbeteiligung_2013 = j60_2,
                            Wahlbeteiligung_2017 = n60_2)

#preparing the data set: ZA5320.dta------------------------------------------------

ZA5320 <- ZA5320 %>% select(id, cjahr, cpidstrk, dpidstrk, epidstrk)

ZA5320 <- ZA5320 %>% rename(lfdn = id,
                            PID_2002 = cpidstrk,
                            PID_2005 = dpidstrk,
                            PID_2009 = epidstrk)

#preparing the data set: ZA5321.dta------------------------------------------------

ZA5321 <- ZA5321 %>% select(lfdn, jahr, dpidstrk, epidstrk, fpidstrk, hpidstrk, jpidstrk)

ZA5321 <- ZA5321 %>% rename(PID_2005 = dpidstrk,
                            PID_2007 = epidstrk,
                            PID_2009 = fpidstrk,
                            PID_2011 = hpidstrk,
                            PID_2013 = jpidstrk)

#preparing the data set: ZA5322.dta------------------------------------------------

ZA5322 <- ZA5322 %>% select(lfdn, jahr, hpidstrk, jpidstrk)

ZA5322 <- ZA5322 %>% rename(PID_2011 = hpidstrk,
                            PID_2013 = jpidstrk)

#preparing the data set: ZA5770_birth----------------------------------------------

ZA5770_birth <- ZA5770_birth %>% 
  select(lfdn, jmonat, kmonat, lmonat,mmonat, nmonat)

ZA5770_birth$monat <- ifelse(ZA5770_birth$jmonat > 
                               -95, ZA5770_birth$jmonat,
                             ifelse(ZA5770_birth$kmonat > 
                                      -95, ZA5770_birth$kmonat,
                                    ifelse(ZA5770_birth$lmonat > 
                                             -95, ZA5770_birth$lmonat,
                                           ifelse(ZA5770_birth$mmonat > 
                                                    -95, ZA5770_birth$mmonat,
                                                  ifelse(ZA5770_birth$nmonat > 
                                                           -95, ZA5770_birth$nmonat, NA)))))

ZA5770_birth <- ZA5770_birth %>% select(lfdn,monat)

#preparing the data set: ZA5320_birth----------------------------------------------

ZA5320_birth <- ZA5320_birth %>% 
  select(id, cmonat, dmonat, emonat)

ZA5320_birth$monat <- ifelse(ZA5320_birth$cmonat <
                               13, ZA5320_birth$cmonat,
                             ifelse(ZA5320_birth$dmonat <
                                      13, ZA5320_birth$dmonat,
                                    ifelse(ZA5320_birth$emonat <
                                             13, ZA5320_birth$emonat, NA)))

ZA5320_birth <- ZA5320_birth %>% select(id,monat)

#preparing the data set: ZA5321_birth----------------------------------------------

ZA5321_birth <- ZA5321_birth %>% 
  select(lfdn, dmonat, emonat, fmonat, jmonat)

ZA5321_birth$monat <- ifelse(ZA5321_birth$dmonat < 
                               13, ZA5321_birth$dmonat,
                             ifelse(ZA5321_birth$emonat < 
                                      13, ZA5321_birth$emonat,
                                    ifelse(ZA5321_birth$fmonat <
                                             13, ZA5321_birth$fmonat,
                                           ifelse(ZA5321_birth$jmonat < 
                                                    13, ZA5321_birth$jmonat, NA))))

ZA5321_birth <- ZA5321_birth %>% select(lfdn,monat)

#preapring the data set: ZA5322_birth----------------------------------------------

ZA5322_birth <- ZA5322_birth %>% 
  select(lfdn, fmonat, gmonat, jmonat)

ZA5322_birth$monat <- ifelse(ZA5322_birth$fmonat < 
                               13, ZA5322_birth$fmonat,
                             ifelse(ZA5322_birth$gmonat < 
                                      13, ZA5322_birth$gmonat,
                                    ifelse(ZA5322_birth$jmonat < 
                                             13, ZA5322_birth$jmonat, NA)))

ZA5322_birth <- ZA5322_birth %>% select(lfdn,monat)

###################################################################################




###################################################################################

#joining the data set ZA5770 and 5770_birth----------------------------------------

ZA5770_full <- left_join(ZA5770, ZA5770_birth, by = "lfdn", na_matches = "never")

#joining month and year------------------------------------------------------------

ZA5770_full$jahr <- as_factor(ZA5770_full$jahr)

ZA5770_full <- ZA5770_full %>% unite(month_year, monat, jahr, sep = "-") %>% 
  mutate(month_year = as.yearmon(month_year, "%m-%Y")) %>% 
  mutate(month_year = as.Date(month_year)) %>% 
  select(lfdn, month_year, PID_2013, PID_2017)

ZA5770_full$PID_2013 <- as.numeric(ZA5770_full$PID_2013)
ZA5770_full$PID_2017 <- as.numeric(ZA5770_full$PID_2017)

#joining data set ZA5320 and ZA5320_birth------------------------------------------

ZA5320_birth <- ZA5320_birth %>% rename(lfdn = id)
ZA5320_full <- left_join(ZA5320, ZA5320_birth, by = "lfdn", na_matches = "never")

#joining month and year------------------------------------------------------------

ZA5320_full$jahr <- as_factor(ZA5320_full$cjahr)

ZA5320_full <- ZA5320_full %>% unite(month_year, monat, cjahr, sep = "-") %>% 
  mutate(month_year = as.yearmon(month_year, "%m-%Y")) %>% 
  mutate(month_year = as.Date(month_year)) %>% 
  select(-jahr)

ZA5320_full$PID_2002 <- as.numeric(ZA5320_full$PID_2002)
ZA5320_full$PID_2005 <- as.numeric(ZA5320_full$PID_2005)
ZA5320_full$PID_2009 <- as.numeric(ZA5320_full$PID_2009)

#joining data set ZA5321 and ZA5321_birth------------------------------------------

ZA5321_full <- left_join(ZA5321, ZA5321_birth, by = "lfdn", na_matches = "never")

#joining month and year------------------------------------------------------------

ZA5321_full$jahr <- as_factor(ZA5321_full$jahr)

ZA5321_full <- ZA5321_full %>% unite(month_year, monat, jahr, sep = "-") %>% 
  mutate(month_year = as.yearmon(month_year, "%m-%Y")) %>% 
  mutate(month_year = as.Date(month_year))

ZA5321_full$PID_2005 <- as.numeric(ZA5321_full$PID_2005)
ZA5321_full$PID_2007 <- as.numeric(ZA5321_full$PID_2007)
ZA5321_full$PID_2009 <- as.numeric(ZA5321_full$PID_2009)
ZA5321_full$PID_2011 <- as.numeric(ZA5321_full$PID_2011)
ZA5321_full$PID_2013 <- as.numeric(ZA5321_full$PID_2013)

#joining ZA5322 and ZA5322_birth---------------------------------------------------

ZA5322_full <- left_join(ZA5322, ZA5322_birth, by = "lfdn", na_matches = "never")

#joining month and year------------------------------------------------------------

ZA5322_full$jahr <- as_factor(ZA5322_full$jahr)

ZA5322_full <- ZA5322_full %>% unite(month_year, monat, jahr, sep = "-") %>% 
  mutate(month_year = as.yearmon(month_year, "%m-%Y")) %>% 
  mutate(month_year = as.Date(month_year))

ZA5322_full$PID_2011 <- as.numeric(ZA5322_full$PID_2011)
ZA5322_full$PID_2013 <- as.numeric(ZA5322_full$PID_2013)

###################################################################################




###################################################################################

#Saving the data set with merged data ZA5770

save(ZA5770_full, file = "data/ZA5770_full.RData")

#Saving the data set with merged data ZA5320

save(ZA5320_full, file = "data/ZA5320_full.RData")

#Saving the data set with merged data ZA5321

save(ZA5321_full, file = "data/ZA5321_full.RData")

#Saving the data set with merged data ZA5322

save(ZA5322_full, file = "data/ZA5322_full.RData")

###################################################################################
