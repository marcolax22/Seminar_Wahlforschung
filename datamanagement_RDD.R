#loading all packages--------------------------------------------------------------

library(tidyverse)
library(haven)
library(naniar)
library(lubridate)
library(zoo)

###################################################################################

load("data/ZA5770_full.RData")
load("data/ZA5320_full.RData")
load("data/ZA5321_full.RData")
load("data/ZA5322_full.RData")

###################################################################################




###################################################################################

#dataset for PID_2013--------------------------------------------------------------

ZA5770_RDD <- ZA5770_full %>%
  select(lfdn, month_year, PID_2013) %>%
  mutate(
    PID = case_when(
      PID_2013 == 1 ~ "5",
      PID_2013 == 2 ~ "4",
      PID_2013 == 3 ~ "3",
      PID_2013 == 4 ~ "2",
      PID_2013 == 5 ~ "1",
      PID_2013 == -97 ~ "0"))

ZA5770_RDD <- ZA5770_RDD %>% filter(month_year >= "1991-01-01" & month_year <= "1991-12-01") %>% 
  select(lfdn, month_year, PID) %>% 
  add_column(Wahljahr = 2009)
ZA5770_RDD <- ZA5770_RDD %>% filter(is.na(PID)==FALSE)

#dataset for PID_2017--------------------------------------------------------------

ZA5770_RDD2 <- ZA5770_full %>%
  select(lfdn, month_year, PID_2017) %>%
  mutate(
    PID = case_when(
      PID_2017 == 1 ~ "5",
      PID_2017 == 2 ~ "4",
      PID_2017 == 3 ~ "3",
      PID_2017 == 4 ~ "2",
      PID_2017 == 5 ~ "1",
      PID_2017 == -97 ~ "0"))  

ZA5770_RDD2 <- ZA5770_RDD2 %>% filter(month_year >= "1995-01-01" & month_year <= "1995-12-01") %>% 
  select(lfdn, month_year, PID) %>% 
  add_column(Wahljahr = 2013)
ZA5770_RDD2 <- ZA5770_RDD2 %>% filter(is.na(PID)==FALSE)
  
###################################################################################




###################################################################################

#dataset for RDD PID_2002----------------------------------------------------------

ZA5320_RDD <- ZA5320_full %>%
  select(lfdn, month_year, PID_2002) %>%
  mutate(
    PID = case_when(
      PID_2002 == 1 ~ "5",
      PID_2002 == 2 ~ "4",
      PID_2002 == 3 ~ "3",
      PID_2002 == 4 ~ "2",
      PID_2002 == 5 ~ "1",
      PID_2002 == 100 ~ "0"))  

ZA5320_RDD <- ZA5320_RDD %>% filter(month_year >= "1980-01-01" & month_year <= "1980-12-01") %>% 
  select(lfdn, month_year, PID) %>% 
  add_column(Wahljahr = 1998)
ZA5320_RDD <- ZA5320_RDD %>% filter(is.na(PID)==FALSE)

#dataset for RDD PID_2005----------------------------------------------------------

ZA5320_RDD2 <- ZA5321_full %>%
  select(lfdn, month_year, PID_2005) %>%
  mutate(
    PID = case_when(
      PID_2005 == 1 ~ "5",
      PID_2005 == 2 ~ "4",
      PID_2005 == 3 ~ "3",
      PID_2005 == 4 ~ "2",
      PID_2005 == 5 ~ "1",
      PID_2005 == 100 ~ "0")) 

ZA5320_RDD2 <- ZA5320_RDD2 %>% filter(month_year >= "1984-01-01" & month_year <= "1984-12-01") %>% 
  select(lfdn, month_year, PID) %>% 
  add_column(Wahljahr = 2002)
ZA5320_RDD2 <- ZA5320_RDD2 %>% filter(is.na(PID)==FALSE)

#dataset for RDD PID_2009----------------------------------------------------------

ZA5320_RDD3 <- ZA5320_full %>%
  select(lfdn, month_year, PID_2009) %>%
  mutate(
    PID = case_when(
      PID_2009 == 1 ~ "5",
      PID_2009 == 2 ~ "4",
      PID_2009 == 3 ~ "3",
      PID_2009 == 4 ~ "2",
      PID_2009 == 5 ~ "1",
      PID_2009 == 100 ~ "0")) 

ZA5320_RDD3 <- ZA5320_RDD3 %>% filter(month_year >= "1987-01-01" & month_year <= "1987-12-01") %>% 
  select(lfdn, month_year, PID) %>% 
  add_column(Wahljahr = 2005)
ZA5320_RDD3 <- ZA5320_RDD3 %>% filter(is.na(PID)==FALSE)

###################################################################################




###################################################################################

#dataset for RDD PID_2005----------------------------------------------------------

ZA5321_RDD <- ZA5321_full %>%
  select(lfdn, month_year, PID_2005) %>%
  mutate(
    PID = case_when(
      PID_2005 == 1 ~ "5",
      PID_2005 == 2 ~ "4",
      PID_2005 == 3 ~ "3",
      PID_2005 == 4 ~ "2",
      PID_2005 == 5 ~ "1",
      PID_2005 == 100 ~ "0"))

ZA5321_RDD <- ZA5321_RDD %>% filter(month_year >= "1984-01-01" & month_year <= "1984-12-01") %>% 
  select(lfdn, month_year, PID) %>% 
  add_column(Wahljahr = 2002)
ZA5321_RDD <- ZA5321_RDD %>% filter(is.na(PID)==FALSE)

#dataset for RDD PID_2007----------------------------------------------------------

ZA5321_RDD2 <- ZA5321_full %>%
  select(lfdn, month_year, PID_2007) %>%
  mutate(
    PID = case_when(
      PID_2007 == 1 ~ "1",
      PID_2007 == 2 ~ "2",
      PID_2007 == 3 ~ "3",
      PID_2007 == 4 ~ "4",
      PID_2007 == 5 ~ "5",
      PID_2007 == 100 ~ "0"))

ZA5321_RDD2 <- ZA5321_RDD2 %>% filter(month_year >= "1987-01-01" & month_year <= "1987-12-01") %>% 
  select(lfdn, month_year, PID) %>% 
  add_column(Wahljahr = 2005)
ZA5321_RDD2 <- ZA5321_RDD2 %>% filter(is.na(PID)==FALSE)

#dataset for RDD PID_2009----------------------------------------------------------

ZA5321_RDD3 <- ZA5321_full %>%
  select(lfdn, month_year, PID_2009) %>%
  mutate(
    PID = case_when(
      PID_2009 == 1 ~ "5",
      PID_2009 == 2 ~ "4",
      PID_2009 == 3 ~ "3",
      PID_2009 == 4 ~ "2",
      PID_2009 == 5 ~ "1",
      PID_2009 == 100 ~ "0"))

ZA5321_RDD3 <- ZA5321_RDD3 %>% filter(month_year >= "1987-01-01" & month_year <= "1987-12-01") %>% 
  select(lfdn, month_year, PID) %>% 
  add_column(Wahljahr = 2005)
ZA5321_RDD3 <- ZA5321_RDD3 %>% filter(is.na(PID)==FALSE)

#dataset for RDD PID_2013----------------------------------------------------------

ZA5321_RDD4 <- ZA5321_full %>%
  select(lfdn, month_year, PID_2013) %>%
  mutate(
    PID = case_when(
      PID_2013 == 1 ~ "5",
      PID_2013 == 2 ~ "4",
      PID_2013 == 3 ~ "3",
      PID_2013 == 4 ~ "2",
      PID_2013 == 5 ~ "1",
      PID_2013 == 100 ~ "0"))

ZA5321_RDD4 <- ZA5321_RDD4 %>% filter(month_year >= "1991-01-01" & month_year <= "1991-12-01") %>% 
  select(lfdn, month_year, PID) %>% 
  add_column(Wahljahr = 2009)
ZA5321_RDD4 <- ZA5321_RDD4 %>% filter(is.na(PID)==FALSE)

###################################################################################




###################################################################################

#dataset for RDD PID_2011----------------------------------------------------------

ZA5322_RDD <- ZA5322_full %>%
  select(lfdn, month_year, PID_2011) %>%
  mutate(
    PID = case_when(
      PID_2011 == 1 ~ "5",
      PID_2011 == 2 ~ "4",
      PID_2011 == 3 ~ "3",
      PID_2011 == 4 ~ "2",
      PID_2011 == 5 ~ "1",
      PID_2011 == 97 ~ "0"))

ZA5322_RDD <- ZA5322_RDD %>% filter(month_year >= "1991-01-01" & month_year <= "1991-12-01") %>% 
  select(lfdn, month_year, PID) %>% 
  add_column(Wahljahr = 2009)
ZA5322_RDD <- ZA5322_RDD %>% filter(is.na(PID)==FALSE)

#dataset for RDD PID_2013----------------------------------------------------------

ZA5322_RDD2 <- ZA5322_full %>%
  select(lfdn, month_year, PID_2013) %>%
  mutate(
    PID = case_when(
      PID_2013 == 1 ~ "5",
      PID_2013 == 2 ~ "4",
      PID_2013 == 3 ~ "3",
      PID_2013 == 4 ~ "2",
      PID_2013 == 5 ~ "1",
      PID_2013 == 100 ~ "0"))

ZA5322_RDD2 <- ZA5322_RDD2 %>% filter(month_year >= "1991-01-01" & month_year <= "1991-12-01") %>% 
  select(lfdn, month_year, PID) %>% 
  add_column(Wahljahr = 2009)
ZA5322_RDD2 <- ZA5322_RDD2 %>% filter(is.na(PID)==FALSE)

###################################################################################




###################################################################################

#Joining the different datasets

RDD_PID <- bind_rows(ZA5770_RDD, ZA5770_RDD2, ZA5320_RDD, ZA5320_RDD2, ZA5320_RDD3,
                     ZA5321_RDD, ZA5321_RDD2, ZA5321_RDD3, ZA5321_RDD3, ZA5321_RDD4) %>% 
  mutate(month = format(as.Date(month_year), "%m")) %>% 
  select(-month_year)

RDD_PID$month <- as.numeric(RDD_PID$month)

#Datasets with different Election years missing for robustness check

RDD_PID_x1998 <- RDD_PID %>% filter(Wahljahr != 1998)

RDD_PID_x2002 <- RDD_PID %>% filter(Wahljahr != 2002)

RDD_PID_x2005 <- RDD_PID %>% filter(Wahljahr != 2005)

RDD_PID_x2009 <- RDD_PID %>% filter(Wahljahr != 2009)

RDD_PID_x2013 <- RDD_PID %>% filter(Wahljahr != 2013)


###################################################################################




###################################################################################

#Saving the data set

save(RDD_PID, file = "data/RDD_PID.RData")
save(RDD_PID_x1998, file = "data/RDD_PID_x1998.RData")
save(RDD_PID_x2002, file = "data/RDD_PID_x2002.RData")
save(RDD_PID_x2005, file = "data/RDD_PID_x2005.RData")
save(RDD_PID_x2009, file = "data/RDD_PID_x2009.RData")
save(RDD_PID_x2013, file = "data/RDD_PID_x2013.RData")

###################################################################################

