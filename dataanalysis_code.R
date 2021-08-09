#loading packages------------------------------------------------------------------

library(tidyverse)
#library(broom)
library(rdrobust)
#library(modelsummary)
#library(rddensity)

#import data set-------------------------------------------------------------------

load("data/ZA_RDD.RData")
load("data/ZA_RDD2.RData")

#RDD Design------------------------------------------------------------------------

dates_vline <- as.Date(c("1995-09-01"))           
dates_vline <- which(ZA_RDD$month_year %in% dates_vline)
  
#Graphic of the RDD Design----------------------------------------------------------
test <- ggplot(ZA_RDD, mapping = aes(x=month_year, y=PID_2013)) +
        geom_point(size = 0.75, alpha = 0.5) +
        geom_smooth(color = "black")

test + scale_x_date(date_labels = "%m-%Y") + 
  geom_vline(xintercept = as.numeric(ZA_RDD$month_year[dates_vline]), col = "black", lwd = 1) +
  labs(x = "Date of Birth", y = "Party Identification") +
  theme_classic()
 
#RDD Design 2------------------------------------------------------------------------

dates_vline <- as.Date(c("1991-09-01"))           
dates_vline <- which(ZA_RDD2$month_year %in% dates_vline)

#Graphic of the RDD Design 2----------------------------------------------------------
test <- ggplot(ZA_RDD2, mapping = aes(x=month_year, y=PID_2013)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(color = "black")

test + scale_x_date(date_labels = "%m-%Y") + 
  geom_vline(xintercept = as.numeric(ZA_RDD2$month_year[dates_vline]), col = "black", lwd = 1) +
  labs(x = "Date of Birth", y = "Party Identification") +
  theme_classic()

#Regression-------------------------------------------------------------------------

reg <- rdrobust::rdbwselect(y = ZA_RDD$PID_2013,
                            x = ZA_RDD$month_year,
                            c = ZA_RDD$Unter_18,
                            all = TRUE)
