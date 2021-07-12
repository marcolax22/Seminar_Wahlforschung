#loading packages------------------------------------------------------------------

library(broom)
library(rdrobust)
library(modelsummary)
library(rddensity)

#import data set-------------------------------------------------------------------

load("data/ZA5770_full.RData")

#RDD Design------------------------------------------------------------------------

dates_vline <- as.Date(c("1997-09-01"))           
dates_vline <- which(ZA5770_full$month_year %in% dates_vline)

test <- ggplot(ZA5770_full, mapping = aes(x=month_year, y=PID_2013)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth()

test + scale_x_date(date_labels = "%m-%Y") + geom_vline(xintercept = as.numeric(ZA5770_full$month_year[dates_vline]),
             col = "black", lwd = 2)
