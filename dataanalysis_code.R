#loading packages------------------------------------------------------------------

library(tidyverse)
library(rdd)
library(Hmisc)
library(stargazer)
library(data.table)

###################################################################################

load("data/RDD_PID.RData")
load("data/RDD_PID_x1998.RData")
load("data/RDD_PID_x2002.RData")
load("data/RDD_PID_x2005.RData")
load("data/RDD_PID_x2009.RData")
load("data/RDD_PID_x2013.RData")

###################################################################################




###################################################################################

#Calculating the bandwith----------------------------------------------------------

IKbandwidth(RDD_PID$month, RDD_PID$PID, cutpoint = 9, verbose = FALSE, kernel = "triangular")

#RDD Design------------------------------------------------------------------------

RDD1 <- RDestimate(PID ~ month, data = RDD_PID, bw= 4.69825, cutpoint = 9)
RDD_1 <- summary(RDD1)

plot(RDD1)
abline(v = 09)

#Robustness Check------------------------------------------------------------------

#x1998
#data to small for calculating bandwith, we now use the same as in our full RDD model

RDD2 <- RDestimate(PID ~ month, data = RDD_PID_x1998, bw= 4.69825, cutpoint = 9)
RDD_2 <- summary(RDD2)

plot(RDD2)
abline(v = 09)

#x2002
IKbandwidth(RDD_PID_x2002$month, RDD_PID_x2002$PID, cutpoint = 9, verbose = FALSE, kernel = "triangular")

RDD3 <- RDestimate(PID ~ month, data = RDD_PID_x2002, bw= 3.273045, cutpoint = 9)
RDD_3 <- summary(RDD3)

plot(RDD3)
abline(v = 09)

#x2005
IKbandwidth(RDD_PID_x2005$month, RDD_PID_x2005$PID, cutpoint = 9, verbose = FALSE, kernel = "triangular")

RDD4 <- RDestimate(PID ~ month, data = RDD_PID_x2005, bw= 3.090531, cutpoint = 9)
RDD_4 <- summary(RDD4)

plot(RDD4)
abline(v = 09)

#x2009
IKbandwidth(RDD_PID_x2009$month, RDD_PID_x2009$PID, cutpoint = 9, verbose = FALSE, kernel = "triangular")

RDD5 <- RDestimate(PID ~ month, data = RDD_PID_x2009, bw= 4.760281, cutpoint = 9)
RDD_5 <- summary(RDD5)

plot(RDD5)
abline(v = 09)

#x2013
IKbandwidth(RDD_PID_x2013$month, RDD_PID_x2013$PID, cutpoint = 9, verbose = FALSE, kernel = "triangular")

RDD6 <- RDestimate(PID ~ month, data = RDD_PID_x2013, bw= 3.732385, cutpoint = 9)
RDD_6 <- summary(RDD6)

plot(RDD6)
abline(v = 09)

###################################################################################





###################################################################################

#Tables----------------------------------------------------------------------------

table <- bind_rows(RDD1$est, RDD2$est, RDD3$est, RDD4$est, RDD5$est, RDD6$est)

text <- c("Vollständiges RDD", "RDD ohne Wahljahr 1998", "RDD ohne Wahljahr 2002", 
          "RDD ohne Wahljahr 2005", "RDD ohne Wahljahr 2009", "RDD ohne Wahljahr 2013")

table$Model <- text
table <- table %>% select(Model, LATE, `Half-BW`, `Double-BW`)

setDT(table)  


stargazer(table, summary=F, title="Ergebnisse der Regression Discontinuity Designs", 
          align=T, digits=4, no.space=T, flip=F)

###################################################################################
