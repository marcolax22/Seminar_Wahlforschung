#loading packages------------------------------------------------------------------

library(tidyverse)
library(rdd)
library(Hmisc)
library(rddensity)

###################################################################################

load("data/RDD_PID.RData")
load("data/RDD_PID_x1998.RData")
load("data/RDD_PID_x2002.RData")
load("data/RDD_PID_x2005.RData")
load("data/RDD_PID_x2009.RData")
load("data/RDD_PID_x2013.RData")

###################################################################################




###################################################################################

#RDD Design------------------------------------------------------------------------

RDD1 <- RDestimate(PID ~ month, data = RDD_PID, bw= 1100, cutpoint = 09)
RDD <- summary(RDD1)

plot(RDD1)
abline(v = 09)

#Robustness Check------------------------------------------------------------------------

#x1998
RDD2 <- RDestimate(PID ~ month, data = RDD_PID_x1998, bw= 1100, cutpoint = 09)
RDD <- summary(RDD2)

plot(RDD2)
abline(v = 09)

#x2002
RDD3 <- RDestimate(PID ~ month, data = RDD_PID_x2002, bw= 1100, cutpoint = 09)
RDD <- summary(RDD3)

plot(RDD3)
abline(v = 09)

#x2005
RDD4 <- RDestimate(PID ~ month, data = RDD_PID_x2005, bw= 1100, cutpoint = 09)
RDD <- summary(RDD4)

plot(RDD4)
abline(v = 09)

#x2009
RDD5 <- RDestimate(PID ~ month, data = RDD_PID_x2009, bw= 1100, cutpoint = 09)
RDD <- summary(RDD5)

plot(RDD5)
abline(v = 09)

#x2013
RDD6 <- RDestimate(PID ~ month, data = RDD_PID_x2013, bw= 1100, cutpoint = 09)
RDD <- summary(RDD6)

plot(RDD6)
abline(v = 09)

###################################################################################




###################################################################################

#Regression------------------------------------------------------------------







###################################################################################
