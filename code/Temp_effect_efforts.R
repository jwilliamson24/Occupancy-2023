## -------------------------------------------------------------------------------------------------------
##
## Script name: all_spp_occ_nimble.R 
##
## Purpose of script: Occupancy model using nimble, all species, five treatments,
## Results in a plot of effect sizes for all five treatments
##
## Jasmine Williamson
## Date Created: 2023-11-21
##
## --------------------------------------------------------------------------------------------------------

rm(list=ls())
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/Occupancy-2023/data")
library(nimble)
library(ggplot2)
library(data.table)
library(tidyverse)
library(mcmcplots)
library(MCMCvis)
library(boot)
source('attach.nimble_v2.R')

## Load Data--------------------------------------------------------------------------------------------------


load("./all.spp_model.RData")
attach.nimble(mcmc.output.1$samples)

#  1 = BS     
#  2 = BU     
#  3 = HB     
#  4 = HU     
#  5 = UU     


# Inverse logit the detection intercept to get detection probabilities
det.probs.inv <- inv.logit(DetectionIntercept)


## Looking into Temp effect -----------------------------------------------------------------------------------------------
# need to predict across a range of temperature values to really see what's happening with these
betaTemp <- betaTemp[,-2]
betaTemp <- as.matrix(betaTemp)
#hist(betaTemp) 
#hist(betaTemp2)

# # Create vector of temp values for x-axis
# range(data$temp)
# temp.vect = c(30:85) 
# 
# # Scale values the same way as in model
# temp.vect.scaled = c(scale(temp.vect))
# 
# # Make empty matrix for ??? draws over 469 surveys?
# matrix <- matrix(NA, nrow=1500, ncol=56)
# 
# # Create count predictions for each temp
# for (j in 1:length(temp.vect.scaled)) {
#     det.prob <- DetectionIntercept + betaTemp*temp.vect.scaled[j] + betaTemp2*temp.vect.scaled[j]^2
#     
#     matrix[,j] <- inv.logit(det.prob)
# }
# medians <- sapply(1:ncol(matrix), function(i) median(matrix[,i]))
# CIs <- sapply(1:ncol(matrix), function(i) quantile(matrix[,i], c(0.025,0.975)))
# temp.pred <- data.frame(Temp = c(1:length(temp.vect.scaled)), Median = medians, L_CI = CIs[1, ], U_CI = CIs[2, ])
# 
# ggplot(temp.pred,aes(x = as.factor(Temp), y = Median)) +
#   geom_point() +
#   geom_ribbon(data=temp.pred, aes(x = Temp, ymin = L_CI, ymax = U_CI),inherit.aes=F,alpha=0.5)+
#   geom_line(data=temp.pred,aes(x=as.factor(Temp),y=Median)) +
#   labs(x = "Temp",
#        y = "Median Det Probability") + 
#   theme_classic()

#length(scaled_temp)
#range(scaled_temp)
x_vals <- seq(-2.1, 3, by = 0.01)

l.det.prob <- mean(DetectionIntercept) + mean(betaTemp)*x_vals + mean(betaTemp2)*x_vals^2
det.prob <- inv.logit(l.det.prob)
mean_temp <- data$temp |> mean()
sd_temp <- data$temp |> sd()

temp_vals <- x_vals*sd_temp + mean_temp

plot(l.det.prob ~ temp_vals) #doesnt work
