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
setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Projects/OSS Project/Working Data")
library(nimble)
library(ggplot2)
library(data.table)
library(tidyverse)
library(mcmcplots)
library(MCMCvis)
library(boot)
source('attach.nimble_v2.R')

load("./all.spp_model.RData")
attach.nimble(mcmc.output.1$samples)

# Inv logit TreatmentIntercept to get Occupancy Estimates
trt.int.inv <- inv.logit(TreatmentIntercept)

treatment_matrix <- trt.int.inv # Using the inv logit treatment estimates


 box.colors.1 <- c('lightgreen','steelblue', 'coral2', '#f9d62e','#b967ff' )
# boxplot(TreatmentIntercept_new, 
#         main= "treatment effect size",
#         xlab = "trt difference from control",
#         ylab = "effect size",
#         col = box.colors.1)

# Build matrix of trt - control effects
diff.BS <- TreatmentIntercept[,1]-TreatmentIntercept[,5]
diff.BU <- TreatmentIntercept[,2]-TreatmentIntercept[,5]
diff.HB <- TreatmentIntercept[,3]-TreatmentIntercept[,5]
diff.HU <- TreatmentIntercept[,4]-TreatmentIntercept[,5]
diff.UU <- TreatmentIntercept[,5]-TreatmentIntercept[,5]
trt.diff.new <- cbind(diff.BS, diff.BU, diff.HB, diff.HU, diff.UU)

new.names <- c("Salvage Logged", "Wildfire", "Harvest, Wildfire", "Harvest", "Control")
colnames(trt.diff.new) <- new.names
desired.order <- c("Control", "Wildfire", "Harvest, Wildfire", "Harvest", "Salvage Logged")

# Boxplot showing negative effect sizes of each treatment
png("C:/Users/jasmi/OneDrive/Documents/Occupancy-2023/figures/Trt_effect_size_nimble/Boxplot_trt_effect.png")
boxplot(trt.diff.new [, match(desired.order, colnames(trt.diff.new))], 
        main= "Effect Size by Treatment",
        xlab = "Treatment",
        ylab = "Effect Size",
        col = box.colors.1)
dev.off()
