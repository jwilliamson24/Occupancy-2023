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

## Load Data--------------------------------------------------------------------------------------------------

data <- read.csv("sitecovs_obs_long.csv")
data$site <- as.numeric(data$site)
data$all.obs <- as.numeric(data$all.obs)
data$oss.obs <- as.numeric(data$oss.obs)
data$enes.obs <- as.numeric(data$enes.obs)

data2 <- read.csv("site_treatments.csv")
data2$site <- as.numeric(data2$site)
data2$treatment <- as.numeric(factor(data2$treatment))
table(data2$treatment)

#  1 = BS     
#  2 = BU     
#  3 = HB     
#  4 = HU     
#  5 = UU     

scaled_temp <- c(scale(data$temp))

## Build Model------------------------------------------------------------------------------------------------

all.spp.model.1 <- nimbleCode ({
  
  # Priors

  for(t in 1:n.treatments){
    TreatmentIntercept[t] ~ dunif(-10,10)
  }#t
  
  DetectionIntercept ~ dunif(-5,5)
  betaTemp ~ dunif(-5, 5)
  betaTemp2 ~ dunif(-5, 0)
  
  # Likelihood
  
  # Process/Biological model = Occupancy
  # need two pieces: one defining psi and covariates, and one defining z dist
  for(i in 1:n.sites) {
      logit(psi[i]) <- TreatmentIntercept[treatment[i]]  #psi=occupancy probability
      z[i] ~ dbern(psi[i])  # z=1 if occupied, z=latent true occupancy
  }#i
  
  # Observation model = Detection
  # need two pieces: one for det coeff and one defining Y distribution
  for(j in 1:n.obs) {
      logit(p[j]) <- DetectionIntercept + betaTemp*temp[j] + betaTemp2*temp[j]^2 
      #p=detection probability for site i and survey j
      Y[j] ~ dbern(p[j] * z[site[j]]) #Y=my actual data observations
      #z=1 or 0, turns this on or off
  }#j

})

# Parameters monitored
parameters <- c("z","p","TreatmentIntercept","DetectionIntercept","betaTemp", "betaTemp2")

# MCMC Settings
ni <- 40000
nt <- 40
nb <- 20000
nc <- 3

# Data
nimble.data = list(Y=data$all.obs,
                   temp=scaled_temp)

nimble.constants = list(n.sites = length(unique(data$site)),
                        n.treatments = length(unique(data2$treatment)),
                        treatment=data2$treatment,
                        site=as.numeric(as.factor(data$site)),
                        n.obs = length(data$all.obs))

mcmc.output.1 <- nimbleMCMC(code = all.spp.model.1,
                          data = nimble.data,
                          constants=nimble.constants,
                          monitors = parameters,
                          niter = ni,
                          nburnin = nb,
                          nchains = nc,
                          thin=nt,
                          summary=TRUE,
                          samplesAsCodaMCMC = TRUE)

attach.nimble(mcmc.output.1$samples)
save(mcmc.output.1, file="./all.spp_model.RData")
load("./all.spp_model.RData")


## Assessing Convergence------------------------------------------------------------------------------------------------------
#mcmcplot(mcmc.output.1$samples)

# Gelman-Rubin diagnostic (AKA RHat or PSRF)
z <- mcmc.output.1$samples
g <- matrix(NA, nrow=nvar(z), ncol=2)
for (v in 1:nvar(z)) { g[v,] <- gelman.diag(z[,v])$psrf }
PSRF <- bind_cols(colnames(z$chain1),g) %>% rename(Parameter = ...1 ,PSRF = ...2 ,PSRFUpperCI = ...3) 

PSRF # Values are below 1.05, so that's good


## Interpreting Model Outputs----------------------------------------------------------------------------------------------------
#z

# Inverse logit the detection intercept to get detection probabilities
det.probs.inv <- inv.logit(DetectionIntercept)
hist(det.probs.inv)

# Looking at trace plots and parameter estimates
MCMCtrace(object = mcmc.output.1$samples,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = c("DetectionIntercept", "betaTemp", "TreatmentIntercept"))

#mean(det.probs.inv) # = 0.3207378
#mean(det.probs.inv>0)  # = 1
#median(det.probs.inv)  # = 0.3198026
boxplot(det.probs.inv)

# Inv logit TreatmentIntercept to get Occupancy Estimates
trt.int.inv <- inv.logit(TreatmentIntercept)
#median(trt.int.inv[,1]) # 0.513384    BS
#median(trt.int.inv[,2]) # 0.9684722    BU
#median(trt.int.inv[,3]) # 0.7607153    HB
#median(trt.int.inv[,4]) # 0.4214717    HU
#median(trt.int.inv[,5]) # 0.9977257    UU


## Plotting Model Outputs-------------------------------------------------------------------------------------------------------


## Boxplot for Treatment Effects-----------------------------------------------------------------------------------------------

# Renaming and reordering the treatment intercepts for the boxplot
# treatment_matrix <- TreatmentIntercept
treatment_matrix <- trt.int.inv # Using the inv logit treatment estimates

new.names <- c("Salvage Logged", "Wildfire", "Harvest, Wildfire", "Harvest", "Control")
colnames(treatment_matrix) <- new.names
desired.order <- c("Control", "Wildfire", "Harvest, Wildfire", "Harvest", "Salvage Logged")

box.colors <- c('lightgreen','steelblue', 'coral2', '#f9d62e', '#b967ff' )

# Boxplot of Treatment Estimates
boxplot(treatment_matrix[, match(desired.order, colnames(treatment_matrix))], 
        main = "Treatment Intercepts for All Species", 
        xlab = "Treatment", ylab = "Occupancy Probability",
        col = box.colors)

# hist(TreatmentIntercept[,5])
# hist(TreatmentIntercept[,3])


## Looking into Temp effect -----------------------------------------------------------------------------------------------
# need to predict across a range of temperature values to really see what's happening with these
betaTemp <- betaTemp[,-2]
betaTemp <- as.matrix(betaTemp)
hist(betaTemp) 
hist(betaTemp2)

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

length(scaled_temp)
range(scaled_temp)
x_vals <- seq(-2.1, 3, by = 0.01)

l.det.prob <- mean(DetectionIntercept) + mean(betaTemp)*x_vals + mean(betaTemp2)*x_vals^2
det.prob <- inv.logit(l.det.prob)
mean_temp <- data$temp |> mean()
sd_temp <- data$temp |> sd()

temp_vals <- x_vals*sd_temp + mean_temp

plot(l.det.prob ~ temp_vals) #doesnt work

# Difference in Treatment and Control Effects   ------------------------------------------------------------
# View(TreatmentIntercept)
# diff_BS <- TreatmentIntercept[,5]-TreatmentIntercept[,1]
# diff_BU <- TreatmentIntercept[,5]-TreatmentIntercept[,2]
# diff_HB <- TreatmentIntercept[,5]-TreatmentIntercept[,3]
# diff_HU <- TreatmentIntercept[,5]-TreatmentIntercept[,4]
# diff_UU <- TreatmentIntercept[,5]-TreatmentIntercept[,5]
# 
# TreatmentIntercept_new <- cbind(diff_BS, diff_BU, diff_HB, diff_HU, diff_UU)
# View(TreatmentIntercept_new)
# 
# mean(diff_BS>0) # 0.9926667
# mean(diff_BU>0) # 0.714
# mean(diff_HB>0) # 0.9426667
# mean(diff_HU>0) # 1
# 
# hist(diff_BS)
# hist(diff_BU)
# hist(diff_HB)
# hist(diff_HU)

 box.colors.1 <- c('#b967ff','steelblue', 'coral2', '#f9d62e','lightgreen' )
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
view(trt.diff.new)

new.names <- c("Salvage Logged", "Wildfire", "Harvest, Wildfire", "Harvest", "Control")
colnames(trt.diff.new) <- new.names
desired.order <- c("Control", "Wildfire", "Harvest, Wildfire", "Harvest", "Salvage Logged")

# Boxplot showing negative effect sizes of each treatment
boxplot(trt.diff.new [, match(desired.order, colnames(trt.diff.new))], 
        main= "Effect Size by Treatment",
        xlab = "Treatment",
        ylab = "Effect Size",
        col = box.colors.1)

