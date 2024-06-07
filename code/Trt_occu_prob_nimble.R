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

load("./all.spp_model.RData")
attach.nimble(mcmc.output.1$samples)

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
#hist(det.probs.inv)

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
png("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/Occupancy-2023/figures/Trt_occu_prob_nimble/Boxplot_trt_occu_prob_nimble.png")
boxplot(treatment_matrix[, match(desired.order, colnames(treatment_matrix))], 
        main = "Treatment Intercepts for All Species", 
        xlab = "Treatment", ylab = "Occupancy Probability",
        col = box.colors)
dev.off()
