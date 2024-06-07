## -------------------------------------------------------------------------------------------------------
##
## Script name: Count_figures_2023.R 
##
## Purpose of script: Count Summary Figures made for TWS 2023
##
## Jasmine Williamson
## Date Created: 2023-11-02
##
## --------------------------------------------------------------------------------------------------------

rm(list=ls())
library(ggplot2)
library(unmarked)
library(RColorBrewer)
library(tidyverse)
library(ggpattern)

setwd("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/Occupancy-2023/Analysis Pleth Conf 2023")
treatmentcount.species <- read.csv("treatmentcount.species.csv")
sitecovs <- read.csv("sitecovs.csv", row.names = 1)
sitecovs$treatment <- factor(sitecovs$treatment, 
                             levels = c("UU", "BU", "HB", "HU", "BS"))

# Reshape the data for count by species
reshaped_data <- treatmentcount.species %>%
  pivot_longer(cols = c(enes, oss), names_to = "species", values_to = "count")
#head(reshaped_data)


# Standardize the counts by treatment sample size --------------------------------------------------------

# Aggregate to data frame with one row for each treatment
total_sal_counts <- aggregate(count ~ treatment + species, data = reshaped_data, FUN = sum)


trt_sample_size <- as.data.frame(table(sitecovs$treatment)) #Count # times each trt type was sampled
names(trt_sample_size) <- c("treatment", "sample.size")
trt_counts_merged <- merge(total_sal_counts, trt_sample_size, by = "treatment") #merge the two

# Standardize counts = total observed sal count / number of sites sampled in that treatment
trt_counts_merged$standardized.count <- trt_counts_merged$count / trt_counts_merged$sample.size
trt_counts_merged$treatment <- factor(trt_counts_merged$treatment, 
                             levels = c("UU", "BU", "HB", "HU", "BS"))


##------------------------------------------------------------------------------------------------------
# Bar plots of counts by treatment and species

# Single bar for each trt with total counts
p1 <- ggplot(reshaped_data, aes(x=treatment, y=count, fill=treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('Salamander Counts by Treatment and Species') +
  xlab('Treatment') +
  ylab('Count')

#A bar for each spp, color fill by species
p2 <- ggplot(reshaped_data, aes(x=treatment, y=count, fill=species)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('Salamander Counts by Treatment and Species') +
  xlab('Treatment') +
  ylab('Count') +
  scale_fill_manual('Species', values=c('coral2','steelblue'))


##------------------------------------------------------------------------------------------------------
# Standardized barplot per treatment 
png("C:/Users/jasmi/OneDrive/Documents/Academic/OSU/Git/Occupancy-2023/figures/Counts_figures_2023/barplot_standzd_counts.png")
ggplot(trt_counts_merged, aes(x=trt_counts_merged$treatment, y=standardized.count, fill=treatment, pattern=species)) +
  geom_bar(stat='identity', position='dodge', color="black") +
  ggtitle('Salamander Counts by Treatment and Species') +
  xlab('Treatment') +
  ylab('Count') +
  scale_fill_manual('Treatment', values=c('lightgreen','steelblue', 'coral2', '#f9d62e', '#b967ff' ))
dev.off()
