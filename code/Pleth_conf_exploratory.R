
# Plethodontid Biologist Conference Figures - Exploratory 2023 Data Analysis


rm(list=ls())
library(ggplot2)
library(unmarked)
library(RColorBrewer)
library(tidyverse)
library(ggpattern)

setwd("~/Academic/OSU/Projects/OSS Project/Analysis Pleth Conf 2023")
counts <- read.csv("OSS_data_2023_counts.csv")
climate <- read.csv("OSS_data_2023_climate.csv")
treatmentcount <- read.csv("OSS_data_2023_treatmentcount.csv")
treatmentcount.species <- read.csv("treatmentcount.species.csv")
temp <- read.csv("OSS_data_2023_temp.csv")

# Reshape the data for count by species
reshaped_data <- treatmentcount.species %>%
  pivot_longer(cols = c(enes, oss), names_to = "species", values_to = "count")


# Barplot of counts by date

png("C:/Users/jasmi/OneDrive/Documents/Occupancy-2023/figures/Pleth_conf_exploratory/counts by date 2023.png")
barplot(counts$count,
        space=1,
        main="Counts by Date",
        ylab="Daily Count",
        xlab="month",
        col=terrain.colors
        (length(unique(counts$month)))
        [as.factor(counts$month)])
dev.off()

# Basic climate plots

ggplot(temp, aes(x = ID, y = Temp, group = 1)) +
        geom_point(color="blue") +
        geom_line(color="blue", linewidth=2) +
        theme_classic()+
        theme(
                panel.background = element_rect(fill='transparent'),
                plot.background = element_rect(fill='transparent', color=NA),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.background = element_rect(fill='transparent'),
                legend.box.background = element_rect(fill='transparent')
        )
ggsave("temp_line.png", temps, bg="transparent", 
  path = "C:/Users/jasmi/OneDrive/Documents/Occupancy-2023/figures/Pleth_conf_exploratory")



##------------------------------------------------------------------------------------------------------


#A bar for each spp, color fill by species
ggplot(reshaped_data, aes(x=treatment, y=count, fill=species)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('Salamander Counts by Treatment and Species') +
  xlab('Treatment') +
  ylab('Count') +
  scale_fill_manual('Species', values=c('coral2','steelblue'))
ggsave("counts by trt spp 2023.png", 
      path = "C:/Users/jasmi/OneDrive/Documents/Occupancy-2023/figures/Pleth_conf_exploratory")


#A single bar with enes and oss counts
ggplot(reshaped_data, aes(x=treatment, y=count, fill=treatment)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('Salamander Counts by Treatment and Species') +
  xlab('Treatment') +
  ylab('Count')



#The code below is to be used for plotting reference; these are not good/usable plots

# Boxplot by treatment
p1 <- ggplot(treatmentcount, aes(x=treatment, y=count, fill=treatment)) + 
        geom_boxplot(alpha=0.3) +
        theme(legend.position="none") +
        scale_fill_brewer(palette="Dark2")


# Boxplot by treatment and species
p2 <- ggplot(reshaped_data, aes(x = treatment, y = count, fill = species)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.3) +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "top")


# Create the boxplot with different colors for treatments and patterns for species
p3 <- ggplot(reshaped_data, aes(x = treatment, y = count, fill = treatment, linetype = species)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.3) +
  scale_fill_manual(values = scales::brewer_pal(palette = "Dark2")(n_distinct(reshaped_data$treatment))) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme(legend.position = "top")

#Color fill by treatment, pattern by species
ggplot(reshaped_data, aes(x=treatment, y=count, fill=treatment, pattern=species)) +
  geom_bar_pattern(stat='identity', position='dodge', pattern_fill="black") +
  scale_fill_manual(values = c('BU' = 'red', 'HU' = 'blue', 'UU' = 'green', 'BS' = 'purple', 'HB' = 'orange')) +
  scale_pattern_manual(values = c('oss' = 'stripe', 'enes' = 'none')) +
  ggtitle('Salamander Counts by Treatment and Species') +
  xlab('Treatment') +
  ylab('Count')

ggplot(reshaped_data, aes(x=treatment, y=count, fill=treatment, pattern=species)) +
  geom_bar_pattern(stat='identity', position='dodge', pattern_fill="black") +
  scale_fill_brewer(palette = "Accent") +
  ggtitle('Salamander Counts by Treatment and Species') +
  xlab('Treatment') +
  ylab('Count')

#Color fill by treatment, no patterns
ggplot(reshaped_data, aes(x=treatment, y=count, fill=treatment, pattern=species)) +
  geom_bar_pattern(aes(pattern_density = species), stat='identity', position=position_dodge(width = 0.7), 
                   pattern_fill="black", width = 0.8) +
  scale_fill_brewer(palette = "Accent") +
  scale_pattern_manual(values = c('oss' = 'none', 'enes' = 'none')) +
  scale_pattern_density_manual(values = c('oss' = 0.3, 'enes' = 1)) +
  ggtitle('Salamander Counts by Treatment and Species') +
  xlab('Treatment') +
  ylab('Count')



















