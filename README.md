# Occupancy-2023

This repo houses occupancy analyses using only 2023 field season data. This includes initial exploratory figures, count data, covariate effects, package Unmarked analysis, and Nimble models that produce occupancy estimates and treatment effect sizes.

## Project description

The goal of this analysis is to assess the differences in occupancy estimates for two salamander species (*Batrachoseps wrighti*, *Ensatina eschscholtzii*) across five treatment categories over one season (spring 2023).

Code | Treatment | # Sites 2023
-----|-----------|------------------
UU | Unharvested, unburned | 13
BU | Burned, unharvested | 14
HU | Harvested, unburned | 15
HB | Harvested, burned | 16
BS | Burned, salvage logged | 9


## Code Files
#### [*Counts_figures_2023*](C:\Users\jasmi\OneDrive\Documents\Occupancy-2023\code\Counts_figures_2023.R): standardized barplot of counts
#### [*Pleth_conf_exploratory*](C:\Users\jasmi\OneDrive\Documents\Occupancy-2023\code\Pleth_conf_exploratory.R): exploratory bar and boxplots used for 2023 Pleth Conference, unstandardized
#### [*Temp_effect_efforts*](C:\Users\jasmi\OneDrive\Documents\Occupancy-2023\code\Temp_effect_efforts.R): efforts to explore temp effect from nimble model, in progress
#### [*Trt_effect_size_nimble*](C:\Users\jasmi\OneDrive\Documents\Occupancy-2023\code\Trt_effect_size_nimble.R): plot that shows effect sizes of treatments using nimble model from trt_occu_prob_nimble
#### [*Trt_occu_prob_nimble*](C:\Users\jasmi\OneDrive\Documents\Occupancy-2023\code\Trt_occu_prob_nimble.R): custom bayesian Nimble code for all spp model resulting in plot that shows occupancy probability estimates of treatments
#### [*Unmarked_occu_TWS_2023*](C:\Users\jasmi\OneDrive\Documents\Occupancy-2023\code\Unmarked_occu_TWS_2023.R): initial occupancy analysis efforts for both spp using unmarked package, presented at TWS 2023

## Modeling

The current nimble model (Jan 2024) uses data for both species according to the following equation:


## Key Takeaways

Standardized counts of salamanders in 2023 by treatment:
![counts figure](figures\Counts_figures_2023\barplot_counts_standardized.png)

The resulting occupancy estimates:

![All species occu estimates boxplot](figures\Trt_occu_prob_nimble\Boxplot_trt_occu_prob_nimble.png)

The resulting effect sizes:

![All species treatment effect sizes boxplot](figures\Trt_effect_size_nimble\Boxplor_trt_effect_size_nimble.png)
