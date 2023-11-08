library(tidyverse)
set.seed(123)
playoff_data <- read.csv("202223nbaplayoffs.csv", header = T)
reg_data <- read.csv("202223regseasonnodupes.csv", header = T)

## SRS sampling
common_sample_indices <- sample(1:nrow(reg_data), 50)

sample_size <- 50
population_size <- 200

# SRS for Regular season
srs_sample_reg <- reg_data[common_sample_indices, ]
srs_est_reg <- mean(srs_sample_reg$PTS)
fpc <- 1 - sample_size / population_size
se_reg <- sqrt(var(srs_sample_reg$PTS)/sample_size * fpc)
quantile <- qnorm(0.95)
CI_reg <- c(srs_est_reg - quantile*se_reg, srs_est_reg + quantile*se_reg)


# SRS for Playoffs
srs_sample_playoffs <- playoff_data[common_sample_indices, ]
srs_est_playoffs <- mean(srs_sample_playoffs$PTS)
se_playoffs <- sqrt(var(srs_sample_playoffs$PTS)/sample_size * fpc)
CI_playoffs <- c(srs_est_playoffs - quantile*se_playoffs, srs_est_playoffs +
                   quantile*se_playoffs)

## Stratified Sampling for regular season:
stratum_sizes <- reg_data %>%
  group_by(Pos) %>%
  summarize(StratumSize = n())

within_strata_vars <- reg_data %>%
  group_by(Pos) %>%
  summarize(SD = sd(PTS))

## We see that Within-Strata Variance are varying so we use optimal allocation
## Here we assume the cost of sampling from each strata is the same

se_strata <- stratum_sizes$StratumSize * within_strata_vars$SD
samp_sizes_strat <- round(se_strata/sum(se_strata) * sample_size)

samp_sizes_strat[3] = 9

## We round the point guard strata size down to ensure the total sample size 
## is 50, although it should technically round up. 


final_samples <- data.frame()  # Initialize an empty dataframe to store the final samples

for (i in 1:5) {
  samp_size <- samp_sizes_strat[i]
  current_stratum <- unique(reg_data$Pos)[i]
  
  stratum_samples <- reg_data %>%
    group_by(Pos) %>%
    filter(Pos == current_stratum) %>%
    sample_n(samp_size)
  
  final_samples <- rbind(final_samples, stratum_samples)
}

## Check to make sure sampling is done correctly: 
check_stratsize <- final_samples %>%
  group_by(Pos) %>%
  summarize(Size = n())


## Estimating Stratified for Regular Season: 
N <- nrow(reg_data)
Nh_data <- reg_data %>% group_by(Pos) %>% summarize(n = n())
Nh <- Nh_data$n
pos_avg <- final_samples %>% group_by(Pos) %>% summarize(mean = mean(PTS))
pos_means <- pos_avg$mean

strat_est_reg <- sum((Nh/N)*pos_means)

pos_vars_dat <- final_samples %>% group_by(Pos) %>% summarize(Var = var(PTS))
pos_vars <- pos_vars_dat$Var

se_strat_reg <- sqrt(sum((Nh/N)^2 * (1-(samp_sizes_strat/Nh)) * 
                           (pos_vars/samp_sizes_strat)))
CI_strat_reg <- c(strat_est_reg - quantile*se_strat_reg, 
                  strat_est_reg + quantile*se_strat_reg)

## Getting stratified sample for Playoffs: 
strat_players <- final_samples$Player
playoffs_strat <- playoff_data %>% filter(Player %in% strat_players)

## Estimating stratified for playoffs: 
N <- nrow(playoff_data)
pos_avg_p <- playoffs_strat %>% group_by(Pos) %>% summarize(mean = mean(PTS))
pos_means_p <- pos_avg_p$mean

strat_est_playoff <- sum((Nh/N)*pos_means_p)

pos_vars_dat_p <- playoffs_strat %>% group_by(Pos) %>% summarize(Var = var(PTS))
pos_vars_p <- pos_vars_dat_p$Var

se_strat_p <- sqrt(sum((Nh/N)^2 * (1-(samp_sizes_strat/Nh)) * 
                           (pos_vars_p/samp_sizes_strat)))
CI_strat_reg <- c(strat_est_playoff - quantile*se_strat_p, 
                  strat_est_playoff + quantile*se_strat_p)

## Proportion Estimate for players who score more in playoffs than regular 
## season:

## SRS
merged_data <- merge(srs_sample_reg, srs_sample_playoffs, by = "Player", 
                     suffixes = c("_reg", "_playoff"))
prop_playoff_srs <- mean(merged_data$PTS_playoff > merged_data$PTS_reg)
se_prop_srs <- sqrt(fpc*((prop_playoff_srs* (1-prop_playoff_srs))/sample_size))
CI_prop_srs <- c(prop_playoff_srs - quantile*se_prop_srs, 
                 prop_playoff_srs + quantile*se_prop_srs)

## Stratified: 
merged_strat <- merge(final_samples, playoffs_strat, by = "Player", 
                      suffixes = c("_reg", "_playoff"))
merged_strat$Playoff_Higher <- ifelse(merged_strat$PTS_playoff > 
                                        merged_strat$PTS_reg, 1, 0)

props <- merged_strat %>% group_by(Pos_reg) %>%
  summarize(Prop = mean(Playoff_Higher))
strat_props <- props$Prop
strat_est_prop <- sum((Nh/N)*strat_props)

vars_prop_dat <- merged_strat %>% group_by(Pos_reg) %>%
  summarize(Var = var(Playoff_Higher))
vars_prop <- vars_prop_dat$Var

se_prop_strat <- sqrt(sum((Nh/N)^2 * (1-(samp_sizes_strat/Nh)) * 
                            (vars_prop/samp_sizes_strat)))
CI_prop_strat <- c(strat_est_prop - quantile*se_prop_strat, 
                   strat_est_prop + quantile*se_prop_strat)



