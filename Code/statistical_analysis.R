library(tidyverse)
library(nortest)
library(brms)
library(BayesFactor)
set.seed(246)

######### Re-analysis of original RH data #########
# normality test
rh_2023_dat <- read.csv('./../Data/RH2023_study2A.csv', header = T, stringsAsFactors = T) %>% 
  filter(generator == 'bingo') %>% 
  mutate(response = 'probability') %>% 
  dplyr::rename(subject = participant_id,
                repetition = prediction_recode,
                ground_truth = rate)
rh_2023_normality_test <- lillie.test(rh_2023_dat$repetition)

######### Experiments #########
dat <- read.csv('./../Data/IID_dat.csv', header = T, stringsAsFactors = T)

##### Experiments 1a: p = 0.5, IID, probability judgment #####
dat_prob_50 <- dat %>% 
  filter(response == 'probability' & ground_truth == 50) %>% 
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# normality test
prob_50_normality_test <- lillie.test(dat_prob_50$repetition)

# one-sample Wilcoxon signed-rank test
prob_50_rank_sum_test <- wilcox.test(dat_prob_50$repetition,
                                     mu = 50,
                                     alternative = 'two.sided',
                                     conf.int = T)
prob_50_Z <- qnorm(prob_50_rank_sum_test$p.value/2) # Z score, dividing by 2 because it's two-sided
prob_50_r <- abs(prob_50_Z) / sqrt(length(unique(dat_prob_50$subject))) # effect size

# Bayesian t-test
prob_50_bf <- ttestBF(dat_prob_50$repetition - dat_prob_50$ground_truth, mu = 0) # Bayes factor
prob_50_chains <- posterior(prob_50_bf, iterations = 10000)
summary(prob_50_chains) # get median and 95% credible interval of the posterior distribution for effect size

##### Experiments 1b: p = 0.5, IID, point prediction #####
dat_point_50 <- dat %>% 
  filter(response == 'point' & ground_truth == 50) %>% 
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# normality test
point_50_normality_test <- lillie.test(dat_point_50$repetition)

# one-sample Wilcoxon signed-rank test
point_50_rank_sum_test <- wilcox.test(dat_point_50$repetition,
                                      mu = 50,
                                      alternative = 'two.sided')
point_50_Z <- qnorm(point_50_rank_sum_test$p.value/2)
point_50_r <- abs(point_50_Z) / sqrt(length(unique(dat_point_50$subject)))

# Bayesian t-test
point_50_bf <- ttestBF(dat_point_50$repetition - dat_point_50$ground_truth, mu = 0)
point_50_chains <- posterior(point_50_bf, iterations = 10000)
summary(point_50_chains)

# Bayesian mixed effects regression model
dat_point_50_streak <- dat %>% 
  filter(response == 'point' & ground_truth == 50) %>% 
  group_by(subject, streak) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()
exp1_mdl <- brm(formula = repetition ~ 1 + I(streak^2) + streak + (1 + I(streak^2) + streak|subject),
                data = dat_point_50_streak,
                iter = 4000,
                seed = 1)
summary(exp1_mdl)

# comparing thresholding model to data
thresholding_prediction <- dat %>% 
  filter(response == 'probability') %>% 
  mutate(`Thresholding model` = case_when(repetition < ground_truth ~ 0, repetition > ground_truth ~ 100))
# when proportion of balls with terminal streak color equals ground truth, randomly predict the next color based on ground truth
thresholding_prediction$`Thresholding model`[is.na(thresholding_prediction$`Thresholding model`)] <- sapply(which(is.na(thresholding_prediction$`Thresholding model`)), 
                                                                                                            function(i) sample(c(0, 100), size = 1, prob = c(1 - thresholding_prediction$ground_truth[i]/100, thresholding_prediction$ground_truth[i]/100)))
thresholding_prediction <- thresholding_prediction %>% 
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(`Thresholding model`))

thresholding_ground_truth_50_bf <- ttestBF(dat_point_50$repetition, thresholding_prediction$repetition[thresholding_prediction$ground_truth == 50])
thresholding_ground_truth_50_chains <- posterior(thresholding_ground_truth_50_bf, iterations = 10000)
summary(thresholding_ground_truth_50_chains)

##### Experiments 2a: p = 0.6, IID, probability judgment #####
dat_prob_60 <- dat %>% 
  filter(response == 'probability' & ground_truth == 60) %>%  
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# mean and median
prob_60_mean <- mean(dat_prob_60$repetition)
prob_60_median <- median(dat_prob_60$repetition)

# normality test
prob_60_normality_test <- lillie.test(dat_prob_60$repetition)

# one-sample Wilcoxon signed-rank test
prob_60_rank_sum_test <- wilcox.test(dat_prob_60$repetition,
                                     mu = 60,
                                     alternative = 'two.sided',
                                     conf.int = T)
prob_60_Z <- qnorm(prob_60_rank_sum_test$p.value/2)
prob_60_r <- abs(prob_60_Z) / sqrt(length(unique(dat_prob_60$subject)))

# Bayesian t-test
prob_60_bf <- ttestBF(dat_prob_60$repetition - dat_prob_60$ground_truth, mu = 0)
prob_60_chains <- posterior(prob_60_bf, iterations = 10000)
summary(prob_60_chains)

##### Experiments 2a: p = 0.4, IID, probability judgment #####
dat_prob_40 <- dat %>% 
  filter(response == 'probability' & ground_truth == 40) %>%  
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# mean and median
prob_40_mean <- mean(dat_prob_40$repetition)
prob_40_median <- median(dat_prob_40$repetition)

# normality test
prob_40_normality_test <- lillie.test(dat_prob_40$repetition)

# one-sample Wilcoxon signed-rank test
prob_40_rank_sum_test <- wilcox.test(dat_prob_40$repetition,
                                     mu = 40,
                                     alternative = 'two.sided',
                                     conf.int = T)
prob_40_Z <- qnorm(prob_40_rank_sum_test$p.value/2)
prob_40_r <- abs(prob_40_Z) / sqrt(length(unique(dat_prob_40$subject)))

# Bayesian t-test
prob_40_bf <- ttestBF(dat_prob_40$repetition - dat_prob_40$ground_truth, mu = 0)
prob_40_chains <- posterior(prob_40_bf, iterations = 10000)
summary(prob_40_chains)

##### Experiments 2b: p = 0.6, IID, point prediction #####
dat_point_60 <- dat %>% 
  filter(response == 'point' & ground_truth == 60) %>%  
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# normality test
point_60_normality_test <- lillie.test(dat_point_60$repetition)

# one-sample Wilcoxon signed-rank test
point_60_rank_sum_test <- wilcox.test(dat_point_60$repetition,
                                      mu = 60,
                                      alternative = 'two.sided',
                                      conf.int = T)
point_60_Z <- qnorm(point_60_rank_sum_test$p.value/2)
point_60_r <- abs(point_60_Z) / sqrt(length(unique(dat_point_60$subject)))

# Bayesian t-test
point_60_bf <- ttestBF(dat_point_60$repetition - dat_point_60$ground_truth, mu = 0)
point_60_chains <- posterior(point_60_bf, iterations = 10000)
summary(point_60_chains)

##### Experiments 2b: p = 0.4, IID, point prediction #####
dat_point_40 <- dat %>% 
  filter(response == 'point' & ground_truth == 40) %>%  
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# normality test
point_40_normality_test <- lillie.test(dat_point_40$repetition)

# one-sample Wilcoxon signed-rank test
point_40_rank_sum_test <- wilcox.test(dat_point_40$repetition,
                                      mu = 40,
                                      alternative = 'two.sided',
                                      conf.int = T)
point_40_Z <- qnorm(point_40_rank_sum_test$p.value/2)
point_40_r <- abs(point_40_Z) / sqrt(length(unique(dat_point_40$subject)))

# Bayesian t-test
point_40_bf <- ttestBF(dat_point_40$repetition - dat_point_40$ground_truth, mu = 0)
point_40_chains <- posterior(point_40_bf, iterations = 10000)
summary(point_40_chains)

# comparing thresholding model to data (ground truth = 60)
thresholding_ground_truth_60_bf <- ttestBF(dat_point_60$repetition, thresholding_prediction$repetition[thresholding_prediction$ground_truth == 60])
thresholding_ground_truth_60_chains <- posterior(thresholding_ground_truth_60_bf, iterations = 10000)
summary(thresholding_ground_truth_60_chains)

# comparing thresholding model to data (ground truth = 40)
thresholding_ground_truth_40_bf <- ttestBF(dat_point_40$repetition, thresholding_prediction$repetition[thresholding_prediction$ground_truth == 40])
thresholding_ground_truth_40_chains <- posterior(thresholding_ground_truth_40_bf, iterations = 10000)
summary(thresholding_ground_truth_40_chains)

##### Experiment3: p = 0.5, replication of Rao & Hastie (2023), probability  judgment #####
rh_replication <- read.csv('./../Data/RH_replication_dat.csv', header = T, stringsAsFactors = T) %>% 
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# proportion of participants whose P(repeat) was less than 40%
paste0(round(length(which(rh_replication$repetition < 40)) / length(rh_replication$repetition) * 100, 2), '%')

# normality test
rh_replication_normality_test <- lillie.test(rh_replication$repetition)

# one-sample Wilcoxon signed-rank test
rh_replication_rank_sum_test <- wilcox.test(rh_replication$repetition,
                                            mu = 50,
                                            alternative = 'two.sided')
rh_replication_Z <- qnorm(rh_replication_rank_sum_test$p.value/2)
rh_replication_r <- abs(rh_replication_Z) / sqrt(length(unique(rh_replication$subject)))

# Bayesian t-test
rh_replication_bf <- ttestBF(rh_replication$repetition - rh_replication$ground_truth, mu = 0)
rh_replication_chains <- posterior(rh_replication_bf, iterations = 10000)
summary(rh_replication_chains)

# two-sample Wilcoxon test
RHsequence_IID_two_sample_wilcox <- wilcox.test(rh_replication$repetition,
                                                dat_prob_50$repetition,
                                                alternative = 'two.sided')
RHsequence_IID_Z <- qnorm(RHsequence_IID_two_sample_wilcox$p.value/2)
RHsequence_IID_r <- abs(RHsequence_IID_Z) / sqrt(length(unique(rh_replication$subject)) + length(unique(dat_prob_50$subject)))

# Bayesian mixed effects regression model
rh_replication_streak <- read.csv('./../Data/RH_replication_dat.csv', header = T, stringsAsFactors = T) %>% 
  group_by(subject, streak) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()
exp3_mdl <- brm(formula = repetition ~ 1 + I(streak^2) + streak + (1 + I(streak^2) + streak|subject),
                data = rh_replication_streak,
                seed = 1)
summary(exp3_mdl)
