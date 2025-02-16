library(tidyverse)
library(nortest)
library(brms)
library(BayesFactor)

######### Re-analysis of original RH data #########
# normality test
rh_2023_dat <- read.csv('RH2023_study2A.csv', header = T, stringsAsFactors = T) %>% 
  filter(generator == 'bingo') %>% 
  mutate(response = 'probability') %>% 
  dplyr::rename(subject = participant_id,
                repetition = prediction_recode,
                ground_truth = rate)
lillie.test(rh_2023_dat$repetition)

######### Experiments #########
dat <- read.csv('IID_dat.csv', header = T, stringsAsFactors = T)

##### Experiments 1a: p = 0.5, IID, probability judgment #####
dat_prob_50 <- dat %>% 
  filter(response == 'probability' & ground_truth == 50) %>% 
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# normality test
lillie.test(dat_prob_50$repetition)

# one-sample Wilcoxon signed-rank test
rank_sum_test <- wilcox.test(dat_prob_50$repetition,
                             mu = 50,
                             alternative = 'two.sided',
                             conf.int = T)
Z = qnorm(rank_sum_test$p.value/2) # Z score, dividing by 2 because it's two-sided
r = abs(Z) / sqrt(length(unique(dat_prob_50$subject))) # effect size

# Bayesian t-test
bf <- ttestBF(dat_prob_50$repetition - dat_prob_50$ground_truth, mu = 0) # Bayes factor
chains = posterior(bf, iterations = 10000)
summary(chains) # get median and 95% credible interval of the posterior distribution for effect size

##### Experiments 1b: p = 0.5, IID, point prediction #####
dat_point_50 <- dat %>% 
  filter(response == 'point' & ground_truth == 50) %>% 
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# normality test
lillie.test(dat_point_50$repetition)

# one-sample Wilcoxon signed-rank test
rank_sum_test <- wilcox.test(dat_point_50$repetition,
                             mu = 50,
                             alternative = 'two.sided')
Z = qnorm(rank_sum_test$p.value/2)
r = abs(Z) / sqrt(length(unique(dat_point_50$subject)))

# Bayesian t-test
bf <- ttestBF(dat_point_50$repetition - dat_point_50$ground_truth, mu = 0)
chains = posterior(bf, iterations = 10000)
summary(chains)

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

##### Experiments 2a: p = 0.6, IID, probability judgment #####
dat_prob_60 <- dat %>% 
  filter(response == 'probability' & ground_truth == 60) %>%  
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# mean and median
mean(dat_prob_60$repetition)
median(dat_prob_60$repetition)

# normality test
lillie.test(dat_prob_60$repetition)

# one-sample Wilcoxon signed-rank test
rank_sum_test <- wilcox.test(dat_prob_60$repetition,
                             mu = 60,
                             alternative = 'two.sided',
                             conf.int = T)
Z = qnorm(rank_sum_test$p.value/2)
r = abs(Z) / sqrt(length(unique(dat_prob_60$subject)))

# Bayesian t-test
bf <- ttestBF(dat_prob_60$repetition - dat_prob_60$ground_truth, mu = 0)
chains = posterior(bf, iterations = 10000)
summary(chains)

##### Experiments 2a: p = 0.4, IID, probability judgment #####
dat_prob_40 <- dat %>% 
  filter(response == 'probability' & ground_truth == 40) %>%  
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# mean and median
mean(dat_prob_40$repetition)
median(dat_prob_40$repetition)

# normality test
lillie.test(dat_prob_40$repetition)

# one-sample Wilcoxon signed-rank test
rank_sum_test <- wilcox.test(dat_prob_40$repetition,
                             mu = 40,
                             alternative = 'two.sided',
                             conf.int = T)
Z = qnorm(rank_sum_test$p.value/2)
r = abs(Z) / sqrt(length(unique(dat_prob_40$subject)))

# Bayesian t-test
bf <- ttestBF(dat_prob_40$repetition - dat_prob_40$ground_truth, mu = 0)
chains = posterior(bf, iterations = 10000)
summary(chains)

##### Experiments 2b: p = 0.6, IID, point prediction #####
dat_point_60 <- dat %>% 
  filter(response == 'point' & ground_truth == 60) %>%  
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# normality test
lillie.test(dat_point_60$repetition)

# one-sample Wilcoxon signed-rank test
rank_sum_test <- wilcox.test(dat_point_60$repetition,
                             mu = 60,
                             alternative = 'two.sided',
                             conf.int = T)
Z = qnorm(rank_sum_test$p.value/2)
r = abs(Z) / sqrt(length(unique(dat_point_60$subject)))

# Bayesian t-test
bf <- ttestBF(dat_point_60$repetition - dat_point_60$ground_truth, mu = 0)
chains = posterior(bf, iterations = 10000)
summary(chains)

##### Experiments 2b: p = 0.4, IID, point prediction #####
dat_point_40 <- dat %>% 
  filter(response == 'point' & ground_truth == 40) %>%  
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# normality test
lillie.test(dat_point_40$repetition)

# one-sample Wilcoxon signed-rank test
rank_sum_test <- wilcox.test(dat_point_40$repetition,
                             mu = 40,
                             alternative = 'two.sided',
                             conf.int = T)
Z = qnorm(rank_sum_test$p.value/2)
r = abs(Z) / sqrt(length(unique(dat_point_40$subject)))

# Bayesian t-test
bf <- ttestBF(dat_point_40$repetition - dat_point_40$ground_truth, mu = 0)
chains = posterior(bf, iterations = 10000)
summary(chains)

##### Experiment3: p = 0.5, replication of Rao & Hastie (2023), probability  judgment #####
rh_replication <- read.csv('RH_replication_dat.csv', header = T, stringsAsFactors = T) %>% 
  group_by(subject, ground_truth) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

# proportion of participants whose P(repeat) was less than 40%
length(which(rh_replication$repetition < 40)) / length(rh_replication$repetition)

# normality test
lillie.test(rh_replication$repetition)

# one-sample Wilcoxon signed-rank test
rank_sum_test <- wilcox.test(rh_replication$repetition,
                             mu = 50,
                             alternative = 'two.sided')
Z = qnorm(rank_sum_test$p.value/2)
r = abs(Z) / sqrt(length(unique(rh_replication$subject)))

# Bayesian t-test
bf <- ttestBF(rh_replication$repetition - rh_replication$ground_truth, mu = 0)
chains = posterior(bf, iterations = 10000)
summary(chains)

# two-sample Wilcoxon test
two_sample_wilcox <- wilcox.test(rh_replication$repetition,
                             dat_prob_50$repetition,
                             alternative = 'two.sided')
Z = qnorm(two_sample_wilcox$p.value/2)
r = abs(Z) / sqrt(length(unique(rh_replication$subject)) + length(unique(dat_prob_50$subject)))

# Bayesian mixed effects regression model
rh_replication_streak <- read.csv('RH_replication_dat.csv', header = T, stringsAsFactors = T) %>% 
  group_by(subject, streak) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()
exp3_mdl <- brm(formula = repetition ~ 1 + I(streak^2) + streak + (1 + I(streak^2) + streak|subject),
                data = rh_replication_streak,
                seed = 1)
summary(exp3_mdl)
