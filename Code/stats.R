library(tidyverse)
library(lmerTest)
library(nortest)

dat <- read.csv('./../Data/IID_dat.csv', header = T, stringsAsFactors = T)

##### Experiments 1a & 1b: p = 0.5, IID #####
dat_prob_50 <- dat %>% 
  filter(response == 'probability' & ground_truth == 50) %>% 
  group_by(subject) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

length(which(dat_prob_50$repetition < 40)) / length(dat_prob_50$repetition)
mean(dat_prob_50$repetition)
median(dat_prob_50$repetition)

lillie.test(dat_prob_50$repetition) # Lilliefors (Kolmogorov-Smirnov) test for normality

rank_sum_test <- wilcox.test(dat_prob_50$repetition,
                             mu = 50,
                             alternative = 'two.sided',
                             conf.int = T)
rank_sum_test
Z = qnorm(rank_sum_test$p.value/2) # Z score, dividing by 2 because it's two-sided
Z
r = abs(Z) / sqrt(length(unique(dat_prob_50$subject))) # effect size
r
# dat_prob_50 %>% wilcox_effsize(repetition ~ 1, mu = 50)

dat_point_50 <- dat %>% 
  filter(response == 'point' & ground_truth == 50) %>% 
  group_by(subject) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

lillie.test(dat_point_50$repetition)

rank_sum_test <- wilcox.test(dat_point_50$repetition,
                             mu = 50,
                             alternative = 'two.sided')
rank_sum_test
Z = qnorm(rank_sum_test$p.value/2)
Z
r = abs(Z) / sqrt(length(unique(dat_point_50$subject)))
r

dat_point_50_streak <- dat %>% 
  filter(response == 'point' & ground_truth == 50) %>% 
  group_by(subject, streak) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

exp1_mdl <- brm(formula = repetition ~ 1 + I(streak^2) + streak + (1 + I(streak^2) + streak|subject),
                data = dat_point_50_streak,
                file = 'cache/exp1_mdl',
                iter = 4000,
                seed = 1)
summary(exp1_mdl)

##### Experiments 2a & 2b: p = 0.6 or 0.4, IID #####
dat_prob_60 <- dat %>% 
  filter(response == 'probability' & ground_truth == 60) %>%  
  group_by(subject) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()
mean(dat_prob_60$repetition)
median(dat_prob_60$repetition)
lillie.test(dat_prob_60$repetition)
rank_sum_test <- wilcox.test(dat_prob_60$repetition,
                             mu = 60,
                             alternative = 'two.sided',
                             conf.int = T)
rank_sum_test
Z = qnorm(rank_sum_test$p.value/2)
Z
r = abs(Z) / sqrt(length(unique(dat_prob_60$subject)))
r

dat_prob_40 <- dat %>% 
  filter(response == 'probability' & ground_truth == 40) %>%  
  group_by(subject) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()
mean(dat_prob_40$repetition)
median(dat_prob_40$repetition)
lillie.test(dat_prob_40$repetition)
rank_sum_test <- wilcox.test(dat_prob_40$repetition,
                             mu = 40,
                             alternative = 'two.sided',
                             conf.int = T)
rank_sum_test
Z = qnorm(rank_sum_test$p.value/2)
Z
r = abs(Z) / sqrt(length(unique(dat_prob_40$subject)))
r

dat_point_60 <- dat %>% 
  filter(response == 'point' & ground_truth == 60) %>%  
  group_by(subject) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()
lillie.test(dat_point_60$repetition)
rank_sum_test <- wilcox.test(dat_point_60$repetition,
                             mu = 60,
                             alternative = 'two.sided',
                             conf.int = T)
rank_sum_test
Z = qnorm(rank_sum_test$p.value/2)
Z
r = abs(Z) / sqrt(length(unique(dat_point_60$subject)))
r

dat_point_40 <- dat %>% 
  filter(response == 'point' & ground_truth == 40) %>%  
  group_by(subject) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()
lillie.test(dat_point_40$repetition)
rank_sum_test <- wilcox.test(dat_point_40$repetition,
                             mu = 40,
                             alternative = 'two.sided',
                             conf.int = T)
rank_sum_test
Z = qnorm(rank_sum_test$p.value/2)
Z
r = abs(Z) / sqrt(length(unique(dat_point_40$subject)))
r

##### Experiment3: p = 0.5, replication of Rao & Hastie (2023) #####
rh_dat <- read.csv('RH_replication_dat.csv', header = T, stringsAsFactors = T) %>% 
  group_by(subject) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

length(which(rh_dat$repetition < 40)) / length(rh_dat$repetition)
mean(rh_dat$repetition)
median(rh_dat$repetition)

lillie.test(rh_dat$repetition)
rank_sum_test <- wilcox.test(rh_dat$repetition,
                             mu = 50,
                             alternative = 'two.sided')
rank_sum_test
Z = qnorm(rank_sum_test$p.value/2)
Z
r = abs(Z) / sqrt(length(unique(rh_dat$subject)))
r

two_sample_wilcox <- wilcox.test(rh_dat$repetition,
                             dat_prob_50$repetition,
                             alternative = 'two.sided')
two_sample_wilcox
Z = qnorm(two_sample_wilcox$p.value/2)
Z
r = abs(Z) / sqrt(length(unique(rh_dat$subject)) + length(unique(dat_prob_50$subject)))
r

rh_dat_streak <- read.csv('RH_replication_dat.csv', header = T, stringsAsFactors = T) %>% 
  group_by(subject, streak) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup()

exp3_mdl <- brm(formula = repetition ~ 1 + I(streak^2) + streak + (1 + I(streak^2) + streak|subject),
                data = rh_dat_streak,
                file = 'cache/exp3_mdl',
                seed = 1)
summary(exp3_mdl)
