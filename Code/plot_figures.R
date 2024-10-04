library(tidyverse)
library(patchwork)
library(boot)
source('helper.R')

dat <- read.csv('IID_dat.csv', header = T, stringsAsFactors = T)

### histograms ###
p1 <- (hist_plot('probability', 50) | hist_plot('point', 50)) /
  (hist_plot('probability', 60) | hist_plot('point', 60)) /
  (hist_plot('probability', 40) | hist_plot('point', 40))

pdf('./../figures/fig2.pdf', onefile = T, width = 8, height = 6)
p1
dev.off()

### streak length plots ###
# probability judgment mean and median
prob_dat_summary_streak <- dat %>% 
  filter(response == 'probability') %>% 
  group_by(ground_truth, subject, streak) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup() %>% 
  mutate(streak = factor(streak, levels = seq(1, 8, by = 1)),
         ground_truth = factor(ground_truth, levels = c(50, 60, 40), ordered = T))

p2 <- prob_streak_plot(prob_dat_summary_streak)

# point prediction mean and models
dat_point <- dat %>% filter(response == 'point') %>% mutate(model = 'data')
point_dat_summary_streak <- dat_point %>% 
  group_by(ground_truth, subject, streak) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup() %>% 
  mutate(streak = factor(streak, levels = seq(1, 8, by = 1)),
         ground_truth = factor(ground_truth, levels = c(50, 60, 40), ordered = T))

dat_prob <- dat %>% filter(response == 'probability')
objective <- dat_prob %>% mutate(model = 'objective')
subjective <- dat_prob %>% mutate(model = 'subjective')

k <- 9 # number of samples
decision_rule <- function(x) {
  # if more than half are 1, then the point prediction is 1, otherwise it's 0.
  return(as.double(sum(rbinom(n = k, size = 1, prob = x)) >= ((k+1)/2)) * 100)
}
set.seed(246)
subjective <- dat_prob %>% 
  mutate(repetition = sapply(dat_prob$repetition/100, decision_rule), # sampling from probability judgments
         streak = factor(streak, levels = seq(1, 8, by = 1)),
         ground_truth = factor(ground_truth, levels = c(50, 60, 40), ordered = T))

set.seed(246)
objective <- dat_prob %>% 
  mutate(repetition = sapply(dat_prob$ground_truth/100, decision_rule), # sampling from the ground truth
         streak = factor(streak, levels = seq(1, 8, by = 1)),
         ground_truth = factor(ground_truth, levels = c(50, 60, 40), ordered = T))

p3 <- point_streak_plot(point_dat_summary_streak)

pdf('./../figures/fig3.pdf', onefile = T, width = 12, height = 8)
p2 + p3
dev.off()

### IID vs RH ###
p4_1 <- dist_plot(dat %>% filter(ground_truth == 50 & response == 'probability')) +
  labs(title = 'Exp 1a (IID sequences)', x = NULL, y = 'Frequency') 

rh_dat <- read.csv('RH_replication_dat.csv', header = T, stringsAsFactors = T) 
p4_2 <- dist_plot(rh_dat) +
  labs(title = 'Exp 3 (RH sequences)', x = NULL, y = NULL) 

prob_dat_summary_streak <- dat %>% 
  filter(response == 'probability' & ground_truth == 50) %>% 
  group_by(subject, streak) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup() %>% 
  mutate(streak = factor(streak, levels = seq(1, 8, by = 1)))
p5_1 <- rh_prob_streak_plot(prob_dat_summary_streak) +
  theme(legend.position = 'none') +
  labs(x = 'Terminal streak length', y = 'Probability of streak repeating (%)')

rh_dat_summary_streak <- rh_dat %>% 
  group_by(subject, streak) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup() %>% 
  mutate(streak = factor(streak, levels = seq(1, 8, by = 1)))  
p5_2 <- rh_prob_streak_plot(rh_dat_summary_streak) +
  labs(x = 'Terminal streak length', y = NULL)

pdf('./../figures/fig4.pdf', onefile = T, width = 8, height = 7)
(p4_1 | p4_2) /
  (p5_1 | p5_2)
dev.off()


