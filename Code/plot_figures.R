library(tidyverse)
library(patchwork)
library(boot)
source('helper.R')
set.seed(246)

######### Re-analysis of original RH data #########
rh_2023_dat <- read.csv('RH2023_study2A.csv', header = T, stringsAsFactors = T) %>% 
  filter(generator == 'bingo') %>% 
  mutate(response = 'probability') %>% 
  dplyr::rename(subject = participant_id,
                repetition = prediction_recode,
                ground_truth = rate)

dat <- rh_2023_dat
p0 <- hist_plot('probability', 50) + labs(title = NULL)

pdf('./../Figures/fig3.pdf', onefile = T, width = 5, height = 3)
p0
dev.off()

######### Experiments #########
dat <- read.csv('IID_dat.csv', header = T, stringsAsFactors = T) %>% 
  mutate(streak = factor(streak, levels = seq(1, 8, by = 1)))

### most common sequences with each terminal streak length ###
seq_frequencies <- data.frame(table(dat$sequence[dat$ground_truth == 50])) %>% 
  arrange(desc(Freq)) %>% 
  left_join(dat %>% filter(!duplicated(sequence)) %>% select(streak, sequence) %>% dplyr::rename(Var1 = sequence)) %>% 
  filter(!duplicated(streak))

df <- dat %>% 
  filter(sequence %in% seq_frequencies$Var1) %>% 
  group_by(sequence, subject, response) %>% 
  dplyr::summarize(repetition = mean(repetition)) %>% 
  ungroup() %>% 
  mutate(response = factor(response, levels = c('probability', 'point'), labels = c('Probability', 'Point'), ordered = T))

p1 <- most_common_hist(df)

pdf('./../Figures/fig2.pdf', onefile = T, width = 9, height = 3)
p1
dev.off()

### histograms ###
p2 <- (hist_plot('probability', 50) | hist_plot('point', 50)) /
  (hist_plot('probability', 60) | hist_plot('point', 60)) /
  (hist_plot('probability', 40) | hist_plot('point', 40))

pdf('./../Figures/fig4.pdf', onefile = T, width = 8, height = 6)
p2
dev.off()

### streak length plots ###
# representativeness model
dat <- dat %>% 
  mutate(sequence = sequence %>% str_remove_all('\\[|\\]|,|\\s')) %>% # remove brackets, commas, space
  mutate(ends_with_1 = str_sub(sequence, -1) == '1') # mark sequences that end with red
dat$m <- sapply(dat$sequence, function(x) sum(as.numeric(unlist(strsplit(x, ''))))) # count the number of red balls in the sequence

dat <- dat %>% 
  mutate(m = ifelse(ends_with_1, m, 8-m), # number of balls with terminal streak color, denoted as M
         proportion = m/8*100, # proportion of balls with terminal streak color
         repre_mdl_prediction = ifelse(response == 'probability', 
                                       1/(1+exp(-(9*ground_truth/100 - m)))*100, # probability judgment: compute X = 9*P(ground truth) - M, P(repeat) = 1/(1+exp(-X))
                                       case_when(proportion > ground_truth ~ 0, # point prediction: predict reversal if proportion of balls with terminal streak color is greater than ground truth
                                                 proportion < ground_truth ~ 100))) # point prediction: predict reversal if proportion of balls with terminal streak color is less than ground truth
# when proportion of balls with terminal streak color equals ground truth, randomly predict the next color based on ground truth
dat$repre_mdl_prediction[is.na(dat$repre_mdl_prediction)] <- sapply(which(is.na(dat$repre_mdl_prediction)), 
                                                                    function(i) sample(c(0, 100), size = 1, prob = c(1 - dat$ground_truth[i]/100, dat$ground_truth[i]/100)))

# probability judgment mean and median
prob_dat_summary_streak <- dat %>% 
  filter(response == 'probability') %>% 
  group_by(ground_truth, subject, streak) %>% 
  dplyr::summarize(repetition = mean(repetition),
                   repre_mdl_prediction = mean(repre_mdl_prediction)) %>% 
  ungroup() %>% 
  mutate(ground_truth = factor(ground_truth, levels = c(50, 60, 40), ordered = T))

p3 <- prob_streak_plot(prob_dat_summary_streak)

# point prediction mean
point_dat_summary_streak <- dat %>% 
  filter(response == 'point') %>% 
  group_by(ground_truth, subject, streak) %>% 
  dplyr::summarize(repetition = mean(repetition),
                   repre_mdl_prediction = mean(repre_mdl_prediction)) %>% 
  ungroup() %>% 
  mutate(ground_truth = factor(ground_truth, levels = c(50, 60, 40), ordered = T))

# attempt to reconstruct point predictions by sampling from probability judgments
dat_prob <- dat %>% filter(response == 'probability')

# sample until there are k more samples supporting one prediction over the other
k <- 5
decision_rule <- function(x) {
  count_continue <- 0 # number of samples supporting repetition 
  count_discontinue <- 0 # number of samples supporting reversal
  while (T) {
    if (rbinom(n = 1, size = 1, prob = x) == 1) { # sample from subjective or objective probability x
      count_continue <- count_continue + 1
    } else {
      count_discontinue <- count_discontinue + 1
    }
    if (abs(count_continue - count_discontinue) >= k) {
      break
    }
  }
  return (as.double(count_continue > count_discontinue) * 100)
}

dat_prob$subjective <- sapply(dat_prob$repetition/100, decision_rule) # sampling from probability judgments
dat_prob$objective <- sapply(dat_prob$ground_truth/100, decision_rule) # sampling from the ground truth

sampling_model_summary_streak <- dat_prob %>% 
  group_by(ground_truth, subject, streak) %>% 
  dplyr::summarize(subjective = mean(subjective),
                   objective = mean(objective)) %>% 
  ungroup() %>% 
  mutate(ground_truth = factor(ground_truth, levels = c(50, 60, 40), ordered = T))

p4 <- point_streak_plot(point_dat_summary_streak, sampling_model_summary_streak)

pdf('./../Figures/fig5.pdf', onefile = T, width = 12, height = 8)
p3 + p4 + plot_annotation(tag_levels = 'A')
dev.off()

### IID vs RH ###
p5_1 <- dist_plot(dat %>% filter(ground_truth == 50 & response == 'probability') %>% mutate(streak = as.numeric(as.character(streak)))) +
  labs(title = 'Exp 1a (IID sequences)', x = NULL, y = 'Frequency') 

rh_replication <- read.csv('RH_replication_dat.csv', header = T, stringsAsFactors = T)
p5_2 <- dist_plot(rh_replication) +
  labs(title = 'Exp 3 (RH sequences)', x = NULL, y = NULL) 

prob_dat_summary_streak <- dat %>% 
  filter(response == 'probability' & ground_truth == 50) %>% 
  group_by(subject, streak) %>% 
  dplyr::summarize(repetition = mean(repetition),
                   repre_mdl_prediction = mean(repre_mdl_prediction)) %>% 
  ungroup()
p6_1 <- rh_prob_streak_plot(prob_dat_summary_streak) +
  theme(legend.position = 'none') +
  labs(x = 'Terminal streak length', y = 'Probability of streak repeating (%)')

# representativeness model
rh_replication <- rh_replication %>% 
  mutate(sequence = sequence %>% str_remove_all('\\[|\\]|,|\\s')) %>% 
  mutate(ends_with_1 = str_sub(sequence, -1) == '1') 
rh_replication$m <- sapply(rh_replication$sequence, function(x) sum(as.numeric(unlist(strsplit(x, ''))))) 

rh_replication <- rh_replication %>% 
  mutate(m = ifelse(ends_with_1, m, 8-m), 
         proportion = m/8*100,
         repre_mdl_prediction = 1/(1+exp(-(9*ground_truth/100 - m)))*100)

rh_replication_summary_streak <- rh_replication %>% 
  group_by(subject, streak) %>% 
  dplyr::summarize(repetition = mean(repetition),
                   repre_mdl_prediction = mean(repre_mdl_prediction)) %>% 
  ungroup() %>% 
  mutate(streak = factor(streak, levels = seq(1, 8, by = 1)))  

p6_2 <- rh_prob_streak_plot(rh_replication_summary_streak) +
  labs(x = 'Terminal streak length', y = NULL)

pdf('./../Figures/fig6.pdf', onefile = T, width = 8, height = 7)
(p5_1 | p5_2) /
  (p6_1 | p6_2) + plot_annotation(tag_levels = list(c('A', '', 'B', '')))
dev.off()

######### Supplement #########
# exploring subjective and objective probability models with different k values
k_values <- c(2, 10, 20)
for (k in k_values) {
  dat_prob[[paste0('Subjective model, k=', k)]] <- sapply(dat_prob$repetition/100, decision_rule)
  dat_prob[[paste0('Objective model, k=', k)]] <- sapply(dat_prob$ground_truth/100, decision_rule)
}

# thresholding model
dat_prob <- dat_prob %>% mutate(`Thresholding model` = case_when(repetition < ground_truth ~ 0, repetition > ground_truth ~ 100))
# when proportion of balls with terminal streak color equals ground truth, randomly predict the next color based on ground truth
dat_prob$`Thresholding model`[is.na(dat_prob$`Thresholding model`)] <- sapply(which(is.na(dat_prob$`Thresholding model`)), 
                                                                              function(i) sample(c(0, 100), size = 1, prob = c(1 - dat_prob$ground_truth[i]/100, dat_prob$ground_truth[i]/100)))

sampling_model_summary_streak <- dat_prob %>% 
  group_by(ground_truth, subject, streak) %>% 
  dplyr::summarize(`Subjective model, k=2` = mean(`Subjective model, k=2`),
                   `Objective model, k=2` = mean(`Objective model, k=2`),
                   `Subjective model, k=10` = mean(`Subjective model, k=10`),
                   `Objective model, k=10` = mean(`Objective model, k=10`),
                   `Subjective model, k=20` = mean(`Subjective model, k=20`),
                   `Objective model, k=20` = mean(`Objective model, k=20`),
                   `Thresholding model` = mean(`Thresholding model`),) %>% 
  ungroup() %>% 
  mutate(ground_truth = factor(ground_truth, levels = c(50, 60, 40), ordered = T)) %>% 
  pivot_longer(cols = contains('model'), names_to = 'model', values_to = 'repetition')

p7 <- supplement_point_streak_plot(point_dat_summary_streak %>% select(-repre_mdl_prediction) %>% mutate(model = 'Data'), sampling_model_summary_streak)

pdf('./../Figures/supplement.pdf', onefile = T, width = 6, height = 7)
p7
dev.off()
