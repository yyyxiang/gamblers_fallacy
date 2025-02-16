hist_plot <- function(res, gt) {
  dat_summary <- dat %>% 
    filter(response == res) %>% 
    filter(ground_truth == gt) %>% 
    group_by(subject) %>% 
    dplyr::summarize(repetition = mean(repetition)) %>% 
    ungroup()
  
  base_hist <- hist(dat_summary$repetition, plot = F)
  p <- ggplot(dat_summary, aes(x = repetition)) +
    geom_histogram(breaks = base_hist$breaks, fill = 'lightgray', color = 'black', linewidth = 0.4, closed = 'left') +
    geom_vline(xintercept = gt, color = 'red', linetype = 'dashed', linewidth = 1) +
    scale_x_continuous(breaks = c(0, 20, 40, 50, 60, 80, 100),
                       limits = c(0, 100)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = paste0(str_to_title(res), ', ', gt, '%'), x = 'P(repeat) (%)', y = 'Frequency') 
  return (p)
}

most_common_hist <- function(data) {
  
  p <- ggplot(data, aes(x = repetition)) +
    geom_histogram(breaks = seq(0, 100, by = 10), color = 'black', fill = 'lightblue', linewidth = 0.4, closed = 'left') +
    geom_vline(xintercept = 50, color = 'red', linetype = 'dashed', linewidth = 1) +
    scale_x_continuous(breaks = c(0, 50, 100),
                       limits = c(0, 100)) +
    facet_grid(response~sequence) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = 'Probability of streak repeating (%)', y = 'Frequency')
  return (p)
}

median_ci <- function(x, conf = 0.95) {
  boot_median <- boot(x, function(data, indices) median(data[indices]), R = 1000)
  ci <- boot.ci(boot.out = boot_median, type = 'perc', conf = conf)
  data.frame(y = median(x),
             ymin = ci$percent[4],
             ymax = ci$percent[5])
}

prob_streak_plot <- function(dat_summary_streak) {
  p <- dat_summary_streak %>% 
    ggplot(aes(streak, repetition)) +
    geom_hline(yintercept = 50, linetype = 'dashed', color = 'dimgray') +
    geom_hline(yintercept = 40, linetype = 'dashed', color = 'dimgray') +
    geom_hline(yintercept = 60, linetype = 'dashed', color = 'dimgray') +
    stat_summary(aes(y = repre_mdl_prediction, color = 'Representativeness model', shape = 'Representativeness model'), fun = mean, geom = 'point', size = 2) +
    stat_summary(aes(y = repre_mdl_prediction, color = 'Representativeness model'), fun.data = mean_cl_boot, geom = 'errorbar', width = 0) +
    stat_summary(aes(y = repre_mdl_prediction, color = 'Representativeness model'), fun = mean, geom = 'line', group = 1) +
    stat_summary(aes(color = 'Mean', shape = 'Mean'), fun = mean, geom = 'point', size = 2) +
    stat_summary(aes(color = 'Mean'), fun.data = mean_cl_boot, geom = 'errorbar', width = 0) +
    stat_summary(aes(color = 'Mean'), fun = mean, geom = 'line', group = 1) +
    stat_summary(aes(color = 'Median', shape = 'Median'), fun = median, geom = 'point', size = 2) +
    stat_summary(aes(color = 'Median'), fun.data = median_ci, geom = 'errorbar', width = 0) +
    stat_summary(aes(color = 'Median'), fun = median, geom = 'line', group = 1) +
    scale_color_manual(values = c('Mean' = '#CF8E80', 'Median' = '#916953', 'Representativeness model' = '#8dd3c7')) +
    scale_shape_manual(values = c('Mean' = 16, 'Median' = 17, 'Representativeness model' = 15)) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_x_discrete(drop = F) +
    scale_y_continuous(breaks = c(0, 20, 40, 50, 60, 80, 100)) +
    guides(color = guide_legend(override.aes = list(shape = c(16, 17, 15)), title = NULL),
           shape = 'none') +
    facet_grid(ground_truth ~ .) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = 'Probability judgment', x = 'Terminal streak length', y = 'Probability of streak repeating (%)')
  return (p)
}

point_streak_plot <- function(dat_summary_streak, sampling_model_summary_streak) {
  p <- dat_summary_streak %>% 
    ggplot(aes(streak, repetition)) +
    geom_hline(yintercept = 50, linetype = 'dashed', color = 'dimgray') +
    geom_hline(yintercept = 40, linetype = 'dashed', color = 'dimgray') +
    geom_hline(yintercept = 60, linetype = 'dashed', color = 'dimgray') +
    stat_summary(aes(y = repre_mdl_prediction, color = 'Representativeness model', shape = 'Representativeness model'), fun = mean, geom = 'point', size = 2) +
    stat_summary(aes(y = repre_mdl_prediction, color = 'Representativeness model'), fun.data = mean_cl_boot, geom = 'errorbar', width = 0, linewidth = 1) +
    stat_summary(aes(y = repre_mdl_prediction, color = 'Representativeness model'), fun = mean, geom = 'line', group = 1, linewidth = 1) +
    stat_summary(data = sampling_model_summary_streak, aes(y = objective, color = 'Objective model', shape = 'Objective model'), fun = mean, geom = 'point', size = 2) +
    stat_summary(data = sampling_model_summary_streak, aes(y = objective, color = 'Objective model'), fun.data = mean_cl_boot, geom = 'errorbar', width = 0, linewidth = 1) +
    stat_summary(data = sampling_model_summary_streak, aes(y = objective, color = 'Objective model'), fun = mean, geom = 'line', group = 1, linewidth = 1) +
    stat_summary(data = sampling_model_summary_streak, aes(y = subjective, color = 'Subjective model', shape = 'Subjective model'), fun = mean, geom = 'point', size = 2) +
    stat_summary(data = sampling_model_summary_streak, aes(y = subjective, color = 'Subjective model'), fun.data = mean_cl_boot, geom = 'errorbar', width = 0, linewidth = 1) +
    stat_summary(data = sampling_model_summary_streak, aes(y = subjective, color = 'Subjective model'), fun = mean, geom = 'line', group = 1, linewidth = 1) +
    stat_summary(aes(color = 'Data', shape = 'Data'), fun = mean, geom = 'point', size = 3) +
    stat_summary(aes(color = 'Data'), fun.data = mean_cl_boot, geom = 'errorbar', width = 0, linewidth = 1) +
    stat_summary(aes(color = 'Data'), fun = mean, geom = 'line', group = 1, linewidth = 1) +
    scale_color_manual(values = c('Data' = '#CF8E80', 'Subjective model' = '#7ec0ff', 'Objective model' = '#ff87b3', 'Representativeness model' = '#8dd3c7'),
                       breaks = c('Data', 'Subjective model', 'Objective model', 'Representativeness model')) +
    scale_shape_manual(values = c('Data' = 16, 'Subjective model' = 18, 'Objective model' = 20, 'Representativeness model' = 15),
                       breaks = c('Data', 'Subjective model', 'Objective model', 'Representativeness model')) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_x_discrete(drop = F) +
    scale_y_continuous(breaks = c(0, 20, 40, 50, 60, 80, 100)) +
    guides(color = guide_legend(override.aes = list(shape = c(16, 18, 20, 15)), title = NULL),
           shape = 'none') +
    facet_grid(ground_truth ~ .) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = 'Point prediction', x = 'Terminal streak length', y = 'Proportion predicting streak will repeat (%)')
  return (p)
}

dist_plot <- function(data) {
  p <- ggplot(data, aes(x = streak)) +
    geom_histogram(binwidth = 1, boundary = 0.5, fill = 'lightgray', color = 'black', linewidth = 0.4) +
    scale_x_continuous(breaks = seq(1, 8, by = 1),
                       limits = c(0.5, 8.5)) +
    coord_cartesian(ylim = c(0, 2000)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  return (p)
}

rh_prob_streak_plot <- function(dat_summary_streak) {
  p <- dat_summary_streak %>% 
    ggplot(aes(streak, repetition)) +
    geom_hline(yintercept = 50, linetype = 'dashed', color = 'dimgray') +
    stat_summary(aes(y = repre_mdl_prediction, color = 'Representativeness model', shape = 'Representativeness model'), fun = mean, geom = 'point', size = 2) +
    stat_summary(aes(y = repre_mdl_prediction, color = 'Representativeness model'), fun.data = mean_cl_boot, geom = 'errorbar', width = 0) +
    stat_summary(aes(y = repre_mdl_prediction, color = 'Representativeness model'), fun = mean, geom = 'line', group = 1) +
    stat_summary(aes(color = 'Mean', shape = 'Mean'), fun = mean, geom = 'point', size = 2) +
    stat_summary(aes(color = 'Mean'), fun.data = mean_cl_boot, geom = 'errorbar', width = 0) +
    stat_summary(aes(color = 'Mean'), fun = mean, geom = 'line', group = 1) +
    stat_summary(aes(color = 'Median', shape = 'Median'), fun = median, geom = 'point', size = 2) +
    stat_summary(aes(color = 'Median'), fun.data = median_ci, geom = 'errorbar', width = 0) +
    stat_summary(aes(color = 'Median'), fun = median, geom = 'line', group = 1) +
    scale_color_manual(values = c('Mean' = '#CF8E80', 'Median' = '#916953', 'Representativeness model' = '#8dd3c7')) +
    scale_shape_manual(values = c('Mean' = 16, 'Median' = 17, 'Representativeness model' = 15)) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_x_discrete(drop = F) +
    scale_y_continuous(breaks = c(0, 20, 40, 50, 60, 80, 100)) +
    guides(color = guide_legend(override.aes = list(shape = c(16, 17, 15)), title = NULL),
           shape = 'none') +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  return (p)
}

supplement_point_streak_plot <- function(point_dat_summary_streak, sampling_model_summary_streak) {
  p <- sampling_model_summary_streak %>% 
    ggplot(aes(streak, repetition, color = model)) +
    geom_hline(yintercept = 50, linetype = 'dashed', color = 'dimgray') +
    geom_hline(yintercept = 40, linetype = 'dashed', color = 'dimgray') +
    geom_hline(yintercept = 60, linetype = 'dashed', color = 'dimgray') +
    stat_summary(fun = mean, geom = 'point', size = 2) +
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0, linewidth = 1) +
    stat_summary(aes(group = model), fun = mean, geom = 'line', linewidth = 1) +
    stat_summary(data = point_dat_summary_streak, fun = mean, geom = 'point', size = 2) +
    stat_summary(data = point_dat_summary_streak, fun.data = mean_cl_boot, geom = 'errorbar', width = 0, linewidth = 1) +
    stat_summary(data = point_dat_summary_streak, group = 1, fun = mean, geom = 'line', linewidth = 1) +
    scale_color_manual(name = NULL,
                       values = c('Data' = '#CF8E80', 
                                  'Subjective model, k=2' = '#cfe5cf', 'Objective model, k=2' = '#f4c2bd', 
                                  'Subjective model, k=10' = '#b7c9b2', 'Objective model, k=10' = '#eaa4a5', 
                                  'Subjective model, k=20' = '#A2D2FF', 'Objective model, k=20' = '#FFAFCC', 
                                  'Thresholding model' = '#CDB4DB'),
                       breaks = c('Data', 
                                  'Subjective model, k=2', 'Objective model, k=2',
                                  'Subjective model, k=10', 'Objective model, k=10',
                                  'Subjective model, k=20', 'Objective model, k=20',
                                  'Thresholding model')) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_x_discrete(drop = F) +
    scale_y_continuous(breaks = c(0, 20, 40, 50, 60, 80, 100)) +
    facet_grid(ground_truth ~ .) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = 'Point prediction', x = 'Terminal streak length', y = 'Proportion predicting streak will repeat (%)')
  return (p)
}
