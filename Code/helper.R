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
    stat_summary(aes(color = 'Mean', shape = 'Mean'), fun = mean, geom = 'point', size = 2) +
    stat_summary(aes(color = 'Mean'), fun.data = mean_cl_boot, geom = 'errorbar', width = 0) +
    stat_summary(aes(color = 'Mean'), fun = mean, geom = 'line', group = 1) +
    stat_summary(aes(color = 'Median', shape = 'Median'), fun = median, geom = 'point', size = 2) +
    stat_summary(aes(color = 'Median'), fun.data = median_ci, geom = 'errorbar', width = 0) +
    stat_summary(aes(color = 'Median'), fun = median, geom = 'line', group = 1) +
    scale_color_manual(values = c('Mean' = '#CF8E80', 'Median' = '#916953')) +
    scale_shape_manual(values = c('Mean' = 16, 'Median' = 17)) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_x_discrete(drop = F) +
    scale_y_continuous(breaks = c(0, 20, 40, 50, 60, 80, 100)) +
    guides(color = guide_legend(override.aes = list(shape = c(16, 17)), title = NULL),
           shape = 'none') +
    facet_grid(ground_truth ~ .) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = 'Probability judgment', x = 'Terminal streak length', y = 'Probability of streak repeating (%)')
  return (p)
}

point_streak_plot <- function(dat_summary_streak) {
  p <- dat_summary_streak %>% 
    ggplot(aes(streak, repetition)) +
    geom_hline(yintercept = 50, linetype = 'dashed', color = 'dimgray') +
    geom_hline(yintercept = 40, linetype = 'dashed', color = 'dimgray') +
    geom_hline(yintercept = 60, linetype = 'dashed', color = 'dimgray') +
    stat_summary(data = objective, aes(color = 'Objective model', shape = 'Objective model'), fun = mean, geom = 'point', size = 3) +
    stat_summary(data = objective, aes(color = 'Objective model'), fun.data = mean_cl_boot, geom = 'errorbar', width = 0, linewidth = 1) +
    stat_summary(data = objective, aes(color = 'Objective model'), fun = mean, geom = 'line', group = 1, linewidth = 1) +
    stat_summary(data = subjective, aes(color = 'Subjective model', shape = 'Subjective model'), fun = mean, geom = 'point', size = 3) +
    stat_summary(data = subjective, aes(color = 'Subjective model'), fun.data = mean_cl_boot, geom = 'errorbar', width = 0, linewidth = 1) +
    stat_summary(data = subjective, aes(color = 'Subjective model'), fun = mean, geom = 'line', group = 1, linewidth = 1) +
    stat_summary(aes(color = 'Data', shape = 'Data'), fun = mean, geom = 'point', size = 3) +
    stat_summary(aes(color = 'Data'), fun.data = mean_cl_boot, geom = 'errorbar', width = 0, linewidth = 1) +
    stat_summary(aes(color = 'Data'), fun = mean, geom = 'line', group = 1, linewidth = 1) +
    scale_color_manual(values = c('Data' = '#A2D2FF', 'Subjective model' = '#CDB4DB', 'Objective model' = '#FFAFCC'),
                       breaks = c('Data', 'Subjective model', 'Objective model')) +
    scale_shape_manual(values = c('Data' = 16, 'Subjective model' = 15, 'Objective model' = 17),
                       breaks = c('Data', 'Subjective model', 'Objective model')) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_x_discrete(drop = F) +
    scale_y_continuous(breaks = c(0, 20, 40, 50, 60, 80, 100)) +
    guides(color = guide_legend(override.aes = list(shape = c(16, 15, 17)), title = NULL),
           shape = 'none') +
    facet_grid(ground_truth ~ .) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = 'Point prediction', x = 'Terminal streak length', y = 'Probability of streak repeating (%)')
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
    stat_summary(aes(color = 'Mean', shape = 'Mean'), fun = mean, geom = 'point', size = 2) +
    stat_summary(aes(color = 'Mean'), fun.data = mean_cl_boot, geom = 'errorbar', width = 0) +
    stat_summary(aes(color = 'Mean'), fun = mean, geom = 'line', group = 1) +
    stat_summary(aes(color = 'Median', shape = 'Median'), fun = median, geom = 'point', size = 2) +
    stat_summary(aes(color = 'Median'), fun.data = median_ci, geom = 'errorbar', width = 0) +
    stat_summary(aes(color = 'Median'), fun = median, geom = 'line', group = 1) +
    scale_color_manual(values = c('Mean' = '#CF8E80', 'Median' = '#916953')) +
    scale_shape_manual(values = c('Mean' = 16, 'Median' = 17)) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_x_discrete(drop = F) +
    scale_y_continuous(breaks = c(0, 20, 40, 50, 60, 80, 100)) +
    guides(color = guide_legend(override.aes = list(shape = c(16, 17)), title = NULL),
           shape = 'none') +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  return (p)
}
