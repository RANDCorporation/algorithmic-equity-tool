library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)

source("~/Documents/Current_Projects/RWJF_Imputed_Fairness/New Code/algorithmic-equity-tool/Algorithmic Equity Tool (R Application)/stat_functions_probs.R")
source("~/Documents/Current_Projects/RWJF_Imputed_Fairness/New Code/algorithmic-equity-tool/Algorithmic Equity Tool (R Application)/bootstrap_sample_functions.R")

## load in data here
test_data <- read_csv("")

## Function to compute metrics
compute_difference_metrics = function(input_data,
                                      epsilon = c(-0.05, 0.05),
                                      epsilon_prime = c(-0.05, 0.05),
                                      eps_as_relative = TRUE){
  ## get all bootstrap values without bias added
  metrics = c("tpr", 
              "tnr", 
              "ppv", 
              "npv", 
              "accuracy", 
              "selrate")
  metrics <- paste0(metrics, "_prob")
  names(metrics) <- metrics
  group_cols <- grep('G', colnames(input_data), value = TRUE)
  
  ## add bias to bootstrap result directly
  param3_list <- lapply(list(input_data), 
                        function(dat){
                          group_cols <- grep('G', colnames(dat), value = TRUE)
                          apply(dat[, group_cols], 
                                MARGIN = 2,
                                FUN = get_param3,
                                Y = dat$Y, 
                                Yhat = dat$Yhat)
                        })
  param3_update = as_tibble(param3_list[[1]])
  
  ## test
  return_metrics <- get_eq_per_uncertainty(list(input_data), 
                                           pro.methods = 'Method1',
                                           epsilon = epsilon,
                                           epsilon_prime = epsilon_prime,
                                           param3 = param3_update,
                                           relative_eps = eps_as_relative)
  
  return(return_metrics)
}
## Function to generate plot
create_difference_plot = function(difference_metric,
                                  plot_difference_from_group,
                                  plot_metric){
  
  overall_metrics = difference_metric %>%
    group_by(rep) %>%
    mutate(no_bias_diff = no_bias - no_bias[G == plot_difference_from_group]) %>%
    filter(G == 'Overall') %>%
    group_by(metric) %>%
    reframe(mean_est = mean(no_bias_diff),
            mean_low = quantile(no_bias_diff, 0.025, na.rm = TRUE),
            mean_high = quantile(no_bias_diff, 0.975, na.rm = TRUE))
  
  
  res_df = difference_metric %>%
    filter(Method == '1') %>%
    group_by(rep) %>%
    mutate(no_bias_diff = no_bias - no_bias[G == plot_difference_from_group],
           lower_bias_diff = lower_bias - lower_bias[G == plot_difference_from_group],
           upper_bias_diff = upper_bias - upper_bias[G == plot_difference_from_group]) %>%
    group_by(G,
             metric) %>%
    reframe(mean_est = mean(no_bias_diff, na.rm = TRUE),
            no_bias_lower = quantile(no_bias_diff, 0.025, na.rm = TRUE),
            no_bias_upper = quantile(no_bias_diff, 0.975, na.rm = TRUE),
            bc_low = quantile(lower_bias_diff, 0.025, na.rm = TRUE),
            bc_high = quantile(upper_bias_diff, 0.975, na.rm = TRUE)) %>%
    mutate(mean_low = bc_low - no_bias_lower + mean_est,
           mean_high = bc_high - no_bias_upper + mean_est)
  
  full_bc <- res_df %>% 
    bind_rows(overall_metrics %>%
                mutate(bc_low = NA_real_, 
                       bc_high = NA_real_)) %>%
    mutate(Method = 'Model 1')
  
  equity.df <- full_bc %>% 
    filter(metric == plot_metric, 
           G != "Overall",
           G != plot_difference_from_group) %>%
    mutate(G_display = if_else(G == "Overall", G, str_split(G, "G_", simplify = T)[, 2]),
           metric_display = plot_metric)
  
  perform.df <- full_bc %>% 
    filter(G == "Overall", 
           metric == plot_metric) %>%
    mutate(metric_display = plot_metric,
           G_display = G)
  
  equity.df %>%
    ggplot(aes(x = mean_est, y = Method, color = G_display)) +
    geom_linerange(aes(xmin = mean_low, xmax = mean_high), lwd = 2, position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(xmin = bc_low, xmax = bc_high), width = 0.05, alpha = 0.9, position = position_dodge(width = 0.25)) +
    labs(color = "Group", y = NULL, x = plot_metric, title = paste0(plot_metric, ' by Group')) +
    theme_classic(base_size = 16) +
    theme(legend.position = 'left', panel.grid.major.x = element_line()) +
    scale_color_viridis_d(end = 0.95) +
    geom_vline(xintercept = 0) +
    xlab(paste0('Difference in ', plot_metric, ' from ', plot_difference_from_group, " by Group"))
}


## Example
## Compute metrics
diff_metrics = compute_difference_metrics(test_data)

## Create plots
create_difference_plot(diff_metrics,
                       'G_BISGaian',
                       'tpr')
create_difference_plot(diff_metrics,
                       'G_BISGwhite',
                       'fnr')









