# Set parallel option
if(.Platform$OS.type == "windows") {
  parallel_opt_set <- "no"
} else {
  parallel_opt_set <- "multicore"
}

# Get metrics for a fixed set of group columns. Function for boot call in bootstrap_sample ####################################
## Inputs:
### data: full data set
### bs_ind: indices provided by boot:boot call
### group_cols: names of group columns from full data set
### metrics: vector of performance metric functions

## Outputs:
### vector of performance metric values. ordered by metric, then groups (combined then group factor level order)
###############################################################################################################################
bootstrap_only <- function(bs_ind,
                           data, 
                           group_cols, 
                           metrics,
                           epsilon,
                           epsilon_prime,
                           relative_eps,
                           param3) {
  # Get bootstrap sample
  data_bs <- data[bs_ind,]
  group_cols_bs <- data[bs_ind, group_cols]
  bs_val <- get_metrics_by_race_vec(data = data_bs, 
                                    group_mat = group_cols_bs, 
                                    metrics = metrics) %>%
    pivot_wider(names_from = G, 
                values_from = Value) %>%
    mutate(metric = sub('_prob', '', metric))
  
  ## compute bias range for each boot
  epsilon_minmax = get_minmax_epsilon(group_cols,
                                      data_input = data_bs, 
                                      epsilon = epsilon, 
                                      epsilon_prime = epsilon_prime,
                                      compute_relative = relative_eps)
  
  #### Mean bias corrections
  marg_mean = bs_val %>%
    pull(Overall)
  names(marg_mean) = bs_val %>%
    pull(metric)
  metrics_mean = lapply(group_cols, 
                        function(group){
                          temp = bs_val %>%
                            pull(group)
                          names(temp) = bs_val %>%
                            pull(metric)
                          return(temp)
                        })
  names(metrics_mean) = group_cols
  bias_est1 = get_epsilon_bc(group_cols,
                             metric_marg = marg_mean, 
                             metric_vals = metrics_mean,
                             epsilon = epsilon_minmax$epsilon$min_vals, 
                             epsilon_prime = epsilon_minmax$epsilon_prime$max_vals, 
                             param_3 = param3)
  bias_est2 = get_epsilon_bc(group_cols,
                             metric_marg = marg_mean, 
                             metric_vals = metrics_mean,
                             epsilon = epsilon_minmax$epsilon$max_vals, 
                             epsilon_prime = epsilon_minmax$epsilon_prime$min_vals, 
                             param_3 = param3)
  
  out = bs_val %>% 
    pivot_longer(c(Overall, contains('G')), names_to = 'G') %>% 
    pivot_wider(names_from = c(metric), values_from = value) %>%
    mutate(est = 'no_bias') %>%
    bind_rows(bind_rows(bias_est1) %>%
                mutate(G = group_cols,
                       est = 'lower_bias')) %>%
    bind_rows(bind_rows(bias_est2) %>%
                mutate(G = group_cols,
                       est = 'upper_bias')) %>%
    pivot_longer(c(-G, -est), names_to = 'metric', values_to = 'value') %>%
    pivot_wider(names_from = est, values_from = value)
    
  return(out)
}


# Get bootstrapped differences from baseline group based on bootstrap_samples ####################################
## Inputs:
### baseline_group: groups in the data
### bootstrap_results: bootstrap metrics run on all groups
### result_names: names of the metrics crossed with the groups
### metrics: vector of performance metric functions

## Outputs:
### bootstrap differences between baseline group and all other groups for each metric 
###############################################################################################################################
bootstrap_difference <- function(baseline_group, 
                                 bootstrap_results, 
                                 result_names, 
                                 metrics) {
  result_diff = bootstrap_results
  result_diff$t0 = lapply(metrics,
                          function(metric_name){
                            temp = result_diff$t0[grep(metric_name, result_names)] - 
                              result_diff$t0[grep(paste0('(?=.*', metric_name, ')(?=.*', baseline_group, ')'), result_names, perl = TRUE)]
                          }) %>%
    unlist
  ## add bootstrap results
  result_diff$t = lapply(metrics,
                         function(metric_name){
                           temp = result_diff$t[, grep(metric_name, result_names)]
                           colnames(temp) = result_names[grep(metric_name, result_names)]
                           as_tibble(temp) %>% 
                             mutate(across(everything(), 
                                           \(x) (x - !!sym(paste(metric_name, baseline_group, sep = '_')))))
                         }) %>%
    bind_cols %>% 
    as.matrix
  return(result_diff)
}


# Perform bootstrapping and get confidence intervals #################################################
## Inputs:
### data: full data set, must have group probability/indicator variable G_* columns
### B: number of bootstrap resamples
### alpha: size of confidence interval
### ci_types: type of confidence interval (any allowed in boot::boot.ci except "stud")
### baseline_group: optional group (character value) for computing differences
### parallel_opt: option for parallelizing (any allowed in boot::boot)

## Outputs:
### matrix of CI and SE values. Rows: combined data, then groups in factor level order.
######################################################################################################
bootstrap_sample <- function(data, 
                             B, 
                             alpha = 0.05, 
                             ci_types = c("norm", "basic", "perc", "bca", "all"),
                             metrics = c("accuracy", "selrate", "tnr", "tpr", "fpr", "fnr", "ppv", "npv"),
                             parallel_opt = "no") {
  # Check inputs
  ci_types <- match.arg(ci_types, several.ok = T)
  metrics <- match.arg(metrics, several.ok = T)
  # Add "_prob" to metric names to get probability versions
  metrics <- paste0(metrics, "_prob")
  names(metrics) <- metrics
  
  # Bootstrap sampling error
  group_cols <- grep('G', colnames(data), value = TRUE)
  cores_n <- parallel::detectCores()
  cores_max <- min(4, cores_n)
  result_bs <- boot::boot(data, 
                          bootstrap_only, 
                          R = B, 
                          group_cols = data[, group_cols], 
                          metrics = metrics, 
                          parallel = parallel_opt, 
                          ncpus = cores_max)
  result_names = apply(expand_grid(metrics, c("Overall", group_cols)), 1, paste, collapse = "_")
  ## add differences between groups and baseline
  ## add results for difference with each group as baseline
  result_diffs = lapply(group_cols, 
                        bootstrap_difference,
                        result_bs,
                        result_names, 
                        metrics)
  names(result_diffs) = group_cols
  
  # Get bootstrap CIs
  ## Confidence intervals
  ci_temp <- lapply(1:length(result_bs$t0), 
                    function(est_ind){
                      ### Use boot.ci for each method specified in ci_types
                      if(is.na(result_bs$t0[est_ind])){
                        #### Fill out with NAs if estimate is NA
                        temp_cols <- apply(expand_grid(ci_types, c(1, 2)), 1, paste, collapse = "")
                        temp_cols <- gsub("norm", "normal", gsub("perc", "percent", temp_cols))
                        temp_vec <- rep(NA_real_, length(temp_cols))
                        names(temp_vec) <- temp_cols
                        bind_rows(temp_vec)
                      } else{
                        ci_result <- boot::boot.ci(result_bs, 
                                                   conf = 1 - alpha, 
                                                   type = ci_types, 
                                                   index = est_ind)
                        ci_diffs <- lapply(result_diffs,
                                          boot::boot.ci, 
                                          conf = 1 - alpha, 
                                          type = ci_types, 
                                          index = est_ind)
                        ### Get CI endpoints for each method and combine
                        #### Fill out with NAs if ci_result is NULL (because of no variation in estimates)
                        if(is.null(ci_result)) {
                          temp_cols <- apply(expand_grid(ci_types, c(1, 2)), 1, paste, collapse = "")
                          temp_cols <- gsub("norm", "normal", gsub("perc", "percent", temp_cols))
                          temp_vec <- rep(NA_real_, length(temp_cols))
                          names(temp_vec) <- temp_cols
                          bind_rows(temp_vec)
                        } else{
                          ci_diff = lapply(ci_diffs,
                                           function(diff){
                                             lapply(diff[4:length(diff)], 
                                                    function(m){
                                                      m[, (ncol(m) - 1):ncol(m)] 
                                                    })[[1]]
                                           }) %>%
                            bind_rows %>%
                            t %>%
                            data.frame
                          colnames(ci_diff) = c('ci_low', 'ci_high')
                          ci_result_df = unlist(lapply(ci_result[4:length(ci_result)], 
                                                       function(m){
                                                         m[, (ncol(m) - 1):ncol(m)] 
                                                       }))
                          names(ci_result_df) = c('ci_low', 'ci_high')
                          return(list('ci_result' = ci_result_df, 
                                      'ci_diff' = ci_diff
                                      ))
                        }
                      }
                    })
  names(ci_temp) <- apply(expand_grid(metrics, c("Overall", group_cols)), 1, paste, collapse = "_")
  ## Means
  mean_diff = as_tibble(sapply(result_diffs, function(diff){ diff$t0 }))
  colnames(mean_diff) = group_cols
  mean_diff_df = tibble(group = names(ci_temp),
                        mean_est = result_bs$t0,
                        difference = 'None Selected') %>%
    bind_rows(lapply(mean_diff, function(diff){ tibble(group = names(ci_temp),
                                                       mean_est = diff) }) %>%
                bind_rows() %>%
                mutate(difference = rep(names(mean_diff), each = length(result_bs$t0))))
  ## Standard error estimates
  se_temp <- apply(result_bs$t, MARGIN = 2, sd)
  se_diff_temp <- lapply(result_diffs,
                         function(result_diff){
                           apply(result_diff$t, MARGIN = 2, sd)
                         })
  se_diff_df = tibble(group = names(ci_temp),
                      se = se_temp,
                      difference = 'None Selected') %>%
    bind_rows(lapply(se_diff_temp, function(diff){ tibble(group = names(diff),
                                                          se = diff) }) %>%
                bind_rows() %>%
                mutate(difference = rep(names(se_diff_temp), each = length(se_temp))))

  ## Combine CI and SEs
  return_df <- lapply(ci_temp,
                      bind_rows) %>%
    bind_rows(.id = "group") %>%
    mutate(difference = rep(c('None Selected', group_cols), length(ci_temp))) %>%
    ## add mean estimates
    full_join(mean_diff_df,
              by = c('group', 'difference')) %>%
    ## add SE estimates
    full_join(se_diff_df,
              by = c('group', 'difference')) %>%
    ## split group and metric variables
    mutate(metric = str_split(group, "_prob_", simplify = T)[, 1],
           G = str_split(group, "_prob_", simplify = T)[, 2])
  ## label percentile intervals as "ci_low" and "ci_high" if present
  if("percent1" %in% colnames(return_df)) {
    return_df <- rename(return_df,
                        ci_low = percent1_marginal, 
                        ci_high = percent2_marginal,
                        ci_low_diff = percent1_difference, 
                        ci_high_diff = percent2_difference)
  }
  return(return_df)
}
