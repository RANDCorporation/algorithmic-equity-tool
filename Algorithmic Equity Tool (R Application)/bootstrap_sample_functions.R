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
### group_cols: group columns from full data set
### metrics: vector of performance metric functions

## Outputs:
### vector of performance metric values. ordered by metric, then groups (combined then group factor level order)
###############################################################################################################################
bootstrap_only <- function(data, bs_ind, group_cols, metrics) {
  # Get bootstrap sample
  data_bs <- data[bs_ind,]
  group_cols_bs <- group_cols[bs_ind,]
  bs_val <- get_metrics_by_race_vec(data = data_bs, group_mat = group_cols_bs, metrics = metrics)
  return(bs_val$Value)
}


# Perform bootstrapping and get confidence intervals #################################################
## Inputs:
### data: full data set, must have group probability/indicator variable G_* columns
### B: number of bootstrap resamples
### alpha: size of confidence interval
### ci_types: type of confidence interval (any allowed in boot::boot.ci except "stud")
### parallel_opt: option for parallelizing (any allowed in boot::boot)

## Outputs:
### matrix of CI and SE values. Rows: combined data, then groups in factor level order.
######################################################################################################
bootstrap_sample <- function(data, B, alpha=0.05, 
                             ci_types = c("norm","basic","perc","bca","all"),
                             metrics = c("accuracy","selrate","tnr", "tpr", "fpr","fnr","ppv","npv"),
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
  result_bs <- boot::boot(data, bootstrap_only, R = B, group_cols = data[,group_cols], metrics = metrics, 
                          parallel = parallel_opt, ncpus = cores_max)

  # Get bootstrap CIs
  ## Standard error estimates
  se_temp <- apply(result_bs$t, MARGIN = 2, sd)
  ## Confidence intervals
  ci_temp <- lapply(1:length(result_bs$t0), function(est_ind) {
    ### Use boot.ci for each method specified in ci_types
    if(is.na(result_bs$t0[est_ind])) {
      #### Fill out with NAs if estimate is NA
      temp_cols <- apply(expand_grid(ci_types, c(1,2)), 1, paste, collapse = "")
      temp_cols <- gsub("norm", "normal", gsub("perc", "percent", temp_cols))
      temp_vec <- rep(NA_real_, length(temp_cols))
      names(temp_vec) <- temp_cols
      bind_rows(temp_vec)
    } else {
      ci_result <- boot::boot.ci(result_bs, conf = 1-alpha, type = ci_types, index = est_ind)
      ### Get CI endpoints for each method and combine
      #### Fill out with NAs if ci_result is NULL (because of no variation in estimates)
      if(is.null(ci_result)) {
        temp_cols <- apply(expand_grid(ci_types, c(1,2)), 1, paste, collapse = "")
        temp_cols <- gsub("norm", "normal", gsub("perc", "percent", temp_cols))
        temp_vec <- rep(NA_real_, length(temp_cols))
        names(temp_vec) <- temp_cols
        bind_rows(temp_vec)
      } else {
        unlist(lapply(ci_result[4:length(ci_result)], function(m) { m[,(ncol(m)-1):ncol(m)] }))
      }
    }
  })
  names(ci_temp) <- apply(expand_grid(metrics, c("Overall", group_cols)), 1, paste, collapse = "_")
  return_df <- bind_rows(ci_temp, .id = "group") %>%
    ## add SE estimates
    mutate(se = se_temp,
           mean_est = result_bs$t0)  %>%
    ## split group and metric variables
    mutate(metric = str_split(group, "_prob_", simplify = T)[,1],
           G = str_split(group, "_prob_", simplify = T)[,2])
  ## label percentile intervals as "ci_low" and "ci_high" if present
  if("percent1" %in% colnames(return_df)) {
    return_df <- mutate(return_df,
                        ci_low = percent1, ci_high = percent2) %>%
      select(-c(percent1, percent2))
  }
  return(return_df)
}
