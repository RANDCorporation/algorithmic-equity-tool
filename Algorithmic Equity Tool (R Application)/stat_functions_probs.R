library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

###############################
# Performance metric functions
###############################
selrate_prob = function(g_prob, Y, Yhat){
  if(is.null(g_prob)){
    return(mean(Yhat == 1))
  } else{
    return(sum(Yhat * g_prob) / sum(g_prob))
  }
}

accuracy_prob = function(g_prob, Y, Yhat){
  if(is.null(g_prob)){
    return(mean(Y == Yhat))
  } else{
    return(sum((Yhat == Y) * g_prob) / sum(g_prob))
  }
}

error_prob = function(g_prob, Y, Yhat){
  if(is.null(g_prob)){
    return(mean(Y != Yhat))
  } else{
    return(sum((Yhat != Y) * g_prob) / sum(g_prob))
  }
}

tpr_prob = function(g_prob, Y, Yhat){
  if(is.null(g_prob)){
    return(sum((Yhat == 1) * (Y == 1)) / sum(Y == 1))
  } else{
    return(sum((Yhat == 1) * (Y == 1) * g_prob) / sum((Y == 1) * g_prob))
  }
}

fpr_prob = function(g_prob, Y, Yhat){
  if(is.null(g_prob)){
    return(sum((Yhat == 1) * (Y == 0)) / sum((Y == 0)))
  } else{
    return(sum((Yhat == 1) * (Y == 0) * g_prob) / sum((Y == 0) * g_prob))
  }
}

tnr_prob = function(g_prob, Y, Yhat){
  if(is.null(g_prob)){
    return(sum((Yhat == 0) * (Y == 0)) / sum(Y == 0))
  } else{
    return(sum((Yhat == 0) * (Y == 0) * g_prob) / sum((Y == 0) * g_prob))
  }
}

fnr_prob = function(g_prob, Y, Yhat){
  if(is.null(g_prob)){
    return(sum((Yhat == 0) * (Y == 1)) / sum(Y == 1))
  } else{
    return(sum((Yhat == 0) * (Y == 1) * g_prob) / sum((Y == 1) * g_prob))
  }
}

ppv_prob = function(g_prob, Y, Yhat){
  if(is.null(g_prob)){
    return(sum(Yhat * Y) / sum(Yhat))
  } else{
    return(sum(Yhat * Y * g_prob) / sum(Yhat * g_prob))
  }
}

npv_prob = function(g_prob, Y, Yhat){
  if(is.null(g_prob)){
    return(sum((Yhat == 0) * (Y == 0)) / sum(Yhat == 0))
  } else{
    return(sum((Yhat == 0) * (Y == 0) * g_prob) / sum((Yhat == 0) * g_prob))
  }
}

####################################
# Functions to get and shape metrics
####################################
# Helper function to truncate at 0 and 1 (accepts vector)
trunc_01 <- function(v) {
  pmin(pmax(v, 0), 1)
}

# Get suggested third sensitivity parameter values for a single group #################################################
## Inputs:
### G_prob: group probability column
### Y, Yhat: outcome, prediction columns

## Outputs: vector of param_3 values (corresponding to the following h_1 functions, in order: Y=1,Y=0,Yhat=1,Yhat=0,1)
#######################################################################################################################
get_param3 <- function(G_prob, Y, Yhat) {
  sens_param_3_Y1 <- mean(G_prob * Y) / mean(Y)
  sens_param_3_Yhat1 <- mean(G_prob * Yhat) / mean(Yhat)
  sens_param_3_Y0 <- mean(G_prob * (1 - Y)) / mean(1 - Y)
  sens_param_3_Yhat0 <- mean(G_prob * (1 - Yhat)) / mean(1 - Yhat)
  sens_param_3_all <- mean(G_prob)
  
  return(c(sens_param_3_Y1, sens_param_3_Y0, sens_param_3_Yhat1, sens_param_3_Yhat0, sens_param_3_all))
}

# Get minimum and maximum valid epsilon, epsilon' combinations for all possible metrics ###############################
## Inputs:
### data_gp: data with group probability variable G_prob, Y, Yhat
### epsilon, epsilon': sensitivity parameter ranges (vectors)

## Outputs:
### epsilon: list with components 'max_vals' (vector of max valid epsilon values), 'min_vals' (min valid values), both named by metric
### epsilon_prime: same as above for epsilon'
#######################################################################################################################
get_minmax_epsilon <- function(data_gp, epsilon, epsilon_prime, relative_eps = TRUE) {
  # Calculate constraints to remove impossible combinations of epsilon, epsilon'
  expec_Y1Yhat1 <- mean(data_gp$G_prob * data_gp$Y * data_gp$Yhat) / mean(data_gp$Y * data_gp$Yhat)
  expec_Y1Yhat0 <- mean(data_gp$G_prob * data_gp$Y * (1 - data_gp$Yhat)) / mean(data_gp$Y * (1 - data_gp$Yhat))
  expec_Y0Yhat1 <- mean(data_gp$G_prob * (1 - data_gp$Y) * data_gp$Yhat) / mean((1 - data_gp$Y) * data_gp$Yhat)
  expec_Y0Yhat0 <- mean(data_gp$G_prob * (1 - data_gp$Y) * (1 - data_gp$Yhat)) / mean((1 - data_gp$Y) * (1 - data_gp$Yhat))
  expec_YeqYhat <- mean(data_gp$G_prob * as.numeric(data_gp$Y==data_gp$Yhat))/mean(as.numeric(data_gp$Y == data_gp$Yhat))
  expec_YneqYhat <- mean(data_gp$G_prob * as.numeric(data_gp$Y != data_gp$Yhat)) / mean(as.numeric(data_gp$Y != data_gp$Yhat))
  expec_Yhat1 <- mean(data_gp$G_prob * data_gp$Yhat) / mean(data_gp$Yhat)
  expec_Yhat0 <- mean(data_gp$G_prob * (1 - data_gp$Yhat)) / mean(1 - data_gp$Yhat)
  expec_vec_names <- c("expec_Y1Yhat1", 
                       "expec_Y1Yhat0", 
                       "expec_Y0Yhat1", 
                       "expec_Y0Yhat0", 
                       "expec_YeqYhat", 
                       "expec_YneqYhat", 
                       "expec_Yhat1", 
                       "expec_Yhat0")
  expec_vec <- sapply(expec_vec_names, 
                      function(x){ 
                        get(x) 
                        }, 
                      USE.NAMES = T)
  ## Set any NaN expectations to 0
  expec_vec <- sapply(expec_vec, 
                      function(val){
                        if(is.na(val)){ 
                          val <- 0 
                          } else{ 
                            val 
                            }
                        })
  epsilon_ret <- list(min_vals = vector(), max_vals = vector())
  epsilonp_ret <- list(min_vals = vector(), max_vals = vector())
  
  ## Multiple epsilon by mean if using relative bias
  if(relative_eps){
    epsilon = epsilon * mean(data_gp$G_prob)
    epsilon_prime = epsilon_prime * mean(data_gp$G_prob)
  }
  # TPR
  epsilon_ret$max_vals['tpr'] <- max(epsilon[which(epsilon >= expec_vec['expec_Y1Yhat1'] - 1 & 
                                                     epsilon <= expec_vec['expec_Y1Yhat1'])])
  epsilonp_ret$max_vals['tpr'] <- max(epsilon_prime[which(epsilon_prime >= expec_vec['expec_Y1Yhat0'] - 1 & 
                                                            epsilon_prime <= expec_vec['expec_Y1Yhat0'])])
  
  epsilon_ret$min_vals['tpr'] <- min(epsilon[which(epsilon>=expec_vec['expec_Y1Yhat1'] - 1 & 
                                                     epsilon <= expec_vec['expec_Y1Yhat1'])])
  epsilonp_ret$min_vals['tpr'] <- min(epsilon_prime[which(epsilon_prime >= expec_vec['expec_Y1Yhat0'] - 1 &
                                                            epsilon_prime <= expec_vec['expec_Y1Yhat0'])])
  
  # TNR
  epsilon_ret$max_vals['tnr'] <- max(epsilon[which(epsilon >= expec_vec['expec_Y0Yhat0'] - 1 &
                                                     epsilon <= expec_vec['expec_Y0Yhat0'])])
  epsilonp_ret$max_vals['tnr'] <- max(epsilon_prime[which(epsilon_prime >= expec_vec['expec_Y0Yhat1'] - 1 & 
                                                            epsilon_prime <= expec_vec['expec_Y0Yhat1'])])
  
  epsilon_ret$min_vals['tnr'] <- min(epsilon[which(epsilon >= expec_vec['expec_Y0Yhat0'] - 1 & 
                                                     epsilon <= expec_vec['expec_Y0Yhat0'])])
  epsilonp_ret$min_vals['tnr'] <- min(epsilon_prime[which(epsilon_prime >= expec_vec['expec_Y0Yhat1'] - 1 &
                                                            epsilon_prime <= expec_vec['expec_Y0Yhat1'])])
  
  # PPV
  epsilon_ret$max_vals['ppv'] <- max(epsilon[which(epsilon >= expec_vec['expec_Y1Yhat1'] - 1 &
                                                     epsilon <= expec_vec['expec_Y1Yhat1'])])
  epsilonp_ret$max_vals['ppv'] <- max(epsilon_prime[which(epsilon_prime >= expec_vec['expec_Y0Yhat1'] - 1 &
                                                            epsilon_prime <= expec_vec['expec_Y0Yhat1'])])
  
  epsilon_ret$min_vals['ppv'] <- min(epsilon[which(epsilon >= expec_vec['expec_Y1Yhat1'] - 1 &
                                                     epsilon <= expec_vec['expec_Y1Yhat1'])])
  epsilonp_ret$min_vals['ppv'] <- min(epsilon_prime[which(epsilon_prime >= expec_vec['expec_Y0Yhat1'] - 1 &
                                                            epsilon_prime <= expec_vec['expec_Y0Yhat1'])])
  
  # NPV
  epsilon_ret$max_vals['npv'] <- max(epsilon[which(epsilon >= expec_vec['expec_Y0Yhat0'] - 1 &
                                                     epsilon <= expec_vec['expec_Y0Yhat0'])])
  epsilonp_ret$max_vals['npv'] <- max(epsilon_prime[which(epsilon_prime >= expec_vec['expec_Y1Yhat0'] - 1 &
                                                            epsilon_prime <= expec_vec['expec_Y1Yhat0'])])
  
  epsilon_ret$min_vals['npv'] <- min(epsilon[which(epsilon >= expec_vec['expec_Y0Yhat0'] - 1 &
                                                     epsilon <= expec_vec['expec_Y0Yhat0'])])
  epsilonp_ret$min_vals['npv'] <- min(epsilon_prime[which(epsilon_prime >= expec_vec['expec_Y1Yhat0'] - 1 &
                                                            epsilon_prime <= expec_vec['expec_Y1Yhat0'])])
  
  # Accuracy
  epsilon_ret$max_vals['accuracy'] <- max(epsilon[which(epsilon >= expec_vec['expec_YeqYhat'] - 1 &
                                                          epsilon <= expec_vec['expec_YeqYhat'])])
  epsilonp_ret$max_vals['accuracy'] <- max(epsilon_prime[which(epsilon_prime >= expec_vec['expec_YneqYhat'] - 1 &
                                                                 epsilon_prime <= expec_vec['expec_YneqYhat'])])
  
  epsilon_ret$min_vals['accuracy'] <- min(epsilon[which(epsilon >= expec_vec['expec_YeqYhat'] - 1 &
                                                          epsilon <= expec_vec['expec_YeqYhat'])])
  epsilonp_ret$min_vals['accuracy'] <- min(epsilon_prime[which(epsilon_prime >= expec_vec['expec_YneqYhat'] - 1 &
                                                                 epsilon_prime <= expec_vec['expec_YneqYhat'])])
  
  # Selection rate
  epsilon_ret$max_vals['selrate'] <- max(epsilon[which(epsilon >= expec_vec['expec_Yhat1'] - 1 &
                                                         epsilon <= expec_vec['expec_Yhat1'])])
  epsilonp_ret$max_vals['selrate'] <- max(epsilon_prime[which(epsilon_prime >= expec_vec['expec_Yhat0'] - 1 &
                                                                epsilon_prime <= expec_vec['expec_Yhat0'])])
  
  epsilon_ret$min_vals['selrate'] <- min(epsilon[which(epsilon >= expec_vec['expec_Yhat1'] - 1 &
                                                         epsilon <= expec_vec['expec_Yhat1'])])
  epsilonp_ret$min_vals['selrate'] <- min(epsilon_prime[which(epsilon_prime >= expec_vec['expec_Yhat0'] - 1 &
                                                                epsilon_prime <= expec_vec['expec_Yhat0'])])
  
  return(list(epsilon = epsilon_ret, epsilon_prime = epsilonp_ret))
}

# Get bias corrections for a single group and metric-specific epsilon, epsilon' combinations ##########################
## Inputs:
### data_gp: data with group probability variable G_prob, Y, Yhat
### epsilon, epsilon_prime: named vectors of sensitivity parameter values, one for each metric
### param_3: vector of values for third sensitivity parameter (from get_param3 or user input)
### metric_marg: vector of marginal versions of each metric (named using metric names with "_marg" ending)
### metric_vals: vector of group-specific performance metric values for the selected group (named using metric names) 

## Outputs: 
### Named vector of bias-corrected metrics
#######################################################################################################################
get_epsilon_bc <- function(data_gp, 
                           metric_marg, 
                           metric_vals, 
                           epsilon, 
                           epsilon_prime, 
                           param_3,
                           bias_only = FALSE) {
  
  # Verify all metrics are included in all inputs
  metric_names <- c("tpr", "tnr", "ppv", "npv", "accuracy", "selrate")
  if(!(all(metric_names %in% names(metric_marg)) & all(metric_names %in% names(metric_vals)) 
       & all(metric_names %in% names(epsilon)) & all(metric_names %in% names(epsilon_prime)))){
    stop("All metrics must have corresponding elements in 'metric_marg', 'metric_vals', 'epsilon', and 'epsilon_prime'.")
  }
  
  # Calculate bias corrections and bias-corrected metrics
  return_vec = bias_vec = vector(length = length(metric_vals))
  names(return_vec) = names(bias_vec) = names(metric_vals)
  ## TPR
  bias_vec['tpr'] <- ((1 - metric_vals['tpr']) * metric_marg['tpr'] * epsilon['tpr'] - 
                         metric_vals['tpr'] * (1 - metric_marg['tpr']) * epsilon_prime['tpr']) / param_3[1]
  return_vec['tpr'] <- metric_vals['tpr'] + bias_vec['tpr']
  ## TNR
  bias_vec['tnr'] <- ((1 - metric_vals['tnr']) * metric_marg['tnr'] * epsilon['tnr'] - 
                         metric_vals['tnr'] * (1 - metric_marg['tnr']) * epsilon_prime['tnr']) / param_3[2]
  return_vec['tnr'] <- metric_vals['tnr'] + bias_vec['tnr']
  ## PPV
  bias_vec['ppv'] <- ((1 - metric_vals['ppv']) * metric_marg['ppv'] * epsilon['ppv'] - 
                         metric_vals['ppv'] * (1 - metric_marg['ppv']) * epsilon_prime['ppv']) / param_3[3]
  return_vec['ppv'] <- metric_vals['ppv'] + bias_vec['ppv']
  ## NPV
  bias_vec['npv'] <- ((1 - metric_vals['npv']) * metric_marg['npv'] * epsilon['npv'] - 
                         metric_vals['npv'] * (1 - metric_marg['npv']) * epsilon_prime['npv']) / param_3[4]
  return_vec['npv'] <- metric_vals['npv'] + bias_vec['npv']
  ## Accuracy
  bias_vec['accuracy'] <- ((1 - metric_vals['accuracy']) * metric_marg['accuracy'] * epsilon['accuracy'] - 
                         metric_vals['accuracy'] * (1 - metric_marg['accuracy']) * epsilon_prime['accuracy']) / param_3[5]
  return_vec['accuracy'] <- metric_vals['accuracy'] + bias_vec['accuracy']
  ## Selection rate
  bias_vec['selrate'] <- ((1 - metric_vals['selrate']) * metric_marg['selrate'] * epsilon['selrate'] - 
                             metric_vals['selrate'] * (1 - metric_marg['selrate']) * epsilon_prime['selrate']) / param_3[5]
  return_vec['selrate'] <- metric_vals['selrate'] + bias_vec['selrate']
  
  if(bias_only){
    return(bias_vec)
  } else{
    return(return_vec)
    }
}

# Get metric values overall and by group for a vector of metric functions ############
## Inputs:
### data: data set including Y and Yhat
### group_mat: group columns (binary or probabilities, one for each group)
### metrics: character vector of performance metric function names ("_prob" versions)
### epsilon, epsilon_prime: vectors of sensitivity parameter values

## Outputs:
### tibble with metric values for each group and combined (Value) plus metric and G columns
######################################################################################
get_metrics_by_race_vec <- function(data, group_mat, metrics){
  metric_vec <- unlist(lapply(metrics, 
                              function(m){
                                c(get(m)(g_prob = NULL,
                                         Y = data$Y, 
                                         Yhat = data$Yhat),
                                  apply(group_mat, 
                                        MARGIN = 2,
                                        FUN = get(m),
                                        Y = data$Y, 
                                        Yhat = data$Yhat))
                              }))
  metric_df <- cbind(expand_grid(metric = metrics, 
                                 G = c("Overall", paste0("G_", 1:ncol(group_mat)))),
                     Value = metric_vec)
  return(metric_df)
}

# Get equity and performance statistics for a list of dataframes #################################
## Inputs:
### dfs: List of dataframes (with G_* indicator variable or probability columns)
### pro.methods: names of methods used to produce dfs
### eq_metric: character function name of equity metric (_prob version)
### per_metric: character function name of performance metric (_prob version)

## Outputs: List with the following components
### equity, performance: equity/performance metric dfs
##########################################################################################
get_equity_performance <- function(dfs, pro.methods, eq_metric, per_metric){
  g_cols <- grep('G', colnames(dfs[[1]]), value = TRUE)
  
  ## Get equity metric and label
  equity <- cbind(Method = rep(pro.methods, each = length(g_cols) + 1),
                       do.call(rbind, lapply(1:length(pro.methods), function(ind){
                         return(get_metrics_by_race_vec(data = dfs[[ind]], 
                                                        group_mat = select(dfs[[ind]], all_of(g_cols)), metrics = eq_metric))
                       })))
  ## Get performance metric and label
  performance <- data.frame(Method = pro.methods,
                            Value = do.call(rbind, lapply(dfs, function(df){
                              return(get(per_metric)(g_prob=NULL, Y=df$Y, Yhat=df$Yhat))
                            })))
  
  return(list(equity = equity, performance = performance))
}

# Get bootstrapped equity and performance statistics for a list of dataframes ############
## Inputs:
### dfs: List of dataframes (with G_* indicator variable or probability columns)
### pro.methods: names of methods used to produce dfs

## Outputs: List with the following components
### equity, performance: equity/performance metric dfs
##########################################################################################
get_eq_per_uncertainty <- function(dfs, pro.methods) {
  metrics_bs <- lapply(dfs, function(d) {
    bootstrap_sample(d, 
                     B = 500, 
                     alpha = 0.05, 
                     ci_types = "perc",
                     metrics = c("accuracy", "selrate", "tnr", "tpr", "ppv", "npv"),
                     parallel_opt = parallel_opt_set)
  })
  ## Bind bootstrap results from all dfs together
  return_metrics <- metrics_bs %>% 
    bind_rows(.id = "Method") %>% 
    select(Method, 
           metric, 
           G, 
           difference,
           mean_est,
           ci_low,
           ci_high)
  return(return_metrics)
}

#########################
# Constraint functions
#########################
# Compute equity metric by race given thresholds (optimized over in apply_equal_stats_prob) #############
## Inputs
### thresholds: group-specific thresholds for dichotomizing Prob
### data: must contain group probability/indicator var columns (G_*), predicted probability (Prob)
### eq_metric: equity metric (character name of _prob version of function)
### target_metric: performance metric (character name of _prob version of function)

## Outputs: overall error plus maximum absolute equity metric difference
#########################################################################################################
get_all_threshold_stats_prob = function(thresholds, data, eq_metric, target_metric = "error_prob"){
  group_names = grep('G', colnames(data), value = TRUE)
  ## compute combined est threshold
  data$threshold_prob = data[, group_names] %>% 
    as.matrix %*% as.matrix(thresholds)
  ## compute new Yhat based on threshold
  data = data %>% 
    mutate(Yhat = (Prob > threshold_prob) * 1)

  equity.df = get_metrics_by_race_vec(data, group_mat = data[, group_names], metrics = eq_metric)$Value[-1]
  errors.df = get(target_metric)(NULL, data$Y, data$Yhat)
  
  # compute max of pairwise equity errors
  equity.dist = max(dist(equity.df))
  
  # add total error to equity metric
  total.dist = equity.dist + sum(errors.df)
  
  #return(equity.dist)
  return(total.dist)
}

# Equalized opportunity and equalized error (according to chosen target metric) ##############
## Inputs:
### data: must contain group probability/indicator var columns (G_*), predicted probability (Prob)
### eq_metric: equity metric (character name of _prob version of function)
### base_thresh: base threshold for classifying predictions
### target_metric: performance metric (character name of _prob version of function)

## Outputs: List with the following components
### data: original data with Yhat classified using optimal thresholds
### thresh: optimal thresholds
### message: status of the optimization
##############################################################################################
apply_equal_stats_prob = function(data, eq_metric, base_thresh, target_metric = "error_prob"){
  # set constants
  message = FALSE
  group_names = grep('G', colnames(data), value = TRUE)
  
  thresh.row = nloptr::nloptr(x0 = rep(base_thresh, 
                                       times = length(group_names)), 
                              eval_f = get_all_threshold_stats_prob,
                              ub = rep(1, 
                                       times = length(group_names)),
                              lb = rep(0, 
                                       times = length(group_names)),
                              opts = list("algorithm" = "NLOPT_GN_DIRECT_L",
                                          "xtol_rel" = 1.0e-4, 
                                          "maxeval" = 1000), 
                              data = data,
                              eq_metric = eq_metric,
                              target_metric = target_metric)
  thresh.row = thresh.row$solution  
  names(thresh.row) = paste0(group_names, "_thresh")
  
  data$threshold_prob = data[, group_names] %>% 
    as.matrix %*% as.matrix(thresh.row)

  data = data %>% 
    mutate(Yhat = (Prob > threshold_prob) * 1)
  return(list(data = data, thresh = thresh.row, message = message))
}

# Optimal equalized odds threshold classifier using group probabilities #################################
## Inputs:
### data: must contain group probability/indicator var columns (G_*), predicted probability (Prob), outcome (Y)
### base_thresh: starting values for group thresholds
### target_metric: metric for overall error (character name of _prob version of function)

## Outputs:
### data: original data with Yhat classified using optimal thresholds
### thresh: optimal thresholds
### message: status of the optimization
##############################################################################################################
apply_equalized_odds_prob = function(data, base_thresh, target_metric = "error_prob"){
  # set constants
  message = FALSE
  Y = data$Y
  group_names = grep('G', colnames(data), value = TRUE)
  Prob = data$Prob
  # Optimization
  thresh.row = nloptr::nloptr(x0 = rep(base_thresh, times= length(group_names)), 
                              eval_f = get_all_threshold_eo,
                              ub = rep(1, times= length(group_names)),
                              lb = rep(0, times= length(group_names)),
                              opts = list( "algorithm" = "NLOPT_GN_DIRECT_L",
                                           "xtol_rel" = 1.0e-4, "maxeval" = 1000),
                              data = data,
                              target_metric = target_metric)
  thresh.row = thresh.row$solution  
  names(thresh.row) = paste0(group_names, "_thresh")
  # Get individual thresholds using optimization results and group probabilities
  data$threshold_prob = data[, group_names] %>% 
    as.matrix %*% as.matrix(thresh.row)
  
  data = data %>% 
    mutate(Yhat = (Prob > threshold_prob) * 1) 
  return(list(data = data, thresh = thresh.row, message = message))
}

# Get equalized odds + error equity metric at given thresholds (optimized over in apply_equalized_odds_prob) ###########
## Inputs: 
### thresholds: current threshold values for each group
### data: must contain group probability/indicator var columns (G_*), predicted probability (Prob), outcome (Y)
### target_metric: metric for overall error

## Outputs:
### overall error + maximum sum of absolute FPR, FNR differences
###########################################################################################################################
get_all_threshold_eo <- function(thresholds, data, target_metric){
  group_names = grep('G', colnames(data), value = TRUE)
  ## compute individual thresholds using group probabilities
  data$threshold_prob = data[, group_names] %>% 
    as.matrix %*% as.matrix(thresholds)
  ## compute new Yhat based on individual thresholds
  data = data %>% 
    mutate(Yhat = (Prob > threshold_prob) * 1)
  
  # compute weighted FPR and FNR by race and overall performance metric
  fpr.equity.df = get_metrics_by_race_vec(data, group_mat = data[, group_names], metrics = "fpr_prob")$Value[-1]
  fnr.equity.df = get_metrics_by_race_vec(data, group_mat = data[, group_names], metrics = "fnr_prob")$Value[-1]
  
  errors.df = get(target_metric)(NULL, data$Y, data$Yhat)
  
  # compute max of pairwise equity errors
  fpr.equity.dist = dist(fpr.equity.df)
  fnr.equity.dist = dist(fnr.equity.df)
  equity.dist = max(abs(fnr.equity.dist) + abs(fpr.equity.dist))
  
  # add total error to equity metric
  total.dist = equity.dist + errors.df
  
  return(total.dist)
}




