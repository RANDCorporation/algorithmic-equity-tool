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
get_minmax_epsilon <- function(groups,
                               data_input,
                               epsilon, 
                               epsilon_prime, 
                               compute_relative) {
  ## set epsilon for each group
  epsilon_g = 
    epsilon_gp = 
    epsilon_seq = 
    epsilon_prime_seq = vector('list', length(groups))
  names(epsilon_g) = 
    names(epsilon_gp) = 
    names(epsilon_seq) = 
    names(epsilon_prime_seq) = groups
  for(group in groups){
    epsilon_g[[group]] = epsilon
    epsilon_gp[[group]] = epsilon_prime
    
    ## Multiple epsilon by mean if using relative bias
    if(compute_relative){
      epsilon_g[[group]] = epsilon_g[[group]] * mean(data_input %>% pull(!!sym(group)))
      epsilon_gp[[group]] = epsilon_gp[[group]] * mean(data_input %>% pull(!!sym(group)))
    }
  }
  
  ## constrain biggest group to be the same size as sum of all other groups (ensures feasibility of total predicted probabilities)
  if(compute_relative){
    which_max_group = names(which.max(colMeans(data_input[, groups])))
    other_group_prop = 1 - colMeans(data_input[, which_max_group, drop = FALSE])
    epsilon_g[[which_max_group]] = epsilon_g[[which_max_group]] * (other_group_prop / (1 - other_group_prop))
    epsilon_gp[[which_max_group]] = epsilon_gp[[which_max_group]] * (other_group_prop / (1 - other_group_prop))
  }
  
  ## make sequence
  for(group in groups){
    epsilon_seq[[group]] = seq(min(epsilon_g[[group]]), 
                               max(epsilon_g[[group]]), 
                               (max(epsilon_g[[group]]) - min(epsilon_g[[group]])) / 10) 
    epsilon_prime_seq[[group]] = seq(min(epsilon_gp[[group]]), 
                                     max(epsilon_gp[[group]]), 
                                     (max(epsilon_gp[[group]]) - min(epsilon_gp[[group]])) / 10) 
  }
  
  ## create boundaries for additional limits
  epsilon_ret_min = 
    epsilon_ret_max = 
    epsilonp_ret_min = 
    epsilonp_ret_max = vector('list', length(groups))
  names(epsilon_ret_min) = 
    names(epsilonp_ret_min) =
    names(epsilon_ret_max) = 
    names(epsilonp_ret_max) = groups
  for(group in groups){
    data_gp = data_input %>%
      select(!!sym(group), Y, Yhat) %>%
      mutate(G_prob = !!sym(group)) %>% 
      select(-!!sym(group))
    # Calculate constraints to remove impossible combinations of epsilon, epsilon'
    expec_Y1Yhat1 <- mean(data_gp$G_prob * data_gp$Y * data_gp$Yhat) / mean(data_gp$Y * data_gp$Yhat)
    expec_Y1Yhat0 <- mean(data_gp$G_prob * data_gp$Y * (1 - data_gp$Yhat)) / mean(data_gp$Y * (1 - data_gp$Yhat))
    expec_Y0Yhat1 <- mean(data_gp$G_prob * (1 - data_gp$Y) * data_gp$Yhat) / mean((1 - data_gp$Y) * data_gp$Yhat)
    expec_Y0Yhat0 <- mean(data_gp$G_prob * (1 - data_gp$Y) * (1 - data_gp$Yhat)) / mean((1 - data_gp$Y) * (1 - data_gp$Yhat))
    expec_YeqYhat <- mean(data_gp$G_prob * as.numeric(data_gp$Y == data_gp$Yhat)) / mean(as.numeric(data_gp$Y == data_gp$Yhat))
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

    # TPR
    epsilon_ret_max[[group]]['tpr'] <- max(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_Y1Yhat1'] - 1 & 
                                                       epsilon_seq[[group]] <= expec_vec['expec_Y1Yhat1'])])
    epsilonp_ret_max[[group]]['tpr'] <- max(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Y1Yhat0'] - 1 & 
                                                              epsilon_prime_seq[[group]] <= expec_vec['expec_Y1Yhat0'])])
    
    epsilon_ret_min[[group]]['tpr'] <- min(epsilon_seq[[group]][which(epsilon_seq[[group]]>=expec_vec['expec_Y1Yhat1'] - 1 & 
                                                       epsilon_seq[[group]] <= expec_vec['expec_Y1Yhat1'])])
    epsilonp_ret_min[[group]]['tpr'] <- min(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Y1Yhat0'] - 1 &
                                                              epsilon_prime_seq[[group]] <= expec_vec['expec_Y1Yhat0'])])
    
    # TNR
    epsilon_ret_max[[group]]['tnr'] <- max(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_Y0Yhat0'] - 1 &
                                                       epsilon_seq[[group]] <= expec_vec['expec_Y0Yhat0'])])
    epsilonp_ret_max[[group]]['tnr'] <- max(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Y0Yhat1'] - 1 & 
                                                              epsilon_prime_seq[[group]] <= expec_vec['expec_Y0Yhat1'])])
    
    epsilon_ret_min[[group]]['tnr'] <- min(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_Y0Yhat0'] - 1 & 
                                                       epsilon_seq[[group]] <= expec_vec['expec_Y0Yhat0'])])
    epsilonp_ret_min[[group]]['tnr'] <- min(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Y0Yhat1'] - 1 &
                                                              epsilon_prime_seq[[group]] <= expec_vec['expec_Y0Yhat1'])])
    
    # FNR
    epsilon_ret_max[[group]]['fnr'] <- max(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_Y1Yhat0'] - 1 & 
                                                                        epsilon_seq[[group]] <= expec_vec['expec_Y1Yhat0'])])
    epsilonp_ret_max[[group]]['fnr'] <- max(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Y1Yhat1'] - 1 & 
                                                                               epsilon_prime_seq[[group]] <= expec_vec['expec_Y1Yhat1'])])
    
    epsilon_ret_min[[group]]['fnr'] <- min(epsilon_seq[[group]][which(epsilon_seq[[group]]>=expec_vec['expec_Y1Yhat0'] - 1 & 
                                                                        epsilon_seq[[group]] <= expec_vec['expec_Y1Yhat0'])])
    epsilonp_ret_min[[group]]['fnr'] <- min(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Y1Yhat1'] - 1 &
                                                                               epsilon_prime_seq[[group]] <= expec_vec['expec_Y1Yhat1'])])
    
    # FPR
    epsilon_ret_max[[group]]['fpr'] <- max(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_Y0Yhat1'] - 1 &
                                                                        epsilon_seq[[group]] <= expec_vec['expec_Y0Yhat1'])])
    epsilonp_ret_max[[group]]['fpr'] <- max(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Y0Yhat0'] - 1 & 
                                                                               epsilon_prime_seq[[group]] <= expec_vec['expec_Y0Yhat0'])])
    
    epsilon_ret_min[[group]]['fpr'] <- min(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_Y0Yhat1'] - 1 & 
                                                                        epsilon_seq[[group]] <= expec_vec['expec_Y0Yhat1'])])
    epsilonp_ret_min[[group]]['fpr'] <- min(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Y0Yhat0'] - 1 &
                                                                               epsilon_prime_seq[[group]] <= expec_vec['expec_Y0Yhat0'])])
    
    # PPV
    epsilon_ret_max[[group]]['ppv'] <- max(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_Y1Yhat1'] - 1 &
                                                       epsilon_seq[[group]] <= expec_vec['expec_Y1Yhat1'])])
    epsilonp_ret_max[[group]]['ppv'] <- max(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Y0Yhat1'] - 1 &
                                                              epsilon_prime_seq[[group]] <= expec_vec['expec_Y0Yhat1'])])
    
    epsilon_ret_min[[group]]['ppv'] <- min(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_Y1Yhat1'] - 1 &
                                                       epsilon_seq[[group]] <= expec_vec['expec_Y1Yhat1'])])
    epsilonp_ret_min[[group]]['ppv'] <- min(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Y0Yhat1'] - 1 &
                                                              epsilon_prime_seq[[group]] <= expec_vec['expec_Y0Yhat1'])])
    
    # NPV
    epsilon_ret_max[[group]]['npv'] <- max(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_Y0Yhat0'] - 1 &
                                                       epsilon_seq[[group]] <= expec_vec['expec_Y0Yhat0'])])
    epsilonp_ret_max[[group]]['npv'] <- max(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Y1Yhat0'] - 1 &
                                                              epsilon_prime_seq[[group]] <= expec_vec['expec_Y1Yhat0'])])
    
    epsilon_ret_min[[group]]['npv'] <- min(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_Y0Yhat0'] - 1 &
                                                       epsilon_seq[[group]] <= expec_vec['expec_Y0Yhat0'])])
    epsilonp_ret_min[[group]]['npv'] <- min(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Y1Yhat0'] - 1 &
                                                              epsilon_prime_seq[[group]] <= expec_vec['expec_Y1Yhat0'])])
    
    # Accuracy
    epsilon_ret_max[[group]]['accuracy'] <- max(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_YeqYhat'] - 1 &
                                                            epsilon_seq[[group]] <= expec_vec['expec_YeqYhat'])])
    epsilonp_ret_max[[group]]['accuracy'] <- max(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_YneqYhat'] - 1 &
                                                                   epsilon_prime_seq[[group]] <= expec_vec['expec_YneqYhat'])])
    
    epsilon_ret_min[[group]]['accuracy'] <- min(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_YeqYhat'] - 1 &
                                                            epsilon_seq[[group]] <= expec_vec['expec_YeqYhat'])])
    epsilonp_ret_min[[group]]['accuracy'] <- min(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_YneqYhat'] - 1 &
                                                                   epsilon_prime_seq[[group]] <= expec_vec['expec_YneqYhat'])])
    
    # Selection rate
    epsilon_ret_max[[group]]['selrate'] <- max(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_Yhat1'] - 1 &
                                                           epsilon_seq[[group]] <= expec_vec['expec_Yhat1'])])
    epsilonp_ret_max[[group]]['selrate'] <- max(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Yhat0'] - 1 &
                                                                  epsilon_prime_seq[[group]] <= expec_vec['expec_Yhat0'])])
    
    epsilon_ret_min[[group]]['selrate'] <- min(epsilon_seq[[group]][which(epsilon_seq[[group]] >= expec_vec['expec_Yhat1'] - 1 &
                                                           epsilon_seq[[group]] <= expec_vec['expec_Yhat1'])])
    epsilonp_ret_min[[group]]['selrate'] <- min(epsilon_prime_seq[[group]][which(epsilon_prime_seq[[group]] >= expec_vec['expec_Yhat0'] - 1 &
                                                                  epsilon_prime_seq[[group]] <= expec_vec['expec_Yhat0'])])
    
  }
  
  out = list('epsilon' = list('min_vals' = epsilon_ret_min,
                              'max_vals' = epsilon_ret_max),
             'epsilon_prime' = list('min_vals' = epsilonp_ret_min,
                                    'max_vals' = epsilonp_ret_max))
  
  return(out)
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
get_epsilon_bc <- function(groups,
                           metric_marg, 
                           metric_vals, 
                           epsilon, 
                           epsilon_prime, 
                           param_3,
                           bias_only = FALSE) {
  
  # Verify all metrics are included in all inputs
  metric_names <- c("tpr", 
                    "tnr",
                    "fnr",
                    "fpr",
                    "ppv", 
                    "npv", 
                    "accuracy", 
                    "selrate")
  if(!(all(metric_names %in% names(metric_marg)) & all(metric_names %in% names(metric_vals[[1]])) 
       & all(metric_names %in% names(epsilon[[1]])) & all(metric_names %in% names(epsilon_prime[[1]])))){
    stop("All metrics must have corresponding elements in 'metric_marg', 'metric_vals', 'epsilon', and 'epsilon_prime'.")
  }
  
  return_vec = bias_vec = vector('list', length(groups))
  names(return_vec) = names(bias_vec) = groups
  for(group in groups){
    # Calculate bias corrections and bias-corrected metrics
    return_vec[[group]] = bias_vec[[group]] = vector(length = length(metric_vals[[group]]))
    names(return_vec[[group]]) = names(bias_vec[[group]]) = names(metric_vals[[group]])
    group_param3 = param_3 %>%
      pull(!!sym(group))
    ## TPR
    bias_vec[[group]]['tpr'] <- ((1 - metric_vals[[group]]['tpr']) * metric_marg['tpr'] * epsilon[[group]]['tpr'] - 
                          metric_vals[[group]]['tpr'] * (1 - metric_marg['tpr']) * epsilon_prime[[group]]['tpr']) / group_param3[1]
    return_vec[[group]]['tpr'] <- metric_vals[[group]]['tpr'] + bias_vec[[group]]['tpr']
    ## TNR
    bias_vec[[group]]['tnr'] <- ((1 - metric_vals[[group]]['tnr']) * metric_marg['tnr'] * epsilon[[group]]['tnr'] - 
                          metric_vals[[group]]['tnr'] * (1 - metric_marg['tnr']) * epsilon_prime[[group]]['tnr']) / group_param3[2]
    return_vec[[group]]['tnr'] <- metric_vals[[group]]['tnr'] + bias_vec[[group]]['tnr']
    ## FNR
    bias_vec[[group]]['fnr'] <- ((1 - metric_vals[[group]]['fnr']) * metric_marg['fnr'] * epsilon[[group]]['fnr'] - 
                                   metric_vals[[group]]['fnr'] * (1 - metric_marg['fnr']) * epsilon_prime[[group]]['fnr']) / group_param3[1]
    return_vec[[group]]['fnr'] <- metric_vals[[group]]['fnr'] + bias_vec[[group]]['fnr']
    ## FPR
    bias_vec[[group]]['fpr'] <- ((1 - metric_vals[[group]]['fpr']) * metric_marg['fpr'] * epsilon[[group]]['fpr'] - 
                                   metric_vals[[group]]['fpr'] * (1 - metric_marg['fpr']) * epsilon_prime[[group]]['fpr']) / group_param3[2]
    return_vec[[group]]['fpr'] <- metric_vals[[group]]['fpr'] + bias_vec[[group]]['fpr']
    ## PPV
    bias_vec[[group]]['ppv'] <- ((1 - metric_vals[[group]]['ppv']) * metric_marg['ppv'] * epsilon[[group]]['ppv'] - 
                          metric_vals[[group]]['ppv'] * (1 - metric_marg['ppv']) * epsilon_prime[[group]]['ppv']) / group_param3[3]
    return_vec[[group]]['ppv'] <- metric_vals[[group]]['ppv'] + bias_vec[[group]]['ppv']
    ## NPV
    bias_vec[[group]]['npv'] <- ((1 - metric_vals[[group]]['npv']) * metric_marg['npv'] * epsilon[[group]]['npv'] - 
                          metric_vals[[group]]['npv'] * (1 - metric_marg['npv']) * epsilon_prime[[group]]['npv']) / group_param3[4]
    return_vec[[group]]['npv'] <- metric_vals[[group]]['npv'] + bias_vec[[group]]['npv']
    ## Accuracy
    bias_vec[[group]]['accuracy'] <- ((1 - metric_vals[[group]]['accuracy']) * metric_marg['accuracy'] * epsilon[[group]]['accuracy'] - 
                               metric_vals[[group]]['accuracy'] * (1 - metric_marg['accuracy']) * epsilon_prime[[group]]['accuracy']) / group_param3[5]
    return_vec[[group]]['accuracy'] <- metric_vals[[group]]['accuracy'] + bias_vec[[group]]['accuracy']
    ## Selection rate
    bias_vec[[group]]['selrate'] <- ((1 - metric_vals[[group]]['selrate']) * metric_marg['selrate'] * epsilon[[group]]['selrate'] - 
                              metric_vals[[group]]['selrate'] * (1 - metric_marg['selrate']) * epsilon_prime[[group]]['selrate']) / group_param3[5]
    return_vec[[group]]['selrate'] <- metric_vals[[group]]['selrate'] + bias_vec[[group]]['selrate']
  }
  
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
get_metrics_by_race_vec <- function(data, 
                                    group_mat, 
                                    metrics){
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
                                 G = c("Overall", colnames(group_mat))),
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
get_eq_per_uncertainty <- function(dfs, 
                                   pro.methods,
                                   epsilon,
                                   epsilon_prime,
                                   relative_eps,
                                   param3_update) {
  
  metrics_bs <- lapply(dfs, function(d) {
    group_cols <- grep('G', colnames(d), value = TRUE)
    boot_ind = lapply(1:500,
                      function(ind, n){
                        sample(1:n, n, replace = TRUE)
                      },
                      n = nrow(d))
    boot_test = lapply(boot_ind,
                       bootstrap_only,
                       data = d, 
                       group_cols = group_cols, 
                       metrics = paste0(c("accuracy", "selrate", "tnr", "tpr", "fpr", "fnr", "ppv", "npv"), "_prob"),
                       epsilon = epsilon,
                       epsilon_prime = epsilon_prime,
                       relative_eps = relative_eps,
                       param3 = param3_update)
    
    metrics_bs = bind_rows(boot_test) %>%
      group_by(G,
               metric) %>%
      mutate(rep = 1:n()) %>%
      ungroup
  })
  ## Bind bootstrap results from all dfs together
  return_metrics <- metrics_bs %>% 
    bind_rows(.id = "Method")
 
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




