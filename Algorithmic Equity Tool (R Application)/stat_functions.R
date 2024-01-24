library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

# REMOVE ALL? (use prob versions instead)
#################################### performance metrics
fpr <- function(Y, Yhat){
  return(sum((Yhat == 1) * (Y == 0)) / sum((Y == 0)))
}

tpr <- function(Y, Yhat){
  return(sum((Yhat == 1) * (Y == 1)) / sum(Y == 1))
}

fnr <- function(Y, Yhat){
  return(sum((Yhat == 0) * (Y == 1)) / sum(Y == 1))
}

sp <- function(Y, Yhat){
  return(mean(Yhat == 1))
}

accuracy <- function(Y, Yhat){
  return(mean(Y == Yhat))
}

error <- function(Y, Yhat){
  return(mean(Y != Yhat))
}

ppv <- function(Y, Yhat){
  return(sum((Yhat == 1) * (Y == 1))/sum(Yhat == 1))
}

npv <- function(Y, Yhat){
  return(sum((Yhat == 0) * (Y == 0))/sum(Yhat == 0))
}

########################################post processing methods
# REMOVE? No mentions in server.R (we are using prob version instead)
#equalized opportunity and equalized error
apply_equal_stats <- function(data, FUN, base_thresh){
  
  # set constants
  message <- FALSE
  Y <- data$Y
  G <- as.character(data$G)
  Prob <- data$Prob
  
  get_thresh <- function(x, data){
    # compute equity metric by race
    equity.df <- sapply(1:length(x), function(elem) {
      data.sub <- filter(data, data['G'] == unique(G)[elem])
      FUN(data.sub['Y'], (data.sub['Prob'] > x[elem]) * 1) 
    })
    
    # compute population metric (P(error type| race) * P(race))
    errors.df <- sapply(1:length(x), function(elem) {
      data.sub <- filter(data, data['G'] == unique(G)[elem])
      error(data.sub['Y'], (data.sub['Prob'] > x[elem]) * 1) * (nrow(data.sub) / nrow(data))
    })
    
    # compute max of pairwise equity errors
    equity.dist <- max(dist(equity.df))
    
    # add total error to equity metric
    total.dist <- equity.dist + sum(errors.df)

    return(total.dist)
  }
  
  thresh.row <- nloptr::nloptr(x0 = rep(base_thresh, times= length(unique(G))), 
                               eval_f = get_thresh,ub = rep(1, times= length(unique(G))),
                               lb = rep(0, times= length(unique(G))),opts = list( "algorithm" = "NLOPT_GN_DIRECT_L",
                                "xtol_rel" = 1.0e-4, "maxeval" = 1000), data = data)
  thresh.row <- thresh.row$solution  
  names(thresh.row) <- paste0(unique(data$G), "_thresh")
  
  data <- data %>% mutate(Yhat = (Prob > c(t(thresh.row[paste0(data$G, "_thresh")]))) * 1 )
  return(list(data = data, thresh = thresh.row, message = message))
}

# REMOVE? (use prob version instead)
#####################other utility functions
get_metrics_by_race <- function(method, data, FUN = sp){
  df <- data.frame(
    Method = rep(method, length(unique(data$G)) + 1),
    G = c("Overall", unique(data$G)), 
    Value = c(FUN(Y = data$Y, Yhat = data$Yhat),
      sapply(unique(data$G), function(race) FUN(data$Y[data$G == race], Yhat = data$Yhat[data$G == race]))
    )
  )
  return(df)
}


# REMOVE? (use prob version instead)
#as per Hardt et al (2016)
# this function determines an optimal equalized (or as close to equalized) 
# odds threshold classifier via grid search
apply_equalized_odds <- function(data, base_thresh){
  # set constants
  Y <- data$Y
  G <- as.character(data$G); data$G <- as.character(data$G)
  Prob <- data$Prob

  get_thresh <- function(x, data){
    # compute equity metric by race
    fnr.df <- sapply(1:length(x), function(elem) {
      data.sub <- filter(data, data['G'] == unique(G)[elem])
      fnr(data.sub['Y'], (data.sub['Prob'] > x[elem]) * 1)
    })
    fpr.df <- sapply(1:length(x), function(elem) {
      data.sub <- filter(data, data['G'] == unique(G)[elem])
      fpr(data.sub['Y'], (data.sub['Prob'] > x[elem]) * 1)
    })
    
    # compute population metric (P(error type| race) * P(race))
    errors.df <- sapply(1:length(x), function(elem) {
      data.sub <- filter(data, data['G'] == unique(G)[elem])
      error(data.sub['Y'], (data.sub['Prob'] > x[elem]) * 1) * (nrow(data.sub) / nrow(data))
    })
    
    fnr.dist <- dist(fnr.df)
    fpr.dist <- dist(fpr.df)
    
    return(max(abs(fnr.dist) + abs(fpr.dist)) + (sum(errors.df)))
  }
  
  thresh.row <- nloptr::nloptr(x0 = rep(base_thresh, times= length(unique(G))), 
                               eval_f = get_thresh,ub = rep(1, times= length(unique(G))),
                               lb = rep(0, times= length(unique(G))),opts = list( "algorithm" = "NLOPT_GN_DIRECT_L",
                                                                                     "xtol_rel" = 1.0e-4, "maxeval" = 1000), data = data)
  thresh.row <- thresh.row$solution  
  names(thresh.row) <- paste0(unique(data$G), "_thresh")
  
  data <- data %>% mutate(Yhat = (Prob > c(t(thresh.row[paste0(data$G, "_thresh")]))) * 1 )
  return(list(data = data, thresh = thresh.row))
}

# REMOVE: used only in bootstrap_metrics (moved from bootstrapping_stat_functions)
get_metrics_by_race_prob = function(method, data, FUN = sp_prob){
  g_names = grep('^G', colnames(data), value = TRUE)
  df = tibble(
    Method = rep(method, length(g_names) + 1),
    G = c('Combined', 
          g_names),
    Value = c(FUN(g_prob = NULL, 
                  Y = data$Y, 
                  Yhat = data$Yhat),
              sapply(data[, grep('G', colnames(data))], 
                     FUN, 
                     data$Y, 
                     data$Yhat))
  )
  return(df)
}

# REMOVE: covered by bootstrap_sample() with impute_only() (moved from bootstrapping_stat_functions)
## get uncertainty around metrics for each group
bootstrap_metrics = function(method, data, n_boot = 100, FUN = sp, lower_ci = 0.025, upper_ci = 0.975){
  boot_out = vector('list', n_boot)
  group_col = grep('G', colnames(data), value = TRUE)
  ## bootstrap the group values
  boot_groups = Hmisc::rMultinom(data[, group_col], n_boot)
  ## this part is niave and could probably be sped up
  for(temp_boot in 1:n_boot){
    boot_data = data
    for(temp_g in group_col){
      boot_data[, temp_g] = 0
      boot_data[boot_groups[, temp_boot] == temp_g, temp_g] = 1
    }
    ## get metrics for each bootstrap replicate
    boot_out[[temp_boot]] = get_metrics_by_race_prob(method, boot_data, FUN = FUN)
  }
  boot_output = bind_rows(boot_out) %>%
    group_by(G) %>%
    reframe(Method = first(Method),
            lower_ci = quantile(Value, lower_ci),
            mean_est = mean(Value),
            upper_ci = quantile(Value, upper_ci))
  return(boot_output)
}

