library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)


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
    
    # add total error to oequity metric
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

######################geting statistics
get_equity_performance <- function(dfs, pro.methods, thresh = NULL, eq, per){
  #################################threshold plot
  if(is.null(thresh)){
    thresh.df <- "nothing"
  }
  else{
    thresh.df <- data.frame(Method = rep(pro.methods, each =length(unique(dfs[[1]]$G))),
                            G = rep(unique(dfs[[1]]$G), times = length(pro.methods)), 
                            Threshold = thresh)
  }
  ######################equity metrics code - by race
  if(eq == "Statistical Parity"){
    equity <- do.call(rbind, lapply(1:length(pro.methods), function(ind){
      return(get_metrics_by_race(method = pro.methods[ind], data = dfs[[ind]], FUN = sp))
    }))
    ylab.equity <- "Proportion of Predicted Positives"
  }
  else if(eq == "False Positive Rate"){
    equity <- do.call(rbind, lapply(1:length(pro.methods), function(ind){
      return(get_metrics_by_race(method = pro.methods[ind], data = dfs[[ind]], FUN = fpr))
    }))
    ylab.equity <- "Proportion of False Positives"
  }
  else if(eq == "False Negative Rate"){
    equity <- do.call(rbind, lapply(1:length(pro.methods), function(ind){
      return(get_metrics_by_race(method = pro.methods[ind], data = dfs[[ind]], FUN= fnr))
    }))
    ylab.equity <- "Proportion of False Negatives"
  }
  else if(eq == "Accuracy"){
    equity <- do.call(rbind, lapply(1:length(pro.methods), function(ind){
      return(get_metrics_by_race(method = pro.methods[ind], data = dfs[[ind]], FUN= accuracy))
    }))
    ylab.equity <- "Proportion of Correct Responses"
  }
  else if(eq == "Positive Predictive Value"){
    equity <- do.call(rbind, lapply(1:length(pro.methods), function(ind){
      return(get_metrics_by_race(method = pro.methods[ind], data = dfs[[ind]], FUN= ppv))
    }))
    ylab.equity <- "Proportion of True Positives"
  }
  else if (eq == "Negative Predictive Value"){
    equity <- do.call(rbind, lapply(1:length(pro.methods), function(ind){
      return(get_metrics_by_race(method = pro.methods[ind], data = dfs[[ind]], FUN= npv))
    }))
    ylab.equity <- "Proportion of True Negatives"
  }
  #############################performance metrics code
  if(per == "Overall Accuracy"){
    performance <- data.frame(Method = pro.methods,
                              Value = do.call(rbind, lapply(dfs, function(df){
                                return(mean(df$Y == df$Yhat))}))
    )
    ylab.perform <- "Proportion of Correct Responses"
  }
  else if(per == "False Positive Rate"){
    performance <- data.frame(Method = pro.methods,
                              Value = do.call(rbind, lapply(dfs, function(df){
                                return(nrow(filter(df, Yhat == 1, Y == 0))/ nrow(filter(df, Y == 0)))
                              })))
    ylab.perform <- "Proportion of False Positives"
  }
  else if(per == "False Negative Rate"){
    performance <- data.frame(Method = pro.methods,
                              Value = do.call(rbind, lapply(dfs, function(df){
                                return(nrow(filter(df, Yhat == 0, Y == 1)) / nrow(filter(df, Y == 1)))
                              })))
    ylab.perform <- "Proportion of False Negatives"
  }
  else if(per == "Positive Predictive Value"){
    performance <- data.frame(Method = pro.methods,
                              Value = do.call(rbind, lapply(dfs, function(df){
                                return(ppv(df$Y, df$Yhat))
                              })))
    ylab.perform <- "Proportion of True Positives"
  }
  else if (per == "Negative Predictive Value"){
    performance <- data.frame(Method = pro.methods,
                              Value = do.call(rbind, lapply(dfs, function(df){
                                return(npv(df$Y, df$Yhat))
                              })))
    ylab.perform <- "Proportion of True Negatives"
  }
  
  return(list(equity = equity, performance = performance, eq.lab = ylab.equity, per.lab = ylab.perform,
              thresh.df = thresh.df))
}

