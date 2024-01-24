library(dplyr)


# johndrow and lum pre-processing


####################
# Helper functions #
####################

# identify the atoms of a discrete variable
identify_atoms <- function(X){
  atoms <- sort(unique(X))
  return(atoms)
}

is_discrete <- function(x){
  return(length(unique(x)) != length(x))
}

# determine the pmf
distribution_atomic <- function(x, atoms){
  n <- length(x)
  n_atoms <- length(atoms)
  
  pmf <- vector(mode = "numeric", length = n_atoms)
  for(i in 1:n_atoms){
    pmf[i] <- sum(x == atoms[i]) / n
  }
  
  cdf <- cumsum(pmf)
  
  return(list(pmf = pmf, cdf = cdf))
}

# random transform Xi
atomic_u <- function(x, atoms, cdf){
  U <- vector(mode = "numeric", length = length(x))
  a <- vector(mode = "numeric", length = length(x))
  b <- vector(mode = "numeric", length = length(x))
  
  for(i in 1:length(x)){
    # determine (a, b) 
    j <- which(atoms == x[i])
    if(j == 1){
      a[i] <- 0; b[i] <- cdf[1]
    }else{
      a[i] <- cdf[j - 1]; b[i] <- cdf[j]
    }
    
    U[i] <- runif(1, min = a, max = b)
  }
  
  return(list(a = a, b = b, U = U))
}

# Inverse cdf
G_inverse <- function(U, cdf_target, atoms){
  quantiles <- quantile(U, cdf_target, na.rm = T)
  W1_1 <- cut(x = U, breaks = c(0, quantiles), labels = atoms, include.lowest = TRUE)
  return(W1_1)
}

dist_continuous <- function(x) {
  cdf.func <- ecdf(x)
  prob <- cdf.func(x)
  quantiles <-  quantile(x, prob, na.rm = T)
  W1_1 <- cut(x = x, breaks = c(0, quantiles), labels = x)
  return(W1_1)
}

get_sub_cont <- function(sub, X){
  rank_sub <- rank(sub)
  U_sub <- rank_sub/length(sub)
  W_sub <- quantile(X, U_sub, na.rm = T)
  return(W_sub)
}
get_sub_dis <- function(sub, dist_X, atoms){
  dist_X_sub <- distribution_atomic(sub, atoms = atoms)
  U_sub <- atomic_u(sub, atoms = atoms, cdf = dist_X_sub$cdf)
  W_sub <- G_inverse(U_sub$U, cdf_target = dist_X$cdf, atoms = atoms)
  return(W_sub)
}
###########

johndrow_lum_preprocess <- function(data){
  
  data$ID <- 1:nrow(data)
  outcome.exists <- F
  if("Y" %in% colnames(data)){
    outcome <- data[,c("ID", "Y")]
    data$Y <- NULL 
    outcome.exists = T
  }
  data[,c("Yhat", "Prob")] <- NULL #removing prob?
  vars <- colnames(data)[-which(colnames(data) %in% c("ID", "G"))]
  races <- unique(data$G)
  #apply for each variable that is not G 
  df_W <- data.frame(ID = data$ID)
  
  for(i in 1:length(vars)){
    X <- data[[vars[i]]]
    if(plyr::is.discrete(X)){
      # marginal_distribution
      atoms <- identify_atoms(X)
      dist_X <- distribution_atomic(X, atoms = atoms)
      
      new_col <- data %>% group_by(G) %>% mutate(!! vars[i] := get_sub_dis(!!as.name(vars[i]), dist_X, atoms))
      df_W[[vars[i]]] <- new_col[[vars[i]]]
    }
    
    else if(is.numeric(X)){
      df <- data.frame(ID = integer(),
                       W = numeric())
      
      for(j in 1:length(races)){
        X_sub <- filter(data, G == races[j])[[vars[i]]]
        ID_sub <- filter(data, G == races[j])$ID
        W_sub <- get_sub_cont(X_sub, X)
        df_sub <- data.frame(ID = ID_sub, W = W_sub)
        
        df <- rbind(df, df_sub)
      }
      names(df) <- c("ID", paste0(vars[[i]]))
      df_W <- left_join(df_W, df, by = "ID")
    }
    else {
      return("Error: variables must be discrete or continuous")
    }
  }
  df_W <- left_join(df_W, data[,c("G", "ID")], by = "ID")
  if(outcome.exists) df_W <- left_join(df_W, outcome, by = "ID")
  df_W$ID <- NULL
  return(df_W)
}
