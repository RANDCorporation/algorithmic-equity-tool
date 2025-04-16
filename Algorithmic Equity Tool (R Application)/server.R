# list_of_packages = c("shinyWidgets","shinyBS","dplyr","tidyr","stringr","ggplot2","shiny","grid","gridExtra","openxlsx","plotly","plyr","htmltools","DT","knitr","shinycssloaders","shinythemes","nloptr","boot","parallel")
#
# lapply(list_of_packages,
#        function(x) if(!require(x,character.only = TRUE)) install.packages(x))

options(shiny.maxRequestSize = 10 * 1024 ^ 2)

library(shinyWidgets)
library(shinyBS)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(shiny)
library(grid)
library(gridExtra)
library(openxlsx)
library(plotly)

source('pre_process_functions.R')
source('stat_functions_probs.R')
source('bootstrap_sample_functions.R')


# Colors for plots
## method colors are named for matching with sparklines in threshold table ('thresholds' object)
method_colors <- c('Baseline' = "gray40", 'Equalized Error Rate PP' = "#0D0887FF", 'Equalized Odds PP' = "#9613A1FF", 'Equalized Opp. PP' = "#E76F5BFF", 'Statistical Parity PP' = "#F7E225FF")

## Shapes and colors for models on Assessment tab, "Groups" option
model_shapes <- c(0,16,2,15,1)

# Look-up lists for equity and performance metric function names and plot labels
metric_funcs <- c("Selection Rate"="selrate_prob", "False Positive Rate"="fpr_prob", "False Negative Rate"="fnr_prob", "Accuracy"="accuracy_prob",
                  "Positive Predictive Value"="ppv_prob", "Negative Predictive Value"="npv_prob", "True Positive Rate" = "tpr_prob",
                  "True Negative Rate" = "tnr_prob")

metric_labs <- c("Selection Rate"="Proportion of Positive Predictions", "False Positive Rate"="Proportion of False Positives", 
                 "False Negative Rate"="Proportion of False Negatives", "Accuracy"="Proportion of Correct Predictions",
                 "Positive Predictive Value"="Proportion of Correct Positive Predictions", "Negative Predictive Value"="Proportion of Correct Negative Predictions",
                 "True Positive Rate" = "Proportion of True Positives", "True Negative Rate" = "Proportion of True Negatives")

# Validate data uploads
validateDataset <- function(df){
  ## check for needed column names
  validate(need(all(c('Y', 'Prob', 'Yhat') %in% colnames(df)) & 
                  (length(grep('G', colnames(df), value = TRUE))>0), "Please ensure file contains desired column names. Check 'Before Uploading Data' for desired column names."))
  ## check types
  validate(need(is.numeric(df$Prob), "Probability values need to be numeric."))
  validate(need(all(unique(df$Y) %in% c(0,1)), "Y values need to be binary."))
  validate(need(all(unique(df$Yhat) %in% c(0,1)), "Yhat values need to be binary."))
  if('G'%in%colnames(df)) {
    validate(need(plyr::is.discrete(as.character(df$G)), "G values need to be discrete."))
    validate(need(nlevels(as.factor(df$G))<=6, "Data contains more than 6 groups. Please limit levels of 'G' to 6."))
  } else {
    validate(need(length(grep('G_', colnames(df), value = TRUE))<=6, "Data contains more than 6 groups. Please limit number of group probability columns to 6."))
  }
}

# Identify possible categorical covariates on uploaded data
covariate_types <- function(df) {
  cov_cols <- colnames(df)[!(colnames(df)%in%c(grep('G', colnames(df), value = TRUE), 'Y','Prob','Yhat'))]
  for(c in cov_cols) {
    ## convert to factor if number of unique values is <= 10
    if(length(unique(df[[c]]))<=10){
      df[[c]] <- as.factor(df[[c]])
    }
  }
  return(df)
}

# Hover functions
## Equity plot
hover_eq_function <- function(data, hover, is_probs){
  if(is_probs) {
    point_low <- nearPoints(data, hover, threshold = 20, maxpoints = 1, addDist = TRUE, xvar = "xmin") %>%
      mutate(across(where(is.numeric), ~round(.x, 2)))
    point_high <- nearPoints(data, hover, threshold = 20, maxpoints = 1, addDist = TRUE, xvar = "xmax") %>%
      mutate(across(where(is.numeric), ~round(.x, 2)))
    point_mean <- nearPoints(data, hover, threshold = 20, maxpoints = 1, addDist = TRUE, xvar = "x") %>%
      mutate(across(where(is.numeric), ~round(.x, 2)))

    if (nrow(point_low) == 0 & nrow(point_high) == 0 & nrow(point_mean) == 0) return(NULL)
    point_list <- list("low" = point_low, "high" = point_high, "mean" = point_mean)
    point_long <- bind_rows(point_list, .id = "point_set")
    mindist_ind <- point_long[which(point_long$dist_ == min(point_long$dist_)),"point_set"]
    point <- point_list[[mindist_ind]]

    htmltools::renderTags(
      tags$div(
        point$Method, tags$br(), 
        "Group: ", point$G_display, tags$br(),
        "Mean estimate: (", point$mean_low, ", ", point$mean_high, ")", tags$br(),
        "95% CI: (", point$bc_low, ", ", point$bc_high, ")",
        style = paste0(
          "position: absolute; ",
          "z-index:100; ",
          "top: ", hover$coords_css$y + 2, "px; ",
          "left: ", hover$coords_css$x + 2, "px; ",
          "background-color: rgba(245, 245, 245); ",
          "padding: 3px; ",
          "color: black; ",
          "font-size: 14px; ",
          "border: 1px solid black; "
        )
      )
    )$html
  } else {
    point <- nearPoints(data, hover, threshold = 20, maxpoints = 1, addDist = TRUE, xvar = "x") %>%
      mutate(across(where(is.numeric), ~round(.x, 2)))
    if (nrow(point) == 0) return(NULL)
    
    htmltools::renderTags(
      tags$div(
        point$Method, tags$br(), 
        "Group: ", point$G_display, tags$br(),
        "Point estimate: ", point$mean_est, tags$br(),
        "95% CI: (", point$ci_low, ", ", point$ci_high, ")",
        style = paste0(
          "position: absolute; ",
          "z-index:100; ",
          "top: ", hover$coords_css$y + 2, "px; ",
          "left: ", hover$coords_css$x + 2, "px; ",
          "background-color: rgba(245, 245, 245); ",
          "padding: 3px; ",
          "color: black; ",
          "font-size: 14px; ",
          "border: 1px solid black; "
        )
      )
    )$html
  }
}
## Performance plot
hover_perf_function <- function(data, hover, is_probs){
  point <- nearPoints(data, hover, threshold = 20, maxpoints = 1, addDist = TRUE, xvar = "x") %>%
    mutate(across(where(is.numeric), ~round(.x, 2)))
  if (nrow(point) == 0) return(NULL)
  
  htmltools::renderTags(
    tags$div(
      point$Method, tags$br(), 
      "Group: ", point$G_display, tags$br(),
      "Point estimate: ", point$mean_est, tags$br(),
      "95% CI: (", point$ci_low, ", ", point$ci_high, ")",
      style = paste0(
        "position: absolute; ",
        "z-index:100; ",
        "top: ", hover$coords_css$y + 2, "px; ",
        "left: ", hover$coords_css$x + 2, "px; ",
        "background-color: rgba(245, 245, 245); ",
        "padding: 3px; ",
        "color: black; ",
        "font-size: 14px; ",
        "border: 1px solid black; "
      )
    )
  )$html
}
## Threshold plot
hover_thresh_function <- function(data, hover){
  point <- nearPoints(data, hover, threshold = 100, maxpoints = 1, addDist = T, xvar = "Threshold", yvar = "Method")
  if(nrow(point)>0) {
    data_selected <- filter(data, G == point$G) %>%
      mutate(Threshold = round(Threshold, 2)) %>%
      select(-G)
    table_obj <- renderTable(data_selected)
  } else {
    table_obj <- NULL
  }
  htmltools::renderTags(
    tags$div(
      table_obj,
      style = paste0(
        "position: absolute; ",
        "z-index:100; ",
        "top: ", hover$coords_css$y + 2, "px; ",
        "left: ", hover$coords_css$x + 2, "px; ",
        "background-color: rgba(245, 245, 245, 0.95); ",
        "padding: 3px; ",
        "color: black; ",
        "font-size: 14px; "
      )
    )
  )$html
}

#################shiny server code
server <- function(input, output, session) {
  #####################################################ASSESSMENT TAB ########################
  # Output: create upload boxes for data
  output$mdls_fileInputs <- renderUI({
    html_ui = " "
    for (i in 1:input$mdls_num){
      html_ui <- paste0(html_ui, fileInput(paste0("mdls_file",i), label=paste0("Model ",i), accept = ".csv"))
    }
    HTML(html_ui)
  })
  # Upload files, validate, and store as list of dataframes
  mdl_objs <- reactive({
    # Upload files
    all.upload <- c(is.null(input$mdls_file1))
    if(input$mdls_num >= 2){
      all.upload <- c(all.upload, is.null(input$mdls_file2))
    }
    if(input$mdls_num >= 3){
      all.upload <- c(all.upload, is.null(input$mdls_file3))
    }
    if(input$mdls_num >= 4){
      all.upload <- c(all.upload, is.null(input$mdls_file4))
    }
    if(input$mdls_num == 5){
      all.upload <- c(all.upload, is.null(input$mdls_file5))
    }
    files <- list(input$mdls_file1, input$mdls_file2, input$mdls_file3, input$mdls_file4, input$mdls_file5)
    if(mean(all.upload) == 0){
      mdls <- lapply(1:input$mdls_num, function(num){
        data_is_probs <- FALSE
        data_G_Gprob <- NULL
        inFile <- files[[num]]
        validate(need(endsWith(inFile$datapath,".csv"), "Please upload a csv file."))
        df <- read.csv(inFile$datapath, header=T, sep=",")
        ## Validate df
        validateDataset(df)
        ## Determine if fixed groups or probabilities. If fixed, create indicator vars G_*.
        if('G' %in% colnames(df)) {
          ### If data also contains group probability columns, drop them and add to warning string
          if(length(grep('G_', colnames(df), value = TRUE))>0) {
            data_G_Gprob <- paste0("Model ",num," ")
            df <- select(df, -c(grep('G_', colnames(df), value = TRUE)))
          }
          G_mat <- model.matrix(~.-1, data = data.frame(G=as.factor(df$G)))
          colnames(G_mat) <- paste0("G_", unique(df$G))
          df <- bind_cols(df, G_mat) %>% select(-G)
        } else {
          data_is_probs <- TRUE
        }
        return(list(data = df, thresh = NULL, is_probs = data_is_probs, G_Gprob = data_G_Gprob))
      })
      # Verify all datasets have the same number of groups
      mdls_gct <- lapply(mdls, function(l) {
        if('G' %in% colnames(l$data)) {
          num_gps <- nlevels(l$data$G)
        } else {
          num_gps <- length(grep('G_', colnames(l$data), value = TRUE))
        }
      })
      validate(need(length(unique(unlist(mdls_gct)))==1, "All models must contain the same number of groups. Please check group variables."))
    }
    else{
      return(NULL)
    }
    # Set flag indicating whether any of the uploaded data contains group probabilities
    if(any(sapply(mdls,function(x) x$is_probs))) {
      any_is_probs <- T
    } else { any_is_probs <- F }
    all_G_Gprob <- unlist(lapply(mdls, function(l) l$G_Gprob))
    return(list(mdls = mdls, is_probs = any_is_probs, G_Gprob = all_G_Gprob))
  })
  
  # Output to display warning if data with both 'G' and group probability columns is uploaded
  output$mdls_warn_Gprob <- renderText({
    G_Gprob_str <- req(mdl_objs()$G_Gprob)
    if(length(G_Gprob_str)>0) {
      if(length(G_Gprob_str)==1) {
        models_str <- G_Gprob_str
      } else{
        models_str <- paste0(G_Gprob_str, collapse = "and ")
      }
      paste0("Warning: ", models_str, "data contain both 'G' and group probability ('G_') columns. Group probability columns have been dropped.")
    }
  })
  outputOptions(output, "mdls_warn_Gprob", suspendWhenHidden=FALSE)
  
  # Calculate suggested third sensitivity parameter values for each model
  mdl_param3 <- reactive({
    mdls_list <- req(mdl_objs())
    mdls <- lapply(mdls_list$mdls, "[[", "data")
    data_is_probs <- mdls_list$is_probs
    
    if(data_is_probs) {
      # Calculate values for each model
      param3_list <- lapply(mdls, function(dat) {
        group_cols <- grep('G', colnames(dat), value = TRUE)
        apply(dat[,group_cols], 
              MARGIN = 2,
              FUN=get_param3,
              Y=dat$Y, 
              Yhat=dat$Yhat)
      })
      return(param3_list)
    } else { return(NULL) }
  })
  
  # Get equity and performance measures from uploaded data
  mdl_tables <- reactive({
    mdls_list <- req(mdl_objs())
    dfs.sub <- lapply(mdls_list$mdls, "[[", "data")
    data_is_probs <- mdls_list$is_probs
    pro.methods <- paste0("Model ",1:input$mdls_num)
    names(dfs.sub) <-  pro.methods
    # Bootstrap equity and performance metrics
    return_metrics <- get_eq_per_uncertainty(dfs.sub, pro.methods=pro.methods)
    
    return(list(metrics = return_metrics, methods = pro.methods))
  })
  
  # Add bias corrections if group probability data
  mdl_tables_bc <- reactive({
    mdls_list <- req(mdl_objs())
    ## Get bootstrapped metrics
    mdls_bs <- req(mdl_tables())
    if(mdls_list$is_probs) {
      ## Get param_3 values
      param3_init <- req(mdl_param3_table())
      ### Set dependence on "Recalculate" button so this section will re-run when button is pressed
      mdl_param3_data$updated
      param3_update <- isolate(mdl_param3_data$data)
      ## Get epsilon, epsilon' values (dependence so this section will re-run if values change)
      epsilon <- input$mdls_epsilon
      epsilon_prime <- input$mdls_epsilonp
      if(!is.null(epsilon) & !is.null(epsilon_prime)) {
        ## Get data for each model
        dfs <- lapply(mdls_list$mdls, "[[", "data")
        ## Calculate bias corrections for each model
        equity_metrics <- filter(mdls_bs$metrics, G != "Overall")
        overall_metrics <- filter(mdls_bs$metrics, G == "Overall")
        
        mdls_bc <- lapply(seq_along(mdls_bs$methods), function(i) {
          dat_test <- dfs[[i]]
          dat_marg <- filter(overall_metrics, Method == mdls_bs$methods[i])
          ### Loop over groups
          gp_res <- lapply(unique(equity_metrics$G), function(g) {
            dat_bs <- filter(equity_metrics, Method == mdls_bs$methods[i], G == g)
            metrics_cilow <- unlist(as.vector(select(dat_bs, ci_low)))
            metrics_cihigh <- unlist(as.vector(select(dat_bs, ci_high)))
            metrics_mean <- unlist(as.vector(select(dat_bs, mean_est)))
            names(metrics_cilow) <- names(metrics_cihigh) <- names(metrics_mean) <- dat_bs$metric
            
            marg_cilow <- unlist(as.vector(select(dat_marg, ci_low)))
            marg_cihigh <- unlist(as.vector(select(dat_marg, ci_high)))
            marg_mean <- unlist(as.vector(select(dat_marg, mean_est)))
            names(marg_cilow) <- names(marg_cihigh) <- names(marg_mean) <- dat_marg$metric
            
            dat_gp <- select(dat_test, !!sym(g), Y, Yhat) %>%
              mutate(G_prob = !!sym(g)) %>% select(-!!sym(g))
            
            #### Get min/max valid epsilon, epsilon' combinations for each metric
            epsilon_minmax <- get_minmax_epsilon(data_gp = dat_gp, 
                                                 epsilon = seq(min(epsilon), max(epsilon), 0.01), 
                                                 epsilon_prime = seq(min(epsilon_prime), max(epsilon_prime), 0.01))
            
            #### Mean bias corrections
            mean_bclow <- get_epsilon_bc(data_gp = dat_gp, 
                                         metric_marg = marg_mean, 
                                         metric_vals = metrics_mean,
                                         epsilon = epsilon_minmax$epsilon$min_vals, 
                                         epsilon_prime = epsilon_minmax$epsilon_prime$max_vals, 
                                         param_3 = unlist(as.vector(select(param3_update, !!sym(g)))))
            mean_bchigh <- get_epsilon_bc(data_gp = dat_gp, 
                                          metric_marg = marg_mean, 
                                          metric_vals = metrics_mean,
                                          epsilon = epsilon_minmax$epsilon$max_vals, 
                                          epsilon_prime = epsilon_minmax$epsilon_prime$min_vals, 
                                          param_3 = unlist(as.vector(select(param3_update, !!sym(g)))))
            
            #### Low CI endpoint bias correction
            cilow_bc <- get_epsilon_bc(data_gp = dat_gp, 
                                       metric_marg = marg_cilow, 
                                       metric_vals = metrics_cilow,
                                       epsilon = epsilon_minmax$epsilon$min_vals, 
                                       epsilon_prime = epsilon_minmax$epsilon_prime$max_vals, 
                                       param_3 = unlist(as.vector(select(param3_update, !!sym(g)))))
            #### High CI endpoint bias correction
            cihigh_bc <- get_epsilon_bc(data_gp = dat_gp, 
                                        metric_marg = marg_cihigh, 
                                        metric_vals = metrics_cihigh,
                                        epsilon=epsilon_minmax$epsilon$max_vals, 
                                        epsilon_prime = epsilon_minmax$epsilon_prime$min_vals, 
                                        param_3 = unlist(as.vector(select(param3_update, !!sym(g)))))
            #### Combine results in dataframe
            gp_df <- data.frame(
              metric = dat_bs$metric,
              mean_est = dat_bs$mean_est
            )
            ##### Trim all at 0 and 1
            gp_df$bc_low <- trunc_01(cilow_bc[gp_df$metric])
            gp_df$bc_high <- trunc_01(cihigh_bc[gp_df$metric])
            gp_df$mean_low <- trunc_01(mean_bclow[gp_df$metric])
            gp_df$mean_high <- trunc_01(mean_bchigh[gp_df$metric])
            
            return(gp_df)
          })
          names(gp_res) <- unique(equity_metrics$G)
          gp_res %>% bind_rows(.id = "G")
        })
        names(mdls_bc) <- mdls_bs$methods
        full_bc <- mdls_bc %>% 
          bind_rows(.id = "Method") %>% 
          bind_rows(mutate(overall_metrics, bc_low = NA_real_, bc_high = NA_real_))
        
        return(list(tables = full_bc, is_probs = T))
      } else { return(list(tables = NULL, is_probs = T)) }
    } else { return(list(tables = mdls_bs$metrics, is_probs = F)) }
  })
  
  # Get equity and performance plots
  mdlsgps_acc_per_plt <- reactive({
    tables_list <- req(mdl_tables_bc())
    tables <- tables_list$tables
    if(!is.null(tables)) {
      eq_metric <- input$mdlsgps_equity
      per_metric <- input$mdlsgps_performance
      interface <- req(input$interface_opt_mdlsgps)
      
      equity.df <- tables %>% filter(metric == gsub("_prob", "", metric_funcs[[eq_metric]]), G!="Overall") %>%
        mutate(Method = stringr::str_wrap(Method, width=10),
               G_display = if_else(G=="Overall", G, str_split(G, "G_", simplify = T)[,2]),
               metric_display = eq_metric)
      
      perform.df <- tables %>% filter(G=="Overall", metric == gsub("_prob", "", metric_funcs[[per_metric]])) %>%
        mutate(
          Method = stringr::str_wrap(Method, width=10),
          metric_display = per_metric,
          G_display = G
        )
      
      ## Equity plots
      equity.df$label <- paste0(equity.df$G, equity.df$Method)
      num_mdls <- length(levels(as.factor(equity.df$Method)))
      
      if(interface=="Models") {
        ### Models: placeholder for calculating position_dodge final y values to enable hover on the final plot
        if(tables_list$is_probs) {
          equity.plt.placeholder <- ggplot(equity.df, aes(x = mean_est, y = Method, color = G_display, label = paste0(G, Method))) +
            geom_linerange(aes(xmin = mean_low, xmax = mean_high), lwd = 2, position=position_dodge2(width = .25, reverse = T)) +
            geom_errorbar(aes(xmin = bc_low, xmax = bc_high), width=0.05, alpha = 0.9, position=position_dodge2(width = .25, reverse = T)) +
            coord_cartesian(xlim = c(0,1))
        } else {
          equity.plt.placeholder <- ggplot(equity.df, aes(y = Method, color = G_display, label = paste0(G, Method))) +
            geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 6, lwd = 1, position=position_dodge2(width = .25, reverse = T)) +
            coord_cartesian(xlim = c(0,1))
        }
        equity_hover_df <- ggplot_build(equity.plt.placeholder)$data[[1]] %>%
          left_join(equity.df, by = "label")
        
        ### Models: display plot
        if(tables_list$is_probs) {
          equity.plt <- ggplot(equity_hover_df, aes(y = y, color = G_display)) +
            geom_linerange(aes(xmin = mean_low, xmax = mean_high), lwd = 2) +
            geom_errorbar(aes(xmin = bc_low, xmax = bc_high), width=0.05, alpha = 0.9) +
            coord_cartesian(xlim = c(0,1), ylim = c(0.5,num_mdls+0.5)) +
            labs(color = "Group", y = NULL, x = metric_labs[[eq_metric]], title = paste0(eq_metric, " by Group")) +
            theme_classic(base_size = 16) +
            theme(legend.position = 'left', panel.grid.major.x = element_line()) +
            #scale_color_manual(values = group_colors) +
            scale_color_viridis_d(end=0.95) +
            scale_y_continuous(breaks = seq(1, length(levels(as.factor(equity.df$Method)))), labels = levels(as.factor(equity.df$Method)))
        } else {
          equity.plt <- ggplot(equity_hover_df, aes(y = y, color = G_display)) +
            geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 6, lwd = 1) +
            coord_cartesian(xlim = c(0,1), ylim = c(0.5,num_mdls+0.5)) +
            labs(color = "Group", y = NULL, x = metric_labs[[eq_metric]], title = paste0(eq_metric, " by Group")) +
            theme_classic(base_size = 16) +
            theme(legend.position = 'left', panel.grid.major.x = element_line()) +
            #scale_color_manual(values = group_colors) +
            scale_color_viridis_d(end=0.95) +
            scale_y_continuous(breaks = seq(1, length(levels(as.factor(equity.df$Method)))), labels = levels(as.factor(equity.df$Method)))
        }
        
      } else if(interface=="Groups") {
        ### Groups: placeholder for calculating position_dodge
        if(tables_list$is_probs) {
          equity.plt.placeholder <- ggplot(equity.df, aes(x = mean_est, y = G_display, color = Method, label = paste0(G, Method))) +
            geom_linerange(aes(xmin = mean_low, xmax = mean_high), lwd = 2, position=position_dodge2(width = .25, reverse = T)) +
            geom_errorbar(aes(xmin = bc_low, xmax = bc_high), width=0.05, alpha = 0.9, position=position_dodge2(width = .25, reverse = T)) +
            coord_cartesian(xlim = c(0,1))
        } else {
          equity.plt.placeholder <- ggplot(equity.df, aes(y = G_display, shape = Method, color = G_display, label = paste0(G, Method))) +
            geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 8, lwd = 1, position=position_dodge2(width = .25, reverse = T)) +
            coord_cartesian(xlim = c(0,1))
        }
        equity_hover_df <- ggplot_build(equity.plt.placeholder)$data[[1]] %>%
          left_join(equity.df, by = "label")
        
        ### Groups: display plot
        if(tables_list$is_probs) {
          equity.plt <- ggplot(equity_hover_df, aes(y = y, color = Method)) +
            geom_linerange(aes(xmin = mean_low, xmax = mean_high), lwd = 2) +
            geom_errorbar(aes(xmin = bc_low, xmax = bc_high), width=0.05, alpha = 0.9) +
            coord_cartesian(xlim = c(0,1)) +
            labs(shape = "Model", color = "Group", y = NULL, x = metric_labs[[eq_metric]], title = paste0(eq_metric, " by Group")) +
            theme_classic(base_size = 16) +
            theme(legend.position = 'left', panel.grid.major.x = element_line()) +
            #scale_color_manual(values = model_colors) +
            scale_color_viridis_d(option = "plasma", end = 0.95) +
            scale_y_continuous(breaks = seq(1, length(levels(as.factor(equity.df$G_display)))), labels = levels(as.factor(equity.df$G_display))) +
            guides(color = guide_legend(reverse = T))
        } else {
          equity.plt <- ggplot(equity_hover_df, aes(y = y, shape = Method, color = G_display)) +
            geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 8, lwd = 1) +
            coord_cartesian(xlim = c(0,1)) +
            labs(shape = "Model", color = "Group", y = NULL, x = metric_labs[[eq_metric]], title = paste0(eq_metric, " by Group")) +
            theme_classic(base_size = 16) +
            theme(legend.position = 'left', panel.grid.major.x = element_line()) +
            scale_shape_manual(values = model_shapes) +
            #scale_color_manual(values = group_colors) +
            scale_color_viridis_d(end=0.95) +
            scale_y_continuous(breaks = seq(1, length(levels(as.factor(equity.df$G_display)))), labels = levels(as.factor(equity.df$G_display))) +
            guides(color = guide_legend(reverse = T),
                   shape = guide_legend(override.aes = list(size = .75 )))
        }
      }
      
      ## Performance plots
      perform.df$label <- paste0(perform.df$G, perform.df$Method)
      num_gps <- length(levels(as.factor(equity.df$G_display)))
      
      if(interface=="Models") {
        ### Models: placeholder plot -- use to keep variable names consistent with jittered plots for hover function
        perform.plt.placeholder <- ggplot(perform.df, aes(y = Method, label = paste0(G, Method))) +
          geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 4, lwd = 1) +
          coord_cartesian(xlim = c(0,1))
        
        perform_hover_df <- ggplot_build(perform.plt.placeholder)$data[[1]] %>%
          left_join(perform.df, by = "label")
        
        ### Models: display plot
        perform.plt <- ggplot(perform_hover_df, aes(y = y)) +
          geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 4, lwd = 1) +
          coord_cartesian(xlim = c(0,1),  ylim = c(0.5,num_mdls+0.5)) +
          labs(y = NULL, x = metric_labs[[per_metric]], title = paste0("Overall ", per_metric)) +
          theme_classic(base_size = 16) +
          theme(legend.position = 'none', panel.grid.major.x = element_line()) +
          scale_y_continuous(breaks = seq(1, length(levels(as.factor(equity.df$Method)))), labels = levels(as.factor(equity.df$Method)))
        
      } else if(interface=="Groups") {
        ### Groups: placeholder for calculating position_dodge
        if(tables_list$is_probs) {
          perform.plt.placeholder <- ggplot(perform.df, aes(y = G, color = Method, label = paste0(G, Method))) +
            geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 4, lwd = 1, position=position_dodge2(width = .25, reverse = T)) +
            coord_cartesian(xlim = c(0,1))
        } else {
          perform.plt.placeholder <- ggplot(perform.df, aes(y = G, shape = Method, label = paste0(G, Method))) +
            geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 4, lwd = 1, position=position_dodge2(width = .25, reverse = T)) +
            coord_cartesian(xlim = c(0,1))
        }
                
        perform_hover_df <- ggplot_build(perform.plt.placeholder)$data[[1]] %>%
          left_join(perform.df, by = "label") %>%
          #### Recalculate the middle of the y-axis and incorporate dodge positioning
          mutate(y_med = median(seq(1,num_gps)) + y-1)
        
        ### Display plot
        if(tables_list$is_probs) {
          perform.plt <- ggplot(perform_hover_df, aes(y = y_med, color = Method)) +
            geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 4, lwd = 1) +
            coord_cartesian(xlim = c(0,1), ylim = c(1,num_gps)) +
            labs(shape = "Model", y = NULL, x = metric_labs[[per_metric]], title = paste0("Overall ", per_metric)) +
            theme_classic(base_size = 16) +
            theme(legend.position = 'none', panel.grid.major.x = element_line()) +
            #scale_color_manual(values = model_colors) +
            scale_color_viridis_d(option = "plasma", end=0.95) +
            scale_y_continuous(breaks = median(seq(1,num_gps)), labels = "Overall")
        } else {
          perform.plt <- ggplot(perform_hover_df, aes(y = y_med, shape = Method)) +
            geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 4, lwd = 1) +
            coord_cartesian(xlim = c(0,1), ylim = c(1,num_gps)) +
            labs(shape = "Model", y = NULL, x = metric_labs[[per_metric]], title = paste0("Overall ", per_metric)) +
            theme_classic(base_size = 16) +
            theme(legend.position = 'none', panel.grid.major.x = element_line()) +
            scale_shape_manual(values = model_shapes) +
            scale_y_continuous(breaks = median(seq(1,num_gps)), labels = "Overall")
        }
      }
      
      return(list(equity.plt = equity.plt,
                  perform.plt = perform.plt,
                  equity.df = equity_hover_df,
                  perform.df = perform_hover_df,
                  is_probs = tables_list$is_probs))
    }
  })

  # Output: equity and performance plots
  output$mdlsgps_equity_plt <- renderPlot({
    objs <- req(mdlsgps_acc_per_plt())
    if(!is.null(objs)) {
      objs$equity.plt
    }
  })
  output$mdlsgps_per_plt <- renderPlot({
    objs <- req(mdlsgps_acc_per_plt())
    if(!is.null(objs)) {
      objs$perform.plt
    }
  })
  
  # Output: download versions of equity and performance plots
  output$mdlsgps_download <- downloadHandler(
    filename = "assessment_plts.png",
    content = function(file){
      objs <- req(mdlsgps_acc_per_plt())
      if(!is.null(objs)) {
        png(filename = file, width = 1000, height = 700)
        grid.arrange(objs$equity.plt, objs$perform.plt, ncol = 2, widths = c(1.2, 1))
        dev.off() 
      }
    }
  )
  
  # Output: equity and performance plot hovers
  output$mdlsgps_equity_hoverinfo <- renderText({
    hover <- input$mdlsgps_equity_hover
    objs <- req(mdlsgps_acc_per_plt())
    if(!is.null(objs)) {
      hover_eq_function(objs$equity.df, hover, objs$is_probs)
    }
  })
  output$mdlsgps_per_hoverinfo <- renderText({
    hover <- input$mdlsgps_per_hover
    objs <- req(mdlsgps_acc_per_plt())
    if(!is.null(objs)) {
      hover_perf_function(objs$perform.df, hover, objs$is_probs)
    }
  })
  
  # Output: download button
  output$show_mdlgp_plt_btn <- renderUI({
    if(!is.null(mdlsgps_acc_per_plt())){
      downloadButton("mdlsgps_download", "Download Plots", class = "download-btn")
    }
  })
  
  # Create datatable and fill with default param_3 values
  mdl_param3_data <- reactiveValues(
    data = data.frame(),
    updated = F)
  mdl_param3_table <- reactive({
    mdls_list <- req(mdl_objs())
    param3_default <- req(mdl_param3())
    eq_metric <- input$mdlsgps_equity
    if(mdls_list$is_probs & !is.null(param3_default)) {
      mdl_param3_data$data <- as.data.frame(param3_default[[1]])
    }
  })
  # Output: param_3 editable datatable
  output$mdls_t_param3 <- DT::renderDT({
    eq_metric <- input$mdlsgps_equity
    dat <- round(mdl_param3_data$data, 2)
    if(eq_metric == 'True Positive Rate'){
      DT::datatable(t(dat)[, 1, drop = FALSE], 
                    editable = TRUE, 
                    options = list(dom = 't', ordering = F),
                    colnames = c("Y=1"), 
                    escape = F)
    } else if(eq_metric == 'True Negative Rate'){
      DT::datatable(t(dat)[, 2, drop = FALSE], 
                    editable = TRUE, 
                    options = list(dom = 't', ordering = F),
                    colnames = c("Y=0"), 
                    escape = F)
    } else if(eq_metric == 'Positive Predictive Value'){
      DT::datatable(t(dat)[, 3, drop = FALSE], 
                    editable = TRUE, 
                    options = list(dom = 't', ordering = F),
                    colnames = c("Y&#770=1"), 
                    escape = F)
    } else if(eq_metric == 'Negative Predictive Value'){
      DT::datatable(t(dat)[, 4, drop = FALSE], 
                    editable = TRUE, 
                    options = list(dom = 't', ordering = F),
                    colnames = c("Y&#770=0"), 
                    escape = F)
    } else if(eq_metric == 'Accuracy'){
      DT::datatable(t(dat)[, 5, drop = FALSE], 
                    editable = TRUE, 
                    options = list(dom = 't', ordering = F),
                    colnames = c("All"), 
                    escape = F)
    } else if(eq_metric == 'Selection Rate'){
      DT::datatable(t(dat)[, 5, drop = FALSE], 
                    editable = TRUE, 
                    options = list(dom = 't', ordering = F),
                    colnames = c("All"), 
                    escape = F)
    }
  })
  # Capture edits to param_3 table
  observeEvent(input$mdls_t_param3_cell_edit, {
    eq_metric <- input$mdlsgps_equity
    ## Get values
    info = input$mdls_t_param3_cell_edit
    ## note the swapped rows and columns because we transpose for viewing
    j = as.numeric(info$row)
    if(eq_metric == 'True Positive Rate'){
      i = 1
    } else if(eq_metric == 'True Negative Rate'){
      i = 2
    } else if(eq_metric == 'Positive Predictive Value'){
      i = 3
    } else if(eq_metric == 'Negative Predictive Value'){
      i = 4
    } else if(eq_metric == 'Accuracy'){
      i = 5
    } else if(eq_metric == 'Selection Rate'){
      i = 5
    }
    k = as.numeric(info$value)
    ## Write values to reactive
    mdl_param3_data$data[i, j] <- k
  })
  # Validate param_3 table inputs and trigger new bias correction calculation when "Recalculate" button is clicked
  observeEvent(input$mdls_calc_param3, {
    eq_metric <- input$mdlsgps_equity
    ## default values
    param3_default <- req(mdl_param3())
    ## user input values
    dat <- isolate(mdl_param3_data$data)
    # Normalize values by row
    #dat <- round(dat / rowSums(dat), 2)
    #mdl_param3_data$data <- dat
    if(eq_metric == 'True Positive Rate'){
      i = 1
    } else if(eq_metric == 'True Negative Rate'){
      i = 2
    } else if(eq_metric == 'Positive Predictive Value'){
      i = 3
    } else if(eq_metric == 'Negative Predictive Value'){
      i = 4
    } else if(eq_metric == 'Accuracy'){
      i = 5
    } else if(eq_metric == 'Selection Rate'){
      i = 5
    }
    ## capture changes
    values_unchanged = which(dat[i, , drop = FALSE] == as.data.frame(param3_default[[1]])[i, , drop = FALSE])
    # calculate total change
    difference <- 1 - sum(dat[i, , drop = FALSE])
    # Normalize unchanged values (relative to prior value)
    adjusted_values = dat
    adjusted_values[i, values_unchanged] <- round(dat[i, values_unchanged] + 
                                                    (difference * dat[i, values_unchanged] / 
                                                       sum(dat[i, values_unchanged])), 
                                                  2)
    ## update
    mdl_param3_data$data <- adjusted_values
    # Recalculate bias correction
    mdl_param3_data$updated <- TRUE
  })
  # Reset param_3 table inputs when "Reset" button is clicked
   observeEvent(input$mdls_reset_param3, {
     param3_default <- req(mdl_param3())
     mdl_param3_data$data <- as.data.frame(param3_default[[1]])
   })
  
  # Output: group probability interface
  output$mdls_gpprob_menus <- renderUI({
    mdls_list <- req(mdl_objs())
    param3_dat <- req(mdl_param3_table())
    
    if(mdls_list$is_prob) {
      tags$div(
        tags$b("Group probability options"),
        tags$p("Use the menus below to analyze uncertainty resulting from the use of group probabilities. Sensitivity parameters describe the amount of group probability error accounted for in the analysis. For details, see the Descriptions tab."),
        tagList(
          tags$u("Sensitivity parameters"),
          tags$span(popify(
            actionButton(inputId = "mdls_epsilon_info", class = "gpprob_info", label = icon('info')), 
            title = "",
            content = HTML('<p>Wider parameter ranges correspond to settings with more error in group probabilities. See Descriptions tab for details.</p>'),
            trigger = "focus",
            placement = "bottom"
          ))
        ),
        sliderInput("mdls_epsilon", label = HTML("&#949"), min = -0.1, max = 0.1, value = c(-0.02, 0.02), step = 0.005),
        sliderInput("mdls_epsilonp", label = HTML("&#949'"), min = -0.1, max = 0.1, value = c(-0.02, 0.02), step = 0.005),
        tagList(
          tags$span(HTML("Group proportions within designated (sub)-population")),
          tags$span(popify(
            actionButton(inputId = "mdls_param3_info", class = "gpprob_info", label = icon('info')), 
            title = "",
            content = HTML('<p>These parameters may be estimated from population data or existing studies, as described in the Descriptions tab. Alternatively, use the provided values which are estimated from the uploaded data.</p>'),
            trigger = "focus",
            placement = "bottom"
          ))
        ),
        tags$p("Adjust the (sub)-population values by double clicking on the values in the table below. Unchanged values will automatically update to ensure all values sum to 1. (Slight discrepancies may appear due to rounding.)"),
        DT::DTOutput("mdls_t_param3"),
        actionButton("mdls_calc_param3", label = "Recalculate"),
        actionButton("mdls_reset_param3", label = "Reset")
      )
    }
  })

  ############################################CORRECTION: POST-PROCESSING TAB ########################
  # Read csv, validate data, and apply post-processing
  pp_objs <- reactive({
    inFile <- input$pp_data
    if (is.null(inFile))
      return(NULL)
    validate(need(endsWith(inFile$datapath,".csv"), "Please upload a .csv file."))
    input.data <- read.csv(inFile$datapath, header=T, sep=",")
    validateDataset(input.data)
    ## Determine if data has fixed or probabilistic groups. If fixed, expand G to G_* indicator variables
    data_is_probs <- data_G_Gprob <- F
    if('G' %in% colnames(input.data)) {
      ### If data also contains group probability columns, drop them and add to warning
      if(length(grep('G_', colnames(input.data), value = TRUE))>0) {
        data_G_Gprob <- T
        input.data <- select(input.data, -c(grep('G_', colnames(input.data), value = TRUE)))
      }
      G_mat <- model.matrix(~.-1, data = data.frame(G=as.factor(input.data$G)))
      colnames(G_mat) <- paste0("G_", unique(input.data$G))
      input.data <- bind_cols(input.data, G_mat) %>% select(-G)
    } else {
      data_is_probs <- T
    }
    ## Get names of G_* columns
    g_cols <- grep('G', colnames(input.data), value = TRUE)
    
    ## Get thresholds
    thresh <- rep(input$pp_base_thresh, length(g_cols))
    names(thresh) <- paste0(g_cols, "_thresh")
    data <- list(data = input.data, thresh = thresh)
    
    ## Post-processing
    stat.par.data <- apply_equal_stats_prob(data$data, eq_metric = "selrate_prob", base_thresh = input$pp_base_thresh)
    equal.odds.data <- apply_equalized_odds_prob(data$data, base_thresh = input$pp_base_thresh)
    equal.opp.data <- apply_equal_stats_prob(data$data, eq_metric = "fnr_prob", base_thresh = input$pp_base_thresh)
    eer.data <- apply_equal_stats_prob(data$data, eq_metric = "error_prob", base_thresh = input$pp_base_thresh)
    
    post.data <- list("Baseline"=data, "Statistical Parity PP"=stat.par.data, "Equalized Odds PP"=equal.odds.data, "Equalized Opp. PP"=equal.opp.data, 
                      "Equalized Error Rate PP"=eer.data)
    dfs <- lapply(post.data, "[[", "data")
    thresh <- unname(unlist(lapply(post.data, "[[", "thresh")))
    
    return(list(dfs = dfs, thresh = thresh, is_probs = data_is_probs, G_Gprob = data_G_Gprob))
  })
  
  # Output to display warning if data with both 'G' and group probability columns is uploaded
  output$pp_warn_Gprob <- renderText({
    G_Gprob <- req(pp_objs()$G_Gprob)
    if(!is.null(G_Gprob)) {
      paste0("Warning: data contain both 'G' and group probability ('G_') columns. Group probability columns have been dropped.")
    }
  })
  outputOptions(output, "pp_warn_Gprob", suspendWhenHidden=FALSE)
  
  # Output to display warning if any PP methods set all Yhat to same value
  output$pp_warn_nan <- renderText({
    dfs <- req(pp_objs()$dfs)
    dfs_nan <- unlist(lapply(dfs, function(d) {
      if(length(unique(d$Yhat))==1) { TRUE } else { FALSE }
    }))
    pp_nan <- names(dfs_nan)[which(dfs_nan)]
    
    if(length(pp_nan)>0) {
      paste0("Warning: post-processing method(s) ", knitr::combine_words(pp_nan), " set all predictions to a single value. Some equity and performance metrics will not calculate.")
    }
  })
  outputOptions(output, "pp_warn_nan", suspendWhenHidden=FALSE)
  
  # Calculate suggested third sensitivity parameter values for PP method
  pp_param3 <- reactive({
    objs <- req(pp_objs())
    dfs <- objs$dfs
    data_is_probs <- objs$is_probs
    
    if(data_is_probs) {
      # Calculate values for each model
      param3_list <- lapply(dfs, function(dat) {
        group_cols <- grep('G', colnames(dat), value = TRUE)
        apply(dat[,group_cols], 
              MARGIN = 2,
              FUN=get_param3,
              Y=dat$Y, 
              Yhat=dat$Yhat)
      })
      return(param3_list)
    } else { return(NULL) }
  })
  
  # Calculate equity and performance metrics on post-processed data
  pp_data_tables <- reactive({
    objs <- req(pp_objs())
    dfs <- objs$dfs
    thresh <- objs$thresh

    pro.methods <- c("Baseline", "Statistical Parity PP", "Equalized Odds PP", "Equalized Opp. PP", "Equalized Error Rate PP")

    # Make threshold df for plotting (thresholds are ordered in order of G_* columns)
    g_cols <- grep('G', colnames(dfs[[1]]), value = TRUE)
    thresh.df <- data.frame(Method = rep(pro.methods, each =length(g_cols)),
                            G = rep(str_split(g_cols, "G_", simplify = T)[,2], times = length(pro.methods)), 
                            Threshold = thresh)
    
    # Bootstrap equity and performance metrics
    return_metrics <- get_eq_per_uncertainty(dfs, pro.methods=pro.methods)
    return(list(metrics = return_metrics, methods = pro.methods, thresh.df = thresh.df))
  })
  # Add bias corrections if group probability data
  pp_tables_bc <- reactive({
    objs <- req(pp_objs())
    ## Get bootstrapped metrics
    pp_bs <- req(pp_data_tables())
    if(objs$is_probs) {
      ## Get param_3 values
      param3_init <- req(pp_param3_table())
      ### Set dependence on "Recalculate" button so this section will re-run when button is pressed
      pp_param3_data$updated
      param3_update <- isolate(pp_param3_data$data)
      ## Get epsilon, epsilon' values (dependence so this section will re-run if values change)
      epsilon <- input$pp_epsilon
      epsilon_prime <- input$pp_epsilonp
      if(!is.null(epsilon) & !is.null(epsilon_prime)) {
        ## Calculate bias corrections for each model
        equity_metrics <- filter(pp_bs$metrics, G!="Overall")
        overall_metrics <- filter(pp_bs$metrics, G=="Overall")
        
        pp_bc <- lapply(seq_along(pp_bs$methods), function(i) {
          dat_test <- objs$dfs[[i]]
          dat_marg <- filter(overall_metrics, Method==pp_bs$methods[i])
          ### Loop over groups
          gp_res <- lapply(unique(equity_metrics$G), function(g) {
            dat_bs <- filter(equity_metrics, Method==pp_bs$methods[i], G==g)
            metrics_cilow <- unlist(as.vector(select(dat_bs, ci_low)))
            metrics_cihigh <- unlist(as.vector(select(dat_bs, ci_high)))
            metrics_mean <- unlist(as.vector(select(dat_bs, mean_est)))
            names(metrics_cilow) <- names(metrics_cihigh) <- names(metrics_mean) <- dat_bs$metric
            
            marg_cilow <- unlist(as.vector(select(dat_marg, ci_low)))
            marg_cihigh <- unlist(as.vector(select(dat_marg, ci_high)))
            marg_mean <- unlist(as.vector(select(dat_marg, mean_est)))
            names(marg_cilow) <- names(marg_cihigh) <- names(marg_mean) <- dat_marg$metric
            
            dat_gp <- select(dat_test, !!sym(g), Y, Yhat) %>%
              mutate(G_prob = !!sym(g)) %>% select(-!!sym(g))
            
            #### Get min/max valid epsilon, epsilon' combinations for each metric
            epsilon_minmax <- get_minmax_epsilon(data_gp=dat_gp, epsilon = seq(min(epsilon), max(epsilon), 0.01), epsilon_prime = seq(min(epsilon_prime), max(epsilon_prime), 0.01))
            
            #### Mean bias corrections
            mean_bclow <- get_epsilon_bc(data_gp = dat_gp, metric_marg = marg_mean, metric_vals = metrics_mean,
                                         epsilon = epsilon_minmax$epsilon$min_vals, epsilon_prime=epsilon_minmax$epsilon_prime$max_vals, param_3 = unlist(as.vector(select(param3_update, !!sym(g)))))
            mean_bchigh <- get_epsilon_bc(data_gp = dat_gp, metric_marg = marg_mean, metric_vals = metrics_mean,
                                          epsilon = epsilon_minmax$epsilon$max_vals, epsilon_prime=epsilon_minmax$epsilon_prime$min_vals, param_3 = unlist(as.vector(select(param3_update, !!sym(g)))))
            
            #### Low CI endpoint bias correction
            cilow_bc <- get_epsilon_bc(data_gp = dat_gp, metric_marg = marg_cilow, metric_vals = metrics_cilow,
                                       epsilon = epsilon_minmax$epsilon$min_vals, epsilon_prime=epsilon_minmax$epsilon_prime$max_vals, param_3 = unlist(as.vector(select(param3_update, !!sym(g)))))
            #### High CI endpoint bias correction
            cihigh_bc <- get_epsilon_bc(data_gp = dat_gp, metric_marg = marg_cihigh, metric_vals = metrics_cihigh,
                                        epsilon=epsilon_minmax$epsilon$max_vals, epsilon_prime=epsilon_minmax$epsilon_prime$min_vals, param_3 = unlist(as.vector(select(param3_update, !!sym(g)))))
            #### Combine results in dataframe
            gp_df <- data.frame(
              metric = dat_bs$metric,
              mean_est = dat_bs$mean_est
            )
            ##### Trim all at 0 and 1
            gp_df$bc_low <- trunc_01(cilow_bc[gp_df$metric])
            gp_df$bc_high <- trunc_01(cihigh_bc[gp_df$metric])
            gp_df$mean_low <- trunc_01(mean_bclow[gp_df$metric])
            gp_df$mean_high <- trunc_01(mean_bchigh[gp_df$metric])
            
            return(gp_df)
          })
          names(gp_res) <- unique(equity_metrics$G)
          gp_res %>% bind_rows(.id = "G")
        })
        names(pp_bc) <- pp_bs$methods
        pp_full_bc <- pp_bc %>% bind_rows(.id = "Method") %>% bind_rows(
          mutate(overall_metrics, bc_low = NA_real_, bc_high = NA_real_)
        )
        
        return(list(tables = pp_full_bc, is_probs = T))
      } else { return(list(tables = NULL, is_probs = T)) }
    } else { return(list(tables = pp_bs$metrics, is_probs = F)) }
  })
  
  # Plot thresholds resulting from post-processing (display and download versions)
  thresholds <- reactive({
    objs <- req(pp_data_tables())
    data_objs <- req(pp_objs())
    data_baseline <- data_objs$dfs[[1]]
    thresh <- objs$thresh.df
    pp_processing_vec <- c("Baseline", input$pp_processing)
    
    ## Shape data for weighted group density plots
    data_baseline <- data_objs$dfs[["Baseline"]] %>% pivot_longer(
      starts_with("G_"), names_to = "Group", values_to = "weight"
    ) %>% mutate(G = gsub("G_","",Group))
    
    thresh_df <- thresh %>%
      filter(Method %in% pp_processing_vec)
    
    ## Threshold plot
    thresh.plt <- ggplot(data_baseline) +
      geom_density(aes(x = Prob, weight = weight)) +
      geom_vline(data = thresh_df, mapping = aes(xintercept = Threshold, color = Method), 
                 linewidth = 1) +
      scale_color_manual(values = method_colors) +
      facet_wrap(vars(G), ncol = 1, strip.position = "left") +
      labs(color = "Method", x = "Original data predicted probabilities", title = "Final group thresholds") + 
      theme_classic(base_size = 16) +
      theme(legend.position="right", strip.background = element_blank(),
            strip.text.y.left = element_text(size = 14, angle = 0),
            axis.line.y = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
    
    return(list(thresh.plt = thresh.plt,
                thresh_df = thresh_df))
  })

  # Get equity and performance plots
  pp_acc_per_plt <- reactive({
    tables_list <- req(pp_tables_bc())
    tables <- tables_list$tables
    if(!is.null(tables)) {
      eq_metric <- input$pp_equity
      per_metric <- input$pp_performance
      pp_processing_vec <- c("Baseline", input$pp_processing)
      
      equity.df <- tables %>% filter(metric == gsub("_prob", "", metric_funcs[[eq_metric]]), Method %in% pp_processing_vec,
                                     G!="Overall") %>%
        mutate(
          G_display = if_else(G=="Overall", G, str_split(G, "G_", simplify = T)[,2])
        )
      perform.df <- tables %>% filter(G=="Overall", metric == gsub("_prob", "", metric_funcs[[per_metric]]), Method %in% pp_processing_vec) %>%
        mutate(
          G_display = G
        )
      
      ## Equity plot
      ### Placeholder for calculating position_dodge final y values to enable hover on the final plot
      if(tables_list$is_probs) {
        equity.plt.placeholder <- ggplot(equity.df, aes(x = mean_est, y = G_display, color = Method, label = paste0(G, Method))) +
          geom_linerange(aes(xmin = mean_low, xmax = mean_high), lwd = 2, position=position_dodge2(width = .5, reverse = T)) +
          geom_errorbar(aes(xmin = bc_low, xmax = bc_high), width=0.05, alpha = 0.9, position=position_dodge2(width = .5, reverse = T)) +
          coord_cartesian(xlim = c(0,1))
      } else {
        equity.plt.placeholder <- ggplot(equity.df, aes(y = G_display, color = Method, label = paste0(G, Method))) +
          geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 6, lwd = 1, position=position_dodge2(width = .5, reverse = T)) +
          coord_cartesian(xlim = c(0,1))
      }
      equity.df$label <- paste0(equity.df$G, equity.df$Method)
      equity_hover_df <- ggplot_build(equity.plt.placeholder)$data[[1]] %>%
        left_join(equity.df, by = "label")
      
      ### Display plot
      if(tables_list$is_probs) {
        equity.plt <- ggplot(equity_hover_df, aes(y = y, x = mean_est, color = Method)) +
          geom_linerange(aes(xmin = mean_low, xmax = mean_high), lwd = 2) +
          geom_errorbar(aes(xmin = bc_low, xmax = bc_high), width=0.15, alpha = 0.9) +
          coord_cartesian(xlim = c(0,1)) +
          labs(color = "Method", y = NULL, x = metric_labs[[eq_metric]], title = paste0(eq_metric, " by Group")) +
          theme_classic(base_size = 16) +
          theme(legend.position = 'left', panel.grid.major.x = element_line()) +
          scale_color_manual(values = method_colors) +
          scale_y_continuous(breaks = seq(1, length(levels(as.factor(equity.df$G_display)))), labels = levels(as.factor(equity.df$G_display)))
      } else {
        equity.plt <- ggplot(equity_hover_df, aes(y = y, color = Method)) +
          geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 6, lwd = 1) +
          coord_cartesian(xlim = c(0,1)) +
          labs(color = "Method", y = NULL, x = metric_labs[[eq_metric]], title = paste0(eq_metric, " by Group")) +
          theme_classic(base_size = 16) +
          theme(legend.position = 'left', panel.grid.major.x = element_line()) +
          scale_color_manual(values = method_colors) +
          scale_y_continuous(breaks = seq(1, length(levels(as.factor(equity.df$G_display)))), labels = levels(as.factor(equity.df$G_display)))
      }
      
      ## Performance plot
      ### Placeholder for calculating position_dodge final y values to enable hover on the final plot
      perform.plt.placeholder <- ggplot(perform.df, aes(y = G, color = Method, label = paste0(G, Method))) +
        geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 6, lwd = 1, position=position_dodge2(width = 0.5, reverse = T)) +
        coord_cartesian(xlim = c(0,1))
      
      num_gps <- length(levels(as.factor(equity.df$G_display)))
      perform.df$label <- paste0(perform.df$G, perform.df$Method)
      perform_hover_df <- ggplot_build(perform.plt.placeholder)$data[[1]] %>%
        left_join(perform.df, by = "label") %>%
        #### Recalculate the middle of the y-axis and incorporate dodge positioning
        mutate(y_med = median(seq(1,num_gps)) + y-1)
      
      ### Display plot
      perform.plt <- ggplot(perform_hover_df, aes(y = y_med, color = Method)) +
        geom_pointrange(aes(x = mean_est, xmin = ci_low, xmax = ci_high), fatten = 6, lwd = 1) +
        coord_cartesian(xlim = c(0,1), ylim = c(1,num_gps)) +
        labs(color = "Method", y = NULL, x = metric_labs[[per_metric]], title = paste0("Overall ", per_metric)) +
        theme_classic(base_size = 16) +
        theme(legend.position = 'none', panel.grid.major.x = element_line()) +
        scale_color_manual(values = method_colors) +
        scale_y_continuous(breaks = median(seq(1,num_gps)), labels = "Overall")
      
      return(list(equity.plt = equity.plt,
                  perform.plt = perform.plt,
                  equity.df = equity_hover_df,
                  perform.df = perform_hover_df,
                  is_probs = tables_list$is_probs))
    }
  })

  # Output: threshold plot (with dynamic height)
  pp_threshold_ht <- reactiveVal(100)
  observe({
    data_objs <- req(pp_objs())
    num_gps <- ncol(select(data_objs$dfs[[1]], starts_with("G")))
    pp_threshold_ht(100*num_gps)
  })
  output$pp_threshold <- renderPlot({
    thresholds()$thresh.plt
  }, height = pp_threshold_ht)
  # Output: hover for threshold plot
  output$pp_threshold_hoverinfo <- renderText({
    hover <- input$pp_threshold_hover
    objs <- req(thresholds())
    
    hover_thresh_function(objs$thresh_df, hover)
  })
  
  # Output: equity/performance plots
  output$pp_equity_plt <- renderPlot({
    objs <- req(pp_acc_per_plt())
    objs$equity.plt
  })
  output$pp_per_plt <- renderPlot({
    objs <- req(pp_acc_per_plt())
    objs$perform.plt
  })

  # Output: equity/performance plots for download
  output$pp_download <- downloadHandler(
    filename = "correction_pp_plts.png",
    content = function(file){
      objs <- req(pp_acc_per_plt())
      png(file, width = 1000, height = 700)
      grid.arrange(objs$equity.plt, objs$perform.plt, ncol = 2, widths = c(1.2, 1))
      dev.off()
    }
  )
  # Output: threshold plots for download
  output$thresh_download <- downloadHandler(
    filename = "correction_thresh_plts.png",
    content = function(file){
      objs <- req(thresholds())
      png(file, width = 900, height = pp_threshold_ht()*1.5)
      grid.arrange(objs$thresh.plt)
      dev.off()
    }
  )
  # Output: equity and performance plot hovers
  output$pp_equity_hoverinfo <- renderText({
    hover <- input$pp_equity_hover
    objs <- req(pp_acc_per_plt())
    hover_eq_function(objs$equity.df, hover, objs$is_probs)
  })
  output$pp_per_hoverinfo <- renderText({
    hover <- input$pp_per_hover
    objs <- req(pp_acc_per_plt())

    hover_perf_function(objs$perform.df, hover, objs$is_probs)
  })
  # Output: post-processed data download (as Excel workbook)
  output$dwn_pp_data <- downloadHandler(
    filename = "correction_pp_data.xlsx",
    content = function(file){
      objs <- req(pp_objs())
      dfs <- objs$dfs
      t0s <- objs$t0s
      t1s <- objs$t1s
      objs_thresh <- req(pp_data_tables())
      thresh <- objs$thresh.df
      pp_processing_vec <- c("Baseline", input$pp_processing)
      pro.methods <- c("Baseline", "Statistical Parity PP", "Equalized Odds PP", "Equalized Opp. PP", "Equalized Error Rate PP")
      
      wb <- createWorkbook(file)
      lapply(1:length(pro.methods), function(num){
        if(pro.methods[num] %in% pp_processing_vec){
          addWorksheet(wb, pro.methods[num])
          writeData(wb, pro.methods[num], dfs[[num]], startRow = 1, startCol = 1)
        }
      })
      addWorksheet(wb, "Thresholds Used in Tool")
      writeData(wb, "Thresholds Used in Tool", thresh, startRow = 1, startCol = 1)
      saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )
  # Outputs: download buttons (data, equity/performance plots, threshold plot)
  output$show_pp_data_btn <- renderUI({
    if(!is.null(pp_objs())){
      downloadButton("dwn_pp_data", "Download Data", class = "download-btn")
    }
  })
  output$show_pp_plt_btn <- renderUI({
    if(!is.null(pp_acc_per_plt())){
      downloadButton("pp_download", "Subgroup, Overall Plots", class = "download-btn")
    }
  })
  output$show_thresh_plt_btn <- renderUI({
    if(!is.null(thresholds())){
      downloadButton("thresh_download", "Threshold Plot", class = "download-btn")
    }
  })
  
  # Create datatable and fill with default param_3 values
  pp_param3_data <- reactiveValues(
    data = data.frame(),
    updated = F)
  pp_param3_table <- reactive({
    objs <- req(pp_objs())
    param3_default <- req(pp_param3())
    if(objs$is_probs & !is.null(param3_default)) {
      pp_param3_data$data <- as.data.frame(param3_default[[1]])
    }
  })
  # Output: param_3 editable datatable
  output$pp_t_param3 <- DT::renderDT({
    dat <- round(pp_param3_data$data, 2)
    DT::datatable(dat, editable = TRUE, options = list(dom = 't'),
                  rownames = c("Y=1","Y=0","Y&#770=1","Y&#770=0", "All"), escape = F)
  })
  # Capture edits to param_3 table
  observeEvent(input$pp_t_param3_cell_edit, {
    ## Get values
    info = input$pp_t_param3_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    ## Write values to reactive
    pp_param3_data$data[i,j] <- k
  })
  # Validate param_3 table inputs and trigger new bias correction calculation when "Recalculate" button is clicked
  observeEvent(input$pp_calc_param3, {
    dat <- isolate(pp_param3_data$data)
    # Normalize values by row
    dat <- round(dat/rowSums(dat), 2)
    pp_param3_data$data <- dat
    # Recalculate bias correction
    pp_param3_data$updated <- T
  })
  
  # Output: group probability interface
  output$pp_gpprob_menus <- renderUI({
    objs <- req(pp_objs())
    param3_dat <- req(pp_param3_table())
    
    if(objs$is_prob) {
      tags$div(
        tags$b("Group probability options"),
        tags$p("Use the menus below to analyze uncertainty resulting from the use of group probabilities. Sensitivity parameters describe the amount of group probability error accounted for in the analysis. For details, see the Descriptions tab."),
        tagList(
          tags$u("Sensitivity parameters"),
          tags$span(popify(
            actionButton(inputId="pp_epsilon_info", class = "gpprob_info", label = icon('info')), 
            title = "",
            content = HTML('<p>Wider parameter ranges correspond to settings with more error in group probabilities. See Descriptions tab for details.</p>'),
            trigger = "focus",
            placement = "bottom"
          ))
        ),
        sliderInput("pp_epsilon", label=HTML("&#949"), min=-0.1, max=0.1, value=c(-0.02,0.02), step=0.005),
        sliderInput("pp_epsilonp", label=HTML("&#949'"), min=-0.1, max=0.1, value=c(-0.02,0.02), step=0.005),
        shinyBS::bsCollapse(id = "pp_collapse_param3",
          shinyBS::bsCollapsePanel(
            title = "Sensitivity parameters: more details",
            style = "primary",
            tagList(
              tags$span(HTML("Group proportions conditional on Y or Y&#770")),
              tags$span(popify(
                actionButton(inputId="pp_param3_info", class = "gpprob_info", label = icon('info')), 
                title = "",
                content = HTML('<p>These parameters may be estimated from population data or existing studies, as described in the Descriptions tab. Alternatively, use the provided values which are estimated from the uploaded data.</p>'),
                trigger = "focus",
                placement = "bottom"
              ))
            ),
            DT::DTOutput("pp_t_param3"),
            actionButton("pp_calc_param3", label = "Recalculate")
          )
        )
      )
    }
  })
  
  #############################################################CORRECTION: PRE PREPROCESSING TAB###############
  # Output: show data variable names in "Compare Original/Pre-Processed Variables" box
  output$var_types <- renderUI({
    ## Display current column types
    df <- req(pre_process_upload())
    if(!is.null(colnames(df))) {
      cov_cols <- colnames(df)[!(colnames(df)%in%c(grep('G', colnames(df), value = TRUE), 'Y','Prob','Yhat'))]
      cov_types <- sapply(cov_cols, function(c) class(df[[c]]))
      cov_types <- case_when(cov_types=="numeric" ~ "Continuous", cov_types=="factor" ~ "Categorical", TRUE ~ "Continuous")
      ## Buttons for selecting type, set to current type (types other than numeric/factor are read as numeric)
      html_list <- lapply(seq_along(cov_cols), function(c){
        HTML(paste0(cov_cols[[c]], ": ", radioButtons(paste0("vartype_",cov_cols[[c]]), label = NULL, choices = c("Continuous","Categorical"), inline = T,
                                                      selected = cov_types[[c]])))
      })
      html_list
    }
  })
  # Load and validate data, do initial attempt at identifying categorical covariates
  pre_process_upload <- reactive({
    inFile <- input$preprocess_data
    if (is.null(inFile))
      return(NULL)
    validate(need(endsWith(inFile$datapath,".csv"), "Please upload a .csv file."), errorClass = "test")
    df <- read.csv(inFile$datapath, header=T, sep=",")
    ## For now, only allow data with fixed group variable (no probabilities)
    validate(need('G' %in% colnames(df), "Fixed group variable ('G') needed to preprocess data."))
    ## Identify possible categorical covariates
    input.data <- covariate_types(df)
    return(input.data)
  })
  
  ## Flag for displaying an error if user tries to designate a variable with > 30 unique values as categorical
  var_cat_err <- reactiveValues(var = NULL, message = NULL)
  
  # Correct column types according to user input
  pre_process_covtypes <- reactive({
    data_raw <- req(pre_process_upload())
    
    if(!is.null(colnames(data_raw))) {
      cov_cols <- colnames(data_raw)[!(colnames(data_raw)%in%c(grep('G', colnames(data_raw), value = TRUE), 'Y','Prob','Yhat'))]
      for(c in cov_cols) {
        var_input <- input[[paste0("vartype_",c)]]
        if(!is.null(var_input)) {
          if(var_input=="Continuous") { 
            data_raw[[c]] <- as.numeric(as.character(data_raw[[c]])) 
          } else if(var_input=="Categorical") {
            # Reject input if number of unique values is > 30
            if(length(unique(data_raw[[c]])) > 30) {
              ## Display error message and leave variable as continuous
              var_cat_err$var <- c
            } else {
              data_raw[[c]] <- as.factor(as.character(data_raw[[c]])) 
            }
          }
        }
      }
      return(data_raw)
    } else { return(NULL) }
  })
  # Use observe to reset radio button to Continuous after the >30 values error
  observe({
    req(pre_process_covtypes())
    if(!is.null(isolate(var_cat_err$var))) {
      invalidateLater(500)
      isolate({
        ## Reset radio button
        updateRadioButtons(session, paste0("vartype_",var_cat_err$var), selected = "Continuous")
        ## Display disappearing notification
        showNotification(paste0("Error in ", var_cat_err$var, ": Cannot select categorical for variable with > 30 unique values."), 
                                duration = 5, closeButton = F, type = "error")
        ## Reset var_cat_err$var so we don't get repeating notifications
        var_cat_err$var <- NULL
      })
    }
  })
  
  # Get pre-processed data
  pre_process_data <- reactive({
    input.data <- req(pre_process_covtypes()) 
    if(!is.null(input.data)) {
      ## Pre-processing methods
      if(input$preprocess == "Johndrow and Lum (2019)"){
        out.data <- johndrow_lum_preprocess(input.data)
        file.name <- 'johndrow_lum_preprocessed_data.csv'
        output <- if(is.data.frame(out.data)) list(data = out.data, name = file.name) else list(message = out.data)
      }
      return(output)
    } else { return(NULL) }
  })
  # Shape data and create plots
  preproc_plt_data <- reactive({
    if(!is.null(input$select_var) & input$preprocess != "Select Method"){
      ## Get and shape data
      pre_data <- req(pre_process_data())$data
      org_data <- req(pre_process_covtypes())
      
      ## Discrete variable: original and pre-processed data plot
      if(plyr::is.discrete(pre_data[, input$select_var])){
        ### Reshape categorical variables to get proportions by group
        pre_cat <- pre_data %>%
          select(G, input$select_var) %>%
          pivot_longer(-G, names_to = "Variable", values_to = "Value") %>%
          group_by(G, Variable, Value) %>%
          summarize(n_cat = n()) %>%
          mutate(prop_cat = n_cat/sum(n_cat))
        org_cat <- org_data %>%
          select(G, input$select_var) %>%
          pivot_longer(-G, names_to = "Variable", values_to = "Value") %>%
          group_by(G, Variable, Value) %>%
          summarize(n_cat = n()) %>%
          mutate(prop_cat = n_cat/sum(n_cat))
        compare_df <- rbind(data.frame(Dataset = "Original Data", org_cat),
                         data.frame(Dataset = "Pre-Processed Data", pre_cat))  %>%
          filter(Variable==input$select_var) %>%
          mutate(G_display = as.factor(G),
                 #### Tooltip text variable is named "Group" because the variable name displays in the tooltip (looks like there's no way to turn this off)
                 Category = paste0(Value, "<br>Group: ", G_display, "<br>Proportion: ", round(prop_cat, 2)))
        
        plt <- ggplot(compare_df, aes(x = Value, y = prop_cat, fill = G_display, label = Category)) +
          geom_bar(position = "dodge", alpha = 0.7, stat = "identity") +
          #scale_fill_manual(values = group_colors) +
          scale_fill_viridis_d() +
          labs(x = input$select_var, fill = "Group", y = "Proportion of Group") + 
          theme_classic(base_size = 12) +
          facet_wrap(vars(Dataset), nrow = 1) +
          theme(legend.position="right", strip.background = element_blank(),
                strip.text = element_text(size = 14),
                axis.text = element_text(size = 10),
                legend.title = element_text(size = 12),
                panel.grid.major.y = element_line())
        
        plt_pltly <- ggplotly(plt, tooltip = "label") %>% config(displayModeBar = F) %>%
          layout(hoverlabel=list(bgcolor="#F5F5F5"))
        
        ### Download version of plot
        plt_download <- plt + theme_classic(base_size = 16) +
          theme(panel.grid.major.y = element_line(),
                strip.background = element_blank(),
                strip.text = element_text(size = 16))
        
        plt_discrete <- T
      } else {
        ## Continuous variable: original and pre-processed data plot
        compare_df <- rbind(data.frame(Dataset = "Original Data", select(org_data, G, input$select_var)),
                         data.frame(Dataset = "Pre-Processed Data", select(pre_data, G, input$select_var))) %>%
          mutate(Var1 = get(input$select_var)) %>%
          mutate(G_display = as.factor(G))
        ### Make placeholder plot to get exact boxplot values for tooltip
        plt_tooltip <- ggplot(compare_df, aes(x = G_display, y = Var1, fill = G_display)) + 
          geom_boxplot(alpha = 0.7) +
          facet_wrap(vars(Dataset), nrow = 1) +
          #scale_fill_manual(values = group_colors) +
          scale_fill_viridis_d() +
          labs(y = input$select_var, fill = "Group", x=NULL) + 
          theme_classic(base_size = 12) +
          theme(legend.position="right", strip.background = element_blank(),
                strip.text = element_text(size = 14),
                axis.text = element_text(size = 10),
                legend.title = element_text(size = 12),
                axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                panel.grid.major.y = element_line())
        
        ### Get values for hover
        plt_df <- ggplot_build(plt_tooltip)$data[[1]] %>%
          mutate(G_display = as.factor(group),
                 Dataset = if_else(PANEL == 1, "Original Data", "Pre-Processed Data"),
                 #### Tooltip text variable
                 Group = paste0(G_display, "<br>Median: ", round(middle, 2),
                                "<br>Q1: ", round(lower, 2),
                                "<br>Q3: ", round(upper, 2))) %>%
          select(G_display, Dataset, Group, new_width, ymin, lower, middle, upper, ymax) %>%
          pivot_longer(-c(G_display, Dataset, Group, new_width), names_to = "variable", values_to = "value")

        ### Add invisible column chart with tooltip (tooltip will not show up correctly using just the boxplot)
        plt <- plt_tooltip + geom_col(data = plt_df, aes(x = G_display, y = value, label = Group), alpha = 0,
                              width = min(plt_df$new_width), position = "identity")
        
        plt_pltly <- ggplotly(plt, tooltip = "label") %>% config(displayModeBar = F) %>%
          #### Remove hover for the boxplot (traces 1-6)
          style(hoverinfo = "none", traces = seq(1,6)) %>%
          layout(hoverlabel=list(bgcolor="#F5F5F5"))
       
        ### Download version of plot
        plt_download <- plt + theme_classic(base_size = 16) +
          theme(panel.grid.major.y = element_line(),
                strip.background = element_blank(),
                strip.text = element_text(size = 16))
        
        plt_discrete <- F
      }
      return(list(plt = plt_pltly,
                  plt_download = plt_download,
                  plt_discrete = plt_discrete))
    } else {
      return(NULL)
    }
  })
  
  # Show data variable names in "Compare Original/Pre-Processed Variables" box
  ## (use observeEvent to avoid duplicating error under this heading if data is uploaded with group probabilities)
  observeEvent(pre_process_data(), {
    df <- req(pre_process_data())
    if(!is.null(df)) {
      str <- paste0(selectInput("select_var", label = NULL, choices = case_when(
        is.data.frame(df$data) ~ colnames(df$data)[-which(colnames(df$data) %in% c("Y", "G"))],
        TRUE ~ "Select Method"
      )))
      # Output: show data variable names in "Compare Original/Pre-Processed Variables" box
      output$var_stats <- renderUI({
        HTML(str)
      })
    }
  })
  # Output: plots for selected pre-processed variable
  output$preproc_plt <- renderPlotly({
    if(!is.null(input$select_var)){
      if(input$select_var != "Select Method" & input$preprocess != "Select Method"){
        req(preproc_plt_data()$plt)
      }
    }
  })
  # Output: download pre-processed data
  output$dwn_preproc_data <- downloadHandler(
    filename = "correction_preproc_data.csv",
    content = function(filename){
      out.data <- req(pre_process_data())
      write.csv(out.data$data, file =  filename, row.names = F)
    }
  )
  # Output: download pre-processed variable plot
  output$preproc_download <- downloadHandler(
    filename = "correction_preproc_plts.png",
    content = function(file){
      objs <- req(preproc_plt_data())
      png(file, width = 1000, height = 700)
      grid.arrange(objs$plt_download)
      dev.off()
    }
  )
  # Output: buttons to download pre-processed data and plot
  output$show_preproc_data_btwn <- renderUI({
    if(input$preprocess != "Select Method"){
      objs <- req(pre_process_data())
      validate(need(length(objs) == 2, objs$message))
      downloadButton("dwn_preproc_data", "Download Data", class = "download-btn")
    }
  })
  output$show_preproc_plt_btn <- renderUI({
    if(input$preprocess != "Select Method"){
      objs <- req(pre_process_data())
      validate(need(length(objs) == 2, objs$message))
      downloadButton("preproc_download", "Download Plot", class = "download-btn")
    }
  })
  
  #####################################################################################################DEFINITIONS TAB###################
  # Outputs: typeset functions
  output$acc_form <- renderUI({
    withMathJax("$$P(Y = \\hat{Y})$$")
  })
  output$tp_rate <- renderUI({
    withMathJax("$$P(\\hat{Y} = 1 | Y = 1) = 1 - P(\\hat{Y} = 0 | Y = 1)$$")
  })
  output$tn_rate <- renderUI({
    withMathJax("$$P(\\hat{Y} = 0 | Y = 0) = 1 - P(\\hat{Y} = 1 | Y = 0)$$")
  })
  output$selrate_form <- renderUI({
    withMathJax("$$P(\\hat{Y} = 1)$$")
  })
  output$ppv_form <- renderUI({
    withMathJax("$$P(Y = 1 | \\hat{Y} = 1)$$")
  })
  output$npv_form <- renderUI({
    withMathJax("$$P(Y = 0 | \\hat{Y} = 0)$$")
  })
  output$optimization_form <- renderUI({
    withMathJax("$$\\underset{(\\theta_1, \\theta_2)}{minimize}$$")
  })
  output$err_form <- renderUI({
    withMathJax("$$P(Y  \\neq \\hat{Y} | G = g) = P(Y \\neq \\hat{Y})$$")
  })
  output$tn_bal_form <- renderUI({
    withMathJax("$$P(\\hat{Y} = 0 | Y = 0, G = g) = P(\\hat{Y} = 0 | Y = 0)$$")
  })
  output$tp_bal_form <- renderUI({
    withMathJax("$$P(\\hat{Y} = 1 | Y = 1, G = g) = P(\\hat{Y} = 1 | Y = 1)$$")
  })
  output$equal_opp_form <- renderUI({
    withMathJax("$$P(\\hat{Y} = 1 | Y = 1, G = g) = P(\\hat{Y} = 1 | Y = 1)$$")
  })
  output$sp_form <- renderUI({
    withMathJax("$$P(\\hat{Y} = 1 | G = 0) = P(\\hat{Y} = 1 | G = 1)$$")
  })
  
  # Output: example plot showing baseline and SP PP thresholds
  output$thresh_ex_plot <- renderPlot({
    load('rda_outputs/thresh_ex_plot.rda')
    grid.arrange(thresh_ex_plot, ncol = 1, widths = c(1.2))
  }, bg = "transparent")

  #####################################################DOWNLOAD CODE ###########################
  output$codeDownload <- downloadHandler(
    filename = function(){
      return("rand_ml_equity_tool.zip")
    },
    content = function(fname) {
      fs <- c()
      file_names <- c("pre_process_functions.R", "server.R", "stat_functions.R", "ui.R")
      for (i in 1:length(file_names)) {
        path <- file_names[i]
        fs <- c(fs, path)
      }
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )

  output$tutorialDownload <- downloadHandler(
    filename = function(){
      return("tool_tutorial.zip")
    },
    content = function(fname) {
      fs <- c()
      file_names <- c("data.csv", "tool_tutorial.pdf")
      for (i in 1:length(file_names)) {
        path <- file_names[i]
        fs <- c(fs, path)
      }
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
}

