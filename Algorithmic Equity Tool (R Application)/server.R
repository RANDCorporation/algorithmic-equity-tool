# list_of_packages = c("shinyWidgets","grid", "gridExtra", "openxlsx", "dplyr", "ggplot2")
#
# lapply(list_of_packages,
#        function(x) if(!require(x,character.only = TRUE)) install.packages(x))

library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(shiny)
library(grid)
library(gridExtra)
library(openxlsx)

source('stat_functions.R')
source('pre_process_functions.R')

validateDataset <- function(df){
  validate(need(all(c('Y', 'G', 'Prob', 'Yhat') %in% colnames(df)),
                "Please ensure file contains desired column names. Check 'Before Uploading Data' for desired column names."))
  validate(need(is.numeric(df$Prob), "Probability values need to be numeric."))
  validate(need(all(unique(df$Y) %in% c(0,1)), "Y values need to be binary."))
  validate(need(all(unique(df$Yhat) %in% c(0,1)), "Yhat values need to be binary."))
  validate(need(plyr::is.discrete(as.character(df$G)), "G values need to be discrete."))
}

validateDatasetMdl <- function(df){
  validate(need(all(c('Y', 'G', 'Yhat') %in% colnames(df)),
                "Please ensure file contains desired column names. Check 'Before Uploading Data' for desired column names."))
  validate(need(all(unique(df$Y) %in% c(0,1)), "Y values need to be binary."))
  validate(need(all(unique(df$Yhat) %in% c(0,1)), "Yhat values need to be binary."))
  validate(need(plyr::is.discrete(as.character(df$G)), "G values need to be discrete."))
}


#################shiny server code
server <- function(input, output, session) {
  #####################################################COMPARING MODELS TAB########################
  output$fileInputs <- renderUI({
    html_ui = " "
    for (i in 1:input$mdls_num){
      html_ui <- paste0(html_ui, fileInput(paste0("file",i), label=paste0("Model ",i), accept = ".csv"))
    }
    HTML(html_ui)
  })
  
  output$base_thresh <- renderUI({
    html_ui = " "
    for (i in 1:input$mdls_num){
      html_ui <- paste0(html_ui, column(3, numericInput(paste0("thresh",i), label=paste0("Model ",i), value = 0.5, min = 0, max = 1, step = 0.05)))
    }
    HTML(html_ui)
  })
  
  mdl.objs <- reactive({
    
    all.upload <- c(is.null(input$file1), is.null(input$file2))
    if(input$mdls_num >= 3){
      all.upload <- c(all.upload, is.null(input$file3))
    }
    if(input$mdls_num >= 4){
      all.upload <- c(all.upload, is.null(input$file4))
    }
    if(input$mdls_num == 5){
      all.upload <- c(all.upload, is.null(input$file5))
    }
    files <- list(input$file1, input$file2, input$file3, input$file4, input$file5)
    if(mean(all.upload) == 0){
      mdls <- lapply(1:input$mdls_num, function(num){
        inFile <- files[[num]]
        validate(need(endsWith(inFile$datapath,".csv"), "Please upload a csv file."))
        df <- read.csv(inFile$datapath, header=T, sep=",")
        validateDatasetMdl(df)
        return(list(data = df, thresh = NULL))
      })
    }
    else{
      return(NULL)
    }
    return(mdls)
  })
  
  mdls.processed <- reactive({
    mdls <- req(mdl.objs())
    dfs.sub <- lapply(mdls, "[[", "data")
    return(mdls)
  })
  
  mdl.tables <- reactive({
    mdls <- req(mdls.processed())
    dfs.sub <- lapply(mdls, "[[", "data")
    thresh <- unlist(lapply(mdls, "[[", "thresh"))
    pro.methods <- paste0("Model ",1:input$mdls_num)
    return(get_equity_performance(dfs.sub, pro.methods, thresh, input$mdls_equity, input$mdls_performance))
    
  })
  
  mdls_acc_per_plt <- reactive({
    tables <- req(mdl.tables())
    
    #####################plotting code
    tables$equity$G <- factor(tables$equity$G,levels = c("Overall" , unique(tables$equity$G)[-which(tables$equity$G == "Overall")])) #expand for more races
    equity.df <-tables$equity
    equity.df$Method <- factor(equity.df$Method, levels = sort(unique(equity.df$Method), decreasing = T))
    perform.df <- tables$performance
    perform.df$Method <- factor(perform.df$Method, levels = sort(unique(perform.df$Method), decreasing = T))
    
    
    equity.plt <- ggplot(equity.df,aes(x = Method , y = Value, fill = G)) +
      geom_col(alpha = 0.7, position = "dodge", width = 0.75) +
      labs(title = paste0(input$mdls_equity, " by G"),y = tables$eq.lab, x = "") + ylim(0,1) + #expand colors for multiple racs
      scale_fill_manual(values = c('#7fc97f', '#beaed4', '#fdc086', '#ffff99', '#386cb0')) +
      coord_flip() +theme_bw(base_size = 16)  +
      theme(legend.position="left", axis.text = element_text(size = 14),
            legend.text = element_text(size = 14),plot.title = element_text(size=16))
    
    equity.plt.download <- ggplot(equity.df,aes(x = Method , y = Value, fill = G)) +
      geom_col(alpha = 0.7, position = "dodge", width = 0.75) +
      labs(title = paste0(input$mdls_equity, " by G"),y = tables$eq.lab, x = "") + ylim(0,1) + #expand colors for multiple racs
      scale_fill_manual(values = c('#7fc97f', '#beaed4', '#fdc086', '#ffff99', '#386cb0')) +
      scale_x_discrete(breaks=c("Model 3","Model 2", "Model 1"),
                       labels=c("Pre-processed", "With proxy", "Without proxy")) +
      theme_bw(base_size = 16)  +
      theme(legend.position="bottom", axis.text = element_text(size = 14),
            legend.text = element_text(size = 14) ,plot.title = element_text(size=16))
    
    perform.plt <- ggplot(perform.df, aes(x = Method, y = Value),
                          height = 500) +
      geom_col(alpha = 0.7, fill = "#7fc97f", width = 0.5) +
      labs(title = paste0("Performance Metrics: ", input$mdls_performance), y = tables$per.lab, x= "")  +
      geom_text(aes(label=sprintf("%0.2f", Value)), position=position_dodge(width=0.9), hjust=-0.25, size = 8) + ylim(0,1) +
      scale_x_discrete(labels = NULL, breaks = NULL)  + #expand colors for multiple races
      coord_flip(clip = "off") + theme_bw(base_size = 16)  +
      scale_color_manual(name = "", values = "white") +
      theme(axis.text=element_text(size=14),plot.title = element_text(size=16))
    
    perform.plt.download <- ggplot(perform.df, aes(x = Method, y = Value),
                                   height = 500) +
      geom_col(alpha = 0.7, fill = "#7fc97f", width = 0.5) +
      geom_text(aes(label=sprintf("%0.2f", Value)), position=position_dodge(width=0.9), hjust=-0.25, size = 8) +
      labs(title = paste0("Performance Metrics: ", input$mdls_performance), y = tables$per.lab, x= "") + ylim(0,1) +
      scale_x_discrete()  + #expand colors for multiple races
      theme_bw(base_size = 16)  +
      scale_color_manual(name = "", values = "white") +
      scale_x_discrete(breaks=c("Model 3","Model 2", "Model 1"),
                       labels=c("Pre-processed", "With proxy", "Without proxy")) +
      theme(axis.text=element_text(size=14)) +
      theme(legend.position="bottom", axis.text = element_text(size = 14), legend.text = element_text(size = 14),
            plot.title = element_text(size=16))
    
    
    #eq.per.plts <- grid.arrange(equity.plt, perform.plt, ncol = 2, widths = c(1.4, 1.1))
    return(list(equity.plt = equity.plt,
                perform.plt = perform.plt,
                equity.plt.download = equity.plt.download,
                perform.plt.download = perform.plt.download))
  })
  
  
  ######shiny outputs
  output$mdls_acc_per_plt <- renderPlot({
    objs <- req(mdls_acc_per_plt())
    grid.arrange(objs$equity.plt, objs$perform.plt, ncol = 2, widths = c(1.5, 1.2))
    grid.rect(width = 1., height = 1., gp = gpar(lwd = 2, col = "#7fc97f", fill = NA))
  })
  
  
  #download plts
  output$mdls_download <- downloadHandler(
    filename = function() {
      return(ifelse(nchar(input$mdls_filename) == 0, "comparing_mdls_plts.png", paste0(input$mdls_filename, ".png")))
    },
    content = function(file){
      objs <- req(mdls_acc_per_plt())
      png(filename = file, width = 960, height = 960)
      grid.arrange(objs$equity.plt, objs$perform.plt, ncol = 2, widths = c(1.6, 1.1))
      grid.rect(width = 1., height = 1., gp = gpar(lwd = 2, col = "#7fc97f", fill = NA))
      dev.off()
    }
  )
  
  
  ############################################COMPARING METHODS TAB########################
  #get data frames including with post processing
  data.obj <- reactive({
    
    inFile <- input$data
    if (is.null(inFile))
      return(NULL)
    validate(need(endsWith(inFile$datapath,".csv"), "Please upload a .csv file."))
    input.data <- read.csv(inFile$datapath, header=T, sep=",")
    validateDataset(input.data)
    
    thresh <- rep(input$base_thresh, length(unique(input.data$G)))
    names(thresh) <- paste0(unique(input.data$G), "_thresh")
    data <- list(data = input.data, thresh = thresh)
    ####################post processing code
    stat.par.data <- apply_equal_stats(data$data, FUN = sp, base_thresh = input$base_thresh)
    equal.odds.data <- apply_equalized_odds(data$data, base_thresh = input$base_thresh)
    equal.opp.data <- apply_equal_stats(data$data, FUN = fnr, base_thresh = input$base_thresh)
    eer.data <- apply_equal_stats(data$data, FUN = error, base_thresh = input$base_thresh)
    
    post.data <- list(data, stat.par.data, equal.odds.data, equal.opp.data, eer.data)
    
    dfs <- lapply(post.data, "[[", "data")
    thresh <- unlist(lapply(post.data, "[[", "thresh"))
    
    return(list(dfs = dfs, thresh = thresh))
  })
  
  #get equity and performance metrics
  data.tables <- reactive({
    objs <- req(data.obj())
    dfs <- objs$dfs
    thresh <- objs$thresh
    
    pro.methods <- c("Baseline", "Statistical Parity PP", "Equalized Odds PP", "Equalized Opp. PP", "Equalized Error Rate PP")
    
    return(get_equity_performance(dfs, pro.methods, thresh, input$equity, input$performance))
  })
  
  
  #get thresholds by group plt
  thresholds <- reactive({
    objs <- req(data.tables())
    thresh <- objs$thresh.df
    
    thresh.plt <- ggplot(filter(thresh, Method %in% input$processing), aes(x = Method, y = Threshold, fill = as.factor(G))) + geom_col(position = "dodge", alpha = 0.7, width = 0.75) +
      labs(title = "Thresholds by G per Method", x = "", y = "Threshold")  + ylim(0,1) +
      scale_fill_manual(values = c('#7fc97f', '#beaed4', '#fdc086', '#ffff99')) + theme_bw(base_size = 16) +
      theme(legend.position="none", axis.text = element_text(size = 14), legend.text = element_text(size = 14))
    
    thresh.plt.download <- ggplot(filter(thresh, Method %in% input$processing), aes(x = Method, y = Threshold, fill = as.factor(G))) + geom_col(position = "dodge", alpha = 0.7, width = 0.75) +
      labs(title = "Thresholds by G per Method", x = "", y = "Threshold")  + ylim(0,1) +
      scale_fill_manual(values = c('#7fc97f', '#beaed4', '#fdc086', '#ffff99')) + theme_bw(base_size = 16) +
      theme(legend.position="bottom",
            axis.text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.text = element_text(size = 14))
    labs(fill = "G")
    
    thresh.plts <- grid.arrange(thresh.plt)
    
    return(list(thresh.plts = thresh.plts,
                thresh.plt = thresh.plt,
                thresh.plt.download = thresh.plt.download))
  })
  
  #get equity and performance plt
  acc_per_plt <- reactive({
    
    tables <- req(data.tables())
    #####################plotting code
    tables$equity$G <- factor(tables$equity$G,levels = c("Overall",unique(tables$equity$G)[-which(tables$equity$G == "Overall")])) #edit here for multiple races
    equity.df <- filter(tables$equity, Method %in% input$processing)
    perform.df <- filter(tables$performance, Method %in% input$processing)
    
    equity.plt <- ggplot(equity.df,aes(x = Method, y = Value, fill = G)) +
      geom_col(alpha = 0.7, position = "dodge", width = 0.75) +
      labs(title = paste0(input$equity, " by G"),y = tables$eq.lab, x = "") + ylim(0,1) + #edit colors for multiple races
      scale_fill_manual(values = c('#7fc97f', '#beaed4', '#fdc086', '#ffff99', '#386cb0')) +
      coord_flip() +theme_bw(base_size = 16) +
      theme(legend.position="left", axis.text = element_text(size = 14), legend.text = element_text(size = 14)
            ,plot.title = element_text(size=16))
    
    equity.plt.download <- ggplot(equity.df,aes(x = Method, y = Value, fill = G)) +
      geom_col(alpha = 0.7, position = "dodge", width = 0.75) +
      labs(title = paste0(input$equity, " by G"),y = tables$eq.lab, x = "") + ylim(0,1) + #edit colors for multiple races
      scale_fill_manual(values = c('#7fc97f', '#beaed4', '#fdc086', '#ffff99', '#386cb0')) +
      theme_bw(base_size = 16) +
      theme(legend.position="bottom",
            axis.text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.text = element_text(size = 14),plot.title = element_text(size=16))
    
    perform.plt <- ggplot(perform.df, aes(x = Method, y = Value),
                          height = 500) +
      geom_col(alpha = 0.7, fill = "#7fc97f", width = 0.5) +
      geom_text(aes(label=sprintf("%0.2f", Value)), position=position_dodge(width=0.9), hjust=-0.25, size = 8) +
      labs(title = paste0("Performance Metrics: ", input$performance), y = tables$per.lab, x= "") + ylim(0,1) +
      scale_x_discrete(labels = NULL, breaks = NULL)  +
      coord_flip(clip = "off") + theme_bw(base_size = 16)  +
      scale_color_manual(name = "", values = "white") +
      theme(axis.text=element_text(size=14),plot.title = element_text(size=16))
    
    perform.plt.download <- ggplot(perform.df, aes(x = Method, y = Value),
                                   height = 500) +
      geom_col(alpha = 0.7, fill = "#7fc97f", width = 0.5) +
      geom_text(aes(label=sprintf("%0.2f", Value)), position=position_dodge(width=0.9), hjust=-0.25, size = 8) +
      labs(title = paste0("Performance Metrics: ", input$performance), y = tables$per.lab, x= "") + ylim(0,1) +
      theme_bw(base_size = 16)  +
      scale_color_manual(name = "", values = "white") +
      theme(legend.position="bottom",
            axis.text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.text = element_text(size = 14),plot.title = element_text(size=16))
    
    return(list(equity.plt = equity.plt,
                perform.plt = perform.plt,
                equity.plt.download = equity.plt.download,
                perform.plt.download = perform.plt.download))
    #eq.per.plts <- grid.arrange(equity.plt, perform.plt, ncol = 2, widths = c(1.6, 1.1))
  })
  
  
  ######################shiny outputs
  
  #output differences plt to ui
  output$ratios <- renderPlot({
    ratios()
  })
  
  #output thresholds plt to ui
  output$threshold <- renderPlot({
    if(length(input$processing) > 0){
      thresholds()$thresh.plts
    }
    grid.rect(width = 1., height = 1., gp = gpar(lwd = 2, col = "#7fc97f", fill = NA))
  })
  
  #output equity/performance plts to ui
  output$acc_per_plt <- renderPlot({
    if(length(input$processing) > 0){
      objs <- req(acc_per_plt())
      eq.per.plts <- grid.arrange(objs$equity.plt, objs$perform.plt, ncol = 2, widths = c(1.6, 1.1))#acc_per_plt()
    }
    grid.rect(width = 1., height = 1., gp = gpar(lwd = 2, col = "#7fc97f", fill = NA))
  })
  
  #download plts
  output$download <- downloadHandler(
    filename = function() {
      return(ifelse(nchar(input$filename) == 0, "comparing_methods_plts.png", paste0(input$filename, ".png")))
    },
    content = function(file){
      objs <- req(acc_per_plt())
      eq.per.plts <- grid.arrange(objs$equity.plt, objs$perform.plt, ncol = 2, widths = c(1.6, 1.1))
      png(file, width = 960, height = 960) #, width = 960, height = 960
      grid.arrange(thresholds()$thresh.plt, eq.per.plts, nrow = 2) #, acc_per_plt(), nrow= 1
      dev.off()
    }
  )
  
  #########################################################DOWNLOAD DATA TAB############
  
  output$show_mdl_plt_btn <- renderUI({
    if(!is.null(mdls_acc_per_plt())){
      downloadButton("mdls_download", "Download Plots")
    }
  })
  
  output$show_method_data_btn <- renderUI({
    if(!is.null(data.obj())){
      downloadButton("dwn_post_mdls", "Download Data")
    }
  })
  
  output$show_method_plt_btn <- renderUI({
    if(!is.null(acc_per_plt())){
      downloadButton("download", "Download Plots")
    }
  })
  
  output$mdls_sub_metrics <- renderUI({
    html_text <- paste0(h4(paste0("Subgroup Performance Metrics Selected: ", input$mdls_equity)))
    HTML(html_text)
  })
  
  output$mdls_all_metrics <- renderUI({
    html_text <- paste0(h4(paste0("Overall Performance Metrics Selected: ", input$mdls_performance)))
    HTML(html_text)
  })
  
  output$sub_metrics <- renderUI({
    html_text <- paste0(h4(paste0("Subgroup Performance Metrics Selected: ", input$equity)))
    HTML(html_text)
  })
  
  output$all_metrics <- renderUI({
    html_text <- paste0(h4(paste0("Overall Performance Metrics Selected: ", input$performance)))
    HTML(html_text)
  })
  
  output$post_mdls <- renderUI({
    html_text <- paste0(h4(paste0("Post Processing Methods Selected: ", paste(input$processing, collapse = ", "))))
    HTML(html_text)
  })
  
  output$dwn_post_mdls <- downloadHandler(
    filename = function(){
      return(ifelse(nchar(input$data_filename) == 0,"post_process_methods.xlsx", paste0(input$filename, ".xlsx")))
    },
    content = function(file){
      objs <- req(data.obj())
      dfs <- objs$dfs
      t0s <- objs$t0s
      t1s <- objs$t1s
      objs <- req(data.tables())
      thresh <- objs$thresh.df
      
      pro.methods <- c("Baseline", "Statistical Parity PP", "Equalized Odds PP", "Equalized Opp. PP", "Equalized Error Rate PP")
      
      
      wb <- createWorkbook(file)
      lapply(1:length(pro.methods), function(num){
        if(pro.methods[num] %in% input$processing){
          addWorksheet(wb, pro.methods[num])
          writeData(wb, pro.methods[num], dfs[[num]], startRow = 1, startCol = 1)
        }
      })
      addWorksheet(wb, "Thresholds Used in Tool")
      writeData(wb, "Thresholds Used in Tool", thresh, startRow = 1, startCol = 1)
      saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )
  
  #############################################################PRE PREPROCESSING TAB###############
  
  output$show_preprocess_btwn <- renderUI({
    if(input$preprocess != "Select Method"){
      objs <- req(pre_process_data())
      validate(need(length(objs) == 2, objs$message))
      downloadButton("dwn_preprocess", "Download Data")
    }
  })
  
  pre_process_data <- reactive({
    inFile <- input$preprocess_data
    if (is.null(inFile))
      return(NULL)
    if (!endsWith(inFile$datapath,".csv")) return(list(message = "Please upload a .csv file.")) #show this error
    input.data <- read.csv(inFile$datapath, header=T, sep=",")
    if( !('G' %in% colnames(input.data))) return(list(message = "'G' variable needed to preprocess data.")) #show this error message
    #various preprocessing methods
    if(input$preprocess == "Johndrow and Lum (2019)"){
      out.data <- johndrow_lum_preprocess(input.data)
      file.name <- 'johndrow_lum_preprocessed_data.csv'
      output <- if(is.data.frame(out.data)) list(data = out.data, name = file.name) else list(message = out.data)
    }
    return(output)
  })
  
  output$var_stats <- renderUI({
    df <- req(pre_process_data())
    if("data" %in% names(df)){
      str <- paste0(selectInput("select_var", label = NULL, choices = case_when(
        is.data.frame(df$data) ~ colnames(df$data)[-which(colnames(df$data) %in% c("Y", "G"))],
        TRUE ~ "Select Method"
      )))
      HTML(str)
    }
  })
  
  output$var_output <- renderUI({
    if(!is.null(input$select_var)){
      if(input$select_var != "Select Method" & input$preprocess != "Select Method"){
        pre_data <- req(pre_process_data())$data
        if(plyr::is.discrete(pre_data[, input$select_var])){
          plotOutput("var_table") #can't get discrete option to work currently
        }
        else{
          plotOutput("var_plot")
        }
      }
    }
  })
  
  output$var_table <- renderPlot({
    if(!is.null(input$select_var) & input$preprocess != "Select Method"){
      pre.data <- req(pre_process_data())$data
      org.data <- read.csv(input$preprocess_data$datapath, header=T, sep=",")
      
      org.plt <- ggplot(org.data, aes(x = get(input$select_var), fill = as.factor(G))) +
        geom_bar(position = "dodge", alpha = 0.7, aes(y = ..count../sum(..count..))) +
        scale_fill_manual(values = c("#004080", "#008080", "#800040", "#408000", "#804000")) +
        labs(x = input$select_var, fill = "G", title = "Original Data", y = "Proportion of Observations") + theme_bw(base_size = 16) +
        theme(legend.position="left",
              axis.text = element_text(size = 14),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.text = element_text(size = 14))
      pre.plt <- ggplot(org.data, aes(x = get(input$select_var), fill = as.factor(G))) +
        geom_bar(position = "dodge", alpha = 0.7, aes(y = ..count../sum(..count..))) +
        scale_fill_manual(values = c("#004080", "#008080", "#800040", "#408000", "#804000")) +
        labs(x = input$select_var, y = "", fill = "G", title = "Pre-Processed Data") + theme_bw(base_size = 16) +
        theme(legend.position = "none",
              axis.text = element_text(size = 14),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.text = element_text(size = 14))
      grid.arrange(org.plt, pre.plt, ncol = 2)
      grid.rect(width = 1., height = 1., gp = gpar(lwd = 2, col = "#7fc97f", fill = NA))
    }
  })
  
  output$var_plot <- renderPlot({
    if(!is.null(input$select_var) & input$preprocess != "Select Method"){
      pre.data <- req(pre_process_data())$data
      org.data <- read.csv(input$preprocess_data$datapath, header=T, sep=",")
      compare <- rbind(data.frame(Dataset = "Original", org.data[,c("G", input$select_var)]),
                       data.frame(Dataset = "Pre-Processed", pre.data[,c("G", input$select_var)]))
      plt <- ggplot(compare, aes(x = Dataset, y = get(input$select_var), fill = as.factor(G))) + geom_boxplot(alpha = 0.7)  +
        scale_fill_manual(values = c('#7fc97f', '#beaed4', '#fdc086', '#ffff99', '#386cb0')) +
        labs(y = input$select_var, fill = "G") + theme_bw(base_size = 16) +
        theme(legend.position="right",
              axis.text = element_text(size = 14),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.text = element_text(size = 14))
      grid.arrange(plt)
      grid.rect(width = 1., height = 1., gp = gpar(lwd = 2, col = "#7fc97f", fill = NA))
    }
  })
  
  output$dwn_preprocess <- downloadHandler(
    filename = function(){
      out.data <- req(pre_process_data())
      return(out.data$name)
    },
    content = function(filename){
      out.data <- req(pre_process_data())
      write.csv(out.data$data, file =  filename, row.names = F)
    }
  )
  #####################################################################################################DEFINITIONS TAB###################
  output$acc_form <- renderUI({
    withMathJax("$$P(Y = \\hat{Y})$$")
  })
  output$fn_rate <- renderUI({
    withMathJax("$$P(\\hat{Y} = 0 | Y = 1) = 1 - P(\\hat{Y} = 1 | Y = 1)$$")
  })
  output$fp_rate <- renderUI({
    withMathJax("$$P(\\hat{Y} = 1 | Y = 0) = 1 - P(\\hat{Y} = 0 | Y = 0)$$")
  })
  output$ppr_form <- renderUI({
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
  output$fp_bal_form <- renderUI({
    withMathJax("$$P(\\hat{Y} = 1 | Y = 0, G = g) = P(\\hat{Y} = 1 | Y = 0)$$")
  })
  output$fn_bal_form <- renderUI({
    withMathJax("$$P(\\hat{Y} = 0 | Y = 1, G = g) = P(\\hat{Y} = 0 | Y = 1)$$")
  })
  output$equal_opp_form <- renderUI({
    withMathJax("$$P(\\hat{Y} = 1 | Y = 1, G = g) = P(\\hat{Y} = 1 | Y = 1)$$")
  })
  output$sp_form <- renderUI({
    withMathJax("$$P(\\hat{Y} = 1 | G = 0) = P(\\hat{Y} = 1 | G = 1)$$")
  })
  # output$johndrow_exp <- renderUI({
  #   withMathJax(#IRINEO: FILL IN HERE)
  # })
  
  #####################################################DOWNLOAD CODE TAB###########################
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

