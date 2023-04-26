library(shiny)
library(shinyWidgets)

#note, dev branch has 3 cosmetic chaneges: overall color bar plot, threshold label, remove threshold legend

ui <- navbarPage(
  title = "RAND Algorithmic Equity Tool",
  theme = shinythemes::shinytheme("flatly"),
  # add this
  tags$head(tags$style(HTML('.navbar-static-top {background-color: #1475AD;}',
                            '.navbar-default .navbar-nav>.active>a {background-color: #1475AD;}'))),
  #.navbar-default {
  #  background-color: #b1b1b3 !important;
  #}
  position = c("fixed-top"),
  collapsible = TRUE,
  tabPanel(
    "About",
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    h4("About this tool."
    ),
    tags$br(),
    h3("Related Links"),
    h4("Accompanying research report:"),
    h4("GitHub repository with source code and tutorial:"),
    tags$br(),
  ),
  tabPanel(
    "Comparing Models",
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$head(
      tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
        font-size:20px;
      }
    "))
    ),
    sidebarLayout(
      sidebarPanel(style = "position: fixed; width: 30%;",
                   tags$br(),
                   setBackgroundColor(
                     color = c( "#f2fafd")
                   ),
                   tags$style(".well {
                    background-color:	#fcfcfc;
                    border-width: 1px;
                    border-color: #1475AD;
                    height: 85vh; 
                    overflow-y: auto;
                    color: #000000;
                   }"),
                   tags$b("Comparing Equity Statistics on Different Models"),
                   tags$div("Use this tab to compare equity statistics between different models. Choose different options for comparing
                          group level and overall statistics for each model.
                          For discussion of the methods please see the Definitions tab. To download outputs see the 'Download Outputs' tab."),
                   tags$br(),
                   tags$b("Before Uploading Data"),
                   tags$br(),
                   tags$i("The following variables must be included and labelled as such for each dataset"),
                   tags$ul(
                     tags$li("Y: Ground truth (as binary, 0/1) of observation"),
                     tags$li("G: G of observation (as categories)"),
                     tags$li("Prob: Predicted probability of observation"),
                     tags$li("Yhat: Predicted outcome (as binary, 0/1) of observation"),
                   ),
                   radioButtons("mdls_num", "How many models will you compare?",
                                choices = list(2,3,4,5),selected = 2, inline = TRUE),
                   uiOutput("fileInputs"),
                   selectInput(inputId = "mdls_equity", label = "Subgroup Performance Metrics (y axis)",
                               choices = c("Accuracy", "Statistical Parity", "False Positive Rate","False Negative Rate","Positive Predictive Value", "Negative Predictive Value")),
                   selectInput(inputId = "mdls_performance", label = "Overall Performance Metrics (y axis)",
                               choices = c("Overall Accuracy", "False Positive Rate", "False Negative Rate", "Positive Predictive Value", "Negative Predictive Value")),
                   
      ),
      
      mainPanel(
        tags$head(tags$style("#mdls_acc_per_plt{height:75vh !important;}")),
        shinycssloaders::withSpinner(plotOutput("mdls_acc_per_plt", width = "100%")),
        tags$br()
        
      ),
      
    )
  ),
  
  tabPanel(
    "Comparing Post-Processing Methods",
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    sidebarLayout(
      sidebarPanel(style = "position: fixed; width: 30%;",
                   tags$br(),
                   setBackgroundColor(
                     color = c( "#f2fafd")
                   ),
                   tags$style(".well {
                    background-color:	#fcfcfc;
                    border-width: 1px;
                    border-color: #1475AD;
                    height: 85vh; 
                    overflow-y: auto;
                    color: #000000;
                   }"),
                   tags$b("Comparing Different Post-Processing Methods on a Specific Model"),
                   tags$div("Upload a dataset to then compare different post-processing methods being applied to it. Toggle between any or all methods, and the chosen group-level and 
                            overall statistics being applied to the post-processed data. For discussion of the methods please see the Definitions tab. To download the plots or post-processed data 
                            see the 'Download Outputs' tab."),
                   tags$br(),
                   tags$div("Additional Notes:"),
                   tags$div("1) At least one post processing method must be selected for plots to be generated."),
                   tags$div("2) Plots may take a minute or two to generate for each file initially due to the optimization algorithms used to determine thresholds."),
                   tags$br(),
                   tags$b("Before Uploading Data"),
                   tags$br(),
                   tags$i("The following variables must be included and labelled as such"),
                   tags$ul(
                     tags$li("Y: Ground truth (as binary, 0/1) of observation"),
                     tags$li("G: G of observation (as categories)"),
                     tags$li("Prob: Predicted probability of observation"),
                     tags$li("Yhat: Predicted outcome (as binary, 0/1) of observation"),
                   ),
                   fileInput("data", "Upload Data", accept = '.csv'),
                   sliderInput("base_thresh", "Model's Original Threshold", value = 0.5, min = 0, max = 1, step = 0.05),
                   checkboxGroupInput("processing", label = "Select Post-Processing Methods (x axis)",
                                      choices = c("Baseline", "Statistical Parity PP", "Equalized Odds PP", "Equalized Opp. PP", "Equalized Error Rate PP"),
                                      selected = "Baseline"),
                   selectInput(inputId = "equity", label = "Subgroup Performance Metrics (y axis)",
                               choices = c("Accuracy", "Statistical Parity", "False Positive Rate","False Negative Rate", "Positive Predictive Value", "Negative Predictive Value")),
                   selectInput(inputId = "performance", label = "Overall Performance Metrics (y axis)",
                               choices = c("Overall Accuracy", "False Positive Rate", "False Negative Rate", "Positive Predictive Value", "Negative Predictive Value"))
                   
      ),
      
      mainPanel(
        
        tags$head(tags$style("#threshold{height:30vh !important;}")),
        tags$head(tags$style("#acc_per_plt{height:55vh !important;}")),
        shinycssloaders::withSpinner(plotOutput("threshold", height = "400px")), 
        tags$br(),
        shinycssloaders::withSpinner(plotOutput("acc_per_plt", height = "800px")),
        tags$br()
        
      ),
      
    )
  ),
  tabPanel(
    "Generate Pre-Processed Data",
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    sidebarLayout(
      sidebarPanel(style = "position: fixed; width: 30%;",
                   tags$br(),
                   setBackgroundColor(
                     color = c( "#f2fafd")
                   ),
                   tags$style(".well {
                    background-color:	#fcfcfc;
                    border-width: 1px;
                    border-color: #1475AD;
                    height: 85vh; 
                    overflow-y: auto;
                    color: #000000;
                   }"),
                   tags$b("Pre-Processing Data"),
                   tags$br(),
                   tags$div("Use this tab to tranform the data before feeding it into a model. The dataset must include covariates and a variable named 'G'. 
       Pre-processing methods are named after the papers they were derived from, while further
       discussion of these methods are described in the Definitions tab."),
                   tags$br(),
                   tags$div("
       To start, upload a dataset, select the pre-processing method desired, and then click 'Download Data' once the button appears.
       Pre-processing will be done on all of the data in the dataset uploaded, with original row positions preserved.
       The download button will appear in the Download Outputs tab once the data is done processing. Meanwhile, the right panel will display
                            a basic comparison between the original and preprocessed data, split by G."),
                   tags$br(),
                   tags$b("Before Uploading Data"),
                   tags$ul(
                     tags$li("Outcome variables must be labeled as `Y` in order for it to not be included in pre-preprocessing algorithm."),
                     tags$li("The protected
           variable must be labeled as 'G' for it to be used appropriately in the pre-processing script.")
                   ),
                   tags$i("Note: Any variables named 'Yhat' or 'Prob' will be dropped."),
                   tags$br(),
                   tags$br(),
                   tags$b("Upload Data"),
                   fileInput("preprocess_data", label = NULL, accept = '.csv'),
                   tags$b("Select Pre-Process Method"),
                   selectInput("preprocess", label = NULL, choices = c("Johndrow and Lum (2019)")),
                   tags$b("Compare Original/Pre-Processed Variables"),
                   uiOutput("var_stats")
                   
      ),
      mainPanel(
        tags$head(tags$style("#var_plot{height:85vh !important;}")),
        tags$head(tags$style("#var_table{height:85vh !important;}")),
        uiOutput("var_output")
      )
    )
  ),
  tabPanel(
    "Download Outputs",
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    titlePanel("Downloading Post-Processed Data and Plots"),
    tags$br(),
    h4("Use this tab to download data and plots from the first three tabs of this tool. The processed data you will receive will be based on the inputs selected from these tabs. 
       You will get one file per tab, outputted as an .xlsx. Each tab will be data for each model or method applied, with the last tab containing information about thresholds applied
       to each model/method in the tool. Below there is a summary of what models/methods from each tab that will downloaded. In regards to downloading plots, you will get what is currently 
       in the respective tabs condensed into a .png file. For pre-processed data, the output will
       be a .csv file. Download buttons will only appear once files have been uploaded to the appropriate tabs."
    ),
    h3("Comparing Models tab"),
    tags$br(),
    fluidRow(
      column(8,
             tags$em(h4("Plots")),
             uiOutput("mdls_sub_metrics"),
             uiOutput("mdls_all_metrics"),
             tags$br(),
             h4("Title of Saved Plots"),
             textInput("mdls_filename", label = NULL, placeholder = "(Optional) Enter filename for plots to be saved then press 'Download Plots'", width = '25%'),
             uiOutput("show_mdl_plt_btn")
      )
    ),
    
    tags$br(),
    h3("Comparing Post-Processing Methods tab"),
    tags$br(),
    fluidRow(
      column(4, 
             tags$em(h4("Plots")),
             uiOutput("sub_metrics"),
             uiOutput("all_metrics"),
             tags$br(),
             h4("Title of Saved Plots"),
             textInput("filename", label = NULL, placeholder = "(Optional) Enter filename for plots to be saved then press 'Download Plots'", width = '45%'),
             uiOutput("show_method_plt_btn")
      ),
      column(8,
             tags$em(h4("Data")),
             uiOutput("post_mdls"),
             tags$br(),
             h4("Title of Post-Processed Data File"),
             textInput("data_filename", label = NULL, placeholder = "(Optional) Enter filename for plots to be saved then press 'Download Plots'", width = '45%'),
             uiOutput("show_method_data_btn")
      )
    ),
    tags$br(),
    h3("Generate Pre-Processed Data tab"),
    tags$br(),
    uiOutput("show_preprocess_btwn")
  ),
  tabPanel(
    "Descriptions",
    withMathJax(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    titlePanel("Definitions and descriptions"),
    tags$br(),
    h4("This tab contains definitions of terms referred to in the previous tabs.
       \\(Y\\) is the true outcome, \\(\\hat{Y}\\) is the predicted outcome, and \\(G\\) is the sensitive attribute.
       For more detail on the methods implemented in this tool, please refer to Cabreros, et al. (2022)."),
    h2("Performance Metrics"),
    h4("Overall performance metrics are calculated on the whole data set. Subgroup performance metrics are computed for each subpopulation defined by a level of the sensitive attribute \\(G\\)."),
    h3("Accuracy"),
    h4("The proportion of correctly predicted outcomes."),
    uiOutput("acc_form"),
    h3("False Negative Rate"),
    h4("Among the population for whom \\(Y = 1\\), the proportion of incorrectly predicted negative outcomes. Equivalent to 1 minus the True Positive Rate."),
    uiOutput("fn_rate"), 
    h3("False Positive Rate"),
    h4("Among the population for whom \\(Y = 0\\), the proportion of incorrectly predicted positive outcomes. Equivalent to 1 minus the True Negative Rate."),
    uiOutput("fp_rate"), 
    h3("Positive Predicted Rate"),
    h4("The proportion of positive predicted outcomes."),
    uiOutput("ppr_form"), 
    h3("Positive Predicted Value (PPV)"),
    h4("Among the population predicted to be positive, \\(\\hat{Y} = 1\\), the proportion that is truly positive."),
    uiOutput("ppv_form"), 
    h3("Negative Predictive Value (NPV)"),
    h4("Among the population predicted to be negative, \\(\\hat{Y} = 0\\), the proportion that is truly negative."),
    uiOutput("npv_form"),
    h2("Post-Processing Methods"),
    h4("The post processing methods find thresholds for positive prediction for each level of \\( G \\) that minimize differential performance while optimizing overall performance. 
        Further details of this optimization procedure are found in Cabreros, et al. (2022). Each of the post-processing options available are described below."),
    h3("Statistical Parity PP"),
    h4("This method finds optimal thresholds such that the Predicted Positive Rate is equal across each level of \\( G \\)."),
    uiOutput("sp_form"), 
    h3("Equalized Odds PP"),
    h4("This method finds optimal thresholds such that both the False Positive and False Negative Rates are balanced across
       all levels of \\( G \\)."),
    h4("False Positive Balance indicates:"),
    uiOutput("fp_bal_form"),
    h4("and False Negative Balance indicates:"),
    uiOutput("fn_bal_form"), 
    h4("Equivalently, this method seeks to equalize True Negative and True Positive Rates across all levels of \\( G \\)"),
    h3("Equalized Opportunity PP"),
    h4("This method finds optimal thresholds such that the True Positive Rate is the same across all levels of \\( G \\)."),
    uiOutput("equal_opp_form"),
    h4("Equivalently, this method seeks to equalize False Negative Rates to be equal across all levels of \\( G \\)."),
    h3("Equalized Error Rate PP"),
    h4("This method finds optimal thresholds such that the error rates are equal across all levels of \\( G \\). 
        Equalized error rates are achieved when each level of \\( G \\) have the same accuracy."),
    uiOutput("err_form"), 
    h2("Pre-Processing Methods"),
    h4("We have implemented a simple pre-processing approach adopted from Johndrow and Lum (2019) that ensures pairwise independence between 
       each covariate and the sensitive attribute \\( G \\). We note that Johndrow and Lum (2019) also describe a method to enforce 
       joint independence, which is not implemented as of the current version of the tool."),
    tags$br(),
    h2("Citations"),
    h4("Irineo Cabreros, Joshua Snoke, Osonde Osoba, Inez Khan, Marc Elliott. Advancing Equitable Decision-Making for the Department of Defense through Fairness in Machine Learning. 
        Santa Monica, CA: RAND Corporation, 2022."),
    h4("James E. Johndrow and Kristian Lum. An algorithm for removing sensitive information: application to race-independent recidivism prediction. 
        Annals of Applied Statistics, Vol. 13, No. 1, March, 2019, pp. 189-220.")
  )
)

