library(shiny)
library(shinyWidgets)
library(plotly)

ui <- navbarPage(
  title = "RAND Algorithmic Equity Tool",
  theme = shinythemes::shinytheme("flatly"),
                    # Styling for navbar
  header = tags$head(tags$style(HTML('.navbar-static-top {background-color: #1475AD;}',
                            '.navbar-default .navbar-nav>.active>a {background-color: #1475AD;}')),
                     ## Keep navbar from overlapping content at smaller screen sizes, when navbar links break onto 2 (< approx 1232px) and 3 (< approx 982px). Navbar fully collapses at <768 px.
                     tags$style(type="text/css", "@media screen and (max-width: 1232px) and (min-width: 768px) { body { padding-top: 50px; } }"),
                     tags$style(type="text/css", "@media screen and (max-width: 982px) and (min-width: 768px) { body { padding-top: 100px; } }"),
                     tags$style(HTML(".download-btn { padding: 5px;}")),
                     # Styling for Assessment and Correction group probability sensitivity parameter menus
                     tags$style(HTML("#mdls_collapse_param3 .panel-title, #pp_collapse_param3 .panel-title { font-size: 15px; text-decoration: underline;}")),
                     tags$style(HTML("#mdls_collapse_param3 .panel-heading, #pp_collapse_param3 .panel-heading { color: black; background-color: white; padding-left: 0px; }")),
                     tags$style(HTML("#mdls_collapse_param3 .panel-primary, #pp_collapse_param3 .panel-primary { border-color: white; }")),
                     tags$style(HTML("#mdls_collapse_param3 .panel-heading .panel-title a.collapsed:after, #pp_collapse_param3 .panel-heading .panel-title a.collapsed:after { transform: rotate(180deg); }
                                      #mdls_collapse_param3 .panel-heading .panel-title a:after, #pp_collapse_param3 .panel-heading .panel-title a:after { content:'⏶'; text-align: right; float:right;}
                                      #mdls_collapse_param3 .panel-heading .panel-title a:not([class]):after, #pp_collapse_param3 .panel-heading .panel-title a:not([class]):after { transform: rotate(180deg); }")),
                     tags$style(HTML(".gpprob_info { background-color: #95a5a6; border-color: #95a5a6; width: 20px; height: 20px; line-height: 0em;
                                     position: relative; top:-5px; font-size: 10px; padding: 2px; margin-left:5px; }")),
                     tags$style(HTML(".popover{ padding: 10px;}")),
                     # Styling for errors and warnings
                     tags$style(HTML("#mdls_warn_Gprob.shiny-text-output, #pp_warn_Gprob.shiny-text-output, 
                                     #pp_warn_nan.shiny-text-output {color: red; font-style: italic; padding-bottom: 15px; font-size: 15px; font-weight: normal; }")),
                     ## Show errors and warnings only once in left sidebar and in top left of main panel. Otherwise, make them transparent.
                     tags$style(HTML("#mdlsgps_equity_plt.shiny-output-error-validation, #pp_equity_plt.shiny-output-error-validation, 
                                     #mdls_warn_Gprob.shiny-output-error-validation, #pp_warn_Gprob.shiny-output-error-validation {color: red; font-style: italic; padding-bottom: 15px; font-size: 15px; font-weight: normal; }")),
                     tags$style(HTML(".shiny-output-error-validation, #pp_warn_nan.shiny-output-error-validation {color: rgba(0, 0, 0, 0); }"))
                     ),
  position = c("fixed-top"),
  collapsible = TRUE,
  tabPanel(
    "About",
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    h3("Introduction"),
    h4("In recent years, there has been a growing awareness that Machine Learning (ML) algorithms can reinforce or exacerbate human biases. 
       The RAND Algorithmic Equity Tool was developed to help assess and correct biases in algorithms that assist in decision-making processes. 
       In particular, the tool helps users visualize tradeoffs between different types of fairness and overall model performance. It also provides tools to 
       mitigate bias through post-processing or pre-processing."
    ), 
    h4("This tool was originally produced as part of a research effort for RAND, with the goal of assisting the Department of Defense (DoD) as they invest in the development of ML algorithms for a growing number of applications. 
       The tool has been extended to address the issue of using proxy measures for group labels, which is common in healthcare settings where information on race and ethnicity is often missing or imputed. 
       The two companion reports further discusses this tool, its creation, and its development."
    ),
    h4("While ML algorithms are deployed in a wide variety of applications, this tool is specifically designed for algorithms that assist in decision-making processes. 
       In particular, this tool is useful when algorithmic output is used to influence binary decisions about individuals. 
       Hypothetical examples within this framework are (1) an algorithm that produces individual-level employee performance scores which are subsequently considered in promotional decisions or
       (2) an algorithm that produces recommendations for follow-up treatment from medical diagnostic testing."
    ),
    tags$br(),
    h3("Related Links"),
    #tags$a(href = "https://www.rand.org/pubs/research_reports/RRA1542-1.html", 
    #       "Accompanying research report"),
    #tags$br(),
    tags$a(href = "https://github.com/RANDCorporation/algorithmic-equity-tool", 
           "GitHub repository with source code, accompanying tutorials, and research reports"),
    tags$br(),
    h3('Contact'),
    h4('Please reach out for any questions or issues related to this tool:'),
    tags$p(tags$a(href = "https://www.rand.org/about/people/s/snoke_joshua.html", 
           "Joshua Snoke")),
    tags$p(tags$a(href = "https://github.com/swastvedt",
           "Solvejg Wastvedt"))
  ),
  tabPanel(
    "Assessment",
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
                   tags$b("Comparing Performance Among Models and Groups"),
                   tags$p("Use this tab to compare performance metrics among models and groups. Choose options for
                          group level and overall statistics and select the comparison point (models or groups) below. If group probability columns are uploaded, additional sidebar menus will appear."),
                   tags$p("For discussion of methods, see the Descriptions tab. Hover over plots for more information."),
                   tags$br(),
                   tags$b("Before Uploading Data"),
                   tags$br(),
                   tags$i("The following variables must be included and labelled as indicated for each dataset:"),
                   tags$ul(
                     tags$li("Y: Ground truth outcome (binary, 0/1)"),
                     tags$li("G: Group (categorical variable)", 
                             tags$ul(style="list-style-type:none;", tags$li(style="margin-left:-25px;", tags$strong("OR:"), "Group probability columns named G_", tags$em("[group name]")))
                     ),
                     #tags$li("Prob: Predicted probability of outcome"),
                     tags$li("Yhat: Predicted outcome (binary, 0/1)"),
                   ),
                   radioButtons("mdls_num", "How many models will you compare?",
                                choices = list(1,2,3,4,5), selected = 2, inline = TRUE),
                   uiOutput("mdls_fileInputs"),
                   textOutput("mdls_warn_Gprob"),
                   selectInput(inputId = "mdlsgps_equity", label = "Subgroup Performance Metrics",
                               choices = c("Accuracy", "Selection Rate", "True Negative Rate", "True Positive Rate", "Positive Predictive Value", "Negative Predictive Value")),
                   selectInput(inputId = "mdlsgps_performance", label = "Overall Performance Metrics",
                               choices = c("Accuracy", "Selection Rate", "True Negative Rate", "True Positive Rate", "Positive Predictive Value", "Negative Predictive Value")),
                   radioButtons(inputId = "interface_opt_mdlsgps", label = "Choose which to compare:",
                                choices = c("Models", "Groups"), selected = "Models", inline = T),
                   uiOutput("mdls_gpprob_menus")
      ),
      mainPanel(
        tags$head(tags$style("#mdlsgps_equity_plt{height:75vh !important;}")),
        tags$head(tags$style("#mdlsgps_per_plt{height:75vh !important;}")),
        fluidRow(
          column(7,
                 style = "position:relative; padding:1px",
                 shinycssloaders::withSpinner(plotOutput("mdlsgps_equity_plt", width = "100%",
                                                         hover = hoverOpts(id="mdlsgps_equity_hover", delayType = "debounce", delay = 100)
                 ), type=7),
                 htmlOutput("mdlsgps_equity_hoverinfo")
          ),
          column(5,
                 style = "position:relative; padding:1px",
                 shinycssloaders::withSpinner(plotOutput("mdlsgps_per_plt", width = "100%",
                                                         hover = hoverOpts(id="mdlsgps_per_hover", delayType = "debounce", delay = 100)
                 ), type=7),
                 htmlOutput("mdlsgps_per_hoverinfo")
          )
        ),
        tags$br(),
        fluidRow(
          uiOutput("show_mdlgp_plt_btn")
        ),
        tags$br()
      ),
    )
  ),
  tabPanel(
    "Correction: Post-Processing",
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
                   tags$b("Comparing Post-Processing Methods"),
                   tags$p("Upload a dataset to perform various post-processing methods and compare the results. Toggle between any or all methods (checkboxes) and choose group-level and 
                            overall statistics to compare (dropdowns). If group probability columns are uploaded, additional sidebar menus will appear."),
                   tags$p("For discussion of methods, see the Descriptions tab. Hover over plots for more information."),
                   tags$br(),
                   tags$div("Note: Plots may take a minute or two to generate due to the algorithms used to determine thresholds."),
                   tags$br(),
                   tags$b("Before Uploading Data"),
                   tags$br(),
                   tags$i("The following variables must be included and labelled as indicated:"),
                   tags$ul(
                     tags$li("Y: Ground truth outcome (binary, 0/1)"),
                     tags$li("G: Group (categorical variable)", 
                             tags$ul(style="list-style-type:none;", tags$li(style="margin-left:-25px;", tags$strong("OR:"), "Group probability columns named G_", tags$em("[group name]")))
                     ),
                     tags$li("Prob: Predicted probability of outcome"),
                     tags$li("Yhat: Predicted outcome (binary, 0/1)"),
                   ),
                   fileInput("pp_data", "Upload Data", accept = '.csv'),
                   textOutput("pp_warn_Gprob"),
                   textOutput("pp_warn_nan"),
                   numericInput("pp_base_thresh", "Model's Original Threshold", value = 0.5, min = 0, max = 1, step=0.05),
                   checkboxGroupInput("pp_processing", label = "Select Post-Processing Methods",
                                      choices = c("Statistical Parity PP", "Equalized Odds PP", "Equalized Opp. PP", "Equalized Error Rate PP")),
                   selectInput(inputId = "pp_equity", label = "Subgroup Performance Metrics",
                               choices = c("Accuracy", "Selection Rate", "True Negative Rate", "True Positive Rate", "Positive Predictive Value", "Negative Predictive Value")),
                   selectInput(inputId = "pp_performance", label = "Overall Performance Metrics",
                               choices = c("Accuracy", "Selection Rate", "True Negative Rate", "True Positive Rate", "Positive Predictive Value", "Negative Predictive Value")),
                   uiOutput("pp_gpprob_menus")
      ),
      mainPanel(
        tags$head(tags$style("#pp_equity_plt{height:75vh !important;}")),
        tags$head(tags$style("#pp_per_plt{height:75vh !important;}")),
        fluidRow(
          column(7,
                 style = "position:relative; padding:1px",
                 shinycssloaders::withSpinner(plotOutput("pp_equity_plt", width = "100%",
                                                         hover = hoverOpts(id="pp_equity_hover", delayType = "debounce", delay = 100)
                 ), type=7),
                 htmlOutput("pp_equity_hoverinfo")
          ),
          column(5,
                 style = "position:relative; padding:1px",
                 shinycssloaders::withSpinner(plotOutput("pp_per_plt", width = "100%",
                                                         hover = hoverOpts(id="pp_per_hover", delayType = "debounce", delay = 100)
                 ), type=7),
                 htmlOutput("pp_per_hoverinfo")
          )
        ),
        tags$br(),
        fluidRow(
          column(8,
                 style = "padding:1px",
                 shinycssloaders::withSpinner(plotOutput("pp_threshold", width = "100%", height = "100%",
                                                             hover = hoverOpts(id="pp_threshold_hover", delayType = "debounce", delay = 100)), type=7),
                 htmlOutput("pp_threshold_hoverinfo")),
          column(1, tags$br()),
          column(3, 
                 fluidRow( uiOutput("show_pp_plt_btn") ),
                 tags$br(),
                 fluidRow(uiOutput("show_thresh_plt_btn")),
                 tags$br(),
                 fluidRow(uiOutput("show_pp_data_btn"))
                 )
        ),
        tags$br(),
        fluidRow( 
          
        ),
        tags$br()
      ),
    )
  ),
  tabPanel(
    "Correction: Pre-Processing",
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
                   tags$p("Use this tab to tranform data before feeding it into a model. The dataset must include covariates and a variable named 'G'."),
                   tags$p("Upload a dataset and select a method using the dropdown. Pre-processing will be done on all of the data with original row positions preserved.
                          This tab will display a comparison between the original and preprocessed data for each input variable.
                          The goal of pre-processing is to equalize each variable's distribution across groups (G)."),
                   tags$p("For descriptions of pre-processing methods, see the Descriptions tab. Hover over plots for more information."),
                   tags$br(),
                   tags$b("Before Uploading Data"),
                   tags$ul(
                     tags$li("Outcome variables must be named `Y` in order to not be preprocessed."),
                     tags$li("The protected group variable must be a categorical variable named 'G'."),
                     tags$li("Group probability columns are not supported.")
                   ),
                   tags$i("Note: Any variables named 'Yhat' or 'Prob' will be dropped."),
                   tags$br(),
                   tags$br(),
                   tags$b("Upload Data"),
                   fileInput("preprocess_data", label = NULL, accept = '.csv'),
                   tags$b("Confirm Variable Types"),
                   uiOutput("var_types"),
                   htmlOutput("cat_err_message", style = "color:red"),
                   tags$b("Select Pre-Process Method"),
                   selectInput("preprocess", label = NULL, choices = c("Johndrow and Lum (2019)")),
                   tags$b("Compare Original/Pre-Processed Variables"),
                   uiOutput("var_stats"),
      ),
      mainPanel(
        tags$head(tags$style("#preproc_plt{height:75vh !important;}")),
        fluidRow(
          column(12,
                 shinycssloaders::withSpinner(plotlyOutput("preproc_plt", width = "100%"), type=7),
          )
        ),
        tags$br(),
        fluidRow(
          column(2, uiOutput("show_preproc_plt_btn") ),
          column(10, uiOutput("show_preproc_data_btwn") )
        )
      )
    )
  ),
  tabPanel(
    "Descriptions",
    withMathJax(),
    titlePanel("Definitions and descriptions"),
    tags$br(),
    p("This tab contains definitions of terms referred to in the previous tabs.
       \\(Y\\) is the true outcome, \\(\\hat{Y}\\) is the predicted outcome, and \\(G\\) is the protected group.
       For more detail on the methods implemented in this tool, please refer to the reports linked on the GitHub repository."),
    h3("Performance Metrics"),
    p("Overall performance metrics are calculated on the whole data set. Subgroup performance metrics are computed for each subpopulation defined by a level of the protected group \\(G\\)."),
    h4("Accuracy"),
    p("The proportion of correctly predicted outcomes."),
    uiOutput("acc_form"),
    h4("True Positive Rate"),
    p("Among the population for whom \\(Y = 1\\), the proportion of correctly predicted positive outcomes. Equivalent to 1 minus the False Negative Rate."),
    uiOutput("tp_rate"),
    h4("True Negative Rate"),
    p("Among the population for whom \\(Y = 0\\), the proportion of correctly predicted negative outcomes. Equivalent to 1 minus the False Positive Rate."),
    uiOutput("tn_rate"),
    h4("Selection Rate"),
    p("The proportion of positive predicted outcomes."),
    uiOutput("selrate_form"),
    h4("Positive Predicted Value (PPV)"),
    p("Among the population predicted to be positive, \\(\\hat{Y} = 1\\), the proportion that is truly positive."),
    uiOutput("ppv_form"),
    h4("Negative Predictive Value (NPV)"),
    p("Among the population predicted to be negative, \\(\\hat{Y} = 0\\), the proportion that is truly negative."),
    uiOutput("npv_form"),
    
    h3("Post-Processing Methods"),
    p("The post processing methods find group-specific", tags$strong("thresholds"), "that minimize performance differences while optimizing overall performance."),
    p(tags$strong("Thresholds determine when model predicted probabilities are classified as positive."), "Predicted probabilities above the threshold equal a positive prediction."),
    p("For example, the plot below shows hypothetical distributions of predicted probabilities with group-specific thresholds marked with colored vertical lines. Initially, all groups' thresholds are 0.5, meaning predicted probabilities above 0.5 are classified as positive (shaded regions).
      Statistical parity post-processing sets new thresholds to minimize differences among groups' rates of positive prediction. For this hypothetical data, statistical parity post-processing moves Group 2's threshold lower and Group 1's slightly higher, equalizing the shaded regions."),
    plotOutput("thresh_ex_plot", height = 400, width = 400),
    p("Further details of this optimization procedure are found in Cabreros, et al. (2023). Each of the post-processing options available are described below."),
    h4("Statistical Parity PP"),
    p("This method finds optimal thresholds such that the Predicted Positive Rate is equal across all levels of \\( G \\)."),
    uiOutput("sp_form"),
    h4("Equalized Odds PP"),
    p("This method finds optimal thresholds such that both the True Positive and True Negative Rates are balanced across
       all levels of \\( G \\)."),
    p("True Positive Balance indicates:"),
    uiOutput("tp_bal_form"),
    p("and True Negative Balance indicates:"),
    uiOutput("tn_bal_form"),
    p("Equivalently, this method seeks to equalize False Negative and False Positive Rates across all levels of \\( G \\)"),
    h4("Equalized Opportunity PP"),
    p("This method finds optimal thresholds such that the True Positive Rate is the same across all levels of \\( G \\)."),
    uiOutput("equal_opp_form"),
    p("Equivalently, this method seeks to equalize False Negative Rates to be equal across all levels of \\( G \\)."),
    h4("Equalized Error Rate PP"),
    p("This method finds optimal thresholds such that the error rates are equal across all levels of \\( G \\).
        Equalized error rates are achieved when each level of \\( G \\) has the same accuracy."),
    uiOutput("err_form"),
    
    h3("Group probability analysis"),
    p("When data with group probability columns are uploaded on the", tags$strong("Assessment"), "or", tags$strong("Correction: Post-Processing"), "tabs, the tool estimates the amount of (statistical) bias resulting from the use of probabilities instead of a fixed group variable.
      Plots display a mean interval, rather than a dot, to indicate the range of possible mean values given the sensitivity parameters entered. Confidence intervals are likewise adjusted.
      In general, wider ranges for \\(\\epsilon\\) and \\(\\epsilon'\\) correspond to scenarios with greater error in the group probabilities and thus wider mean and confidence intervals. Cells in the", tags$i("Group proportions conditional on \\(Y\\) or \\(\\hat{Y}\\)"), "table may be estimated from existing studies. 
      For example, the top left cell is the proportion of group 1 among all observations having the outcome (\\(Y=1\\)). Alternatively, use the default table values, which are estimated from uploaded data using the group probability columns."),
    # NOTE: add link to paper on arxiv
    HTML('<p>See forthcoming manuscript for details of the sensitivity analysis approach and definitions of each sensitivity parameter. Note that for computational simplicity, the app uses a slightly different
         bootstrapping procedure from that described in the paper. The app obtains bootstrap confidence intervals first and subsequently calculates bias corrections, rather than bias correcting as part of the bootstrapping procedure as described in the paper.</p>'),
    
    h3("Pre-Processing Methods"),
    p("We have implemented a simple pre-processing approach adopted from Johndrow and Lum (2019) that ensures pairwise independence between
       each covariate and the protected group \\( G \\). We note that Johndrow and Lum (2019) also describe a method to enforce
       joint independence which is not implemented as of the current version of the tool."),
    tags$br(),
    h3("Citations"),
    p("Irineo Cabreros, Joshua Snoke, Osonde A. Osoba, Inez Khan, and Marc N. Elliott,
       Advancing Equitable Decisionmaking for the Department of Defense Through Fairness in Machine Learning,
       RR-A1542-1, 2023. https://www.rand.org/pubs/research_reports/RRA1542-1.html"),
    p("James E. Johndrow and Kristian Lum.
       An algorithm for removing sensitive information: application to race-independent recidivism prediction,
       Annals of Applied Statistics, Vol. 13, No. 1, March, 2019, pp. 189-220.")
  )
)

