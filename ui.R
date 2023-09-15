library(shiny)
library(gotop)
library(mailtoR)

fluidPage( 
  #--- HTML <HEAD> CODE ------------------------------------------------------#
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css_hacks.css"),
    tags$title("Power Analysis for Causal Mediation Analysis")
  ),
  
  #--- HTML <BODY> CODE ------------------------------------------------------#
  
  # <header>
  withTags(
    header(
      h3(a(href="https://link.springer.com/epdf/10.3758/s13428-023-02118-0?sharing_token=1VMA7RN8vfU54nIBRnzrT5AH0g46feNdnc402WrhzyqhnbFJ8ttH-axG6v9OanM_5iwiJaRgWDuL7NEZ66rSWH_cplJP-25qWcwkjfA2WGfEjM0KFoAhQD1G4PXly_AaQi5HPUh72WWGGTjOGCTzbuzBk7vedh5qfq0kKXDrnIE%3D","Power Analysis for Causal Mediation Analysis", target="_blank")),
      p(a(href="https://www.education.pitt.edu/people/xuqin","Xu Qin", target="_blank"), "(",
        a(href="mailto:xuqin@pitt.edu","Contact"),
        ")")
    )
  ),
  
  
  # Sidebar for input specification  
  sidebarLayout(
    sidebarPanel(id = "options_well",

                 selectInput(inputId = "obj", 
                             label = "Objective:",
                             choices = list("Calculate power at a target sample Size" = "choose_n",
                                            "Calculate sample size at a target power" = "choose_power"),
                             selected = "choose_power"),
                 
                 uiOutput("obj_options"),
                 
                 selectInput(inputId = "effect", 
                             label = helpText("Causal effect"),
                             choices = list("Total Indirect Effect (Natural Indirect Effect)" = "TIE",
                                            "Pure Direct Effect (Natural Direct Effect)" = "PDE",
                                            "Pure Indirect Effect" = "PIE",
                                            "Total Direct Effect" = "TDE",
                                            "Natural Treatment-by-Mediator Interaction Effect" = "INT"),
                             selected = "NIE"), 
                 
                 hr(),
                 
                 h2(strong("Variable Specification:"), style = "font-size:14px;"),
                 
                 selectInput(inputId = "scale.t", 
                             label = helpText("Scale of treatment \\(T\\)"),
                             choices = list("Binary" = "binary",
                                            "Continuous" = "continuous"),
                             selected = "binary"),
                 
                 uiOutput("obj_treatment"),
                 
                 uiOutput("obj_scale.m"),
                 
                 selectInput(inputId = "scale.y", 
                             label = helpText("Scale of outcome \\(Y\\)"),
                             choices = list("Binary" = "binary",
                                            "Continuous" = "continuous"),
                             selected = "continuous"), 
                 
                 selectInput(inputId = "rand.t", 
                             label = helpText("Randomization of treatment"),
                             choices = list("Yes" = TRUE,
                                            "No" = FALSE),
                             selected = TRUE), 
                 
                 hr(),
                 
                 h3(strong("Model Parameter Specification:"), style = "font-size:14px;"),
                 
                 uiOutput("obj_bt.m"),
                 
                 uiOutput("obj_bt.y"),
                 
                 uiOutput("obj_bm.y"),
                 
                 uiOutput("obj_btm.y"),
                 
                 uiOutput("obj_R2x.t"),
                 
                 uiOutput("obj_R2x.m"),
                 
                 uiOutput("obj_R2x.y"),
                 
                 numericInput(inputId = "n.x", 
                              label = withMathJax(helpText("The number of covariates \\(\\boldsymbol{X}\\)")), 
                              min = 0,
                              value = 1,
                              step = 1),
                 
                 uiOutput("obj_skewness.t"),
                 uiOutput("obj_kurtosis.t"),
                 
                 uiOutput("obj_skewness.m"),
                 uiOutput("obj_kurtosis.m"),
                 
                 uiOutput("obj_skewness.y"),
                 uiOutput("obj_kurtosis.y"),
                 
                 hr(),
                 
                 h4(strong("Power Analysis Specification:"), style = "font-size:14px;"),
                 
                 numericInput(inputId = "sig", 
                              label = helpText("Significance level"), 
                              min = 0, 
                              max = 1,
                              value = 0.05),
                 
                 numericInput(inputId = "Nlow", 
                              label = helpText("Minimum sample size"), 
                              min = 5,
                              value = 50),
                 
                 numericInput(inputId = "Nhigh", 
                              label = helpText("Maximum sample size"), 
                              min = 5,
                              value = 500),
                 
                 numericInput(inputId = "Nsteps", 
                              label = helpText("Sample size step"), 
                              min = 1,
                              value = 50),
                 
                 numericInput(inputId = "powReps", 
                              label = helpText("Number of replications per sample size for power calculation"), 
                              min = 5,
                              value = 1000),
                 
                 numericInput(inputId = "mcmcReps", 
                              label = helpText("Number of Monte Carlo draws per replication for causal mediation analysis"),
                              min = 5,
                              value = 1000),
                 
                 numericInput(inputId = "seed", 
                              label = helpText("Random seed"), 
                              value = 1),
                 
                 actionButton(inputId = "action", 
                              label = "Go",
                              width = "100%", 
                              class = "btn-success"),
    ),
    
    mainPanel(
      # Model 
      uiOutput("models"),
      
      # Hypothesized Effect 
      column(12, align="center", uiOutput("true.effect")),
      
      tags$head(
        tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        font-weight: bold;
      }
    "))
      ),
      
      # Warning message
      conditionalPanel(condition = "output.warnstat == 'Error'",
                       verbatimTextOutput("warnmsg")),      
      # Power curve
      plotOutput("powercurves"),

      # Power table
      tableOutput("powertable"),
      
      # Reset button
      actionButton(inputId = "reset", 
                   label = "Clear"),

      
      uiOutput("text"),
      
      # Note
      uiOutput("note")
    )
  ),
  
  use_gotop()
)