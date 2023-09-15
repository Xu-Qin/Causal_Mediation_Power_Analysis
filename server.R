#--- PREAMBLE (RUNS ONLY ONCE WHEN APP LOADS) --------------------------------#

library(MASS)
library(ggplot2)
library(shiny)

#--- SERVER SCRIPT -----------------------------------------------------------#

function(input, output, session) {
  
  output$models <- renderUI({
    
    if(input$n.x != 0){
      if(input$rand.t == "TRUE"){
        # Display Model for continuous M and Y
        if(input$scale.m == "continuous" & input$scale.y == "continuous"){
          withMathJax(
            "$$\\text{\\(^1\\)Standardized Mediator Model: }M = {\\beta}^{m}_{t}T + \\boldsymbol{X\\beta}^{m}_{x} + \\varepsilon_{M}$$",
            "$$\\text{Standardized Outcome Model: }Y = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\boldsymbol{X\\beta}^{y}_{x} + \\varepsilon_{Y}$$"
          )
        } else if(input$scale.m == "binary" & input$scale.y == "continuous"){
          
          # Display Model for binary M and continuous Y
          withMathJax(
            "$$\\text{Standardized Mediator Model: }M = \\mathbf{1}\\{ M^{*} > 0 \\} \\text{ where } M^{*} = {\\beta}^{m}_{t}T + \\boldsymbol{X\\beta}^{m}_{x} + \\varepsilon_{M^{*}}$$",
            "$$\\text{Standardized Outcome Model: }Y = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\boldsymbol{X\\beta}^{y}_{x} + \\varepsilon_{Y}$$"
          )
        } else if(input$scale.m == "continuous" & input$scale.y == "binary"){
          
          # Display Model for continuous M and binary Y
          withMathJax(
            "$$\\text{Standardized Mediator Model: }M = {\\beta}^{m}_{t}T + \\boldsymbol{X\\beta}^{m}_{x} + \\varepsilon_{M}$$",
            "$$\\text{Standardized Outcome Model: }Y = \\mathbf{1}\\{ Y^{*} > 0 \\} \\text{ where } Y^{*} = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\boldsymbol{X\\beta}^{y}_{x} + \\varepsilon_{Y^{*}}$$"
          )
        } else if(input$scale.m == "binary" & input$scale.y == "binary"){
          
          # Display Model for binary M and Y
          withMathJax(
            "$$\\text{Standardized Mediator Model: }M = \\mathbf{1}\\{ M^{*} > 0 \\} \\text{ where } M^{*} = {\\beta}^{m}_{t}T + \\boldsymbol{X\\beta}^{m}_{x} + \\varepsilon_{M^{*}}$$",
            "$$\\text{Standardized Outcome Model: }Y = \\mathbf{1}\\{ Y^{*} > 0 \\} \\text{ where } Y^{*} = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\boldsymbol{X\\beta}^{y}_{x} + \\varepsilon_{Y^{*}}$$"
          )
        }
      } else {
        if(input$scale.t == "continuous"){
          # Display Model for continuous M and Y
          if(input$scale.m == "continuous" & input$scale.y == "continuous"){
            withMathJax(
              "$$\\text{Standardized Treatment Model (for data generation only): }T = \\boldsymbol{X\\beta}^{t}_{x} + \\varepsilon_{T}$$",
              "$$\\text{Standardized Mediator Model: }M = {\\beta}^{m}_{t}T + \\boldsymbol{X\\beta}^{m}_{x} + \\varepsilon_{M}$$",
              "$$\\text{Standardized Outcome Model: }Y = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\boldsymbol{X\\beta}^{y}_{x} + \\varepsilon_{Y}$$"
            )
          } else if(input$scale.m == "binary" & input$scale.y == "continuous"){
            
            # Display Model for binary M and continuous Y
            withMathJax(
              "$$\\text{Standardized Treatment Model (for data generation only): }T = \\boldsymbol{X\\beta}^{t}_{x} + \\varepsilon_{T}$$",
              "$$\\text{Standardized Mediator Model: }M = \\mathbf{1}\\{ M^{*} > 0 \\} \\text{ where } M^{*} = {\\beta}^{m}_{t}T + \\boldsymbol{X\\beta}^{m}_{x} + \\varepsilon_{M^{*}}$$",
              "$$\\text{Standardized Outcome Model: }Y = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\boldsymbol{X\\beta}^{y}_{x} + \\varepsilon_{Y}$$"
            )
          } else if(input$scale.m == "continuous" & input$scale.y == "binary"){
            
            # Display Model for continuous M and binary Y
            withMathJax(
              "$$\\text{Standardized Treatment Model (for data generation only): }T = \\boldsymbol{X\\beta}^{t}_{x} + \\varepsilon_{T}$$",
              "$$\\text{Standardized Mediator Model: }M = {\\beta}^{m}_{t}T + \\boldsymbol{X\\beta}^{m}_{x} + \\varepsilon_{M}$$",
              "$$\\text{Standardized Outcome Model: }Y = \\mathbf{1}\\{ Y^{*} > 0 \\} \\text{ where } Y^{*} = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\boldsymbol{X\\beta}^{y}_{x} + \\varepsilon_{Y^{*}}$$"
            )
          } else if(input$scale.m == "binary" & input$scale.y == "binary"){
            
            # Display Model for binary M and Y
            withMathJax(
              "$$\\text{Standardized Treatment Model (for data generation only): }T = \\boldsymbol{X\\beta}^{t}_{x} + \\varepsilon_{T}$$",
              "$$\\text{Standardized Mediator Model: }M = \\mathbf{1}\\{ M^{*} > 0 \\} \\text{ where } M^{*} = {\\beta}^{m}_{t}T + \\boldsymbol{X\\beta}^{m}_{x} + \\varepsilon_{M^{*}}$$",
              "$$\\text{Standardized Outcome Model: }Y = \\mathbf{1}\\{ Y^{*} > 0 \\} \\text{ where } Y^{*} = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\boldsymbol{X\\beta}^{y}_{x} + \\varepsilon_{Y^{*}}$$"
            )
          }
        } else {
          # Display Model for continuous M and Y
          if(input$scale.m == "continuous" & input$scale.y == "continuous"){
            withMathJax(
              "$$\\text{Standardized Treatment Model (for data generation only): }T = \\mathbf{1}\\{ T^{*} > 0 \\} \\text{ where } T^{*} = \\boldsymbol{X\\beta}^{t}_{x} + \\varepsilon_{T^{*}}$$",
              "$$\\text{Standardized Mediator Model: }M = {\\beta}^{m}_{t}T + \\boldsymbol{X\\beta}^{m}_{x} + \\varepsilon_{M}$$",
              "$$\\text{Standardized Outcome Model: }Y = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\boldsymbol{X\\beta}^{y}_{x} + \\varepsilon_{Y}$$"
            )
          } else if(input$scale.m == "binary" & input$scale.y == "continuous"){
            
            # Display Model for binary M and continuous Y
            withMathJax(
              "$$\\text{Standardized Treatment Model (for data generation only): }T = \\mathbf{1}\\{ T^{*} > 0 \\} \\text{ where } T^{*} = \\boldsymbol{X\\beta}^{t}_{x} + \\varepsilon_{T^{*}}$$",
              "$$\\text{Standardized Mediator Model: }M = \\mathbf{1}\\{ M^{*} > 0 \\} \\text{ where } M^{*} = {\\beta}^{m}_{t}T + \\boldsymbol{X\\beta}^{m}_{x} + \\varepsilon_{M^{*}}$$",
              "$$\\text{Standardized Outcome Model: }Y = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\boldsymbol{X\\beta}^{y}_{x} + \\varepsilon_{Y}$$"
            )
          } else if(input$scale.m == "continuous" & input$scale.y == "binary"){
            
            # Display Model for continuous M and binary Y
            withMathJax(
              "$$\\text{Standardized Treatment Model (for data generation only): }T = \\mathbf{1}\\{ T^{*} > 0 \\} \\text{ where } T^{*} = \\boldsymbol{X\\beta}^{t}_{x} + \\varepsilon_{T^{*}}$$",
              "$$\\text{Standardized Mediator Model: }M = {\\beta}^{m}_{t}T + \\boldsymbol{X\\beta}^{m}_{x} + \\varepsilon_{M}$$",
              "$$\\text{Standardized Outcome Model: }Y = \\mathbf{1}\\{ Y^{*} > 0 \\} \\text{ where } Y^{*} = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\boldsymbol{X\\beta}^{y}_{x} + \\varepsilon_{Y^{*}}$$"
            )
          } else if(input$scale.m == "binary" & input$scale.y == "binary"){
            
            # Display Model for binary M and Y
            withMathJax(
              "$$\\text{Standardized Treatment Model (for data generation only): }T = \\mathbf{1}\\{ T^{*} > 0 \\} \\text{ where } T^{*} = \\boldsymbol{X\\beta}^{t}_{x} + \\varepsilon_{T^{*}}$$",
              "$$\\text{Standardized Mediator Model: }M = \\mathbf{1}\\{ M^{*} > 0 \\} \\text{ where } M^{*} = {\\beta}^{m}_{t}T + \\boldsymbol{X\\beta}^{m}_{x} + \\varepsilon_{M^{*}}$$",
              "$$\\text{Standardized Outcome Model: }Y = \\mathbf{1}\\{ Y^{*} > 0 \\} \\text{ where } Y^{*} = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\boldsymbol{X\\beta}^{y}_{x} + \\varepsilon_{Y^{*}}$$"
            )
          }
        }
      }
    } else {
      # Display Model for continuous M and Y
      if(input$scale.m == "continuous" & input$scale.y == "continuous"){
        withMathJax(
          "$$\\text{\\(^1\\)Standardized Mediator Model: }M = {\\beta}^{m}_{t}T + \\varepsilon_{M}$$",
          "$$\\text{Standardized Outcome Model: }Y = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\varepsilon_{Y}$$"
        )
      } else if(input$scale.m == "binary" & input$scale.y == "continuous"){
        
        # Display Model for binary M and continuous Y
        withMathJax(
          "$$\\text{Standardized Mediator Model: }M = \\mathbf{1}\\{ M^{*} > 0 \\} \\text{ where } M^{*} = {\\beta}^{m}_{t}T + \\varepsilon_{M^{*}}$$",
          "$$\\text{Standardized Outcome Model: }Y = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\varepsilon_{Y}$$"
        )
      } else if(input$scale.m == "continuous" & input$scale.y == "binary"){
        
        # Display Model for continuous M and binary Y
        withMathJax(
          "$$\\text{Standardized Mediator Model: }M = {\\beta}^{m}_{t}T + \\varepsilon_{M}$$",
          "$$\\text{Standardized Outcome Model: }Y = \\mathbf{1}\\{ Y^{*} > 0 \\} \\text{ where } Y^{*} = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\varepsilon_{Y^{*}}$$"
        )
      } else if(input$scale.m == "binary" & input$scale.y == "binary"){
        
        # Display Model for binary M and Y
        withMathJax(
          "$$\\text{Standardized Mediator Model: }M = \\mathbf{1}\\{ M^{*} > 0 \\} \\text{ where } M^{*} = {\\beta}^{m}_{t}T + \\varepsilon_{M^{*}}$$",
          "$$\\text{Standardized Outcome Model: }Y = \\mathbf{1}\\{ Y^{*} > 0 \\} \\text{ where } Y^{*} = {\\beta}^{y}_{t}T + {\\beta}^{y}_{m}M + {\\beta}^{y}_{tm}TM + \\varepsilon_{Y^{*}}$$"
        )
      }
    }
  })

  # Generate power analysis results
  power.table <- eventReactive(input$action,{
    tryCatch(
      {source("mediation_power.R", local = TRUE)$value},
      error = function(e) {return(e$message)}
    )
  })
  
  # Display notes
  output$note <- renderText({
    paste(tags$br(),
          "Notes:", tags$br(),
          "1. A standardized model standardizes each of the dependent variable and the independent variables to have a mean of 0 and a variance of 1. If the dependent variable is binary, its latent index is standardized instead. Hence, the intercepts of the standardized models are always 0.", tags$br(),
          tags$br(),
          "2. In the calculation of the hypothesized effect, continuous T, M and/or Y are standardized, and binary T, M and/or Y are on the original scale. For example, if T, M, and Y are all continuous, and 'Treatment level of standardized T' and 'Control level of standardized T' are respectively set as 1 and 0, the displayed hypothesized value of PIE indicates the average change in the standardized outcome if the treatment is fixed at 0, while the standardized mediator is changed from the level that would be observed when the treatment is 0 to that when the treatment is one standard deviation above the mean. If T is binary instead, hypothesized PIE indicates the average change in the standardized outcome if the treatment is held at 0, while the standardized mediator is changed from the level that would be observed when the treatment is 0 to that when the treatment is 1.")
  })
  
  # Display output or display error messages
  # Display output or display error messages
  observeEvent(power.table(),
               if (class(power.table()) != "data.frame") {
                 output$powercurves <- NULL
               } else {
                 output$powercurves <- renderPlot({
                   ggplot(
                     data = power.table(),
                     aes(x = Sample_Size, y = Power)) +
                     scale_x_continuous(limits = c(input$Nlow, input$Nhigh), breaks = sort(seq(input$Nlow, input$Nhigh, by = input$Nsteps))) +
                     geom_smooth(method = "loess", se = FALSE) +
                     geom_hline(yintercept = power.table()[which(rownames(power.table()) == nrow(power.table())), "Power"], linetype = "dashed") +
                     geom_vline(xintercept = power.table()[which(rownames(power.table()) == nrow(power.table())), "Sample_Size"], linetype = "dashed") +
                     geom_text(aes(input$Nlow, power.table()[which(rownames(power.table()) == nrow(power.table())), "Power"], label = round(power.table()[which(rownames(power.table()) == nrow(power.table())), "Power"], 2), vjust = 0.5, hjust = 0.5), colour = "blue") +
                     geom_text(aes(power.table()[which(rownames(power.table()) == nrow(power.table())), "Sample_Size"], 0, label = round(power.table()[which(rownames(power.table()) == nrow(power.table())), "Sample_Size"]), vjust = 1, hjust = 0.5), colour = "blue") +
                     labs(y = "Power", x = "Sample Size")
                 })
               }
  )
  
  observeEvent(power.table(),
               if (class(power.table()) != "data.frame") {
                 output$powertable <- NULL
               } else {
                 output$powertable <- renderTable({ 
                   data.frame(Sample_Size = format(power.table()$Sample_Size),
                              Power = power.table()$Power)
                 },  align = 'c', include.rownames=FALSE)
               }
  )
  
  observeEvent(power.table(), {
    output$warnmsg <- renderPrint({
      if(any(is.na(power.table()$Sample_Size))){
        if(max(power.table()$Power[-which(is.na(power.table()$Sample_Size))]) < input$TarPow){
          print("Warning: Power values for all sample sizes are less than the target power value. Please specify a larger maximum sample size.")
        }
        if(min(power.table()$Power[-which(is.na(power.table()$Sample_Size))]) > input$TarPow){
          print("Warning: Power values for all sample sizes are more than the target power value. Please specify a smaller minimum sample size.")
        }
      }
    })
  })
  output$warnstat <- renderText({ifelse(any(is.na(power.table()$Sample_Size)) & max(power.table()$Power[-which(is.na(power.table()$Sample_Size))]) < input$TarPow|any(is.na(power.table()$Sample_Size)) & min(power.table()$Power[-which(is.na(power.table()$Sample_Size))]) > input$TarPow,"Error","No error") })
  outputOptions(output, "warnstat", suspendWhenHidden=FALSE)
  
  # Clear output
  observeEvent(input$reset, {
    output$powercurves <- NULL
    output$powertable <- NULL
    output$warnmsg <- NULL
  })
  
  output$text <- renderUI({
    HTML(paste0("<b>","Please click the 'Clear' button before a new run."))
  })

  # Render Objective Input Options
  output$obj_options <- renderUI({
    if (input$obj == "choose_n") {
      numericInput(inputId = "TarN", 
                   label = helpText("Target sample size"), 
                   min = 5,
                   value = 100)
    } else {
      numericInput(inputId = "TarPow", 
                   label = helpText("Target power"), 
                   min = 0,
                   max = 1,
                   step = 0.1,
                   value = 0.80,
                   width = "100%")
    }
  })
  
  # Render Treatment Specification Input Options
  output$obj_treatment <- renderUI({
    if (input$scale.t == "binary") {
      if (input$rand.t == TRUE){
        numericInput(inputId = "p.t", 
                     label = withMathJax(helpText("The probability of \\(T = 1\\)")), 
                     min = 0, 
                     max = 1,
                     step = 0.1,
                     value = 0.5)
      }
    } else {
      tagList(
        numericInput(inputId = "treat.t", 
                     label = withMathJax(helpText("Treatment level of standardized \\(T\\)")), 
                     value = 1),
        
        numericInput(inputId = "control.t", 
                     label = withMathJax(helpText("Control level of standardized \\(T\\)")), 
                     value = 0)
      )
    }
  })
  
  output$obj_scale.m <- renderUI({
    if(input$scale.y == "binary"){
      selectInput(inputId = "scale.m", 
                  label = helpText("Scale of mediator \\(M\\)"),
                  choices = list("Binary" = "binary",
                                 "Continuous (Normal)" = "continuous"),
                  selected = "continuous")
    } else {
      selectInput(inputId = "scale.m", 
                  label = helpText("Scale of mediator \\(M\\)"),
                  choices = list("Binary" = "binary",
                                 "Continuous" = "continuous"),
                  selected = "continuous")
    }
  })
  
  # Render R2x of the treatment model Input Options
  output$obj_R2x.t <- renderUI({
    if (input$rand.t == FALSE) {
      if(input$scale.t == "continuous"){
        numericInput(inputId = "R2x.t", 
                     label = withMathJax(helpText("Proportion of variance in \\(T\\) explained by \\(\\boldsymbol{X}\\)")), 
                     min = 0,
                     max = 1,
                     step = 0.1,
                     value = 0.2)
      } else {
        numericInput(inputId = "R2x.t", 
                     label = withMathJax(helpText('Proportion of variance in \\(T^{*}\\) explained by \\(\\boldsymbol{X}\\)')), 
                     min = 0,
                     max = 1,
                     step = 0.1,
                     value = 0.2)
      }
    }
  })

  # Render Model Parameter Specification Input Options
  output$obj_bt.m <- renderUI({
    if(input$scale.m == "continuous"){
      numericInput(inputId = "bt.m", 
                   label = withMathJax(helpText('Standardized coefficient of \\(T\\) on \\(M\\) \\({\\beta}^{m}_{t}\\)')),
                   value = 0.2)
    } else {
      numericInput(inputId = "bt.m", 
                   label = withMathJax(helpText('Standardized coefficient of \\(T\\) on \\(M^{*}\\) \\({\\beta}^{m}_{t}\\)')),
                   value = 0.2)
    }
  })
  
  output$obj_bt.y <- renderUI({
    if(input$scale.y == "continuous"){
      numericInput(inputId = "bt.y", 
                   label = withMathJax(helpText('Standardized coefficient of \\(T\\) on \\(Y\\) \\({\\beta}^{y}_{t}\\)')),
                   value = 0.2)
    } else {
      numericInput(inputId = "bt.y", 
                   label = withMathJax(helpText('Standardized coefficient of \\(T\\) on \\(Y^{*}\\) \\({\\beta}^{y}_{t}\\)')),
                   value = 0.2)
    }
  })
  
  output$obj_bm.y <- renderUI({
    if(input$scale.y == "continuous"){
      numericInput(inputId = "bm.y", 
                   label = withMathJax(helpText('Standardized coefficient of \\(M\\) on \\(Y\\) \\({\\beta}^{y}_{m}\\)')),
                   value = 0.2)
    } else {
      numericInput(inputId = "bm.y", 
                   label = withMathJax(helpText('Standardized coefficient of \\(M\\) on \\(Y^{*}\\) \\({\\beta}^{y}_{m}\\)')),
                   value = 0.2)
    }
  })
  
  output$obj_btm.y <- renderUI({
    if(input$scale.y == "continuous"){
      numericInput(inputId = "btm.y", 
                   label = withMathJax(helpText('Standardized coefficient of \\(TM\\) on \\(Y\\) \\({\\beta}^{y}_{tm}\\)')),
                   value = 0.05)
    } else {
      numericInput(inputId = "btm.y", 
                   label = withMathJax(helpText('Standardized coefficient of \\(TM\\) on \\(Y^{*}\\) \\({\\beta}^{y}_{tm}\\)')),
                   value = 0.05)
    }
  })
  
  output$obj_R2x.m <- renderUI({
    if(input$scale.m == "continuous"){
      numericInput(inputId = "R2x.m", 
                   label = withMathJax(helpText("Proportion of variance in \\(M\\) explained by \\(\\boldsymbol{X}\\)")), 
                   min = 0,
                   max = 1,
                   step = 0.1,
                   value = 0.2)
    } else {
      numericInput(inputId = "R2x.m", 
                   label = withMathJax(helpText("Proportion of variance in \\(M^{*}\\) explained by \\(\\boldsymbol{X}\\)")), 
                   min = 0,
                   max = 1,
                   step = 0.1,
                   value = 0.2)
    }
  })

  output$obj_R2x.y <- renderUI({
    if(input$scale.y == "continuous"){
      numericInput(inputId = "R2x.y", 
                   label = withMathJax(helpText("Proportion of variance in \\(Y\\) explained by \\(\\boldsymbol{X}\\)")), 
                   min = 0,
                   max = 1,
                   step = 0.1,
                   value = 0.2)
    } else {
      numericInput(inputId = "R2x.y", 
                   label = withMathJax(helpText("Proportion of variance in \\(Y^{*}\\) explained by \\(\\boldsymbol{X}\\)")), 
                   min = 0,
                   max = 1,
                   step = 0.1,
                   value = 0.2)
    }
  })
  
  output$obj_skewness.t <- renderUI({
    if (input$rand.t == TRUE) {
      if (input$scale.t == "continuous") {
        numericInput(inputId = "skewness.t", 
                     label = withMathJax(helpText("The skewness of the distribution of \\(T\\) (0 if normal)")), 
                     min = 0,
                     value = 0)
      }
    } else {
      if (input$scale.t == "continuous") {
        numericInput(inputId = "skewness.t", 
                     label = withMathJax(helpText("The skewness of the distribution of \\(\\varepsilon_{T}\\) (0 if normal)")), 
                     min = 0,
                     value = 0)
      }
    }
  })
  
  output$obj_skewness.m <- renderUI({
    if (input$scale.m == "continuous" & input$scale.y == "continuous") {
      numericInput(inputId = "skewness.m", 
                   label = withMathJax(helpText("The skewness of the distribution of \\(\\varepsilon_{M}\\) (0 if normal)")), 
                   min = 0,
                   value = 0)
    }
  })
  
  output$obj_skewness.y <- renderUI({
    if (input$scale.y == "continuous") {
      numericInput(inputId = "skewness.y", 
                   label = withMathJax(helpText("The skewness of the distribution of \\(\\varepsilon_{Y}\\) (0 if normal)")), 
                   min = 0,
                   value = 0)
    }
  })
  
  output$obj_kurtosis.t <- renderUI({
    if (input$rand.t == TRUE) {
      if (input$scale.t == "continuous") {
        numericInput(inputId = "kurtosis.t", 
                     label = withMathJax(helpText("The kurtosis of the distribution of \\(T\\) (0 if normal)")), 
                     min = 0,
                     value = 0)
      }
    } else {
      if (input$scale.t == "continuous") {
        numericInput(inputId = "kurtosis.t", 
                     label = withMathJax(helpText("The kurtosis of the distribution of \\(\\varepsilon_{T}\\) (0 if normal)")), 
                     min = 0,
                     value = 0)
      }
    }
  })
  
  output$obj_kurtosis.m <- renderUI({
    if (input$scale.m == "continuous" & input$scale.y == "continuous") {
      numericInput(inputId = "kurtosis.m", 
                   label = withMathJax(helpText("The kurtosis of the distribution of \\(\\varepsilon_{M}\\) (0 if normal)")), 
                   min = 0,
                   value = 0)
    }
  })
  
  output$obj_kurtosis.y <- renderUI({
    if (input$scale.y == "continuous") {
      numericInput(inputId = "kurtosis.y", 
                   label = withMathJax(helpText("The kurtosis of the distribution of \\(\\varepsilon_{Y}\\) (0 if normal)")), 
                   min = 0,
                   value = 0)
    }
  })
  
  # Display hypothesized effects (When calculating the hypothesized effect, I generate a single X because b * x is equivalent to b/sqrt(n.x) * (x_1 + ... + x_n.x))
  output$true.effect <- renderUI({
    #--- INPUT VALUE CHECKS -------------------------------------------------------#
    if(!is.null(input$kurtosis.t)){
      if (input$kurtosis.t < input$skewness.t^2 - 2) {
        validate("Error: Kurtosis must be no less than squared skewness minus 2.")
      }
    }
    if(!is.null(input$kurtosis.m)){
      if (input$kurtosis.m < input$skewness.m^2 - 2) {
        validate("Error: Kurtosis must be no less than squared skewness minus 2.")
      }
    }
    if(!is.null(input$kurtosis.y)){
      if (input$kurtosis.y < input$skewness.y^2 - 2) {
        validate("Error: Kurtosis must be no less than squared skewness minus 2.")
      }
    }
    
    if (input$obj == "choose_n") {
      # CHECK: Is N greater than 5 and an integer?
      if (input$TarN < 5 | !abs(input$TarN - round(input$TarN)) < .Machine$double.eps ^ 0.5) {
        validate("Error: \"Target Sample Size\" must be an integer greater than 5. Please change this value.")
      }
      
      # CHECK: Is TarN greater than Nlow?
      if (input$Nlow >= input$TarN) {
        validate("Error: \"Target Sample Size\" must be larger than \"Minimum Sample Size\". Please change these values.")
      }
      
      # CHECK: Is TarN smaller than Nhigh?
      if (input$Nhigh <= input$TarN) {
        validate("Error: \"Target Sample Size\" must be smaller than \"Maximum Sample Size\". Please change these values.")
      }
    } else {
      # CHECK: Is Target Power between 0 and 1?
      if (input$TarPow < 0 | input$TarPow > 1) {
        validate("Error: \"Target Power\" must be a number between 0 and 1. Please change this value.")
      }
    }  
    
    # CHECK: Is the significance level (%) between 0 and 100?
    if (input$sig < 0 | input$sig > 100) {
      validate("Error: \"Significance Level (%)\" must be a number between 0 and 100. Please change this value.")
    }
    
    # CHECK: Is Nlow greater than 5 and an integer?
    if (input$Nlow < 5 | !abs(input$Nlow - round(input$Nlow)) < .Machine$double.eps ^ 0.5) {
      validate("Error: \"Minimum Sample Size\" must be an integer greater than 5. Please change this value.")
    }
    
    # CHECK: Is Nhigh greater than 5 and an integer?
    if (input$Nhigh < 5 | !abs(input$Nhigh - round(input$Nhigh)) < .Machine$double.eps ^ 0.5) {
      validate("Error: \"Maximum Sample Size\" must be an integer greater than 5. Please change this value.")
    }
    
    # CHECK: Is Nsteps greater than 1 and an integer?
    if (input$Nsteps < 1 | !abs(input$Nsteps - round(input$Nsteps)) < .Machine$double.eps ^ 0.5) {
      validate("Error: \"Sample Size Steps\" must be an integer greater than 1. Please change this value.")
    }
    
    # CHECK: Is Nhigh greater than nlow?
    if (input$Nlow >= input$Nhigh) {
      validate("Error: \"Maxmimum Sample Size\" must be larger than \"Minimum Sampel Size\". Please change these values.")
    }
    
    # CHECK: Is Nsteps smaller than N range?
    if (abs(input$Nhigh - input$Nlow) < input$Nsteps) {
      validate("Error: \"Sample Size Steps\" must be smaller than the sample size range. Please change this value.")
    }  
    
    # CHECK: Is the number of replications > 5 and an integer?
    if (input$powReps < 5 | !abs(input$powReps - round(input$powReps)) < .Machine$double.eps ^ 0.5) {
      validate("Error: \"Number of replications per sample size for power calculation\" must be an integer greater than 5. Please change this value.")
    }
    
    # CHECK: Is the number of MC replications > 5 and an integer?
    if (input$mcmcReps < 5 | !abs(input$mcmcReps - round(input$mcmcReps)) < .Machine$double.eps ^ 0.5) {
      validate("Error: \"Number of Monte Carlo draws per replication for causal mediation analysis\" must be an integer greater than 5. Please change this value.")
    }
    
    # CHECK: Is the number of covariates 0?
    if(input$n.x == 0 & input$rand.t == FALSE)
      validate("Error: If the treatment is not randomized, the number of covariates should not be 0.")
    
    if(input$n.x == 0 & input$R2x.m != 0){
      if(input$scale.m == "continuous")
        validate("Error: If the proportion of variance in M explained by X is not 0, the number of covariates should not be 0. The number of covariates and the proportion of variance in M explained by X can be both 0 only when both the treatment and mediator are randomized.")
      if(input$scale.m == "binary")
        validate("Error: If the proportion of variance in latent M explained by X is not 0, the number of covariates should not be 0. The number of covariates and the proportion of variance in latent M explained by X can be both 0 only when both the treatment and mediator are randomized.")
    }
    
    if(input$n.x == 0 & input$R2x.y != 0){
      if(input$scale.y == "continuous")
        validate("Error: If the proportion of variance in Y explained by X is not 0, the number of covariates should not be 0. The number of covariates and the proportion of variance in Y explained by X can be both 0 only when both the treatment and mediator are randomized.")
      if(input$scale.y == "binary")
        validate("Error: If the proportion of variance in latent Y explained by X is not 0, the number of covariates should not be 0. The number of covariates and the proportion of variance in latent Y explained by X can be both 0 only when both the treatment and mediator are randomized.")
    }
    
    if(input$n.x != 0 & input$R2x.m == 0){
      if(input$scale.m == "continuous")
        validate("Error: If the number of covariates is not 0, the proportion of variance in M explained by X should not be 0. The number of covariates and the proportion of variance in M explained by X can be both 0 only when both the treatment and mediator are randomized.")
      if(input$scale.m == "binary")
        validate("Error: If the number of covariates is not 0, the proportion of variance in latent M explained by X should not be 0. The number of covariates and the proportion of variance in latent M explained by X can be both 0 only when both the treatment and mediator are randomized.")
    }
    
    if(input$n.x != 0 & input$R2x.y == 0){
      if(input$scale.y == "continuous")
        validate("Error: If the number of covariates is not 0, the proportion of variance in Y explained by X should not be 0. The number of covariates and the proportion of variance in Y explained by X can be both 0 only when both the treatment and mediator are randomized.")
      if(input$scale.y == "binary")
        validate("Error: If the number of covariates is not 0, the proportion of variance in latent Y explained by X should not be 0. The number of covariates and the proportion of variance in latent Y explained by X can be both 0 only when both the treatment and mediator are randomized.")
    }
    
    if(input$n.x == 0 & input$R2x.m == 0 & input$R2x.y == 0)
      validate("Warning: This shiny app can assess power or calculate sample size when the number of covariates is 0. However, please note that the number of covariates can be 0 only when both the treatment and mediator are randomized.")
    
    # Below I calculate hypothesized effects by generating data with 100000 observations
    # Reason: beta's that users specify are standardized coefficients. In the data analysis, we do not standardize the coefficients. Y (or Y*) and M (or M*) are always generated to have a mean of 0 and a standard deviation of 1. When T is binary, we are contrasting the two levels of actual T rather than standardized T. When T is continuous, at the specification stage, TM is standardized, but at the analysis stage, TM is not.
    rand.t = input$rand.t
    scale.t = input$scale.t
    scale.m = input$scale.m
    scale.y = input$scale.y
    n.x = input$n.x
    
    set.seed(input$seed)
    sgn.Rxt = sgn.Rxm = sgn.Rxy = 1
    
    # Generate X
    N = 100000 # Based on my testing through simulations, N = 10000 is still not enough for getting an accurate estimation of Rx.t.predictor or Rx.m.predictor.
    # The number of X does not affect hypothesized effect calculation. Hence, I only generate one X here.
    if(n.x != 0){
      x = rnorm(N)	
    } else {
      x = rep(0, N)
    }	
    
    # Generate the treatment
    if(rand.t == TRUE){
      R2x.t = 0
      if(scale.t == "binary"){
        t = rbinom(N, 1, input$p.t)
      } else {
        t = rnorm(N)
      }
    } else {
      R2x.t = input$R2x.t
      if(R2x.t == 0)
        validate("Error: If treatment is not randomized, the Proportion of variance in T explained by X should not be 0.")
      
      if(input$n.x == 0 & R2x.t != 0){
        if(input$scale.t == "continuous")
          validate("Error: If the proportion of variance in T explained by X is not 0, the number of covariates should not be 0. The number of covariates and the proportion of variance in T explained by X can be both 0 only when both the treatment and mediator are randomized.")
        if(input$scale.t == "binary")
          validate("Error: If the proportion of variance in latent T explained by X is not 0, the number of covariates should not be 0. The number of covariates and the proportion of variance in latent T explained by X can be both 0 only when both the treatment and mediator are randomized.")
      }
      
      # Transform R2x to bx
      bx.t = sqrt(R2x.t) * sgn.Rxt
      var.t = 1
      b0.t = 0
      var.et = var.t - var(b0.t + bx.t * x)
      if(var.et < 0)
        validate("Error: The error term of the treatment model has a negative variance in at least one replication. Please respecify the treatment model parameters.")
      et = rnorm(N, 0, sqrt(var.et))
      t = b0.t + bx.t * x + et
      if(scale.t == "binary"){
        t.star = t
        t[t.star > 0] = 1
        t[t.star <= 0] = 0
      } 
    }
    if(scale.t == "binary"){
      if(rand.t == FALSE){
        Rx.t.predictor = as.numeric(cor(x, t))
      } else {
        Rx.t.predictor = 0
      }
    } else {
      Rx.t.predictor = sqrt(R2x.t) * sgn.Rxt
    }
    
    var.m = 1
    b0.m = 0
    bt.m = input$bt.m
    bx.m = sqrt(input$R2x.m) * sgn.Rxm - input$bt.m * Rx.t.predictor
    var.em = var.m - var(b0.m + bt.m * scale(t) + bx.m * x)
    if(var.em < 0)
      validate("Error: The error term of the mediator model has a negative variance in at least one replication. Please respecify the mediator model parameters.")
    em = rnorm(N, 0, sqrt(var.em))
    # For any skewness and kurtosis, the hypothesized effects are the same. Hence, we can use em = rnorm() throughout when calculating hypothesized effects.
    m = b0.m + bt.m * scale(t) + bx.m * x + em  
    if(scale.m == "binary"){
      m.star = m
      m[m.star > 0] = 1
      m[m.star <= 0] = 0
    } 
    if(n.x != 0){
      Rx.tm.predictor = as.numeric(cor(x, t * m)) # The sign of Rx.tm is determined by the relationships between x and t/m. Hence, I do not force it to be positive.
    } else {
      Rx.tm.predictor = 0
    }
    if(scale.m == "binary"){
      if(n.x != 0){
        Rx.m.predictor = as.numeric(cor(x, m))
      } else {
        Rx.m.predictor = 0
      }
    } else {
      Rx.m.predictor = sqrt(input$R2x.m) * sgn.Rxm
    }
    
    var.y = 1
    b0.y = 0
    bt.y = input$bt.y
    bm.y = input$bm.y
    btm.y = input$btm.y
    bx.y = sqrt(input$R2x.y) * sgn.Rxy - input$btm.y * Rx.tm.predictor - input$bm.y * Rx.m.predictor - input$bt.y * Rx.t.predictor
    var.ey = var.y - var(b0.y + bt.y * scale(t) + bm.y * scale(m) + btm.y * scale(t * m) + bx.y * x)
    if(var.ey < 0)
      validate("Error: The error term of the outcome model has a negative variance in at least one replication. Please respecify the outcome model parameters.")
    ey = rnorm(N, 0, sqrt(var.ey))
    y = b0.y + bt.y * scale(t) + bm.y * scale(m) + btm.y * scale(t * m) + bx.y * x + ey
    if(scale.y == "binary"){
      y.star = y
      y[y.star > 0] = 1
      y[y.star <= 0] = 0
    } 
    data = data.frame(t, x, m, y)
    
    if(scale.m == "binary"){
      l.m = glm(m ~ t + x, data = data, family = binomial(link = "probit"))      
    } else {
      l.m = lm(m ~ t + x, data = data)
    }
    if(scale.y == "binary"){
      l.y = glm(y ~ t + m + t : m + x, data = data, family = binomial(link = "probit"))
    } else {
      l.y = lm(y ~ t + m + t : m + x, data = data)
    }
    b0.m = as.numeric(coef(l.m)[1])
    bt.m = as.numeric(coef(l.m)["t"])
    bx.m = as.numeric(coef(l.m)["x"])
    b0.y = as.numeric(coef(l.y)[1])
    bt.y = as.numeric(coef(l.y)["t"])
    bm.y = as.numeric(coef(l.y)["m"])
    btm.y = as.numeric(coef(l.y)["t:m"])
    bx.y = as.numeric(coef(l.y)["x"])
    
    if(scale.t == "binary"){
      treat.t = 1
      control.t = 0
    } else {
      treat.t = input$treat.t
      control.t = input$control.t
    } 
    
    if(scale.m == "continuous" & scale.y == "continuous"){
      if(input$effect == "TIE")
        true.effect = bt.m * (bm.y + btm.y * treat.t) * (treat.t - control.t)
      if(input$effect == "PDE")
        true.effect = (treat.t - control.t) * (bt.y + btm.y * (b0.m + bt.m * control.t))
      if(input$effect == "PIE")
        true.effect = bt.m * (bm.y + btm.y * control.t) * (treat.t - control.t)
      if(input$effect == "TDE")
        true.effect = (treat.t - control.t) * (bt.y + btm.y * (b0.m + bt.m * treat.t))
      if(input$effect == "INT")
        true.effect = bt.m * btm.y * (treat.t - control.t)^2
    }
    
    if(scale.m == "binary" & scale.y == "continuous"){
      pm = function(t.prime.value){
        return(pnorm(b0.m + bt.m * t.prime.value + bx.m * x))
      }
      if(input$effect == "TIE")
        true.effect = (bm.y + btm.y * treat.t) * (pm(treat.t) - pm(control.t))
      if(input$effect == "PDE")
        true.effect = (treat.t - control.t) * (btm.y * pm(control.t) + bt.y)
      if(input$effect == "PIE")
        true.effect = (bm.y + btm.y * control.t) * (pm(treat.t) - pm(control.t))
      if(input$effect == "TDE")
        true.effect = (treat.t - control.t) * (btm.y * pm(treat.t) + bt.y)
      if(input$effect == "INT")
        true.effect = (treat.t - control.t) * btm.y * (pm(treat.t) - pm(control.t))
      
      true.effect = mean(true.effect)
    }
    
    if(scale.m == "continuous" & scale.y == "binary"){
      sd.m = as.numeric(sqrt(var.em))
      py = function(t.value, t.prime.value){
        numerator = b0.y + bt.y * t.value + (bm.y + btm.y * t.value) * (b0.m + bt.m * t.prime.value + bx.m * x) + bx.y * x
        denominator = sqrt(((bm.y + btm.y * t.value) * sd.m)^2 + 1)
        return(pnorm(numerator/denominator))
      }
      if(input$effect == "TIE"){
        y1m1 = py(treat.t, treat.t)
        y1m0 = py(treat.t, control.t)
        true.effect = y1m1 - y1m0
      }
      if(input$effect == "PDE"){
        y1m0 = py(treat.t, control.t)
        y0m0 = py(control.t, control.t)
        true.effect = y1m0 - y0m0
      }
      if(input$effect == "PIE"){
        y0m1 = py(control.t, treat.t)
        y0m0 = py(control.t, control.t)
        true.effect = y0m1 - y0m0
      }
      if(input$effect == "TDE"){
        y1m1 = py(treat.t, treat.t)
        y0m1 = py(control.t, treat.t)
        true.effect = y1m1 - y0m1
      }
      if(input$effect == "INT"){
        y1m1 = py(treat.t, treat.t)
        y1m0 = py(treat.t, control.t)
        y0m1 = py(control.t, treat.t)
        y0m0 = py(control.t, control.t)
        true.effect = (y1m1 - y1m0) - (y0m1 - y0m0)
      }
      true.effect = mean(true.effect)
    }
    
    if(scale.m == "binary" & scale.y == "binary"){
      pm = function(t.prime.value){
        return(pnorm(b0.m + bt.m * t.prime.value + bx.m * x))
      }
      py = function(t.value, m.value){
        return(pnorm(b0.y + bt.y * t.value + (bm.y + btm.y * t.value) * m.value + bx.y * x))
      }
      if(input$effect == "TIE"){
        true.effect = (py(treat.t, 1) - py(treat.t, 0)) * (pm(treat.t) - pm(control.t))
      }
      if(input$effect == "PDE"){
        py.diff.1 = (py(treat.t, 1) - py(control.t, 1))
        py.diff.0 = (py(treat.t, 0) - py(control.t, 0))
        true.effect = (py.diff.1 - py.diff.0) * pm(control.t) + py.diff.0
      }
      if(input$effect == "PIE"){
        true.effect = (py(control.t, 1) - py(control.t, 0)) * (pm(treat.t) - pm(control.t))
      }
      if(input$effect == "TDE"){
        py.diff.1 = (py(treat.t, 1) - py(control.t, 1))
        py.diff.0 = (py(treat.t, 0) - py(control.t, 0))
        true.effect = (py.diff.1 - py.diff.0) * pm(treat.t) + py.diff.0
      }
      if(input$effect == "INT"){
        true.effect = ((py(treat.t, 1) - py(treat.t, 0)) - (py(control.t, 1) - py(control.t, 0))) * (pm(treat.t) - pm(control.t))
      }
      true.effect = mean(true.effect)
    }
    HTML(paste0(tags$sup("2"), "Hypothesized ", input$effect, " = ", round(true.effect, 2)))
  })
  
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}