#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(thematic)
library(shinythemes)
library(shinycssloaders)
library(tidyverse) 
library(effectsize)
library(plyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  
  # Application title
  titlePanel(title = tags$link(rel = "icon",
                               type = "image",
                               href = "https://image.pngaaa.com/393/402393-middle.png"),
             "Power analysis (independent t- test)"),
  HTML("<img src='ind_t_eng.svg'' width='600'>"),
  p(HTML("Power analysis based on the simulation of a population, and the probability of 
         obtaining a significant result with a sample of a given size")),
  p(HTML("Code available from
      <a style=color:#ff5555;  href='https://github.com/JDLeongomez/ProbDnD_EN'>GitHub</a>
      - Created by
      <a style=color:#ff5555;  href='https://jdleongomez.info/en/'>Juan David Leongómez</a>
      · 2023 · <a style=color:#4075de;  href='https://shiny.jdl-svr.lat/ProbDnD/'>Versión en español</a>
      ")),
  p(),
  hr(),
  fluidRow(
    column(2,
           tags$h2("Group parameters"),
           tags$h4("Group 1"),
           textInput(inputId = "label1",
                     label = "Label for group 1",
                     value = "Control",
                     width = '300px'),
           numericInput(inputId = "mean1",
                        label = "Mean",
                        min = -Inf,
                        max = Inf,
                        value = 18,
                        step = 0.0001,
                        width = '300px'),
           numericInput(inputId = "sd1",
                        label = "Standard deviation",
                        min = -Inf,
                        max = Inf,
                        value = 3,
                        step = 0.0001,
                        width = '300px'),
           hr(),
           tags$h4("Group 2"),
           textInput(inputId = "label2",
                     label = "Label for group 2",
                     value = "Experimental",
                     width = '300px'),
           numericInput(inputId = "mean2",
                        label = "Mean",
                        min = -Inf,
                        max = Inf,
                        value = 20,
                        step = 0.0001,
                        width = '300px'),
           numericInput(inputId = "sd2",
                        label = "Standard deviation",
                        min = -Inf,
                        max = Inf,
                        value = 3,
                        step = 0.0001,
                        width = '300px')
           ),
    column(4,
           tags$h1("Population effect size"),
           plotOutput("effectPlot") %>% 
             withSpinner(color = "#ff5555"),
           tags$p(HTML("<b style=color:#ff5555;>NOTE: </b>"),
                  " For the histogram of the simulation to look right,
                      the", HTML("<b> minimum target score</b>"), "must be within the
                      range of the sum of the dice rolled and the modifier.
                      For example, when rolling", HTML("<b> 1d20 + 3</b>"), "the result can only
                      be between 4 and 23. If you select a", HTML("<b> minimum target score</b>"),
                  "smaller than 4 or greater than 23, the histogram will look
                      strange, although the", HTML("<b> probability</b>"), "(shown below) will be correct"),
    ),
    column(2,
           tags$h2("Simulation parameters"),
           tags$h4("Sample size"),
           sliderInput(inputId = "sample_size",
                       label = "Sample size per group",
                       min = 5,
                       max = 1000,
                       value = 30,
                       step = 1,
                       width = '300px'),
           sliderInput(inputId = "alpha",
                       label = HTML("Significance level &alpha; (tipically 0.05)"),
                       min = 0,
                       max = 1,
                       value = 0.05,
                       step = 0.001,
                       width = '300px'),
           selectInput(inputId = "alternative",
                       label = "Hypothesis",
                       choices = c("Group 1 ≠ Group 2", 
                                   "Group 1 > Group 2",
                                   "Group 2 > Group 1")),
           numericInput(inputId = "reps",
                        label = "Number of simulations (No need to change it)",
                        min = 1,
                        max = 10000000,
                        value = 100,
                        step = 1,
                        width = '200px')
           ),
    column(4,
           tags$h1("Statistical power"),
           plotOutput("powerPlot") %>% 
             withSpinner(color = "#ff5555"),
           tags$p(HTML("<b style=color:#ff5555;>NOTE: </b>"),
                  " For the histogram of the simulation to look right,
                      the", HTML("<b> minimum target score</b>"), "must be within the
                      range of the sum of the dice rolled and the modifier.
                      For example, when rolling", HTML("<b> 1d20 + 3</b>"), "the result can only
                      be between 4 and 23. If you select a", HTML("<b> minimum target score</b>"),
                  "smaller than 4 or greater than 23, the histogram will look
                      strange, although the", HTML("<b> probability</b>"), "(shown below) will be correct")
           )
    )
)

server <- function(input, output, session) {
  output$effectPlot <- renderPlot({
    dat <- tibble(A = rnorm(1000, mean = input$mean1, sd = input$sd1),
                  B = rnorm(1000, mean = input$mean2, sd = input$sd2))
    cohen.d <- cohens_d(x = dat$A, y = dat$B,
                        pooled_sd = FALSE,
                        paired = FALSE,
                        ci = 0.95)
    hedges.g <- hedges_g(x = dat$A, y = dat$B,
                         pooled_sd = FALSE,
                         paired = FALSE,
                         ci = 0.95)
    glass.delta <- glass_delta(x = dat$A, y = dat$B,
                               ci = 0.95)
    x = seq(min(dat), max(dat), length=200)
    dat.dist <- data.frame(A = dnorm(x, mean = input$mean1, sd = input$sd1),
                           B = dnorm(x, mean = input$mean2, sd = input$sd2), x = x) %>%
      pivot_longer(cols = A:B, names_to = "Group", values_to = "Value")
    ggplot(data = dat.dist, aes(x = x, fill = Group)) +
      geom_polygon(aes(y = Value), alpha = 0.4) +
      xlab("Value") + ylab("Probability density") + 
      geom_vline(aes(xintercept = input$mean1, color = "white"),
                 linetype="dashed",
                 show.legend = FALSE) +
      geom_vline(aes(xintercept = input$mean2, color = "white"),
                 linetype="dashed",
                 show.legend = FALSE) +
      scale_fill_discrete(labels = c(input$label1, input$label2)) +
      annotate("text", x = min(dat.dist$x), y = Inf, 
               hjust = 0, vjust = 2, size = 7,
               label = paste0("Cohen's d = ", round(abs(cohen.d$Cohens_d), 2))) +
      annotate("text", x = min(dat.dist$x), y = Inf, 
               hjust = 0, vjust = 6,
               label = paste0("Hedge's g = ", round(abs(hedges.g$Hedges_g), 2))) +
      annotate("text", x = min(dat.dist$x), y = Inf, 
               hjust = 0, vjust = 8,
               label = paste0("Glass's delta = ", round(abs(glass.delta$Glass_delta), 2))) +
      geom_segment(aes(x = input$mean1, y = max(dat.dist$Value)*0.5, 
                       xend = input$mean2, yend = max(dat.dist$Value)*0.5), 
                   arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
      annotate("text", x = (input$mean1 + input$mean2)/2, y = 0, 
               vjust = 0.5, hjust = -0.5, angle = 90, size = 5,
               label = paste0("Difference = ", round(abs(input$mean1 - input$mean2), 2))) +
      theme(legend.position="bottom", 
            legend.title=element_text(size=14),
            legend.text = element_text(size = 12))
    })
  
  output$powerPlot <- renderPlot({
    hypoth <- reactive({
      hypo <- ifelse(input$alternative == "Group 1 ≠ Group 2", "two.sided",
           ifelse(input$alternative == "Group 1 > Group 2", "greater",
                  "less"))
      return(hypo)
    })
    dat.sim <- reactive({
      dtos <- tibble(A = rnorm(50000,  mean = input$mean1, sd = input$sd1),
                      B = rnorm(50000, mean = input$mean2, sd = input$sd2))
      return(datos)
    })
    samples.sim <- reactive({
      samps <- map_dfr(seq_len(input$reps), ~dat.sim %>%
                           sample_n(input$sample_size) %>%
                           mutate(sample = as.factor(.x)))
      return(samps)
    })
    testfun <- reactive({
      myf <- function(x, y) {
        comp = (t.test(x, y,
                       alternative = hypoth, 
                      paired = TRUE))
      }
      return(myf)
    })
    #testfun <- testfun()
    comps <- reactive({
      compas <- ddply(samples.sim, .(sample), summarise,
                      p = testfun(x = A, y = B)$p.value,
                      "Significance" = ifelse(p <= input$alpha, "Significant", "Non-significant"))
      return(compas)
    })
    power <- reactive({
      powsum <- sum(comps$Significance == "Significant") / input$reps
      return(powsum)
    })
    ggplot(comps, aes(x = p, fill = Significance)) +
      scale_fill_hue(direction = -1) +
      geom_histogram(bins = 1/input$alpha, breaks = seq(0, 1, input$alpha), alpha = 0.8) +
      labs(y = "Count", x = "p-value") +
      annotate("text", x = 0.5, y = Inf, size = 7, vjust = 2,
               label = paste0("Power = ", round(power, 3))) +
      annotate("text", x = 0.5, y = Inf, vjust = 5,
               label = paste0("Sample size = ", input$sample_size)) +
      theme(legend.position="bottom", 
            legend.title=element_text(size=14),
            legend.text = element_text(size = 12))
  })
}

thematic_shiny()
# Run the application 
shinyApp(ui = ui, server = server)
