#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

Sys.setlocale("LC_ALL", "en_US.UTF-8")
library(shiny)
library(thematic)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse) 
library(effectsize)
library(plyr)
library(scales)

input <<- tibble(
  alts = "Group 1 ≠ Group 2",
  mean1 = 18.1, mean2 = 3.1,
  sd1 = 20.3, sd2 = 2.8,
  reps = 1000,
  sample_size = 50,
  alpha = 0.05
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  
  # Application title
  titlePanel(title = tags$link(rel = "icon",
                               type = "image",
                               href = "https://image.pngaaa.com/393/402393-middle.png"),
             "PowerSimulate (independent t- test)"),
  HTML("<center><img src='powersimulate.svg'' width='600'></center>"),
  tags$h3(HTML("<center>Independent <em>t</em>-test</center>")),
  p(HTML("<center>Code available from
      <a style=color:#ff5555;  href='https://github.com/JDLeongomez/PowerSimulate_ind_t_EN'>GitHub</a>
      - Created by
      <a style=color:#ff5555;  href='https://jdleongomez.info/en/'>Juan David Leongómez</a>
      · 2023 · <a style=color:#4075de;  href='https://shiny.jdl-svr.lat/PowerSimulate_ind_t_ES/'>
      Versión en español</a> 
      · <a style=color:#ff5555;  href='https://shiny.jdl-svr.lat/PowerSimulate_corr_EN'>PowerSimulate: Correlation.</a></center>")),
  hr(),
  p(HTML("<center>Power analysis based on the simulation of a population, and the probability of
         obtaining a significant result with a sample of a given size.<br>Although more direct 
         tools for power analysis exist for <em>t</em>-tests, this application relies on 
         simulations to illustrate the concept of statistical power.</center>")),
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
                        value = 18.1,
                        step = 0.0001,
                        width = '300px'),
           numericInput(inputId = "sd1",
                        label = "Standard deviation",
                        min = -Inf,
                        max = Inf,
                        value = 3.1,
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
                        value = 20.3,
                        step = 0.0001,
                        width = '300px'),
           numericInput(inputId = "sd2",
                        label = "Standard deviation",
                        min = -Inf,
                        max = Inf,
                        value = 2.8,
                        step = 0.0001,
                        width = '300px')
    ),
    column(4,
           tags$h1("Population effect size"),
           tags$h3("If this was the difference in the population"),
           plotOutput("effectPlot") %>% 
             withSpinner(color = "#ff5555"),
           tags$p(HTML("<b style=color:#ff5555;>NOTE:</b> Cohen's 
                       <em>d</em> is the most common effect size for standardised
                       differences between two means. However, it has some limitations:
                       first, it pooles the standard deviations of the two groups, which may not be 
                       ideal if they are quite different, and tends to provide biased estimates
                       when the groups have small sample sizes. For this reason, Hedges' <em>g</em>
                       and Glass's delta are valuable alternatives.")),
    ),
    column(2,
           tags$h2("Simulation parameters"),
           tags$h4("Sample size"),
           tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background:#ff5555}")),
           sliderInput(inputId = "sample_size",
                       label = "Sample size per group",
                       min = 5,
                       max = 1000,
                       value = 30,
                       step = 1,
                       width = '300px'),
           tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background:#ff5555}")),
           sliderInput(inputId = "alpha",
                       label = HTML("Significance level (tipically &alpha; = 0.05)"),
                       min = 0,
                       max = 1,
                       value = 0.05,
                       step = 0.001,
                       width = '300px'),
           selectInput(inputId = "alts",
                       label = "Hypothesis",
                       choices = c("Group 1 ≠ Group 2", 
                                   "Group 1 > Group 2",
                                   "Group 1 < Group 2"
                       )),
           numericInput(inputId = "reps",
                        label = HTML("Number of simulations<br>
                                     <span style='font-weight:normal'>Larger numbers increase accuracy but take more time. 
                                     By default it runs only 100 simulations, but once you have checked all 
                                     parameters, I suggest running 1000+ simulations to increase accuracy.</span>"),
                        min = 1,
                        max = 10000000,
                        value = 100,
                        step = 1,
                        width = '300px')
    ),
    column(4,
           tags$h1("Statistical power"),
           tags$h3("This is the statistical power you would reach"),
           plotOutput("powerPlot") %>% 
             withSpinner(color = "#ff5555"),
           htmlOutput("powText")
    )
  )
)

server <- function(input, output, session) {
  
  # Simulate population
  dat <- reactive({
    datos <- tibble(A = rnorm(100000, mean = input$mean1, sd = input$sd1),
                    B = rnorm(100000, mean = input$mean2, sd = input$sd2))
    return(datos)
  })
  
  # Calculate effect sizes
  cohen.d <- reactive({
    coh.d <- cohens_d(x = dat()$A, y = dat()$B,
                      pooled_sd = FALSE,
                      paired = FALSE,
                      ci = 0.95)
    return(coh.d)
  })
  hedges.g <- reactive({
    hed.g <- hedges_g(x = dat()$A, y = dat()$B,
                      pooled_sd = FALSE,
                      paired = FALSE,
                      ci = 0.95)
    return(hed.g)
  })   
  glass.delta <- reactive({
    gla.d <- glass_delta(x = dat()$A, y = dat()$B,
                         ci = 0.95)
    return(gla.d)
  })
  
  # Create normal distributions with input means and SDs
  dat.dist <- reactive({
    x = seq(min(dat()), max(dat()), length = 200)
    dat.distri <- data.frame(A = dnorm(x, mean = input$mean1, sd = input$sd1),
                             B = dnorm(x, mean = input$mean2, sd = input$sd2), x = x) %>%
      pivot_longer(cols = A:B, names_to = "Group", values_to = "Value")
    return(dat.distri)
  })
  
  # Population distribution plot 
  output$effectPlot <- renderPlot({
    ggplot(data = dat.dist(), aes(x = x, fill = Group)) +
      geom_polygon(aes(y = Value), alpha = 0.8) +
      xlab("Value") + ylab("Probability density") + 
      geom_vline(aes(xintercept = input$mean1, color = "white"),
                 linetype="dashed",
                 show.legend = FALSE) +
      geom_vline(aes(xintercept = input$mean2, color = "white"),
                 linetype="dashed",
                 show.legend = FALSE) +
      scale_fill_manual(values = c("#4075de", "#ff5555"),
                        labels = c(input$label1, input$label2)) +
      annotate("text", x = min(dat.dist()$x), y = Inf, 
               hjust = 0, vjust = 2, size = 7,
               label = paste0("Cohen's d = ", round(abs(cohen.d()$Cohens_d), 2))) +
      annotate("text", x = min(dat.dist()$x), y = Inf, 
               hjust = 0, vjust = 6,
               label = paste0("Hedges' g = ", round(abs(hedges.g()$Hedges_g), 2))) +
      annotate("text", x = min(dat.dist()$x), y = Inf, 
               hjust = 0, vjust = 8,
               label = paste0("Glass's delta = ", round(abs(glass.delta()$Glass_delta), 2))) +
      geom_segment(aes(x = input$mean1, y = max(dat.dist()$Value)*0.5, 
                       xend = input$mean2, yend = max(dat.dist()$Value)*0.5), 
                   arrow = arrow(length = unit(0.02, "npc"), ends = "both")) +
      annotate("text", x = Inf, y = Inf, 
               hjust = 1.1, vjust = 2, size = 5,
               label = paste0("Mean difference = ", round(abs(input$mean1 - input$mean2), 2))) +
      theme(legend.position="bottom", 
            legend.title=element_text(size=14),
            legend.text = element_text(size = 12))
  })
  
  # Create object with selected hypothesis alternative
  altern <<- reactive({
    dplyr::case_when(
      input$alts == "Group 1 ≠ Group 2" ~ "two.sided",
      input$alts == "Group 1 > Group 2" ~ "greater",
      TRUE ~ "less")
  })
  
  sig.lev <<- reactive({
    input$alpha
  })
  
  # Simulate samples and test significance in each
  dat.sim <- reactive({
    req(input$alts)
    dato <- ddply(map_dfr(seq_len(input$reps), ~dat() %>%
                            sample_n(input$sample_size) %>%
                            mutate(sample = as.factor(.x))),
                  .(sample),
                  summarise,
                  p = round(t.test(x = A, y = B,
                                   alternative = altern(), 
                                   paired = TRUE)$p.value, 3),
                  "Significance" = ifelse(p <= sig.lev(), "Significant", "Non-significant"))
    return(dato)
  })
  
  # Power simulation plot 
  output$powerPlot <- renderPlot({
    ggplot(dat.sim(), aes(x = p, fill = Significance)) +
      scale_fill_hue(direction = -1) +
      geom_histogram(bins = 1/input$alpha, breaks = seq(0, 1, input$alpha), alpha = 0.8) +
      scale_fill_manual(values = c("#4075de", "#ff5555")) +
      labs(y = "Count", x = "p-value") +
      scale_x_continuous(breaks = pretty_breaks(n = 20)) +
      annotate("text", x = 0.5, y = Inf, size = 7, vjust = 2,
               label = paste0("Power (1 - β) = ", round(sum(dat.sim()$Significance == "Significant") / input$reps, 3))) +
      annotate("text", x = 0.5, y = Inf, vjust = 5,
               label = paste0("Sample size = ", input$sample_size)) +
      annotate("text", x = 0.5, y = Inf, vjust = 6.5,
               label = paste0("α = ", input$alpha)) +
      theme(legend.position="bottom", 
            legend.title=element_text(size=14),
            legend.text = element_text(size = 12)) +
      guides(fill = guide_legend(reverse=TRUE))
  })
  
  output$powText <- renderText({
    paste("<b style=color:#ff5555;>INTERPRETATION: </b>
          The power is nothing more than the proportion of significant results 
          (<em>p</em> < α). So, if the true difference in the population was as
          specified, with a random sample of <font color=\'#ff5555\'><b>", input$sample_size, 
          "</b></font>, you would get a significant result in aproximately <font color=\'#ff5555\'><b>", 
          percent(round(sum(dat.sim()$Significance == "Significant") / input$reps, 2)),
          "</b></font> of the cases.")
  })
}

# Same theme for plots
thematic_shiny()

# Run the application 
shinyApp(ui = ui, server = server)
