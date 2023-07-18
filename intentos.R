input <- tibble(
  alternative = "The two groups are different",
  mean1 = 18, mean2 = 20,
  sd1 = 3, sd2 = 3,
  reps = 100,
  sample_size = 50,
  alpha = 0.05
)


sel.sides <- ifelse(input$alternative == "The two groups are different", "two.sided",
                  ifelse(input$alternative == "Group 1 > Group 2", "greater",
                         "less"))

dat <- tibble(A = rnorm(1000,  mean = input$mean1, sd = input$sd1),
                   B = rnorm(1000, mean = input$mean2, sd = input$sd2))

dat.long <- dat %>%
    pivot_longer(cols = A:B, names_to = "Group", values_to = "Value")

samples.t <- map_dfr(seq_len(input$reps), ~dat %>%
                            sample_n(input$sample_size) %>%
                            mutate(sample.t = as.factor(.x)))

compfun <- function(x, y) {
  comp = (t.test(x, y,
                 alternative = sel.sides, paired = TRUE))
}

comps.t <- ddply(samples.t, .(sample.t), summarise,
                 p = round(compfun(x = A, y = B)$p.value, 3),
                 "Significance" = ifelse(p <= input$alpha, "Significant", "Non-significant"))

power.t <- sum(comps.t$Significance == "Significant") / input$reps

ggplot(comps.t, aes(x = p, fill = Significance)) +
  scale_fill_hue(direction = -1) +
  geom_histogram(bins = 1/input$alpha, breaks = seq(0, 1, input$alpha)) +
  labs(y = "Count", x = "p-value", title = "Independent t-test") +
  annotate("text", x = 0.5, y = 500, 
           label = paste0("Power = ", round(power.t, 3), "\nSamplke size = ", input$sample_size))





output$powerPlot <- renderPlot({
  sel.sides <- reactive({
    sides <- ifelse(input$alternative == "The two groups are different", "two.sided",
                    ifelse(input$alternative == "Group 1 > Group 2", "greater",
                           "less"))
    return(sides)
  })
  dat <- reactive({
    datPre <- tibble(A = rnorm(1000,  mean = input$mean1, sd = input$sd1),
                     B = rnorm(1000, mean = input$mean2, sd = input$sd2))
    return(datPre)
  })
  dat.long <- reactive({
    dat.longPre <- dat %>%
      pivot_longer(cols = A:B, names_to = "Group", values_to = "Value")
    return(dat.longPre)
  })
  samples.t <- reactive({
    samples.tPre <- map_dfr(seq_len(input$reps), ~dat %>%
                              sample_n(input$sample_size) %>%
                              mutate(sample.t = as.factor(.x)))
    return(samples.tPre)
  })
  compfun <- function(x, y) {
    comp = (t.test(x, y,
                   alternative = sel.sides, paired = TRUE))
  }
  comps.t <- ddply(samples.t, .(sample.t), summarise,
                   p = round(compfun(x = A, y = B)$p.value, 3),
                   "Significance" = ifelse(p <= input$alpha, "Significant", "Non-significant"))
  power.t <- reactive({
    power.tPre <- sum(comps.t$Significance == "Significant") / input$reps
    return(power.tPre)
  })
  ggplot(comps.t, aes(x = p, fill = Significance)) +
    scale_fill_hue(direction = -1) +
    geom_histogram(bins = 1/input$alpha, breaks = seq(0, 1, input$alpha)) +
    labs(y = "Count", x = "p-value", title = "Independent t-test") +
    annotate("text", x = 0.5, y = 500, 
             label = paste0("Power = ", round(power.t, 3), "\nSamplke size = ", input$sample_size))
})
}