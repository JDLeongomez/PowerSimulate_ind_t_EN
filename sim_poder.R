library(tidyverse) # For data manipulation and visualization
library(lsr) # For effect size calculation
library(plyr) # For data manipulation
library(kableExtra)

set.seed(1)

dat <- tibble(A = rnorm(60000, mean = 10, sd = 2),
              B = rnorm(60000, mean = 11, sd = 2))


dat.long <- dat %>% 
  pivot_longer(cols = A:B, names_to = "Condición", values_to = "Valor")

mu <- dat %>% 
  pivot_longer(cols = A:B, names_to = "Condición", values_to = "Valor") %>% 
  group_by(Condición) %>%    
  dplyr::summarise(Mean = mean(Valor),
                   SD= sd(Valor)) 

cohen.d <- cohensD(x = dat$A, y = dat$B,
                   method = "paired")

dat  |>  
  pivot_longer(cols = A:B, names_to = "Condición", values_to = "Valor") |> 
  ggplot(aes(x = Valor, fill = Condición, colour = Condición)) +
  geom_histogram(alpha = 0.3, position = "identity", binwidth = 1) +
  geom_vline(data = mu, aes(xintercept = Mean, color = Condición),
             linetype="dashed") +
  labs(x = "Puntaje", y = "Conteo") +
  annotate("text", x = 5, y = 12000, 
           label = paste0("d de Cohen = ", round(cohen.d, 2)))


num_sims.t = 1000 
sample_size.t = 300
alpha.t = 0.05 

samples.t <- map_dfr(seq_len(num_sims.t), ~dat %>%
                       sample_n(sample_size.t) %>% 
                       mutate(sample.t = as.factor(.x)))

compfun <- function(x, y) {
  comp = (t.test(x, y,
                 alternative = "two.sided", paired = TRUE))
}

comps.t <- ddply(samples.t, .(sample.t), summarise,
                 t = round(compfun(A, B)$statistic, 2),
                 p = round(compfun(A, B)$p.value, 3),
                 "Significancia" = ifelse(p <= alpha.t, "Significativo", "No significativo"))

power.t <- sum(comps.t$Significancia == "Significativo") / num_sims.t

p.t <- ggplot(comps.t, aes(x = p, fill = Significancia)) +
  scale_fill_hue(direction = -1) +
  geom_histogram(bins = 1/alpha.t, breaks = seq(0, 1, alpha.t)) +
  labs(y = "Conteo", x = "Valor p", title = "Pruebas t pareadas") +
  annotate("text", x = 0.5, y = 500, 
           label = paste0("Poder = ", round(power.t, 3), "\nTamaño de muestra = ", sample_size.t))
p.t