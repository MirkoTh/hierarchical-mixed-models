library(tidyverse)
library(lme4)


# Simulate Data -----------------------------------------------------------

N <- 9
k <- 10

b0_mn <- 0
b1_mn <- .75
b0_sd <- 2.5
b1_sd <- 1
error_sd <- 1.5

b0_indiv <- rnorm(N, b0_mn, b0_sd)
b1_indiv <- rnorm(N, b1_mn, b1_sd)

tbl <- tibble(
  idx = factor(rep(seq(1, N, by = 1), each = k)),
  x1 = rep(seq(1, 10, by = 1), N),
  y_independent = b0_mn + b1_mn * x1 + rnorm(N * k, sd = error_sd),
  y_ic = rep(b0_indiv, each = k) + b1_mn * x1 + rnorm(N * k, sd = error_sd),
  y_ic_sl = rep(b0_indiv, each = k) + rep(b1_indiv, each = k) * x1 + rnorm(N * k, sd = error_sd)
)
tbl$x1 <- tbl$x1 - mean(tbl$x1)

tbl_long <- pivot_longer(
  tbl,
  cols = c(y_independent, y_ic, y_ic_sl)
) %>% rename(structure = name, y = value)

tbl_long$structure <- factor(
  tbl_long$structure, 
  levels = c("y_independent", "y_ic", "y_ic_sl"),
  labels = c("1", "2", "3")
)


# Plot the Data -----------------------------------------------------------

pl_base <- ggplot(tbl_long, aes(x1, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ structure) +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 1, angle = 45)) +
  scale_x_continuous(breaks = seq(min(tbl_long$x1), max(tbl_long$x1), by = 1)) +
  labs(
    x = "Nr. Objects \n",
    y = "RT (s)"
  )

pl_points <- pl_base + 
  geom_point(aes(color = idx)) +
  scale_color_brewer(palette = "Set1")

pl_slopes <- pl_points +
  geom_smooth(method = "lm", se = FALSE, aes(color = idx))



# Predict Using Different Model Structure ---------------------------------


plot_preds <- function(tbl) {
  ggplot(tbl, aes(x1, y)) +
    geom_point(aes(color = idx)) +
    geom_line(aes(y = preds, color = idx)) +
    facet_wrap(~ structure) +
    theme_bw() +
    theme(axis.text.x = element_text(hjust = 1, angle = 45)) +
    scale_x_continuous(breaks = seq(min(tbl_long$x1), max(tbl_long$x1), by = 1)) +
    scale_color_brewer(palette = "Set1") +
    labs(
      x = "Nr. Objects \n",
      y = "RT (s)"
    )
}

tbl_ri <- tbl_long %>% filter(structure == 2)
m_ri <- lmer(y ~ x1 + (1 | idx), data = tbl_ri)
tbl_ri$preds <- predict(m_ri, tbl_ri)
plot_preds(tbl_ri)

tbl_rs <- tbl_long %>% filter(structure == 3)
m_rs <- lmer(y ~ x1 + (x1 | idx), data = tbl_rs)
tbl_rs$preds <- predict(m_rs, tbl_rs)
plot_preds(tbl_rs)






