library(tidyverse)
library(lubridate)
library(showtext)
library(neverhpfilter)
library(mFilter)
library(ggthemes)
library(patchwork)
library(tsibble)
library(bayesrules)


theme_ms <- function() {
  theme_minimal(base_family = "IBM Plex Sans") +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey80", color = NA),
          #axis.title.x = element_text(hjust = 0),
          #axis.title.y = element_text(hjust = 1),
          legend.title = element_text(face = "bold"))
}

theme_set(theme_ms())


#---

red_pink <- "#e64173"
turquoise <- "#20B2AA"
orange <- "#FFA500"
red <- "#E02C05"
blue <- "#2b59c3"
green <- "#0FDA6D"
grey_light <- "grey70"
grey_mid <- "grey50"
grey_dark <- "grey20"
purple <- "#6A5ACD"
met_slate <- "#23373b" 



#----

#---- Prep:

dat <- read_csv("data.csv")


dat <- dat %>% 
  mutate(period = seq(as.Date("1947-01-01"), by = "quarter", length.out = 304))


dat_yr <- dat |> 
  mutate(yr = floor_date(period, unit = "year")) |> 
  group_by(yr) |> 
  reframe(avg_psi = mean(psi),
            avg_psigr = mean(psigr),
            yr = year(yr))




dat_yr |> 
  ggplot(aes(x = yr, y = avg_psi)) +
  geom_line(linewidth = .6, color = met_slate) +
  labs(x = "",
       y = "Index 2017=100",
       title = "U.S. labor share of income: 1947–2022",
       subtitle = "Bureau of Labor Statistics’ (BLS) “headline measure”",
       caption = "Source: U.S. Bureau of Labor Statistics.") +
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  geom_vline(xintercept = 1980,
             linetype = 3)
  

dat_yr |> 
  ggplot(aes(x = yr, y = avg_psigr)) +
  geom_line(linewidth = .6, color = blue) +
  labs(x = "",
       y = "%",
       title = "U.S. labor share of income: 1947–2022",
       subtitle = "Share of total compensation",
       caption = "Source: U.S. Bureau of Economic Analysis.") +
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  geom_vline(xintercept = 1980,
             linetype = 3)






#######


dat <- 
  tibble(
    p_grid = seq( from=0 , to=1 , length.out=1000 ),
    prior = dbeta(p_grid, shape1 = 20, shape2 = 60),
    likelihood = dbinom(x = 60, size = 100, prob = p_grid),
    posterior = prior * likelihood,
    std_posterior = posterior / sum(posterior)
  )

pp <- dat %>% 
  ggplot(aes(x = p_grid, y = prior)) +
  geom_line(size = 0.8, color = "#95608e") +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians",
       title = "Prior") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


pp1 <- dat %>% 
  ggplot(aes(x = p_grid, y = likelihood)) +
  geom_line(size = 0.8, color = "#5bc810") +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians",
       title = "Likelihood") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


pp2 <- dat %>% 
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_line(size = 0.8) +
  labs(y = "Plausibility",
       x = "Proportion of Bayesians",
       title = "Posterior") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


pp | pp1 | pp2




gr1 <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  xlim(c(0, 1)) + ylim(c(0, 9)) +
  stat_function(fun = dbeta, args = list(shape1 = 20, shape2 = 60), color = orange, size = 1, alpha = 0.5) +
  stat_function(fun = dbeta, args = list(shape1 = 20, shape2 = 60), geom = "area", fill = orange, alpha = 0.5) +
  labs(x = expression(theta),
       y = "Density",
       title = "Proportion of Emerson students interested in Economics") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)


gr2 <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  xlim(c(0, 1)) + ylim(c(0, 0.09)) +
  stat_function(fun = dbinom, args = list(x = 60, size = 100), color = "#264d84", size = 1, alpha = 0.5) +
  stat_function(fun = dbinom, args = list(x = 60, size = 100), geom = "area", fill = "#264d84", size = 1, alpha = 0.5) +
  labs(x = "y",
       y = "Density",
       title = "Some new data") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)

gr3 <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  xlim(c(0, 1)) + ylim(c(0, 11)) +
  stat_function(fun = dbeta, args = list(shape1 = 80, shape2 = 100), color = turquoise, size = 1, alpha = 0.7) +
  stat_function(fun = dbeta, args = list(shape1 = 80, shape2 = 100), geom = "area", fill = turquoise, alpha = 0.5) +
  labs(x = expression(theta),
       y = "Density",
       title = "Our updated belief") +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13)



######





plot_beta_binomial(alpha = 20, beta = 60, y = 60, n = 100) + labs(y = "hello")

summarize_beta_binomial(alpha = 20, beta = 60, y = 60, n = 100)
