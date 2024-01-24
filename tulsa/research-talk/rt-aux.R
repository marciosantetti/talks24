library(tidyverse)
library(lubridate)
library(showtext)
library(neverhpfilter)
library(mFilter)
library(ggthemes)
library(patchwork)
library(tsibble)


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



library(bayesrules)

plot_normal_normal(mean = 6.5, sd = 0.4, sigma = 0.5,
                   y_bar = 5.735, n = 25)
