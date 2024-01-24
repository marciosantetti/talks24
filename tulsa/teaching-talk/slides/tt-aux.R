library(pacman)
p_load(broom, latex2exp, ggplot2, ggthemes, ggforce, viridis, dplyr, magrittr, knitr, parallel, xaringanExtra,
       tidyverse, sjPlot, extrafont, mathjaxr, ggforce, furrr, kable, kableExtra,
       fontawesome, ggthemr, hrbrthemes, janitor, plotly, ggeasy)


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




##---- Stoppages:

stp <- read_csv("stoppages.csv")


stp |> 
  ggplot(aes(x = year, y = stoppages)) +
  geom_line(linewidth = 0.7, color = blue) +
  geom_point(color = blue) +
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  labs(title = "Work stoppages with at least 1,000 workers: 1947–2022",
       y = "# Stoppages",
       x = "",
       caption = "Source: U.S. Bureau of Labor Statistics.")


##=========================================================================================##


##--- TCU:

tcu <- read_csv("tcu.csv")


tcu <- tcu |> 
  clean_names()

tcu |> ggplot(aes(x = date, y = tcu)) +
  geom_line(linewidth = 0.7, color = grey_dark) +
  #geom_point(color = blue) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  labs(title = "U.S. Capacity utilization: Total index",
       subtitle = "Jan 1967 – Dec 2023",
       y = "% of capacity",
       x = "",
       caption = "Source: U.S. Federal Reserve System.")
  

##=========================================================================================##


##--- UNRATE:

unrate <- read_csv("unrate.csv") |> 
  clean_names()


uu <- unrate |> 
  ggplot(aes(x = date, y = unrate)) +
  geom_line(linewidth = 0.7, color = "#33725f") +
  #geom_point(color = blue) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  labs(title = "U.S. Civilian unemployment rate",
       subtitle = "Jan 1948 – Dec 2023",
       y = "Percent",
       x = "",
       caption = "Source: U.S. Bureau of Labor Statistics.")

uu

##=========================================================================================##


##--- CPI:

cpi <- read_csv("cpi.csv") |> 
  clean_names()

cpi |> 
  ggplot(aes(x = date, y = cpiaucsl_ch1)) + 
  geom_line(linewidth = 0.7, color = "#6f80a1") +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_date(date_breaks = "10 years", date_labels = "%b %Y") +
  labs(title = "U.S. CPI inflation rate",
       subtitle = "Jan 1948 – Dec 2023",
       y = "Change from a year ago",
       x = "",
       caption = "Source: U.S. Bureau of Labor Statistics.")
  
##=========================================================================================##


##--- EMRATIO:


emratio <- read_csv("emratio.csv") |> 
  clean_names()

emratio |> 
  ggplot(aes(x = date, y = emratio)) + 
  geom_line(linewidth = 0.7, color = "#6f80a1") +
  scale_x_date(date_breaks = "10 years", date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(50, 65, 2.5)) +
  labs(title = "U.S. employment-to-population ratio",
       subtitle = "Jan 1948 – Dec 2023",
       y = "Percent",
       x = "",
       caption = "Source: U.S. Bureau of Labor Statistics.")


##=========================================================================================##


##--- G and I:

gi <- read_csv("g-and-i.csv") |> 
  clean_names()


cols <- c("Government expenditures" = "#d87979", "Investment" = "#39384d")


gi |> 
  rename(i = a006re1q156nbea,
         g = a822re1q156nbea) |> 
  ggplot(aes(x = date, y = i, color = "Investment")) +
  geom_line() +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  geom_line(aes(y = g, color = "Government expenditures")) +
  scale_color_manual(values = cols) +
  easy_add_legend_title("Measure") +
  easy_move_legend(to = "bottom") +
  labs(x = "",
       y = "Government and Investment",
       title = "Government expenditures and Investment",
       subtitle = "Shares of U.S. GDP: 1947–2023",
       caption = "U.S. Bureau of Economic Analysis.")



##=========================================================================================##


##--- Employment recoveries:

unrate |> 
  mutate(emp = 100 - unrate) |> 
  filter(date >= "2020-01-01") |> 
  ggplot(aes(x = date, y = emp)) +
  geom_line()


unrate |> 
  mutate(emp = 100 - unrate) |> 
  filter(date >= "2007-01-01") |> 
  ggplot(aes(x = date, y = emp)) +
  geom_line() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "U.S. recent employment recoveries",
       x = "",
       y = "Employment rate (%)",
       caption = "Source: U.S. Bureau of Labor Statistics.")


##=========================================================================================##


##--- FED funds:

ff <- read_csv("fedfunds.csv") |> 
  clean_names()

ff |> 
  ggplot(aes(x = date, y = fedfunds)) +
  geom_line(linewidth = 0.5, color = "#006666") +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 20, 2.5)) +
  scale_x_date(date_breaks = "8 years", date_labels = "%Y") +
  labs(x = "",
       y = "",
       title = "U.S. federal funds rate",
       subtitle = "1954–2023",
       caption = "Source: U.S. Federal Reserve System.")
