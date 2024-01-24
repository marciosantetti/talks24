## @knitr prework

library(tidyverse)
library(lubridate)
library(showtext)
library(neverhpfilter)
library(mFilter)
library(ggthemes)
library(patchwork)
library(ggrepel)
library(tsibble)
library(astsa)
library(corrr)
library(janitor)
library(ggeasy)


## @ knitr load_data

dat <- read_csv("data_with_ham.csv")


dat_ts <- dat %>% 
  #mutate(period = seq(as.Date("1953-04-01"), by = "quarter", length.out = 279)) %>% 
  filter(period >= "1956-01-01") %>% 
  select(period, psi_ham, e_ham, q_ham, psigr_ham, gr_ham, s_ham, g_ham, rat_ham, ratio_ham)




##------------------------------------------------------------------------------------------##
##------------------------------------------------------------------------------------------##
##------------------------------------------------------------------------------------------##

## @knitr dots

red1 <- dat_ts %>% 
  filter(period == "1969-10-01")

red2 <- dat_ts %>% 
  filter(period == "1973-10-01")

red21 <- dat_ts %>% 
  filter(period == "1974-10-01")

red3 <- dat_ts %>% 
  filter(period == "1981-07-01")

red31 <- dat_ts %>% 
  filter(period == "1982-07-01")

red4 <- dat_ts %>% 
  filter(period == "1990-07-01")

red41 <- dat_ts %>% 
  filter(period == "1991-07-01")

red5 <- dat_ts %>% 
  filter(period == "2001-01-01")

red51 <- dat_ts %>% 
  filter(period == "2002-01-01")

red6 <- dat_ts %>% 
  filter(period == "2007-10-01")

red61 <- dat_ts %>% 
  filter(period == "2008-10-01")

red7 <- dat_ts %>% 
  filter(period == "2019-10-01")

red71 <- dat_ts %>% 
  filter(period == "2020-10-01")



##------------------------------------------------------------------------------------------##
##------------------------------------------------------------------------------------------##
##------------------------------------------------------------------------------------------##



## @knitr scatters

#--- (e, psigr) plane:

## 1969-1973:

cc1 <- dat_ts %>% 
  filter(period >= "1969-10-01" & period <= "1974-10-01") %>%
  ggplot(aes(y = psigr_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(psigr_ham, n=-1), NA))) +
  geom_point(data = red21, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red1, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1969Q4—1974Q4",
       x = "e",
       y = expression(psi),
       subtitle = "r = 0.207") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 1973-1981:

cc2 <- dat_ts %>% 
  filter(period >= "1973-10-01" & period <= "1982-07-01") %>%
  ggplot(aes(y = psigr_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(psigr_ham, n=-1), NA))) +
  geom_point(data = red31, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red2, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1973Q4—1982Q3",
       x = "e",
       y = expression(psi),
       subtitle = "r = 0.655") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 1981-1990:

cc3 <- dat_ts %>% 
  filter(period >= "1981-07-01" & period <= "1991-07-01") %>%
  ggplot(aes(y = psigr_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(psigr_ham, n=-1), NA))) +
  geom_point(data = red41, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red3, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1981Q3—1991Q3",
       x = "e",
       y = expression(psi),
       subtitle = "r = 0.506") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 1990-2001:

cc4 <- dat_ts %>% 
  filter(period >= "1990-07-01" & period <= "2002-01-01") %>%
  ggplot(aes(y = psigr_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(psigr_ham, n=-1), NA))) +
  geom_point(data = red51, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red4, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1990Q3—2002Q1",
       x = "e",
       y = expression(psi),
       subtitle = "r = -0.170") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 2001-2007:

cc5 <- dat_ts %>% 
  filter(period >= "2001-01-01" & period <= "2008-10-01") %>%
  ggplot(aes(y = psigr_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(psigr_ham, n=-1), NA))) +
  geom_point(data = red61, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red5, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "2001Q1—2008Q4",
       x = "e",
       y = expression(psi),
       subtitle = "r = -0.382") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)



## 2007-2019:

cc6 <- dat_ts %>% 
  filter(period >= "2007-10-01" & period <= "2020-10-01") %>%
  ggplot(aes(y = psigr_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(psigr_ham, n=-1), NA))) +
  geom_point(data = red71, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red6, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "2007Q4—2020Q4",
       x = "e",
       y = expression(psi),
       subtitle = "r = 0.497") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)

#(cc1 | cc2) / (cc3 | cc4) / (cc5 | cc6)


## @knitr scatter1

cc1 | cc2


## @knitr scatter2

(cc3 | cc4)

## @knitr scatter3

(cc5 | cc6)

## @knitr scatter_unique

dat_ts %>% 
  filter(period >= "2001-01-01" & period <= "2008-10-01") %>%
  ggplot(aes(y = psigr_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(psigr_ham, n=-1), NA))) +
  geom_point(data = red61, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red5, aes(y = psigr_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "Employment rate vs. labor share",
       x = "e",
       y = expression(psi),
       subtitle = "2001Q1—2008Q4") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)

##------------------------------------------------------------------------------------------##
##------------------------------------------------------------------------------------------##
##------------------------------------------------------------------------------------------##



##--- (e, gr) plane:



## 1969-1973:

ccccc1 <- dat_ts %>% 
  filter(period >= "1969-10-01" & period <= "1974-10-01") %>%
  ggplot(aes(y = gr_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(gr_ham, n=-1), NA))) +
  geom_point(data = red21, aes(y = gr_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red1, aes(y = gr_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1969Q4—1974Q4",
       x = "e",
       y = "g",
       subtitle = "r = -0.158") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 1973-1981:

ccccc2 <- dat_ts %>% 
  filter(period >= "1973-10-01" & period <= "1982-07-01") %>%
  ggplot(aes(y = gr_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(gr_ham, n=-1), NA))) +
  geom_point(data = red31, aes(y = gr_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red2, aes(y = gr_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1973Q4—1982Q3",
       x = "e",
       y = "g",
       subtitle = "r = 0.518") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 1981-1990:

ccccc3 <- dat_ts %>% 
  filter(period >= "1981-07-01" & period <= "1991-07-01") %>%
  ggplot(aes(y = gr_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(gr_ham, n=-1), NA))) +
  geom_point(data = red41, aes(y = gr_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red3, aes(y = gr_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1981Q3—1991Q3",
       x = "e",
       y = "g",
       subtitle = "r = 0.355") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 1990-2001:

ccccc4 <- dat_ts %>% 
  filter(period >= "1990-07-01" & period <= "2002-01-01") %>%
  ggplot(aes(y = gr_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(gr_ham, n=-1), NA))) +
  geom_point(data = red51, aes(y = gr_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red4, aes(y = gr_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1990Q3—2002Q1",
       x = "e",
       y = "g",
       subtitle = "r = 0.405") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 2001-2007:

ccccc5 <- dat_ts %>% 
  filter(period >= "2001-01-01" & period <= "2008-10-01") %>%
  ggplot(aes(y = gr_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(gr_ham, n=-1), NA))) +
  geom_point(data = red61, aes(y = gr_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red5, aes(y = gr_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "2001Q1—2008Q4",
       x = "e",
       y = "g",
       subtitle = "r = 0.0365") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)



## 2007-2019:

ccccc6 <- dat_ts %>% 
  filter(period >= "2007-10-01" & period <= "2020-10-01") %>%
  ggplot(aes(y = gr_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(gr_ham, n=-1), NA))) +
  geom_point(data = red71, aes(y = gr_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red6, aes(y = gr_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "2007Q4—2020Q4",
       x = "e",
       y = "g",
       subtitle = "r = 0.502") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)

(ccccc1 | ccccc2) / (ccccc3 | ccccc4) / (ccccc5 | ccccc6)


##------------------------------------------------------------------------------------------##
##------------------------------------------------------------------------------------------##
##------------------------------------------------------------------------------------------##



##--- (e, q) plane:


## 1969-1973:

ccc1 <- dat_ts %>% 
  filter(period >= "1969-10-01" & period <= "1974-10-01") %>%
  ggplot(aes(y = q_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(q_ham, n=-1), NA))) +
  geom_point(data = red21, aes(y = q_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red1, aes(y = q_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1969Q4—1974Q4",
       x = "e",
       y = "q",
       subtitle = "r = 0.124") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 1973-1981:

ccc2 <- dat_ts %>% 
  filter(period >= "1973-10-01" & period <= "1982-07-01") %>%
  ggplot(aes(y = q_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(q_ham, n=-1), NA))) +
  geom_point(data = red31, aes(y = q_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red2, aes(y = q_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1973Q4—1982Q3",
       x = "e",
       y = "q",
       subtitle = "r = -0.0802") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 1981-1990:

ccc3 <- dat_ts %>% 
  filter(period >= "1981-07-01" & period <= "1991-07-01") %>%
  ggplot(aes(y = q_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(q_ham, n=-1), NA))) +
  geom_point(data = red41, aes(y = q_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red3, aes(y = q_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1981Q3—1991Q3",
       x = "e",
       y = "q",
       subtitle = "r = 0.351") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 1990-2001:

ccc4 <- dat_ts %>% 
  filter(period >= "1990-07-01" & period <= "2002-01-01") %>%
  ggplot(aes(y = q_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(q_ham, n=-1), NA))) +
  geom_point(data = red51, aes(y = q_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red4, aes(y = q_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1990Q3—2002Q1",
       x = "e",
       y = "q",
       subtitle = "r = 0.0236") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 2001-2007:

ccc5 <- dat_ts %>% 
  filter(period >= "2001-01-01" & period <= "2008-10-01") %>%
  ggplot(aes(y = q_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(q_ham, n=-1), NA))) +
  geom_point(data = red61, aes(y = q_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red5, aes(y = q_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "2001Q1—2008Q4",
       x = "e",
       y = "q",
       subtitle = "r = 0.471") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)



## 2007-2019:

ccc6 <- dat_ts %>% 
  filter(period >= "2007-10-01" & period <= "2020-10-01") %>%
  ggplot(aes(y = q_ham, x = e_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(e_ham, n=-1), NA),
                   yend = c(tail(q_ham, n=-1), NA))) +
  geom_point(data = red71, aes(y = q_ham, x = e_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red6, aes(y = q_ham, x = e_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "2007Q4—2020Q4",
       x = "e",
       y = "q",
       subtitle = "r = -0.0518") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)

(ccc1 | ccc2) / (ccc3 | ccc4) / (ccc5 | ccc6)



##------------------------------------------------------------------------------------------##
##------------------------------------------------------------------------------------------##
##------------------------------------------------------------------------------------------##



##--- (gr, q) plane:


## 1969-1973:

cccc1 <- dat_ts %>% 
  filter(period >= "1969-10-01" & period <= "1974-10-01") %>%
  ggplot(aes(y = q_ham, x = gr_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(gr_ham, n=-1), NA),
                   yend = c(tail(q_ham, n=-1), NA))) +
  geom_point(data = red21, aes(y = q_ham, x = gr_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red1, aes(y = q_ham, x = gr_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1969Q4—1974Q4",
       x = "g",
       y = "q",
       subtitle = "r = 0.912") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 1973-1981:

cccc2 <- dat_ts %>% 
  filter(period >= "1973-10-01" & period <= "1982-07-01") %>%
  ggplot(aes(y = q_ham, x = gr_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(gr_ham, n=-1), NA),
                   yend = c(tail(q_ham, n=-1), NA))) +
  geom_point(data = red31, aes(y = q_ham, x = gr_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red2, aes(y = q_ham, x = gr_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1973Q4—1982Q3",
       x = "g",
       y = "q",
       subtitle = "r = 0.381") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 1981-1990:

cccc3 <- dat_ts %>% 
  filter(period >= "1981-07-01" & period <= "1991-07-01") %>%
  ggplot(aes(y = q_ham, x = gr_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(gr_ham, n=-1), NA),
                   yend = c(tail(q_ham, n=-1), NA))) +
  geom_point(data = red41, aes(y = q_ham, x = gr_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red3, aes(y = q_ham, x = gr_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1981Q3—1991Q3",
       x = "g",
       y = "q",
       subtitle = "r = 0.357") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 1990-2001:

cccc4 <- dat_ts %>% 
  filter(period >= "1990-07-01" & period <= "2002-01-01") %>%
  ggplot(aes(y = q_ham, x = gr_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(gr_ham, n=-1), NA),
                   yend = c(tail(q_ham, n=-1), NA))) +
  geom_point(data = red51, aes(y = q_ham, x = gr_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red4, aes(y = q_ham, x = gr_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "1990Q3—2002Q1",
       x = "g",
       y = "q",
       subtitle = "r = 0.269") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)


## 2001-2007:

cccc5 <- dat_ts %>% 
  filter(period >= "2001-01-01" & period <= "2008-10-01") %>%
  ggplot(aes(y = q_ham, x = gr_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(gr_ham, n=-1), NA),
                   yend = c(tail(q_ham, n=-1), NA))) +
  geom_point(data = red61, aes(y = q_ham, x = gr_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red5, aes(y = q_ham, x = gr_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "2001Q1—2008Q4",
       x = "g",
       y = "q",
       subtitle = "r = -0.105") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)



## 2007-2019:

cccc6 <- dat_ts %>% 
  filter(period >= "2007-10-01" & period <= "2020-10-01") %>%
  ggplot(aes(y = q_ham, x = gr_ham)) + 
  geom_point(shape = 1) +
  #geom_text_repel(aes(label = ifelse(dd %in% c(first(dd), last(dd)), dd, "")), family = "IBM Plex Sans") + 
  geom_segment(aes(xend = c(tail(gr_ham, n=-1), NA),
                   yend = c(tail(q_ham, n=-1), NA))) +
  geom_point(data = red71, aes(y = q_ham, x = gr_ham), size = 5, shape = 24, fill = "#000000", alpha = 0.8) +
  geom_point(data = red6, aes(y = q_ham, x = gr_ham), size = 5, shape = 24, fill = "#bdbaba", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = 2, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = 2, color = "#bfbfbf") +
  labs(title = "2007Q4—2020Q4",
       x = "g",
       y = "q",
       subtitle = "r = 0.545") +
  easy_plot_title_size(15) +
  easy_x_axis_title_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_subtitle_size(13)

(cccc1 | cccc2) / (cccc3 | cccc4) / (cccc5 | cccc6)


#-----


## @knitr ham

series <- read_csv("hamilton.csv")

series <- series %>%
  mutate(period = yearquarter(period)) 


## @knitr hamfig

h1 <- series %>% ggplot(aes(x=period, y=y)) + geom_line(color = grey_mid, size=1) +
  labs(title = "U.S. Gross Domestic Product: 1949—2020",
       x = NULL,
       y = "GDP")

h2 <- series %>% ggplot(aes(x=period, y=y_trend)) + geom_line(color = turquoise, size=1) +
  labs(title = "Long-run trend",
       x = NULL,
       y = "GDP")

h3 <- series %>% ggplot(aes(x=period, y=y_cycle)) + geom_line(color = red_pink, size=1) +
  labs(title = "Business cycles",
       x = NULL,
       y = "GDP",
       caption = "Source: U.S. Bureau of Economic Analysis.")



(h1 | h2) / h3


