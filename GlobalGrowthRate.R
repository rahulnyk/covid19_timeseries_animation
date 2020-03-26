setwd("~/work/covid19/growth_rate")
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(gganimate)
library(animation)
library(gifski)
library(readr)
library(ggpubr)

options(
  gganimate.nframes = 40, 
  gganimate.fps=10
)

rebuild_dataframe <- F
yesterday <- Sys.Date()
pal <- "inferno"

if (T) {
  jhu_url_confirmed <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                   "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                   "time_series_covid19_confirmed_global.csv", sep = "")
  jhu_url_recovered <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                             "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                             "time_series_covid19_recovered_global.csv", sep = "")
  jhu_url_deaths <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                             "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                             "time_series_covid19_deaths_global.csv", sep = "")
  d1 <- read_csv(jhu_url_confirmed) %>%
    rename(province = "Province/State", country_region = "Country/Region") %>% 
    mutate(type = 'total') %>% 
    pivot_longer(-c(type, province, country_region, Lat, Long), names_to = "Date", values_to = "total") 
  d2 <- read_csv(jhu_url_recovered) %>%
    rename(province = "Province/State", country_region = "Country/Region") %>% 
    mutate(type = 'recovered') %>% 
    pivot_longer(-c(type, province, country_region, Lat, Long), names_to = "Date", values_to = "total")
  d3 <- read_csv(jhu_url_deaths) %>%
    rename(province = "Province/State", country_region = "Country/Region") %>% 
    mutate(type = 'deaths') %>% 
    pivot_longer(-c(type, province, country_region, Lat, Long), names_to = "Date", values_to = "total")
  d <- rbind(d1, d2, d3)
}

data_total <- d %>% 
  mutate(date = mdy(Date)) %>%
  group_by(date, type) %>% summarize(total = sum(total)) %>%
  group_by(type) %>%
  mutate( total_next = lead(total, default = 0), total_prev = lag(total, default = 0)) %>%
  mutate( rate1 = (total - total_prev)*100/total, rate2 = (total_next - total_prev)*100/(2*total_prev) ) %>%
  ungroup() %>% filter(date < '2020-03-25')

data_rate <- data_total %>% select(date, type, rate1, rate2) %>% filter(type != "deaths") 

data_abs <- data_total %>% select(date, type, total) 

data_abs <- data_abs %>% pivot_wider(id_cols = c(date), names_from = type, values_from = total) %>%
  mutate(in_treatment = total - recovered - deaths ) %>%
  pivot_longer(-c(date), names_to = "type", values_to = "total") %>% 
  filter(type != "total") 
  
p1 <- ggplot(data = data_rate,  aes(x = date, y = rate1, color = type, fill = type)) + 
  geom_smooth(aes(group = type), alpha = 0.2, size = 0.6) + 
  geom_point(alpha = 0.2) + geom_line(alpha = 0.2) +
  theme_minimal() +
  # geom_hline(yintercept = mean(data_total$rate1), linetype = '11', alpha = 0.5) +
  theme(
    axis.text=element_text(size=8, color = "darkgrey"), 
    axis.title=element_text(size=10)
  ) + 
  # theme(legend.position = 'off' ) + 
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.6)) +
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major = element_blank()) + 
  labs(y = "Growth rate in Percentage", title = "Groth rate by date") + 
  #facet_wrap(~type, ncol = 1) +
  scale_fill_viridis_d(option=pal, begin = 0, end = 0.6) + 
  scale_color_viridis_d(option=pal, begin = 0, end = 0.6) + ylim(c(-10, 50)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p2 <- ggplot(data = data_abs, aes(x = date, y = total/1000, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  # geom_area(size = 0.4)  +
  theme_minimal()  +
  theme(
    axis.text=element_text(size=8, color = "darkgrey"),
    axis.title=element_text(size=10)
  ) +
  theme(legend.title = element_blank(), legend.position = c(0.2, 0.6) ) +
  theme(plot.title = element_text(size = 12, hjust = 0) ) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  # scale_y_continuous(trans = 'log2') +
  labs(y = "Number of cases (thousands)", title = "Reported number of cases by date") + 
  scale_fill_viridis_d(option=pal, begin = 0, end = 0.9) + 
  scale_color_viridis_d(option=pal, begin = 0, end = 0.9)

print(p2)
p <- ggarrange(
  p1, p2,
  ncol = 1,
  nrow = 2
)
ggsave("output.jpeg", p, device = "jpeg", dpi = 200, width = 4, height = 5)
print(p)






