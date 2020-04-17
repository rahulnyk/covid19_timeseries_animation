setwd("~/work/covid19/covid19_timeseries_animation")
## set path of the directory you save this file in
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(gganimate)
library(animation)
library(gifski)

## animate 
##  T => render animation
##  F => render a plot of latest (yesterdays) snapshot
animate <- T
## source_data: set to true if you want to recreate the dataframe
##  T => download the data from the data repo
##  F => Use data from the global environment
source_data <- T

yesterday <- Sys.Date() -1

options(
  gganimate.nframes = 150, 
  gganimate.fps=10
)

if (T) {
  jhu_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                   "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                   "time_series_covid19_confirmed_global.csv", sep = "")
  d <- read_csv(jhu_url)
}

data <- d %>%
  rename(province = "Province/State", country_region = "Country/Region") %>% 
  pivot_longer(-c(province, country_region, Lat, Long), names_to = "Date", values_to = "total")  %>% 
  mutate(date = mdy(Date))%>% mutate(day = as.integer(strftime(date, format = "%j")) ) %>%
  filter(total != 0) %>% select(province, country_region, date, day, total) 

d_total <- data %>% group_by(date, day) %>% summarize(total = sum(total)) %>%
  mutate(province = "World", country_region = "Global") %>% ungroup()

data <- rbind(data, d_total) %>%
  ## calculate the number of days since first case for each country
  group_by(country_region) %>% mutate( days_since_0 = day - min(day)  ) %>% ungroup() %>%
  ## Calculate total cases per country by summing provinces
  group_by(country_region, date, days_since_0) %>% summarise( total = sum(total)) %>%
  group_by(country_region) %>% 
  mutate( max_cases = max(total)) %>% 
  ungroup() %>%
  filter( (max_cases > 10) | (country_region %in% c("India", "Pakistan", "Bangladesh", "Sri Lanka", "Nepal") ) ) %>%
  mutate(label = paste(total, country_region, sep=" | ") )

y_max <- max(data$total)
x_max <- max(data$days_since_0)
x_label <- x_max + 20
labels <- data %>% 
  filter(date == yesterday) %>% 
  arrange( total ) %>% filter( (max_cases > 12000) | (country_region %in% c("India", "Pakistan", "Bangladesh", "Sri Lanka", "Nepal") ) ) %>%
  mutate(yend = ( 2^( (log2(y_max)/(n()) )*row_number() ))) %>%
  select(country_region, yend)

data <- left_join(data, labels, by = c("country_region") )

if (!animate) {
  data <- data %>% filter(date == yesterday)
}

p <- ggplot(
  data = data, 
  aes(x=days_since_0, y = total, size=(total)^0.3, 
      fill=country_region )
  ) +
  geom_line(aes(color = country_region), size = 0.3, alpha=0.1)  +
  geom_point(shape = 21, color = 'white') +
  geom_label(
    aes(x=x_label, y = yend, label = label, fill=country_region),
    color='white',
    hjust = 0, 
    size=3
  ) + 
  geom_segment(aes(xend = x_label, yend = yend, color=country_region), linetype = "11", size=0.2, alpha=0.3)

p <- p + 
  scale_y_continuous(trans = 'log2', limits = c(1, y_max*2)) +
  xlab("Number of day since first report") + 
  ylab("Number of cases (Cumulative, Log)") +
  xlim(c(0, x_label+20)) 

p <- p + 
  theme_minimal() +
  theme(
    axis.text=element_text(size=8, color = "darkgrey"), 
    axis.title=element_text(size=10)
    ) + 
  theme(legend.position = 'off') + 
  theme(plot.title = element_text(size = 12, hjust = 0.5) ) + 
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank()) + 
  theme(plot.margin = margin(10, 50, 10, 10)) + 
  coord_cartesian(clip = 'off')  +
  scale_color_viridis_d(option="inferno", begin = 0, end = 0.9) + 
  scale_fill_viridis_d(option="inferno", begin = 0, end = 0.9)

p <- p + scale_radius(
  range = c(1, 14),
  trans = "identity",
  guide = "legend"
)

## Animation

if (animate) {
  p <- p + labs(
    title = 'Date {frame_along}', fontface = "bold"
  )
  p <- p + transition_time(date) + ease_aes('cubic-in-out') + 
    transition_reveal(date)
  animate(
    p,
    renderer=gifski_renderer(loop=T), # render gif
    # renderer=av_renderer(), # render video
    res=120,
    height = 800,
    width = 1200,
    end_pause = 30
    )
  anim_save(paste("output", ".gif", sep="" ), animation = last_animation())
} else {
  p <- p + labs(
    title = paste('Date:', yesterday)
  )
  ggsave("output.jpeg", p, device = "jpeg", dpi = 350, width = 10, height = 6 )
  print(p)
}


