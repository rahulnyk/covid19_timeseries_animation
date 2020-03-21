setwd("~/work/covid19")
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

options(
  gganimate.nframes = 240, 
  gganimate.fps=24
)

if (T) {
  jhu_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                   "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                   "time_series_19-covid-Confirmed.csv", sep = "")
  d <- read_csv(jhu_url)
}

data <- d %>%
  rename(province = "Province/State", country_region = "Country/Region") %>% 
  pivot_longer(-c(province, country_region, Lat, Long), names_to = "Date", values_to = "total")  %>% 
  mutate(date = mdy(Date))%>% mutate(day = as.integer(strftime(date, format = "%j")) ) %>%
  filter(total != 0) %>% 
  ## calculate the number of days since first case for each country
  group_by(country_region) %>% mutate( days_since_0 = day - min(day)  ) %>% ungroup() %>%
  ## Calculate total cases per country by summing provinces
  group_by(country_region, date, days_since_0) %>% summarise( total = sum(total)) %>%
  mutate(label = ifelse(( (total > 5000) | (country_region %in% c("India", "Pakistan") ) ), paste(total, country_region, sep=" | "), NA)) %>%
  filter(  (total > 500) | ( country_region %in% c("India", "Pakistan") ) )


if (!animate) {
  at_date = Sys.Date() -1
  data <- data %>% filter(date == at_date)
}

p <- ggplot( data = data, aes(x=days_since_0, y = total, size=(total)^0.5, color=country_region )) +
  geom_point() +
  geom_text(aes(x=65, label = label, color=country_region ), hjust = 0, check_overlap = T, size=3) 

p <- p + xlim(c(0, 70)) + ylim(c(0, 90000))

p <- p + 
  theme_minimal() +
  theme(
    axis.text=element_text(size=10, color = "darkgrey"), 
    axis.title=element_text(size=12)
    ) + 
  theme( legend.position = 'off' ) + 
  theme( plot.title = element_text(size = 16, hjust = 0.5) )
p <- p + 
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank()) + 
  theme(plot.margin = margin(10, 50, 10, 10)) + 
  coord_cartesian(clip = 'off')


p <- p + scale_color_viridis(discrete = TRUE, option="inferno")
p <- p + scale_radius(
  range = c(1, 20),
  trans = "identity",
  guide = "legend"
)

## Animation

p <- p + labs(
  title = 'Date: {frame_time}',
  x = 'Number of day since first report',
  y = 'Number of cases'
)

if (animate) {
  p <- p + transition_time(date) + ease_aes('cubic-in-out')
  animate(
    p,
    # renderer=gifski_renderer(), # render gif
    renderer=av_renderer(), # render video
    res=150,
    height = 720,
    width = 1280
    )
} else {
  print(p)
}


