setwd("~/work/covid19/covid19_timeseries_animation")
library(tidyverse)
library(lubridate)
library(viridis)
library(ggpubr)
source('./source_world_data.r')

options(
  gganimate.nframes = 40, 
  gganimate.fps=10
)

rebuild_dataframe <- T
from <- dmy('1-02-20')
yesterday <- Sys.Date()
pal <- "magma"

if (rebuild_dataframe) {
 d <- source_world_data()
}

data_long <- d %>% select(-c(Lat, Long)) %>% filter(Date > from) %>%
  pivot_longer(-c(StateUt, Country,  Date), values_to = 'Total', names_to = 'Status')

data_total <- data_long %>%
  group_by(Date, Status) %>% 
  summarize( Total = sum(Total) )  %>% ungroup() 

data_rate <- data_total %>% 
  group_by(Status) %>%
  mutate(TotalPrev = lag(Total, default = 0, order_by = Date ), DatePrev = lag(Date, order_by = Date )) %>%
  filter(!is.na(DatePrev)) %>% mutate(DelDay = as.integer(Date-DatePrev)) %>%
  mutate( Rate1 = (Total - TotalPrev)*100/(DelDay*Total) ) %>%
  ungroup() %>%
  filter(!(Status %in% c('Hospitalized', 'Deaths')))

plot_theme <- theme_minimal() + theme(
  axis.text=element_text(size=8, color = "darkgrey"), 
  axis.title=element_text(size=10),
  axis.title.x = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.title = element_text(size = 12, hjust = 0),
  legend.title = element_blank(), 
  legend.position = c(0.2, 0.8)
)

p1 <- ggplot(data = data_rate,  aes(x = Date, y = Rate1, color = Status, fill = Status)) + 
  geom_smooth(aes(group = Status), alpha = 0.2, size = 0.6, linetype=1) + 
  geom_point(alpha = 0.2, size=2) + geom_line(alpha = 0.4) +
  labs(y = "Growth rate in Percentage") + # labs(title = "Groth rate by date") + 
  scale_fill_viridis_d(option=pal, end = 0.6) + 
  scale_color_viridis_d(option=pal, end = 0.6) + 
  ylim(c(-10, 50)) +
  plot_theme 

p2 <- ggplot(
    data = data_total %>% filter(!(Status %in% c('Total'))), 
    aes(x = Date, y = Total/1000, fill = Status)
    ) + 
  geom_area(position = "stack", stat = "identity") +
  # scale_y_continuous(trans = 'log2') +
  labs(y = "Number of cases (thousands)") + # labs(title = "Reported number of cases by date") + 
  scale_fill_viridis_d(option=pal, begin = 0, end = 0.9) + 
  scale_color_viridis_d(option=pal, begin = 0, end = 0.9) +
  plot_theme

p <- ggarrange( p1, p2, ncol = 1, nrow = 2 )
ggsave("output.jpeg", p, device = "jpeg", dpi = 200, width = 4, height = 5)
print(p)






