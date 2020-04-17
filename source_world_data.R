source_world_data <- function(){
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
    mutate(Type = 'Total')
  
  d2 <- read_csv(jhu_url_recovered) %>%
    mutate(Type = 'Recovered')
  
  d3 <- read_csv(jhu_url_deaths) %>%
    mutate(Type = 'Deaths') 
    
  d <- rbind(d1, d2, d3) %>%
  rename(StateUt = "Province/State", Country = "Country/Region") %>% 
  pivot_longer(-c(Type, StateUt, Country, Lat, Long), names_to = "Date", values_to = "Total") %>%
  pivot_wider(names_from = Type, values_from = Total, values_fill = list(Total = 0)) %>%
  mutate(Hospitalized = Total-Recovered-Deaths) %>% mutate( Date = as.Date(Date, format="%m/%d/%y") )
  
  rm(d1, d2, d3)
  
  return(d)
}