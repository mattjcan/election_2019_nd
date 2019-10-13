# PREAMBLE ---------------------------------------------------------------

library(tidyverse)
library(rgdal) 
library(leaflet)
library(knitr)
library(xaringan)
library(rmarkdown)
library(gridExtra)
library(widgetframe)
library(kableExtra)
library(ggthemes)
library(zoo)
library(readxl)
library(lubridate)
library(htmltools)
library(sp)
library(rgdal)

# PLOT FORMATS ----

background <- c("#e5e5df")

theme_mc <- theme_economist() + 
  theme(legend.position="none") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(axis.text = element_text(size = 10, vjust = 0.3, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.line = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, size = 9)) +
  theme(plot.background = element_rect(fill = background)) +  
  theme(panel.background = element_rect(fill = background)) +   
  theme(panel.grid.major.y =  element_line(color = "#b3b3b3", size = 0.2)) +
  theme(axis.text.y = element_blank())

stroke_size <- 0.75

line_color <- "#2166ac"

# IMPORT ------------------------------------------------------------------

d <- "C:/Users/matt/Documents/R/election_2019_nd/" # parent directory for the data

sa1 <- read_csv(paste0(d,"data/polling-place-by-sa1s-2019.csv"), skip = 0)

# TIDY ----

sa1 <- sa1 %>% 
  group_by(year, state_ab, div_nm, pp_id, pp_nm) %>% 
  mutate(p = votes / sum(votes)) %>% 
  arrange(pp_nm)

