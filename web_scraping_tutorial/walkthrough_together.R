rm(list=ls())
library(jsonlite) # read in json formats
library(glue) # tidyverse concatenate text
library(magrittr) # fancy pipe
library(polite) # make sure website is okay to scrape
library(rvest) # Grabbing, formatting, and cleaning html code
library(tidyverse) # Thanks Hadley Wickham!

# Tuition Tracker

read_html("https://tuitiontracker.org/fitness/school.html?unitid=110705") %>% 
  html_nodes()
