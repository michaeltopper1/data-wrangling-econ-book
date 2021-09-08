# Goal: Create a tibble of everyone involved in the UCSB economics department with their name (name), position type (type), position title (position), email (email), and office (office).
rm(list=ls()) # clear environment
library(polite)
library(glue) # tidyverse concatenate text
library(magrittr) # fancy pipe
library(rvest) # Grabbing, formatting, and cleaning html code
library(tidyverse) # Thanks Hadley Wickham!

# Asking if ok to scrape

bow("https://econ.ucsb.edu/people",
    user_agent = "Danny Klinenberg <dklinenberg@ucsb.edu>",
    force = T
    )

# Load in the website
website <- read_html("https://econ.ucsb.edu/people")

# Grab their Names

names<-html_nodes(website,"h3") %>% 
  html_text() %>% 
  as_tibble()

# Grab their Interests

interest<-html_nodes(website,".group-third") %>% 
  html_text() %>% 
  as_tibble()

# grab position, email, and office
other_stuff<-html_nodes(website,".group-second") %>% 
  html_text() %>% 
  as_tibble()

# Problem: The email doesn't show, some people have phone number and website links, and there isn't an easy patter to build off of :/ EX: Xander has four entries if we were to break by \n but Bob Anderson has 5. In addition, this gives us a good chance to scrape through multiple web pages!

# To speed up our process, let's make the tibble with defined rows:

dat <- tribble( ~ "name", ~"type", ~ "position", ~ "email", ~ "office")

# Now let's say we want to get more information on the employees from their individual pages. Problem: The links are different for different positions (i.e. student, reseracher, etc.)

# Loop through websites

type <-
  c("faculty",
    "lecturers",
    "researchers",
    "emeriti",
    "students",
    "staff")

# loop through all of the "types" of people on the cite
for (i in type) {
  # Base of website url
  website_base <- paste0("https://econ.ucsb.edu/people/", i)
  
  # grabbing everyone in this section's name
  website_names <- website_base %>%
    read_html() %>% # read in the html
    html_nodes("h3") %>% # grab the names - "h3" comes from inspectorgadget
    html_text() %>%  # turn it from html to text
    as_tibble() %>%  # make it into a tibble so easier to manipulate
    mutate(value = str_to_lower(value)) %>%  # make all lower case
    mutate(value = str_replace_all(value, "[[:punct:]]", " ")) %>%  #remove punctuation because website doesn't have punctuation (see Ted's page url)
    mutate(value = str_replace_all(value, " {1,}", " ")) %>% # condense all multi-spaced breaks into single space breaks
    mutate(value = str_replace_all(value, " ", "-")) %>%  # replace spaces with dashes for website name
    mutate(value = str_replace(value, "-$", "")) # some of the cleaning led to the names ending with a dash...the websites don't have that dash
  
  #emeriti are in their own category but still have faculty urls
  if (i == "emeriti") {
    website_base <- paste0("https://econ.ucsb.edu/people/", "faculty")
  }
  
  # making rows of the data
  for (j in 1:length(website_names$value)) {
    dat %<>%
      rbind(
        tibble(
          "name" = paste0( # gurantees empty cells aren't skipped
            " ",
            paste0(website_base, "/", website_names$value[j]) %>%
              read_html() %>%
              html_nodes("h1") %>%
              html_text(trim = T)
          ) %>%
            str_trim(),# removes the random space at the beginning
          "type"=i,
          "position" = paste0(
            " ",
            glue_collapse(
              paste0(website_base, "/", website_names$value[j]) %>%
                read_html() %>%
                html_nodes(".field--items .field--item") %>%
                html_text(trim = T),
              sep = " and " # people with multiple titles (e.g. see Dick Startz page)
            )
          ) %>%
            str_trim(),
          "email" = paste0(
            " ",
            paste0(website_base, "/", website_names$value[j]) %>%
              read_html() %>%
              html_nodes(".field--type-email .field--item") %>%
              html_text(trim = T)
          ) %>%
            str_trim(),
          # Many people don't have offices right now, so we add a space at the beginning so that the observations aren't just dropped
          "office" = paste0(
            " ",
            paste0(website_base, "/", website_names$value[j]) %>%
              read_html() %>%
              html_nodes(".field--type-string.field--label-inline .field--item") %>%
              html_text(trim = T)
          ) %>%
            str_trim()
        )
      )
  }
  cat(i, "completed\n")
}


dat$name[which(!(names$value %in% dat$name))]
