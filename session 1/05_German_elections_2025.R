library(tidyverse) # ggplot and the usual goodies
library(rvest) # to scrape wikipedia page
library(lubridate) # to handle conversions from characters to date objects
library(zoo) # to calculate rolling  averages of last k polls
library(showtext)
library(ggtext)

# load fonts we will use
font_add_google("Montserrat", "Montserrat") # official LBS font
font_add_google("Lato", "Lato")

## Automatically use showtext to render text for future devices
showtext_auto()

# data sourced from https://en.wikipedia.org/wiki/Opinion_polling_for_the_2025_German_federal_election
url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2025_German_federal_election"


# get tables that exist on wikipedia page 
tables <- url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called polls 
# Use purr::map() to create a list of all tables in URL
polls <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               janitor::clean_names())

# the dates of the opinion polls are given as, e.g. July 30 - August 2, 2024
# We use a regular expression to extract the latest date and use that 
library(stringr)


# Regex pattern to capture last date of fieldwork dates
pattern <- "(?:.*–\\s*)?(\\d{1,2}\\s+[A-Z][a-z]{2}\\s+\\d{4})$"


# (?:.*–\\s*)?: Optional non-capturing group that matches everything before the dash, including the dash and any whitespace
# (\\d{1,2}\\s+[A-Z][a-z]{2}\\s+\\d{4}): Capturing group that matches:
#   
#   \\d{1,2}: 1-2 digit day
# \\s+: Whitespace
# [A-Z][a-z]{2}: Month abbreviation (first letter capitalized, followed by two lowercase)
# \\s+: Whitespace
# \\d{4}: 4-digit year
# 
# $: Ensures we're at the end of the string


# manipulate- tidy data
election_polls <- polls[[1]] |>   # the relevant table on the wikipedia page contains the list of all opinions polls
  # delete first row
  slice(-1) |> 
  
  filter(samplesize != "–") |> 
  
  mutate(
    # convert characters to numbers
    samplesize = parse_number(samplesize),
    linke = parse_number(linke),
    fw = parse_number(fw),
    bsw = parse_number(bsw),
    others = parse_number(others),
    
    # apply our function to get closing date of poll as a character
    end_date = str_extract(fieldwork_date, pattern, group = 1),

    # and now get it as a date object
    end_date = lubridate::dmy(end_date),
    
    
  ) |> 
  
  
  # drop columns that are not needed
  select(-c(abs, others, lead))


# time series plot
election_polls_long <- election_polls |> 
  pivot_longer(cols = 4:11,
               names_to = "party",
               values_to = "percent") |> 
  
  # order the parties so their colours will match
  mutate(party = factor(party, 
                        levels = c(
                          "union", "af_d", "spd", "grune",
                           "bsw", "fdp", "linke", "fw"
                        ),
                        labels = c(
                          "CDU", "AFD","SPD", "Greens","BSW", "FDP", "Linke", "FW"
                        )))


# use colour codes for parties
# even though party colours is not straight-forward... 
# https://blog.datawrapper.de/partycolors/
my_colour_palette = c(
  "#000000", # CDU (Christian Democratic Union):  (Black)
  "#0077be", #AfD (Alternative for Germany): (Blue)
  "#FF0000", #SPD (Social Democratic Party):  (Red)
  "#64A12D", #Greens (Alliance 90/The Greens):  (Bright Green)
  "#C41E3A",  #BSW
  "#FFFF00", #FDP (Free Democratic Party):  (Yellow)
  "#8B0000",  #Die Linke (The Left):(Dark Red)
  "#008000" #FW
)

election_polls_long %>% 
  ggplot()+
  aes(x=end_date, y= percent, colour = party)+
  geom_point(alpha=0.50)+
  scale_colour_manual(values = my_colour_palette)+
  geom_smooth(se=F)+
  theme_minimal()+
  scale_x_date(date_minor_breaks = "1 month")+
  labs(
    title = "Opinion polling for the 2025 German election",
    x = NULL, y = "Percent %",
    caption = "Source: https://en.wikipedia.org/wiki/Opinion_polling_for_the_2025_German_federal_election"
  ) +
  # ensure title is top-left aligned
  theme(plot.title.position = "plot")+
  theme(text=element_text(size=12, family="Montserrat"))+
  NULL

