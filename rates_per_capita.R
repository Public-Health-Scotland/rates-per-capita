# Code to create rates per capita for HBs (example with Covid-19 cases)
# It first extracts population data for the HBs, then merges it with the cases
# data and finally calculates some crude rates per 100,000 people

###############################################.
## Packages and functions ----
###############################################.
# If the packages are not installed uncomment and run these lines before anything else
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("httr")
# install.packages("jsonlite")

library(dplyr)
library(tidyr) # long to wide format
library(httr) # api connection
library(jsonlite)  # transforming JSON files into dataframes

# This function extracts population data from the Public Health Scotland (NHS)
# open data platform (https://www.opendata.nhs.scot).
# Requires one parameter:
# Resource id - key used for the type of data you want in the open data platform
#               to find the one you need go into the website and find the data you need.
extract_open_data <- function(resource_id) {
  
  # URL used for data extraction
  open_data_url <- "https://www.opendata.nhs.scot/api/3/action/datastore_search?resource_id="
  
  # After finding out the length of the file we can extract the whole thing
  # Limit is set to 1 million, check nothing is bigger than that
  data_extraction <-  GET(paste0(open_data_url, resource_id, "&limit=1000000"))
  # Checking if request has worked
  print(paste0("A value of 200 means the server has received the request:", data_extraction$status_code))
  # It's a JSON file, so transforming into something more usable in R
  data_extraction <- rawToChar(data_extraction$content) %>% fromJSON()
  data_extraction <- data_extraction$result$records %>% #extracting data  
    setNames(tolower(names(.)))   #variables to lower case
  
}

###############################################.
## Create population basefile for latest year available ----
###############################################.
# Extract population data for HBs
hb_pop <- extract_open_data("27a72cc8-d6d8-430c-8b4f-3109a9ceadb1") %>% 
# Formatting the data in the way needed
  filter(year == 2018) %>% #most recent year data
  select(year, hb2014, allages) %>%
  group_by(hb2014, year) %>% 
  summarise(pop = sum(allages)) #one value per area

# Extract names of health boards
hb_names <- extract_open_data("395476ab-0720-4740-be07-ff4467141352") %>% 
  select(hb2014, hb2014name) %>% unique()

# Adding Scotland and taking out NHS
hb_names <- rbind(hb_names, data.frame(hb2014 = "S92000003", hb2014name = "Scotland")) %>% 
  mutate(hb2014name = gsub("NHS ", "", hb2014name))

# Merging names and population
hb_pop <- left_join(hb_pop, hb_names, by = "hb2014") %>% 
  rename(area_name = hb2014name, area_code = hb2014)


###############################################.
## Create population basefile for latest year available ----
###############################################.
# Dataframe with cases per health board and Scotland taken from:https://www.gov.scot/coronavirus-covid-19/
covid <- data.frame(
  area_name = c('Ayrshire and Arran','Borders','Dumfries and Galloway','Fife',
               'Forth Valley','Grampian','Greater Glasgow and Clyde','Highland',
               'Lanarkshire','Lothian','Shetland','Tayside', 'Western Isles', 
               'Orkney', 'Scotland'),
  covid_cases = c(177, 77, 92, 76, 131, 86, 547, 51, 244, 269, 29, 214, 0, 0,  1993))

# Joining with population and calculating crude rates per 100,000 people
covid <- left_join(covid, hb_pop, by = "area_name") %>% 
  mutate(rate_per_capita = round(covid_cases/pop*100000, 1))

write.csv(covid, "covid_with_ratespercapita.csv")

## END
