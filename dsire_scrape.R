library(rvest)
library(jsonlite)
library(lubridate)

#Data not available through regular HTML scraping
dsire_url <- "https://programs.dsireusa.org/system/program"
dsire_page <- read_html(dsire_url)

#Get total records for renewable energies from API
dsire_api <- "https://programs.dsireusa.org/api/v1/programs?technologycategory[]=3&limit=10"
dsire <- fromJSON(dsire_api)
dsire_total <- dsire$recordsTotal

#Scrape all solar records
dsire_solar_api <- paste0("https://programs.dsireusa.org/api/v1/programs?technologycategory[]=1&limit=",dsire_total)
dsire_solar <- fromJSON(dsire_solar_api)
dsire_solar_df <- dsire_solar$data

dsire_solar_df <- dsire_solar_df %>% 
  select(-parameterSets) %>%
  left_join(distinct(dsire_solar_df$stateObj), by = c("state" = "id")) %>%
  select(-stateObj) %>%
  left_join(distinct(dsire_solar_df$typeObj), by = c("type" = "id")) %>%
  select(-typeObj) %>%
  left_join(distinct(dsire_solar_df$categoryObj), by = c("category.x" = "id")) %>%
  select(-c(categoryObj.x, categoryObj.y)) %>%
  left_join(distinct(dsire_solar_df$sectorObj), by = c("sector" = "id")) %>%
  select(-sectorObj) %>%
  mutate(startDate = parse_date_time(startDate, orders = "Ymd"),
         endDate = parse_date_time(endDate, orders = "Ymd"),
         createdTs = parse_date_time(createdTs, orders = "mdY"),
         updatedTs = parse_date_time(updatedTs, orders = "mdY"),
         startImp = as.POSIXct(ifelse(!is.na(startDate), startDate, createdTs), origin = "1970-01-01 UTC"))

#Scrape all wind records
dsire_wind_api <- paste0("https://programs.dsireusa.org/api/v1/programs?technologycategory[]=3&limit=",dsire_total)
dsire_wind <- fromJSON(dsire_wind_api)
dsire_wind_df <- dsire_wind$data

dsire_wind_params <- do.call("rbind",dsire_wind$data$parameterSets)
dsire_wind_params <- dsire_wind_params %>% 
  mutate(source = map(parameters, 1),
                             qualifier = map(parameters, 2),
                             amount = map(parameters, 3),
                             units = map(parameters, 4),
                             paramid = map(parameters, 5),
                             techid = map(technologies, 1),
                             techname = map(technologies, 2),
                             techcategory = map(technologies, 3),
                             sectorid = map(sectors, 1),
                             sectorname = map(sectors, 2),
                             sectortype = map(sectors, 3)) %>%
  select(-c(parameters, technologies, sectors)) %>%
  unnest(c(source, qualifier, amount, units, paramid))
