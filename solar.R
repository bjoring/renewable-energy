library(tidyverse)
library(curl)
library(readxl)
library(ggmap)
library(maps)
library(mapdata)
library(zoo)
library(xts)
library(ggrepel)

years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

# EIA 923 Energy generation data
EIA923query <- "https://www.eia.gov/electricity/data/eia923/archive/xls/f923_"
EIA923type <- ".zip"
EIA923fp <- "EIA923/EIA923_"

# EIA 860 Generator-level data
EIA860query <- "https://www.eia.gov/electricity/data/eia860/archive/xls/eia860"
EIA860type <- ".zip"
EIA860fp <- "EIA860/EIA860_"

# Download EIA datasets
for (year in years) {
  curl_download(paste0(EIA923query,year,EIA923type), paste0(EIA923fp,year,EIA923type), quiet=FALSE)
  curl_download(paste0(EIA860query,year,EIA860type), paste0(EIA860fp,year,EIA860type), quiet=FALSE)
}

# Download EIA small-scale estimations
sseyears <- c(2014,2015,2016,2017,2018,2019,2020)
dir.create("SmallScaleSolar")
for (year in sseyears) {
  if (year <= 2016) {
    query <- "https://www.eia.gov/electricity/data/eia861m/archive/xls/distributed_solar_"
  } else {
    query <- "https://www.eia.gov/electricity/data/eia861m/archive/xls/small_scale_solar_"
  }
  if (year <= 2015) {
    ftype <- ".xls"
  } else {
    ftype <- ".xlsx"
  }
  curl_download(paste0(query,year,ftype), paste0("SmallScaleSolar/small_scale_solar_",year,ftype))
}

# Unzip files to directories
for (year in years) {
  zipdir923 <- paste0(EIA923fp,year)
  zipdir860 <- paste0(EIA860fp,year)
  dir.create(zipdir923)
  dir.create(zipdir860)
  unzip(paste0(EIA923fp,year,EIA923type), exdir = zipdir923)
  unzip(paste0(EIA860fp,year,EIA860type), exdir = zipdir860)
}

# Let's take a look at solar generator data from 2019
plant19 <- read_excel("EIA860/EIA860_2019/2___Plant_Y2019.xlsx", skip = 1)
generator19 <- read_excel("EIA860/EIA860_2019/3_1_Generator_Y2019.xlsx", skip = 1)
solar19 <- read_excel("EIA860/EIA860_2019/3_3_Solar_Y2019.xlsx", skip = 1)

gensources <- generator19 %>%
  group_by(`Energy Source 1`) %>%
  summarize(n = n(),
            cap = sum(`Nameplate Capacity (MW)`))

gensources %>% ggplot() + 
  geom_col(aes(x = `Energy Source 1`, y = n))

gensources %>% ggplot() +
  geom_col(aes(x = `Energy Source 1`, y = cap))

gensources %>% ggplot() + 
  geom_col(aes(x = `Energy Source 1`, y = cap/n))

solar19 %>% 
  group_by(`Fixed Tilt?`) %>%
  summarize(summercap = mean(`Summer Capacity (MW)`, na.rm = TRUE),
            wintercap = mean(`Winter Capacity (MW)`, na.rm = TRUE))

solar19 %>% 
  group_by(`Single-Axis Tracking?`) %>%
  summarize(summercap = mean(`Summer Capacity (MW)`, na.rm = TRUE),
            wintercap = mean(`Winter Capacity (MW)`, na.rm = TRUE))

solar19loc <- left_join(solar19, plant19, by="Plant Code")
write.csv(solar19loc, 'solar_locations_2019.csv')

solar19 %>% ggplot() +
  geom_point(aes(x = `Nameplate Capacity (MW)`, y = `Summer Capacity (MW)`))

usa <- map_data("usa")
states <- map_data("states")
ggplot() + 
  geom_polygon(data = states, aes(long, lat, group = group), color = "black", fill = "white") +
  geom_point(data = solar19loc, aes(Longitude, Latitude, size = `Nameplate Capacity (MW)`), alpha = 0.1) +
  coord_fixed(1.3) +
  scale_x_continuous(limits=c(-130, -60))

# Let's take a look at wind now
wind19 <- read_excel("EIA860/EIA860_2019/3_2_Wind_Y2019.xlsx", skip = 1)
wind19loc <- left_join(wind19, plant19, by="Plant Code")

write.csv(wind19loc, 'wind_locations_2019.csv')

states <- map_data("states")
ggplot() + 
  geom_polygon(data = states, aes(long, lat, group = group), color = "black", fill = "white") +
  geom_point(data = solar19loc, aes(Longitude, Latitude, size = `Nameplate Capacity (MW)`), alpha = 0.1, color = "gold") +
  geom_point(data = wind19loc, aes(Longitude, Latitude, size = `Nameplate Capacity (MW)`), alpha = 0.1, color = "gray") +
  coord_fixed(1.3) +
  scale_x_continuous(limits=c(-130, -60)) +
  scale_y_continuous(limits = c(20, 50))
solar19loc <- left_join(solar19, plant19, by="Plant Code")

# How much solar energy is actually being generated?

fps <- list.files(path = "EIA923", pattern = "Schedules_2", recursive = TRUE, full.names = TRUE)

#Bind all EIA923 years into a single table
utility_gen <- read_excel(fps[1], sheet = 1, skip = 5, na = ".")
colnames(utility_gen) <- str_replace_all(colnames(utility_gen), "\\r\\n", " ")
utility_gen <- utility_gen %>%
  select(c(`Plant Id`, `Plant Name`, `Operator Name`, `Operator Id`, contains("State"), contains("Fuel Type"), YEAR, starts_with("Netgen")))
std_names <- colnames(utility_gen)

for (fp in tail(fps,length(fps)-1)) {
  utility_gen_year <- read_excel(fp, sheet = 1, skip = 5, na = ".") %>%
    select(c(`Plant Id`, `Plant Name`, `Operator Name`, `Operator Id`, contains("State"), contains("Fuel Type"), YEAR, starts_with("Netgen")))
  colnames(utility_gen_year) <- str_replace_all(colnames(utility_gen_year), "\\r\\n", " ")
  colnames(utility_gen_year) <- std_names
  utility_gen <- bind_rows(utility_gen, utility_gen_year)
}

solar_gen <- utility_gen %>% 
  filter(`Reported Fuel Type Code` == "SUN") %>%
  select(c(`Plant Id`, `Plant Name`, `Operator Name`, `Operator Id`, State, YEAR, starts_with("Netgen_")))

solar_gen <- solar_gen %>% 
  pivot_longer(cols = c(starts_with("Netgen_")), names_to = c("Type", "Month"), names_sep = "_", values_to = "MWh") %>%
  mutate(Date = parse_date_time(paste(Month, YEAR, sep=" "), orders = "bY"),
         MWh = as.numeric(MWh))

solar_gen %>% 
  group_by(Date, State) %>%
  summarize(kMWh = sum(MWh, na.rm=TRUE)/1000) %>%
  ggplot() +
  geom_line(aes(x = Date, y = kMWh, group = State, color = State))

solar_gen %>% 
  filter(State != "CA") %>%
  group_by(Date, State) %>%
  summarize(statekMWh = sum(MWh, na.rm=TRUE)/1000) %>%
  ggplot() +
  geom_line(aes(x = Date, y = statekMWh, group = State, color = State))

solar_summary <- solar_gen %>% filter(State != "DC") %>% group_by(Date, State) %>%
  summarize(kMWh = sum(MWh, na.rm=TRUE)/1000)

state <- solar_gen %>% filter(State == "NC") %>% group_by(Date) %>% summarize(kMWh = sum(MWh, na.rm = TRUE)/1000)
state_ts <- ts(state$kMWh, frequency=12)
f <- decompose(state_ts)
plot(f)

statelist <- solar_summary %>% ungroup() %>% distinct(State) %>% select(State)

state_trend <- function(x, statesolar) {
  state_data <- statesolar[statesolar$State == x,]
  start_time = c(year(state_data$Date[1]),month(state_data$Date[1]))
  state_ts <- ts(state_data$kMWh, frequency=12, start = start_time)
  f <- decompose(state_ts)
  return(f$trend)
}

trendlist <- lapply(statelist$State, state_trend, solar_summary)
solartrends <- do.call("ts.union", trendlist)
st <- as.xts(solartrends)
solartrendsdf <- as.data.frame(st)
colnames(solartrendsdf) = statelist$State
solartrendsdf$Date <- parse_date_time(rownames(solartrendsdf), orders = "bY")
solartrendsdf <- solartrendsdf %>% 
  pivot_longer(cols = 1:length(solartrendsdf)-1, names_to="State", values_to="kMWh") %>%
  mutate(Highlight = ifelse(State == "CA" | State == "NC" | State == "TX" | State == "NV" | State == "FL", State, NA))

ggplot(data = solartrendsdf) + 
  geom_line(aes(Date, kMWh, group = State, color = Highlight, size = !is.na(Highlight), alpha = !is.na(Highlight))) +
  geom_text(data = solartrendsdf %>% filter(!is.na(kMWh)) %>% filter(Date == last(Date), !is.na(Highlight)), aes(label = State, x = Date + 1e7, y = kMWh, color = State), size=4) + 
  guides(color = FALSE) +
  scale_size_manual(values=c(0.5, 1.5), guide = 'none') +
  scale_alpha_manual(values=c(0.5, 1), guide = 'none')
  

ggplot(data = solartrendsdf) + 
  geom_line(aes(Date, kMWh, group = State, color = Highlight, size = !is.na(Highlight), alpha = !is.na(Highlight))) +
  geom_label_repel(data = solartrendsdf %>% filter(!is.na(kMWh)) %>% filter(Date == last(Date), !is.na(Highlight)), aes(Date, kMWh, label = Highlight), nudge_x = 1e7, na.rm = TRUE) + 
  scale_size_manual(values=c(0.5, 1.5), guide = 'none') +
  scale_alpha_manual(values=c(0.5, 1), guide = 'none') + 
  guides(color = FALSE)



# What about by small-scale producers?

fps <- list.files(path = "SmallScaleSolar", pattern = "small_scale", recursive = FALSE, full.names = TRUE)
ssgen <- read_excel(fps[1], sheet = 3, skip = 2, na = c("NM","."))
for (fp in tail(fps,length(fps)-1)) {
  if (fp == "SmallScaleSolar/small_scale_solar_2019.xlsx") {
    ssgen_year <- read_excel(fp, sheet = 2, skip = 2, na = c("NM","."))
  } else {
    ssgen_year <- read_excel(fp, sheet = 3, skip = 2, na = c("NM","."))
  }
  ssgen <- bind_rows(ssgen, ssgen_year)
}
ssgen <- ssgen %>%
  filter(str_detect(Year, "^\\d{4}")) %>%
  mutate(MWh = Total...12)
ssgen$Date <- parse_date_time(paste(as.character(ssgen$Month), ssgen$Year, sep=" "), order = "mY")

ssgen %>% 
  group_by(Date, State) %>%
  summarize(statekMWh = sum(MWh, na.rm=TRUE)/1000) %>%
  ggplot() +
  geom_line(aes(x = Date, y = statekMWh, group = State, color = State))

ssgen_summary <- ssgen %>% group_by(Date, State) %>%
  summarize(statekMWh = sum(MWh, na.rm=TRUE)/1000)
