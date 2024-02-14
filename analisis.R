
# libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(magrittr)
library(readr)
library(purrr)
library(janitor)
library(lubridate)
library(googlesheets4)
library(ggchicklet)
library(leaflet)
library(sf)


# raw data ----------------------------------------------------------------
setwd("data/transportation/")
# https://climatetrace.org/data
bd <-  list.files(pattern = "\\_country_emissions.csv$") %>% 
  map_df(~read_csv(.))

setwd("../../")
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?view=chart
country_class <- read_csv("data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_125/Metadata_Country_API_NY.GDP.MKTP.CD_DS2_en_csv_v2_125.csv") %>% 
  clean_names()
gdp <- read_csv("data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_125/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_125.csv", skip = 4) %>% 
  clean_names()

# https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/export/
wordl<- sf::read_sf("data/world-administrative-boundaries/") %>% 
  filter(status == "Member State")

# arrange and join data ---------------------------------------------------

country_class<- country_class %>%  select(country_code:income_group) %>% 
  na.omit()

gdp <-gdp %>%  select(country_code, country_name, gdp=x2022)

aux <- gdp %>%  inner_join(country_class) 

bd <-bd %>%  filter(gas == "co2")  %>% 
  mutate(year = as.numeric(format(start_time,'%Y'))) %>% 
  select(country_code = iso3_country, year, type2 = original_inventory_sector,
         co2 = emissions_quantity) %>% 
  inner_join(aux) %>% 
  mutate( type2 = gsub(pattern = "-transportation", replacement = "", x = type2),
          type2 = gsub(pattern = "-transport", replacement = "", x = type2),
          type2 = str_to_sentence(type2),
          type = case_when(type2 == "Domestic-aviation"~"Aviation", 
                           type2 =="International-aviation"~"Aviation",
                           type2 == "Domestic-shipping"~"Shipping", 
                           type2 =="International-shipping"~"Shipping",
                           T~type2 ) ) 

rm(aux, country_class, gdp)

# exploratory anlisys -----------------------------------------------------
# General
bd %>%  group_by(type) %>%  summarise(co2=sum(co2, na.rm = T)) %>% 
  ggplot(aes(x = fct_reorder(type, co2), y = co2))+
  geom_chicklet(width = .7, fill = "pink")+
  coord_flip()+
  theme_minimal()

bd %>%  group_by(type, year) %>%  summarise(co2=sum(co2, na.rm = T)) %>% 
  ggplot(aes(x = year, y = co2, color = type))+
  geom_line()

bd %>%  group_by(country_name) %>% filter(year==2022) %>% 
  summarise(co2 = sum(co2, na.rm = T)) %>%  top_n(10)%>% 
  ggplot(aes(x = fct_reorder(country_name, co2), y = co2))+
  geom_chicklet(width = .7, fill = "pink")+
  coord_flip()+
  theme_minimal()


bd %>% filter(year == 2022) %>% 
  mutate(income_group = factor(income_group, c("High income", "Upper middle income",
                                               "Lower middle income", "Low income"))) %>% 
  ggplot(aes(x= income_group, y = co2 %>%  log() , color= income_group))+
  geom_boxplot(width =.3)+
  geom_jitter(color="gray", size=0.4, alpha=0.7)+
  # facet_wrap(~year)+
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")

bd %>% filter(year == 2022) %>% 
  ggplot(aes(x= region, y = co2 %>%  log() , color= region))+
  geom_boxplot()+
  geom_jitter(color="gray", size=0.4, alpha=0.7)+
  # facet_wrap(~year)+
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")

bd %>% filter(year == 2022) %>%  
  filter(co2>0) %>% 
  mutate(income_group = factor(income_group, c("High income", "Upper middle income",
                                               "Lower middle income", "Low income"))) %>% 
  ggplot(aes(x= gdp %>%  log(), y = co2 %>%log(), color= income_group))+
  geom_point()+
  theme(legend.position = "bottom")+
  facet_wrap(~type)






aux <- bd %>%  filter(year == 2022) %>% 
  group_by(country_code) %>%  summarise(co2=sum(co2, na.rm = T))

auxi <- wordl %>%  left_join(aux %>% mutate(co2 = log(co2)) %>%  
                               filter(!is.na(co2), is.finite(co2) ),
                             by= c("iso3" = "country_code"))

bins <- c(0,5,9,12, 17, 21, 22, Inf )
pal <- colorBin("YlOrRd", domain = auxi$co2, bins = bins)

mapita <- leaflet(auxi) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))


labels <- sprintf(
  "<strong>%s</strong><br/>%g log CO<sup>2</sup> emissions (tonnes)",
  auxi$name, auxi$co2 %>%  round(1)) %>%
  lapply(htmltools::HTML)

mapita %>% addTiles() %>% 
  addPolygons( fillColor =  ~pal(co2),
               weight = 2,
               opacity = 1,
               color = "white",
               dashArray = "3",
               fillOpacity = 0.7,
               highlightOptions = highlightOptions(
                 weight = 5,
                 color = "#666",
                 dashArray = "",
                 fillOpacity = 0.7,
                 bringToFront = TRUE),
               label = labels,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto"))





# (pretty) final charts  ---------------------------------------------------

bd %>%  filter(type =="Aviation") %>%  
  group_by(type2, year) %>%  summarise(co2 =sum(co2, na.rm = T)) %>% 
  ggplot(aes(x = year, y= co2, color = type2))+
  geom_line()
