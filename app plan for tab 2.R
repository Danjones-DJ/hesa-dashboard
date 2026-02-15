# Libs --------------------------------------------------------------------
pacman::p_load(tidyverse, RColorBrewer, skimr, httr, jsonlite, sf, ggthemes)

options(scipen = 999)


# Load Tariff -------------------------------------------------------------
TariffRaw = read_csv("tariff.uk.csv", show_col_types = FALSE) %>%
  drop_na(`A*A*A*A* and above`)


# Mutate and Organise -----------------------------------------------------

Tariff = TariffRaw %>%
  mutate(
    `AtAndAboveA*A*A` = `A*A*A*A* and above` + `A*A*AC and above` + `A*A*A and above`,
    `BelowA*A*A` = `AAA and above` + `ABB and above` + `BBC and above` + `CCC and above` + `Below CCC`   
  ) %>% 
  select(
    !c(`A*A*A*A* and above`, `A*A*AC and above`, `A*A*A and above`,  
            `AAA and above`, `ABB and above`, `BBC and above`, `CCC and above`,      
            `Below CCC`)
    )

# names(Tariff)
# View(Tariff)


# Group by Uni ------------------------------------------------------------

TariffGrouped = Tariff %>%
  group_by(legal_name, provaddress) %>%
  summarise(
    `AtAndAboveA*A*A` = sum(`AtAndAboveA*A*A`),
    `BelowA*A*A` = sum(`BelowA*A*A`),
    Total = sum(`AtAndAboveA*A*A`) + sum(`BelowA*A*A`)
  ) %>%
  mutate(
    provaddress = case_when(
      legal_name == "School of Oriental and African Studies" ~ "Beacon House, Queens Road, Bristol, BS8 1QU, UK",
      legal_name == "University of Bristol" ~ "10 Thornhaugh Street, Russell Square, London, WC1H 0XG",
      TRUE ~ as.character(provaddress)
    )
  ) %>% drop_na()

# Get Postcode and Long/Lat -----------------------------------------------
postcodeRegex = "[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}"

LocationTariff = TariffGrouped %>%
  mutate(
    Postcode = str_extract(provaddress, postcodeRegex)
  )

# View(LocationTariff)

# Location Helpers --------------------------------------------------------

getLadCode = function(postcode) {
  
  url = paste0("https://api.postcodes.io/postcodes/", URLencode(postcode))
  result = GET(url)
  data = fromJSON(content(result, "text", encoding="UTF-8"))
  
  return(data$result$codes$admin_district)
}

getLadName = function(postcode) {
  
  url = paste0("https://api.postcodes.io/postcodes/", URLencode(postcode))
  result = GET(url)
  data = fromJSON(content(result, "text", encoding="UTF-8"))
  
  return(data$result$admin_district)
}

# Add LAD -----------------------------------------------------------------
LocationTariffLad = LocationTariff %>%
  mutate(
    LAD.Code = getLadCode(Postcode),
    LAD.Name = getLadName(Postcode)
  ) 


LAD.Tariff = LocationTariffLad %>% 
  group_by(LAD.Code, LAD.Name) %>%
  summarise(
    `AtAndAboveA*A*A` = sum(`AtAndAboveA*A*A`),
    `BelowA*A*A` = sum(`BelowA*A*A`),
    Total = sum(Total)
  ) %>% 
  mutate(
    `AtAndAboveA*A*A` = round(((`AtAndAboveA*A*A` / Total) * 100), 2)
  ) %>% select(-Total, -`BelowA*A*A`)


# Load Map ----------------------------------------------------------------
LAD.SF = st_read("Local_Authority_Districts_(December_2024)_Names_and_Codes_in_the_UK.shp")

url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2024_Boundaries_UK_BGC/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson"

LAD.SF <- st_read(url)

LAD.Map = LAD.SF %>%
  left_join(LAD.Tariff, by=c("LAD24CD" = "LAD.Code"))

ggplot() + 
  geom_sf(
    data = LAD.Map,
    aes(fill = `AtAndAboveA*A*A`),
    colour = "black",
    size = 0.1
    ) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "#ffffff",
    name = "% A*A*A or Above",
    direction = -1
  ) + theme_void()

