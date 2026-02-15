# Filter Version ----------------------------------------------------------
pacman::p_load(tidyverse, RColorBrewer, skimr, httr, jsonlite, sf, ggthemes)
options(scipen = 999)

# Load --------------------------------------------------------------------
kc_raw = read_csv("KISCOURSE.csv", show_col_types = FALSE) %>% janitor::clean_names()
inst_raw = read_csv("INSTITUTION.csv", show_col_types = FALSE) %>% janitor::clean_names()
tariff_raw = read_csv("TARIFF.csv", show_col_types = FALSE) %>% janitor::clean_names()

inst = inst_raw %>%
  select(legal_name, provaddress, pubukprn)

kc = kc_raw %>%
  select(
    pubukprn, kismode, kiscourseid, title, kisaimcode, crseurl
  ) %>% 
  filter(kismode=="01") %>%
  left_join(inst) 

df = tariff_raw

df2 = df %>%
  left_join(kc) %>%
  select(pubukprn, kiscourseid, legal_name, provaddress, kismode, mytarsbj = tarsbj, t001, t048, t064, t080, t096, 
         t112, t128, t144, t160, t176, t192, t208, t224, t240) %>% 
  mutate(
    `A*A*A*A* and above` = t224 + t240,
    `A*A*AC and above` = t192 + t208, 
    `A*A*A and above` = t160 + t176,
    `AAA and above` = t144,
    `ABB and above` = t128,
    `BBC and above` = t112,
    `CCC and above` = t096,
    `Below CCC` = t001 + t048 + t064 + t080
  ) %>%
  select(
    !c(kiscourseid, t001, t048, t064, t080, t096, t112, t128, t144, t160, 
       t176, t192, t208, t224, t240)
  )

TariffRaw = df2 %>%   drop_na(`A*A*A*A* and above`)
# View(TariffRaw)

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

# View(TariffGrouped)

# Get Postcode and Long/Lat -----------------------------------------------
postcodeRegex = "[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}"

LocationTariff = TariffGrouped %>%
  mutate(
    Postcode = str_extract(provaddress, postcodeRegex)
  )

getLadInfo <- function(postcode) {
  tryCatch({
    url <- paste0("https://api.postcodes.io/postcodes/", URLencode(postcode))
    res <- GET(url)
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    
    if (data$status == 200) {
      tibble(
        LAD.Code = data$result$codes$admin_district %||% NA_character_,
        LAD.Name = data$result$admin_district %||% NA_character_,
        lat      = data$result$latitude %||% NA_real_,
        lng      = data$result$longitude %||% NA_real_
      )
    } else {
      tibble(LAD.Code = NA_character_, LAD.Name = NA_character_, 
             lat = NA_real_, lng = NA_real_)
    }
  }, error = function(e) {
    tibble(LAD.Code = NA_character_, LAD.Name = NA_character_, 
           lat = NA_real_, lng = NA_real_)
  })
}

# Add LAD -----------------------------------------------------------------

lad_info <- map_dfr(LocationTariff$Postcode, getLadInfo)

LocationTariffLad <- bind_cols(LocationTariff, lad_info)

unique_postcodes <- LocationTariff %>% distinct(Postcode)

lad_lookup <- unique_postcodes %>%
  mutate(info = map(Postcode, getLadInfo)) %>%
  unnest(info)

LocationTariffLad <- LocationTariff %>%
  left_join(lad_lookup, by = "Postcode")

LAD.Tariff = LocationTariffLad %>% 
  group_by(LAD.Code, LAD.Name) %>%
  summarise(
    `AtAndAboveA*A*A` = sum(`AtAndAboveA*A*A`),
    `BelowA*A*A` = sum(`BelowA*A*A`),
    Total = sum(Total)
  ) %>% 
  mutate(
    `AtAndAboveA*A*A` = round(((`AtAndAboveA*A*A` / Total) * 100), 2)
  ) %>% select(-Total, -`BelowA*A*A`) %>% drop_na()


# View(LAD.Tariff)
# Load Map ----------------------------------------------------------------
url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2024_Boundaries_UK_BGC/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson"
LAD.SF <- st_read(url)

LAD.Map = LAD.SF %>%
  left_join(LAD.Tariff, by=c("LAD24CD" = "LAD.Code"))


st_write(LAD.Map, "LAD_Map.geojson")

# -------------------------------------------------------------------------

mapdata = st_read("LAD_Map.geojson")

names(englandmap)
englandmap = mapdata %>% filter(str_starts(LAD24CD, "E"))
ggplot() +
  # Base layer: all areas with just outlines
  geom_sf(data = englandmap, fill = "white", colour = "#a1a1a1", size = 0.2) +
  
  # Filled layer: only areas with data
  geom_sf(
    data = englandmap %>% filter(!is.na(AtAndAboveA.A.A)),
    aes(fill = AtAndAboveA.A.A),
    colour = "#a1a1a1", size = 0.2
  ) +
  
  scale_fill_distiller(palette = "Blues", direction = 1,
                       na.value = "grey95", name = "Entrants with A*A*A or Above (%)",
                       labels = scales::percent_format(scale = 1),
                       guide = guide_colorbar(
                         barheight = unit(6, "cm"),
                         barwidth = unit(0.6, "cm"),
                         frame.colour = "grey40",
                         ticks.colour = "grey40"
                       )
  ) +
  
  labs(
    title = paste0("High Attainment Across Local Authorities in: "),
    subtitle = "High Attainment Across Local Authorities in: " Percentage achieving A*A*A or above"
  ) + 
  theme_map() + 
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      colour = "grey30"
    ),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    plot.caption = element_text(
      size = 9,
      colour = "grey40",
      hjust = 1
    )
  )

createEntrantMap = function(sfdata = LAD.Map, region = "UK") {
  
  if (region == "UK") {
    mapdata = sfdata
  } else if (region == "Scotland") {
    mapdata = sfdata %>% filter(str_starts(LAD24CD, "S"))   # Scotland
  } else if (region == "Northern Ireland") {
    mapdata = sfdata %>% filter(str_starts(LAD24CD, "N"))
  } else if (region == "England") {
    mapdata = sfdata %>% filter(str_starts(LAD24CD, "E"))
  } else if (region == "Wales") {
    mapdata = sfdata %>% filter(str_starts(LAD24CD, "W"))
  } else if (region == "London") {
    mapdata = sfdata %>% filter(str_starts(LAD24CD, "E09"))
  } 
  
  myPlot = ggplot() +
    geom_sf(
      data = mapdata, aes(fill = `AtAndAboveA*A*A`),
      colour = "#a1a1a1", size = 0.2
    ) +
    
    scale_fill_distiller(palette = "RdBu", direction = 1,
                         na.value = "grey95", name = "Entrants with A*A*A or Above (%)",
                         labels = scales::percent_format(scale = 1),
                         guide = guide_colorbar(
                           barheight = unit(6, "cm"),
                           barwidth = unit(0.6, "cm"),
                           frame.colour = "grey40",
                           ticks.colour = "grey40"
                         )
    ) +
    
    labs(
      title = paste0("High Attainment Across Local Authorities in: ", region),
      subtitle = "Percentage achieving A*A*A or above"
    ) + 
    theme_map() +
    theme(
      plot.title = element_text(
        size = 16,
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5,
        colour = "grey30"
      ),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 10),
      plot.caption = element_text(
        size = 9,
        colour = "grey40",
        hjust = 1
      )
    )
  
  return(myPlot)
}

UK = createEntrantMap()
England = createEntrantMap(region="England")
Scotland = createEntrantMap(region="Scotland")
Wales = createEntrantMap(region="Wales")
London = createEntrantMap(region="London")




# -------------------------------------------------------------------------







