# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, RColorBrewer, shiny, bslib)
options(scipen = 999)
tariff = read_csv("tariff.uk.csv")
joblist = read_csv("joblist.uk.csv")
salaries = read_csv("salaries.uk.csv")
nss = read_csv("nss.uk.csv")


# PUBUKPRNS ---------------------------------------------------------------
pubukprns_uk_unis <- c(
  "10003270",  # Imperial College London
  "10007774",  # University of Oxford
  "10007788",  # University of Cambridge
  "10007784",  # University College London
  "10003645",  # King's College London
  "10007790",  # University of Edinburgh
  "10007798",  # University of Manchester
  "10007786",  # University of Bristol
  "10004063",  # London School of Economics & Political Science
  "10007163",  # University of Warwick
  "10006840",  # University of Birmingham
  "10007794",  # University of Glasgow
  "10007795",  # University of Leeds
  "10007158",  # University of Southampton 
  "10007157",  # University of Sheffield
  "10007143",  # Durham University
  "10007154",  # University of Nottingham
  "10007775",  # Queen Mary and Westfield College, University of London
  "10007803",  # University of St Andrews
  "10007850",  # University of Bath
  "10007799",  # University of Newcastle Upon Tyne
  "10006842",  # University of Liverpool
  "10007792",  # University of Exeter
  "10007768", # Lancs
  "10007167",  # University of York
  "10007814",  # Cardiff University
  "10007802", # reading
  "10007805", # Strathclyde
  "10007806", # Sussex
  "10005343",  # The Queen's University of Belfast
  "10004113",  # Loughborough University
  "10000961",  # Brunel University
  "10007780",   # School of Oriental and African Studies
  "10007783"  # aberdeen
)

# Test all unis  ----------------------------------------------------------
kc_raw = read_csv("KISCOURSE.csv", show_col_types = FALSE) %>% janitor::clean_names()
inst_raw = read_csv("INSTITUTION.csv", show_col_types = FALSE) %>% janitor::clean_names()
nss_raw = read_csv("NSS.csv") %>% janitor::clean_names()

inst = inst_raw %>%
  select(legal_name, provaddress, pubukprn)

kc = kc_raw %>%
  select(pubukprn, kismode, kiscourseid, title, kisaimcode, crseurl) %>% 
  filter(kismode=="01") %>%
  left_join(inst) 

nssraw1 = nss_raw %>%
  select(pubukprn, kiscourseid, kismode, nsssbj, nsspop, starts_with("t"))
nssraw2 = nssraw1 %>%
  left_join(kc) %>%
  filter(pubukprn %in% pubukprns_uk_unis) %>%
  filter(!is.na(title)) 

fillNSSkiscourse = nssraw2 %>%
  group_by(pubukprn, kiscourseid, kismode) %>%
  drop_na(t1) %>%
  summarise(
    t1fill = signif(weighted.mean(t1, nsspop, na.rm=TRUE), 3),
    t2fill = signif(weighted.mean(t2, nsspop, na.rm=TRUE), 3),
    t3fill = signif(weighted.mean(t3, nsspop, na.rm=TRUE), 3),
    t4fill = signif(weighted.mean(t4, nsspop, na.rm=TRUE), 3),
    t5fill = signif(weighted.mean(t5, nsspop, na.rm=TRUE), 3),
    t6fill = signif(weighted.mean(t6, nsspop, na.rm=TRUE), 3),
    t7fill = signif(weighted.mean(t7, nsspop, na.rm=TRUE), 3)
  )

fillNSSuni = nssraw2 %>%
  group_by(pubukprn, kismode) %>%
  drop_na(t1) %>%
  summarise(
    t1fill2 = signif(weighted.mean(t1, nsspop, na.rm=TRUE), 3),
    t2fill2 = signif(weighted.mean(t2, nsspop, na.rm=TRUE), 3),
    t3fill2 = signif(weighted.mean(t3, nsspop, na.rm=TRUE), 3),
    t4fill2 = signif(weighted.mean(t4, nsspop, na.rm=TRUE), 3),
    t5fill2 = signif(weighted.mean(t5, nsspop, na.rm=TRUE), 3),
    t6fill2 = signif(weighted.mean(t6, nsspop, na.rm=TRUE), 3),
    t7fill2 = signif(weighted.mean(t7, nsspop, na.rm=TRUE), 3)
  )

nssraw3 = nssraw2 %>%
  left_join(fillNSSkiscourse, by = c("pubukprn", "kiscourseid", "kismode")) %>%
  mutate(
    t1 = coalesce(t1, t1fill),
    t2 = coalesce(t2, t2fill),
    t3 = coalesce(t3, t3fill),
    t4 = coalesce(t4, t4fill),
    t5 = coalesce(t5, t5fill),
    t6 = coalesce(t6, t6fill),
    t7 = coalesce(t7, t7fill)
  )

nssraw4 = nssraw3 %>%
  left_join(fillNSSuni, by = c("pubukprn", "kismode")) %>%
  mutate(
    t1 = coalesce(t1, t1fill2),
    t2 = coalesce(t2, t2fill2),
    t3 = coalesce(t3, t3fill2),
    t4 = coalesce(t4, t4fill2),
    t5 = coalesce(t5, t5fill2),
    t6 = coalesce(t6, t6fill2),
    t7 = coalesce(t7, t7fill2)
  )

nssraw5 = nssraw4 %>%
  select(-c(t1fill, t2fill, t3fill, t4fill, t5fill, t6fill, t7fill,
            t1fill2, t2fill2, t3fill2, t4fill2, t5fill2, t6fill2, t7fill2)) %>%
  group_by(pubukprn, kiscourseid, kismode, title, legal_name, provaddress) %>%
  summarise(
    across(t1:t7, ~ signif(weighted.mean(., nsspop, na.rm = TRUE), 3)),
    nsspop = sum(nsspop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(overall = signif(rowMeans(across(t1:t7), na.rm = TRUE), 3))

View(nssraw5)

NSS = nssraw5 %>% drop_na(t1)

postcodeRegex = "[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}"

NSS.2 = NSS %>%
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
        LAD.Name = data$result$admin_district %||% NA_character_
      )
    } else {
      tibble(LAD.Code = NA_character_, LAD.Name = NA_character_)
    }
  }, error = function(e) {
    tibble(LAD.Code = NA_character_, LAD.Name = NA_character_)
  })
}

unique_postcodes <- NSS.2 %>% distinct(Postcode)

lad_lookup <- unique_postcodes %>%
  mutate(info = map(Postcode, getLadInfo)) %>%
  unnest(info)

NSS.3 <- NSS.2 %>%
  left_join(lad_lookup, by = "Postcode")

View(NSS.3)
write_csv(NSS.3, "NSS.Comparisontables.csv")

# # Filter region -----------------------------------------------------------
# region = "London"
# 
# if (region == "UK") {
#   myNSS = NSS.3
# } else if (region == "Scotland") {
#   myNSS = NSS.3 %>% filter(str_starts(LAD.Code, "S"))   # Scotland
# } else if (region == "Northern Ireland") {
#   myNSS = NSS.3 %>% filter(str_starts(LAD.Code, "N"))
# } else if (region == "England") {
#   myNSS = NSS.3 %>% filter(str_starts(LAD.Code, "E"))
# } else if (region == "Wales") {
#   myNSS = NSS.3 %>% filter(str_starts(LAD.Code, "W"))
# } else if (region == "London") {
#   myNSS = NSS.3 %>% filter(str_starts(LAD.Code, "E09"))
# }
# 
# 
# perUniNSS = myNSS %>% 
#   group_by(legal_name) %>%
#   summarise(
#     "Teaching on course" = weighted.mean(t1, nsspop, na.rm=TRUE),
#     "Assessment and Feedback" = weighted.mean(t2, nsspop, na.rm=TRUE),
#     "Academic Support" = weighted.mean(t3, nsspop, na.rm=TRUE),
#     "Learning Opportunities" = weighted.mean(t4, nsspop, na.rm=TRUE),
#     "Organisation and Management" = weighted.mean(t5, nsspop, na.rm=TRUE),
#     "Learning Resources" = weighted.mean(t6, nsspop, na.rm=TRUE),
#     "Learning Community" = weighted.mean(t7, nsspop, na.rm=TRUE),
#     "Overall" = weighted.mean(overall, nsspop, na.rm=TRUE)
#   )
# 
# perUniNSS 
# 
# 
# NSS.Table = tibble(
#   "Best Overall" = perUniNSS  %>% arrange(desc(Overall)) %>% select(legal_name, Overall) %>% head(3),
#   "Teaching on course" = perUniNSS  %>% arrange(desc("Teaching on course")) %>% select(legal_name, "Teaching on course") %>% head(3),
#   "Assessment and Feedback" = perUniNSS  %>% arrange(desc("Assessment and Feedback")) %>% select(legal_name, "Assessment and Feedback") %>% head(3),
#   "Academic Support" = perUniNSS  %>% arrange(desc("Academic Support")) %>% select(legal_name, "Academic Support") %>% head(3),
#   "Learning Opportunities" = perUniNSS  %>% arrange(desc("Learning Opportunities")) %>% select(legal_name, "Learning Opportunities") %>% head(3),
#   "Organisation and Management" = perUniNSS  %>% arrange(desc("Organisation and Management")) %>% select(legal_name, "Organisation and Management") %>% head(3),
#   "Learning Resources" = perUniNSS  %>% arrange(desc("Learning Resources")) %>% select(legal_name, "Learning Resources") %>% head(3),
#   "Learning Community" = perUniNSS  %>% arrange(desc("Learning Community")) %>% select(legal_name, "Learning Community") %>% head(3),
# )
# 





# Highest Salaries --------------------------------------------------------

 
