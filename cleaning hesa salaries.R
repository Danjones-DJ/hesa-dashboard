# Libs --------------------------------------------------------------------
pacman::p_load(tidyverse, RColorBrewer, skimr)

options(scipen = 999)

# Unis of Choice ----------------------------------------------------------
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
  "10007806",  # University of Bath
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
  "10007780"   # School of Oriental and African Studies
)


# Load --------------------------------------------------------------------
kc_raw = read_csv("KISCOURSE.csv", show_col_types = FALSE) %>% janitor::clean_names()
inst_raw = read_csv("INSTITUTION.csv", show_col_types = FALSE) %>% janitor::clean_names()
sal_raw = read_csv("GOSALARY.csv", show_col_types = FALSE) %>% janitor::clean_names()
tariff_raw = read_csv("TARIFF.csv", show_col_types = FALSE) %>% janitor::clean_names()
joblist_raw = read_csv("JOBLIST.csv", show_col_types = FALSE) %>% janitor::clean_names()

View(kc)
names(kc_raw)

inst = inst_raw %>%
  select(legal_name, provaddress, pubukprn)

kc = kc_raw %>%
  select(
    pubukprn, kismode, kiscourseid, title, kisaimcode, crseurl
  ) %>% 
  filter(kismode=="01") %>%
  left_join(inst) 



# Salaries ----------------------------------------------------------------


df = sal_raw %>%
  filter(pubukprn %in% pubukprns_uk_unis) %>%
  left_join(kc) %>%
  filter(!is.na(title))

# hist(df$goinstmed)
# View(df)


# skim(df)
salaryFill = df %>%
  group_by(pubukprn) %>%
  summarise(
    fillMedian = signif(weighted.mean(goinstmed, gosalpop, na.rm=TRUE), 3),
    fillPop = ceiling(mean(gosalpop, na.rm=TRUE)),
    fillLQ = signif(weighted.mean(goinstlq, gosalpop, na.rm=TRUE), 3),
    fillUQ = signif(weighted.mean(goinstuq, gosalpop, na.rm=TRUE), 3),
  )
# View(salaryFill)

df2 <- df %>% left_join(salaryFill, by = "pubukprn") %>%
  mutate(
    goinstmed = coalesce(goinstmed, fillMedian),
    goinstlq = coalesce(goinstlq, fillLQ),
    goinstuq = coalesce(goinstuq, fillUQ),
    gosalpop = coalesce(gosalpop, fillPop)
  )

write_csv(df2, "salaries.uk.csv")

skim(df2)

# ggplot(data.frame(simsal), aes(x = simsal)) +
#   geom_histogram(
#     bins = 5,
#     fill = "#2C3E50",
#     colour = "white",
#     alpha = 0.9
#   ) +
#   theme_minimal(base_size = 14) +
#   labs(
#     x = "Simulated Salary (£)",
#     y = "Frequency",
#     title = "Simulated Graduate Salary Distribution"
#   ) + theme_classic()



# Joblist -----------------------------------------------------------------

df = joblist_raw %>%
  filter(pubukprn %in% pubukprns_uk_unis) %>%
  left_join(kc)

df2 = df %>%
  filter(
    legal_name == "Queen Mary University of London",
    title == "Accounting and Finance"
  ) %>%
  group_by(pubukprn, kiscourseid, kismode) %>%
  filter(comsbj == first(comsbj)) %>%
  ungroup() %>%
  group_by(pubukprn, title) %>%
  filter(kiscourseid == first(kiscourseid)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(pubukprn, kiscourseid, kismode, comsbj, title, kisaimcode, crseurl, legal_name, provaddress),
    names_from = job,
    values_from = perc,
    values_fill = 0
  ) %>% 
  mutate(
    totalPerc = rowSums(across(10:last_col())),
    across(10:last_col(), ~ round(. / totalPerc * 100, 1))
  ) %>%
  select(-totalPerc) %>%
  distinct(pubukprn, kiscourseid, .keep_all = TRUE) %>%
  pivot_longer(
    cols = 10:ncol(.),
    names_to = "job",
    values_to = "perc"
  ) 

View(df2)


# joblist tets icl --------------------------------------------------------


df = joblist_raw %>%
  filter(pubukprn %in% pubukprns_uk_unis) %>%
  left_join(kc) %>%
  filter(!is.na(title))

write_csv(df, "joblist.uk.csv")


## 
df2 = df %>%
  filter(
    legal_name == "Imperial College of Science, Technology and Medicine",
    title == "Aeronautical Engineering"
  ) %>%
  group_by(pubukprn, kiscourseid, kismode) %>%
  ungroup() %>%
  group_by(pubukprn, title) %>%
  filter(kiscourseid == first(kiscourseid)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(pubukprn, kiscourseid, kismode, comsbj, title, kisaimcode, crseurl, legal_name, provaddress),
    names_from = job,
    values_from = perc,
    values_fill = 0
  ) %>% 
  mutate(
    totalPerc = rowSums(across(10:last_col())),
    across(10:last_col(), ~ round(. / totalPerc * 100, 1))
  ) %>%
  select(-totalPerc) %>%
  distinct(pubukprn, kiscourseid, .keep_all = TRUE) %>%
  pivot_longer(
    cols = 10:ncol(.),
    names_to = "job",
    values_to = "perc"
  ) 


df2

View(df2)


# Tariff ------------------------------------------------------------------


df = tariff_raw %>%
  filter(pubukprn %in% pubukprns_uk_unis) 

df2 = df %>%
  select(pubukprn, kiscourseid, kismode, mytarsbj = tarsbj, t001, t048, t064, t080, t096, 
         t112, t128, t144, t160, t176, t192, t208, t224, t240) %>%
  left_join(kc) %>%
  mutate(
    `A*A*A*A* and above` = t224 + t240,
    `A*A*AC and above` = t192 + t208, 
    `A*A*A and above` = t160 + t176,
    `AAA and above` = t144,
    `ABB and above` = t128,
    `BBC and above` = t112,
    `CCC and above` = t096,
    `Below CCC` = t001 + t048 + t064 + t080
  )
write_csv(df2, "tariff.uk.csv")
View(df2 )



####
df = tariff_raw %>%
  filter(pubukprn %in% pubukprns_uk_unis) %>%
  left_join(kc)
View(df)

df2 = df %>%
  select(pubukprn, kiscourseid, kismode, mytarsbj = tarsbj, t001, t048, t064, t080, t096, 
         t112, t128, t144, t160, t176, t192, t208, t224, t240) %>%
  left_join(kc) %>%
  mutate(
    `A*A*A*A* and above` = t224 + t240,
    `A*A*AC and above` = t192 + t208, 
    `A*A*A and above` = t160 + t176,
    `AAA and above` = t144,
    `ABB and above` = t128,
    `BBC and above` = t112,
    `CCC and above` = t096,
    `Below CCC` = t001 + t048 + t064 + t080
  ) 

write_csv(df2, "tariff.uk.csv")

df3 = df2 %>%
  mutate(
    totalPerc = rowSums(across(24:last_col())),
    across(24:last_col(), ~ round(. / totalPerc * 100, 1))
  ) %>%
  select(-totalPerc) %>%
  pivot_longer(
    cols = c(`A*A*A*A* and above`, `A*A*AC and above`, `A*A*A and above`,
             `AAA and above`, `ABB and above`, `BBC and above`, 
             `CCC and above`, `Below CCC`),
    names_to = "entry_tariff", 
    values_to = "count"
  ) %>%
  mutate(
    entry_tariff = factor(
      entry_tariff,
      levels = c(
        "Below CCC",
        "CCC and above",
        "BBC and above",
        "ABB and above",
        "AAA and above",
        "A*A*A and above",
        "A*A*AC and above",
        "A*A*A*A* and above"
      )
    )
  )

View(df3)


# NSS ---------------------------------------------------------------------

nss_raw = read_csv("NSS.csv") %>% janitor::clean_names()
# View(nss_raw)

df = nss_raw %>%
  select(pubukprn, kiscourseid, kismode, nsssbj, nsspop, starts_with("t"))

# df


df2 = df %>%
  filter(pubukprn %in% pubukprns_uk_unis) %>%
  left_join(kc) %>%
  filter(!is.na(title)) 

fillNSSkiscourse = df2 %>%
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

fillNSSuni = df2 %>%
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

# Stage 1: fill from course-level averages
df3 = df2 %>%
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

# Stage 2: fill remaining NAs from uni-level averages
df4 = df3 %>%
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

df5 = df4 %>%
  select(-c(t1fill, t2fill, t3fill, t4fill, t5fill, t6fill, t7fill,
            t1fill2, t2fill2, t3fill2, t4fill2, t5fill2, t6fill2, t7fill2)) %>%
  group_by(pubukprn, kiscourseid, kismode, title, legal_name, provaddress) %>%
  summarise(
    across(t1:t7, ~ signif(weighted.mean(., nsspop, na.rm = TRUE), 3)),
    nsspop = sum(nsspop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(overall = signif(rowMeans(across(t1:t7), na.rm = TRUE), 3))


write_csv(df5, "nss.uk.csv")
# View(df5)
# vis nss -----------------------------------------------------------------

