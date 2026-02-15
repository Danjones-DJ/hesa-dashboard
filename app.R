# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, RColorBrewer, shiny, bslib, leaflet, sf, ggthemes)
options(scipen = 999)
tariff = read_csv("tariff.uk.csv")
joblist = read_csv("joblist.uk.csv")
salaries = read_csv("salaries.uk.csv")
nss = read_csv("nss.uk.csv")
NSS.3 = read_csv("NSS.Comparisontables.csv")
mapdata = st_read("LAD_Map.geojson")
# Helper: wrap long labels
wrap_labels <- function(x, width = 18) str_wrap(x, width = width)

# Theme -------------------------------------------------------------------

spotify_theme <- bs_theme(
  version = 5,
  bg = "#121212",
  fg = "#FFFFFF",
  primary = "#1DB954",
  secondary = "#282828",
  success = "#1DB954",
  base_font = font_google("Outfit"),
  heading_font = font_google("Outfit"),
  font_scale = 1.05,
  `enable-rounded` = TRUE
)


# CSS ---------------------------------------------------------------------

custom_css <- tags$style(HTML("
  @import url('https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700&display=swap');

  /* ── Global ── */
  html, body {
    background: #121212 !important;
    color: #E0E0E0 !important;
    font-family: 'Outfit', sans-serif !important;
    overflow-x: hidden;
    height: 100%;
    margin: 0;
    padding: 0;
  }

  /* Force everything into viewport */
  .container-fluid {
    padding: 10px 16px !important;
    height: 100vh;
    display: flex;
    flex-direction: column;
  }

  /* ── Filter bar ── */
  .filter-bar {
    background: #181818;
    border: 1px solid rgba(255,255,255,0.06);
    border-radius: 12px;
    padding: 10px 20px;
    margin-bottom: 10px;
    box-shadow: 0 4px 24px rgba(0,0,0,0.4);
    overflow: visible !important;
    z-index: 100;
    position: relative;
    flex-shrink: 0;
  }
  .filter-bar .row,
  .filter-bar .col-sm-6,
  .filter-bar .form-group {
    overflow: visible !important;
  }
  .filter-bar .form-group {
    margin-bottom: 2px !important;
  }
  .selectize-control {
    overflow: visible !important;
  }
  .selectize-dropdown {
    z-index: 10000 !important;
    position: absolute !important;
  }
  .filter-bar .filter-label {
    color: #1DB954;
    font-weight: 600;
    font-size: 0.65rem;
    text-transform: uppercase;
    letter-spacing: 1.5px;
    margin-bottom: 6px;
    display: flex;
    align-items: center;
    gap: 6px;
  }

  /* ── Select inputs ── */
  .form-group > label,
  .control-label {
    color: #B3B3B3 !important;
    font-weight: 500;
    font-size: 0.72rem;
    text-transform: uppercase;
    letter-spacing: 1.2px;
    margin-bottom: 4px;
  }
  .form-select, .form-control,
  .selectize-input, .selectize-control.single .selectize-input {
    background: #282828 !important;
    border: 1px solid rgba(255,255,255,0.08) !important;
    border-radius: 10px !important;
    color: #FFFFFF !important;
    padding: 8px 12px !important;
    font-family: 'Outfit', sans-serif !important;
    font-size: 0.9rem !important;
    transition: all 0.25s ease;
    box-shadow: none !important;
  }
  .selectize-input.focus,
  .form-select:focus, .form-control:focus {
    border-color: #1DB954 !important;
    box-shadow: 0 0 0 3px rgba(29,185,84,0.15) !important;
  }
  .selectize-dropdown,
  .selectize-dropdown-content {
    background: #282828 !important;
    border: 1px solid rgba(255,255,255,0.08) !important;
    border-radius: 10px !important;
    color: #FFFFFF !important;
    font-family: 'Outfit', sans-serif !important;
    box-shadow: 0 12px 40px rgba(0,0,0,0.6) !important;
    margin-top: 4px !important;
  }
  .selectize-dropdown .option {
    color: #E0E0E0 !important;
    padding: 10px 14px !important;
    transition: all 0.15s ease;
  }
  .selectize-dropdown .option:hover,
  .selectize-dropdown .active {
    background: #1DB954 !important;
    color: #FFFFFF !important;
    border-radius: 6px;
  }
  .selectize-input > .item {
    color: #FFFFFF !important;
  }

  /* ── Cards ── */
  .bslib-card {
    background: #181818 !important;
    border: 1px solid rgba(255,255,255,0.06) !important;
    border-radius: 12px !important;
    box-shadow: 0 4px 24px rgba(0,0,0,0.35) !important;
    overflow: hidden;
    transition: transform 0.3s ease, box-shadow 0.3s ease;
    height: 100%;
  }
  .bslib-card:hover {
    transform: translateY(-2px);
    box-shadow: 0 8px 36px rgba(0,0,0,0.5) !important;
  }
  .card-header, .bslib-card .card-header {
    background: transparent !important;
    border-bottom: 1px solid rgba(255,255,255,0.06) !important;
    color: #FFFFFF !important;
    font-weight: 600 !important;
    font-size: 0.85rem !important;
    letter-spacing: -0.2px;
    padding: 10px 16px !important;
    display: flex;
    align-items: center;
    gap: 8px;
  }
  .card-header::before {
    content: '';
    width: 3px;
    height: 14px;
    background: #1DB954;
    border-radius: 2px;
    flex-shrink: 0;
  }
  .card-body {
    padding: 6px 12px 10px !important;
  }

  /* ── Plot grid ── */
  .plot-grid {
    flex: 1;
    display: flex;
    flex-direction: column;
    gap: 10px;
    min-height: 0;
  }
  .plot-row {
    flex: 1;
    margin-bottom: 0 !important;
    min-height: 0;
  }
  .plot-row .col-sm-6 {
    height: 100%;
  }
  .plot-row .col-sm-6 > div {
    height: 100%;
  }

  /* ── Plot area ── */
  .shiny-plot-output {
    border-radius: 8px;
  }

  /* ── Scrollbar ── */
  ::-webkit-scrollbar { width: 8px; }
  ::-webkit-scrollbar-track { background: #121212; }
  ::-webkit-scrollbar-thumb {
    background: #333;
    border-radius: 4px;
  }
  ::-webkit-scrollbar-thumb:hover { background: #1DB954; }

  /* ── Animate in ── */
  @keyframes fadeSlideUp {
    from { opacity: 0; transform: translateY(18px); }
    to   { opacity: 1; transform: translateY(0); }
  }
  .animate-in {
    animation: fadeSlideUp 0.5s ease forwards;
  }
  .animate-in.d1 { animation-delay: 0.08s; opacity: 0; }
  .animate-in.d2 { animation-delay: 0.16s; opacity: 0; }
  .animate-in.d3 { animation-delay: 0.24s; opacity: 0; }
  .animate-in.d4 { animation-delay: 0.32s; opacity: 0; }

  /* ── Responsive ── */
  @media (max-width: 768px) {
    .filter-bar { padding: 10px; }
  }
"))



# UI ----------------------------------------------------------------------


ui <- page_fluid(
  theme = spotify_theme,
  custom_css,
  
  navset_tab(
    nav_panel("Tab One",
              
              ## Filter 
              div(
                class = "filter-bar animate-in",
                div(class = "filter-label", HTML("&#9662; FILTERS")),
                fluidRow(
                  column(12, selectInput("regions", "Select Region",
                                         choices = c("UK", "England", "Wales", "Scotland", "London"),
                                         selected = "UK"
                  )))
              ),
              
              ## Comparison
              
              nav_panel("Leaderboard",
                        fluidRow(
                          column(5,
                                 
                                 
                                 div(
                                   class = "animate-in d1",
                                   card(
                                     height= "700px",
                                     card_header("Leaderboards"),
                                     card_body(
                                       navset_pill(
                                         nav_panel("Overall", tableOutput("tb.overall")),
                                         nav_panel("Teaching", tableOutput("tb.teaching")),
                                         nav_panel("Assessment", tableOutput("tb.ass")),
                                         nav_panel("Support", tableOutput("tb.sup")),
                                         nav_panel("Learning Opportunities", tableOutput("tb.lo")),
                                         nav_panel("Management", tableOutput("tb.mgmt")),
                                         nav_panel("Resources", tableOutput("tb.res")),
                                         nav_panel("Community", tableOutput("tb.com")),
                                       )
                                     )
                                   )
                                 )
                                 
                          ),
                          
                          column(
                            7,
                            div(
                              class = "animate-in d1",
                              card(
                                height = "700px",
                                card_header("Proportion of University entrants achieving A*A*A or above"),
                                card_body(plotOutput("entrantmap", height = "100%"))
                              )
                            )
                          )
                        )
              ),
              
              
              
    ),
    
    nav_panel("Degree Overview",
              # Filter Bar --------------------------------------------------------------
              div(
                class = "filter-bar animate-in",
                div(class = "filter-label", HTML("&#9662; FILTERS")),
                fluidRow(
                  column(6, selectInput("universities", "University",
                                        choices = unique(salaries$legal_name),
                                        selected = unique(salaries$legal_name)[20])),
                  column(6, selectInput("degrees", "Degree", choices = NULL))
                )
              ),
              
              
              # Plot Grid ---------------------------------------------------------------
              
              div(
                class = "plot-grid",
                
                # ── Plot Row 1 ──
                fluidRow(
                  class = "plot-row",
                  column(
                    6,
                    div(
                      class = "animate-in d1",
                      card(
                        height = "400px",
                        card_header("Salary Distribution"),
                        card_body(plotOutput("salaryplot", height = "100%"))
                      )
                    )
                  ),
                  column(
                    6,
                    div(
                      class = "animate-in d2",
                      card(
                        height = "400px",
                        card_header("Entry Tariff Breakdown"),
                        card_body(plotOutput("entryplot", height = "100%"))
                      )
                    )
                  )
                ),
                
                
                # Plot Row 2 --------------------------------------------------------------
                
                fluidRow(
                  class = "plot-row",
                  column(
                    6,
                    div(
                      class = "animate-in d3",
                      card(
                        height = "400px",
                        card_header("Job Outcomes"),
                        card_body(plotOutput("jobplot", height = "100%"))
                      )
                    )
                  ),
                  column(
                    6,
                    div(
                      class = "animate-in d4",
                      card(
                        height = "400px",
                        card_header("NSS Outcomes (Gray = Uni Average)"),
                        card_body(plotOutput("nssplot", height = "100%"))
                      )
                    )
                  )
                )
              ))
    
  )
)



# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  
  # Observe events ----------------------------------------------------------
  
  observeEvent(input$universities, {
    filtered_degrees <- salaries %>%
      filter(legal_name == input$universities) %>%
      pull(title) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "degrees",
                      choices = filtered_degrees,
                      selected = filtered_degrees[1])
  })
  
  
  # Reactives ---------------------------------------------------------------
  filtered_map = reactive({
    req(input$regions)
    region <- input$regions
    
    if (region == "UK") {
      myMapData <- mapdata
    } else if (region == "Scotland") {
      myMapData <- mapdata %>% filter(str_starts(LAD24CD, "S"))
    } else if (region == "Northern Ireland") {
      myMapData <- mapdata %>% filter(str_starts(LAD24CD, "N"))
    } else if (region == "England") {
      myMapData <- mapdata %>% filter(str_starts(LAD24CD, "E"))
    } else if (region == "Wales") {
      myMapData <- mapdata %>% filter(str_starts(LAD24CD, "W"))
    } else if (region == "London") {
      myMapData <- mapdata %>% filter(str_starts(LAD24CD, "E09"))
    }
    
    myMapData
    
  })
  filtered_comparisonnss <- reactive({
    
    req(input$regions)
    
    region <- input$regions
    
    if (region == "UK") {
      myNSS <- NSS.3
    } else if (region == "Scotland") {
      myNSS <- NSS.3 %>% filter(str_starts(LAD.Code, "S"))
    } else if (region == "Northern Ireland") {
      myNSS <- NSS.3 %>% filter(str_starts(LAD.Code, "N"))
    } else if (region == "England") {
      myNSS <- NSS.3 %>% filter(str_starts(LAD.Code, "E"))
    } else if (region == "Wales") {
      myNSS <- NSS.3 %>% filter(str_starts(LAD.Code, "W"))
    } else if (region == "London") {
      myNSS <- NSS.3 %>% filter(str_starts(LAD.Code, "E09"))
    }
    
    myNSS
  })
  
  filtered_salaries <- reactive({
    req(input$universities, input$degrees)
    salaries %>%
      filter(legal_name == input$universities, title == input$degrees)
  })
  
  filtered_tariff <- reactive({
    req(input$universities, input$degrees)
    tariff %>%
      filter(legal_name == input$universities, title == input$degrees)
  })
  
  filtered_joblist <- reactive({
    req(input$universities, input$degrees)
    joblist %>%
      filter(legal_name == input$universities, title == input$degrees) %>%
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
  })
  
  filtered_nssUL <- reactive({
    req(input$universities)
    nss %>%
      filter(legal_name == input$universities) %>%
      summarise(
        meant1 = mean(t1, na.rm = TRUE),
        meant2 = mean(t2, na.rm = TRUE),
        meant3 = mean(t3, na.rm = TRUE),
        meant4 = mean(t4, na.rm = TRUE),
        meant5 = mean(t5, na.rm = TRUE),
        meant6 = mean(t6, na.rm = TRUE),
        meant7 = mean(t7, na.rm = TRUE),
        meanoverall = mean(overall, na.rm = TRUE)
      )
  })
  
  filtered_nssCL <- reactive({
    req(input$universities, input$degrees)
    nss %>%
      filter(legal_name == input$universities & title == input$degrees) %>%
      select(kiscourseid, nsspop, t1, t2, t3, t4, t5, t6, t7, overall) %>%
      slice_max(nsspop, n = 1) %>%
      summarise(t1, t2, t3, t4, t5, t6, t7, overall)
  })
  
  
  # Plot Theme --------------------------------------------------------------
  
  dark_theme <- function(base_size = 4) {
    theme_classic(base_size = base_size) +
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(colour = "#E0E0E0", family = "sans"),
        axis.text = element_text(colour = "#B3B3B3", size = rel(0.8)),
        axis.title = element_text(colour = "#B3B3B3", size = rel(0.85), face = "bold"),
        axis.line = element_line(colour = "#333333"),
        plot.margin = margin(8, 12, 8, 8)
      )
  }
  
  
  # Plot 1 ------------------------------------------------------------------
  
  output$salaryplot <- renderPlot({
    df <- filtered_salaries()
    
    df2 <- df %>%
      summarise(
        med = weighted.mean(goinstmed, gosalpop, na.rm = TRUE),
        lq  = weighted.mean(goinstlq, gosalpop, na.rm = TRUE),
        uq  = weighted.mean(goinstuq, gosalpop, na.rm = TRUE),
        pop = sum(gosalpop, na.rm = TRUE)
      ) %>%
      mutate(mu = med, sigma = (uq - lq) / 1.349)
    
    simsal <- rnorm(n = 50, mean = df2$mu, sd = df2$sigma)
    
    ggplot(data.frame(simsal), aes(x = simsal)) +
      geom_histogram(bins = 5, fill = "#1DB954", colour = "#121212", alpha = 0.9) +
      scale_x_continuous(labels = function(x) paste0("£", format(round(x), big.mark = ","))) +
      labs(x = "Simulated Salary", y = NULL) +
      dark_theme()
    
  }, res = 180, bg = "transparent")
  
  
  # Plot 2 ------------------------------------------------------------------
  output$entryplot <- renderPlot({
    df <- filtered_tariff()
    
    df3 = df %>%
      group_by(pubukprn, kismode, title, kisaimcode, legal_name, provaddress) %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        totalPerc = rowSums(across(7:last_col())),
        across(7:last_col(), ~ round(. / totalPerc * 100, 1))
      ) %>% 
      pivot_longer(
        cols = c(`A*A*A*A* and above`, `A*A*AC and above`, `A*A*A and above`,
                 `AAA and above`, `ABB and above`, `BBC and above`,
                 `CCC and above`, `Below CCC`),
        names_to = "entry_tariff",
        values_to = "count"
      ) %>%
      mutate(
        entry_tariff = factor(entry_tariff, levels = c(
          "Below CCC", "CCC and above", "BBC and above", "ABB and above",
          "AAA and above", "A*A*A and above", "A*A*AC and above", "A*A*A*A* and above"
        ))
      )
    
    n_levels <- length(unique(df3$entry_tariff[df3$count != 0]))
    green_pal <- colorRampPalette(c("#0D4A25", "#1DB954", "#6BF09C"))(max(n_levels, 3))
    
    df3 %>%
      filter(count != 0) %>%
      ggplot(aes(x = entry_tariff, y = count, fill = entry_tariff)) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = green_pal) +
      scale_x_discrete(labels = wrap_labels) +
      labs(x = NULL, y = "Percentage of Entrants") +
      dark_theme() +
      theme(legend.position = "none")
    
  }, res = 180, bg = "transparent")
  
  
  # Plot 3 ------------------------------------------------------------------
  
  output$jobplot <- renderPlot({
    df <- filtered_joblist()
    
    n_jobs <- length(unique(df$job[df$perc != 0]))
    green_pal <- colorRampPalette(c("#0D4A25", "#1DB954", "#6BF09C"))(max(n_jobs, 3))
    
    df %>%
      filter(perc != 0) %>%
      ggplot(aes(x = reorder(job, perc), y = perc, fill = job)) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = green_pal) +
      scale_x_discrete(labels = wrap_labels) +
      labs(x = NULL, y = "Percentage of Graduates") +
      dark_theme() +
      theme(legend.position = "none")
    
  }, res = 180, bg = "transparent")
  
  
  # Plot 4 ------------------------------------------------------------------
  
  output$nssplot <- renderPlot({
    courseUniLevels <- filtered_nssCL()
    uniLevels <- filtered_nssUL()
    
    set1  <- sapply(1:ncol(courseUniLevels), function(i) courseUniLevels[[i]])
    mean1 <- sapply(1:ncol(uniLevels), function(i) uniLevels[[i]])
    
    df <- tibble(
      category = c("Teaching", "Assessment &\nFeedback", "Academic\nSupport",
                   "Learning\nOpportunities", "Organisation &\nManagement",
                   "Learning\nResources", "Learning\nCommunity", "Overall"),
      setVal  = set1,
      meanVal = mean1
    ) %>%
      mutate(direction = ifelse(setVal >= meanVal, "above", "below"))
    
    df$category <- factor(df$category, levels = rev(df$category))
    
    ggplot(df, aes(y = category)) +
      geom_segment(aes(x = meanVal, xend = setVal, yend = category, color = direction),
                   linewidth = 1.5, show.legend = FALSE) +
      geom_point(aes(x = meanVal), color = "#535353", size = 4) +
      geom_point(aes(x = setVal, color = direction), size = 5, show.legend = FALSE) +
      scale_color_manual(values = c("above" = "#1DB954", "below" = "#E63946")) +
      scale_x_continuous(expand = expansion(mult = c(0.08, 0.08))) +
      labs(x = "Score (100 = Best)", y = NULL) +
      dark_theme() +
      theme(
        panel.grid.major.x = element_line(color = "#222222", linewidth = 0.3),
        legend.position = "none"
      )
    
  }, res = 180, bg = "transparent")
  # Plot5 -------------------------------------------------------------------
  output$tb.overall = renderTable({
    myNSS = filtered_comparisonnss()
    
    myNSS %>% 
      group_by(legal_name) %>%
      summarise(Overall = weighted.mean(overall, nsspop, na.rm = TRUE)) %>%
      arrange(desc(Overall)) %>%
      select(legal_name, Overall) %>%
      rename("University" = legal_name, "Overall Score" = Overall)
  })
  
  output$tb.teaching = renderTable({
    myNSS = filtered_comparisonnss()
    
    myNSS %>% 
      group_by(legal_name) %>%
      summarise(Teaching = weighted.mean(t1, nsspop, na.rm = TRUE)) %>%
      arrange(desc(Teaching)) %>%
      select(legal_name, Teaching) %>%
      rename("University" = legal_name, "Teaching on the course" = Teaching)
  })
  
  output$tb.ass = renderTable({
    myNSS = filtered_comparisonnss()
    
    myNSS %>% 
      group_by(legal_name) %>%
      summarise(Ass = weighted.mean(t2, nsspop, na.rm = TRUE)) %>%
      arrange(desc(Ass)) %>%
      select(legal_name, Ass) %>%
      rename("University" = legal_name, "Assessment and Feedback" = Ass)
  })
  
  output$tb.sup = renderTable({
    myNSS = filtered_comparisonnss()
    
    myNSS %>% 
      group_by(legal_name) %>%
      summarise(Sup = weighted.mean(t3, nsspop, na.rm = TRUE)) %>%
      arrange(desc(Sup)) %>%
      select(legal_name, Sup) %>%
      rename("University" = legal_name, "Academic Support" = Sup)
  })
  
  output$tb.lo = renderTable({
    myNSS = filtered_comparisonnss()
    
    myNSS %>% 
      group_by(legal_name) %>%
      summarise(Lo = weighted.mean(t4, nsspop, na.rm = TRUE)) %>%
      arrange(desc(Lo)) %>%
      select(legal_name, Lo) %>%
      rename("University" = legal_name, "Learning Opportunities" = Lo)
  })
  
  output$tb.mgmt = renderTable({
    myNSS = filtered_comparisonnss()
    
    myNSS %>% 
      group_by(legal_name) %>%
      summarise(MGMT = weighted.mean(t5, nsspop, na.rm = TRUE)) %>%
      arrange(desc(MGMT)) %>%
      select(legal_name, MGMT) %>%
      rename("University" = legal_name, "Organisation and Management" = MGMT)
  })
  
  output$tb.res = renderTable({
    myNSS = filtered_comparisonnss()
    
    myNSS %>% 
      group_by(legal_name) %>%
      summarise(Res = weighted.mean(t6, nsspop, na.rm = TRUE)) %>%
      arrange(desc(Res)) %>%
      select(legal_name, Res) %>%
      rename("University" = legal_name, "Learning Resources" = Res)
  })
  
  output$tb.com = renderTable({
    myNSS = filtered_comparisonnss()
    
    myNSS %>% 
      group_by(legal_name) %>%
      summarise(Com = weighted.mean(t7, nsspop, na.rm = TRUE)) %>%
      arrange(desc(Com)) %>%
      select(legal_name, Com) %>%
      rename("University" = legal_name, "Learning Community" = Com)
  })
  
  output$entrantmap = renderPlot({
    
    myMap = filtered_map()
    
    ggplot() +
      geom_sf(data = myMap, fill = "#2a2a2a", colour = "#444444", size = 0.2) +
      
      geom_sf(
        data = myMap %>% filter(!is.na(AtAndAboveA.A.A)),
        aes(fill = AtAndAboveA.A.A),
        colour = "#444444", size = 0.2
      ) +
      scale_fill_distiller(
        palette = "Greens",
        direction = 1,
        na.value = "#2a2a2a",
        name = "Entrants with A*A*A\nor Above (%)",
        labels = function(x) paste0(x, "%"),
        guide = guide_colorbar(
          barheight = unit(40, "pt"),
          barwidth  = unit(12, "pt"),
          frame.colour = "#666666",
          ticks.colour = "#666666",
          title.position = "top"
        )
      ) +
      theme_map() +
      theme(
        legend.position      = "right",
        legend.margin        = margin(2, 2, 2, 2),
        legend.box.margin    = margin(0, 0, 0, 0),
        legend.background    = element_rect(fill = "#1a1a1a", colour = NA),
        legend.box.background = element_rect(fill = "#1a1a1a", colour = "#444444"),
        legend.key           = element_rect(fill = "#1a1a1a", colour = NA),
        legend.title         = element_text(face = "bold", colour = "white", size = 4),
        legend.text          = element_text(colour = "white", size = 3),
        plot.caption         = element_text(size = 6, colour = "grey40", hjust = 1)
      )
    
  }, res = 180, bg = "transparent")
  
  
  
  
}

# Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)