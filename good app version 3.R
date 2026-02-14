pacman::p_load(tidyverse, RColorBrewer, shiny, bslib)
options(scipen = 999)
tariff = read_csv("tariff.uk.csv")
joblist = read_csv("joblist.uk.csv")
salaries = read_csv("salaries.uk.csv")
nss = read_csv("nss.uk.csv")

# ── Spotify-inspired theme ──────────────────────────────────────────────────
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

# ── Custom CSS ──────────────────────────────────────────────────────────────
custom_css <- tags$style(HTML("
  @import url('https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700&display=swap');

  /* ── Global ── */
  body {
    background: #121212 !important;
    color: #E0E0E0 !important;
    font-family: 'Outfit', sans-serif !important;
    overflow-x: hidden;
  }

  /* ── Filter bar ── */
  .filter-bar {
    background: #181818;
    border: 1px solid rgba(255,255,255,0.06);
    border-radius: 14px;
    padding: 22px 28px;
    margin-bottom: 24px;
    box-shadow: 0 4px 24px rgba(0,0,0,0.4);
    overflow: visible !important;
    z-index: 100;
    position: relative;
  }
  .filter-bar .row,
  .filter-bar .col-sm-6,
  .filter-bar .form-group {
    overflow: visible !important;
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
    font-size: 0.7rem;
    text-transform: uppercase;
    letter-spacing: 1.5px;
    margin-bottom: 12px;
    display: flex;
    align-items: center;
    gap: 6px;
  }

  /* ── Select inputs ── */
  .form-group > label,
  .control-label {
    color: #B3B3B3 !important;
    font-weight: 500;
    font-size: 0.78rem;
    text-transform: uppercase;
    letter-spacing: 1.2px;
    margin-bottom: 6px;
  }
  .form-select, .form-control,
  .selectize-input, .selectize-control.single .selectize-input {
    background: #282828 !important;
    border: 1px solid rgba(255,255,255,0.08) !important;
    border-radius: 10px !important;
    color: #FFFFFF !important;
    padding: 10px 14px !important;
    font-family: 'Outfit', sans-serif !important;
    font-size: 0.95rem !important;
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
    border-radius: 14px !important;
    box-shadow: 0 4px 24px rgba(0,0,0,0.35) !important;
    overflow: hidden;
    transition: transform 0.3s ease, box-shadow 0.3s ease;
  }
  .bslib-card:hover {
    transform: translateY(-3px);
    box-shadow: 0 8px 36px rgba(0,0,0,0.5) !important;
  }
  .card-header, .bslib-card .card-header {
    background: transparent !important;
    border-bottom: 1px solid rgba(255,255,255,0.06) !important;
    color: #FFFFFF !important;
    font-weight: 600 !important;
    font-size: 1rem !important;
    letter-spacing: -0.2px;
    padding: 18px 22px !important;
    display: flex;
    align-items: center;
    gap: 10px;
  }
  .card-header::before {
    content: '';
    width: 4px;
    height: 18px;
    background: #1DB954;
    border-radius: 2px;
    flex-shrink: 0;
  }
  .card-body {
    padding: 16px 22px 22px !important;
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

  /* ── Row spacing ── */
  .plot-row { margin-bottom: 24px; }

  /* ── Responsive ── */
  @media (max-width: 768px) {
    .app-title-bar { padding: 20px; }
    .app-title-bar h1 { font-size: 1.3rem; }
    .filter-bar { padding: 16px; }
  }
"))


# UI ──────────────────────────────────────────────────────────────────────────
ui <- page_fluid(
  theme = spotify_theme,
  custom_css,
  
  # ── Filter Bar ──
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
  
  # ── Plot Row 1 ──
  fluidRow(
    class = "plot-row",
    column(
      6,
      div(
        class = "animate-in d1",
        card(
          card_header("Salary Distribution"),
          card_body(plotOutput("salaryplot", height = "450px"))
        )
      )
    ),
    column(
      6,
      div(
        class = "animate-in d2",
        card(
          card_header("Entry Tariff Breakdown"),
          card_body(plotOutput("entryplot", height = "450px"))
        )
      )
    )
  ),
  
  # ── Plot Row 2 ──
  fluidRow(
    class = "plot-row",
    column(
      6,
      div(
        class = "animate-in d3",
        card(
          card_header("Job Outcomes"),
          card_body(plotOutput("jobplot", height = "450px"))
        )
      )
    ),
    column(
      6,
      div(
        class = "animate-in d4",
        card(
          card_header("NSS Outcomes (Gray represents University Average)"),
          card_body(plotOutput("nssplot", height = "450px"))
        )
      )
    )
  )
)


# Server (unchanged) ──────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  observeEvent(input$universities, {
    
    filtered_degrees <- salaries %>%
      filter(legal_name == input$universities) %>%
      pull(title) %>%
      unique() %>%
      sort()
    
    updateSelectInput(
      session,
      "degrees",
      choices = filtered_degrees,
      selected = filtered_degrees[1]
    )
    
  })
  
  # 🔥 Master filter
  filtered_salaries <- reactive({
    req(input$universities, input$degrees)
    salaries %>%
      filter(
        legal_name == input$universities,
        title == input$degrees
      )
  })
  
  filtered_tariff <- reactive({
    req(input$universities, input$degrees)
    tariff %>%
      filter(
        legal_name == input$universities,
        title == input$degrees
      ) %>%
      distinct(pubukprn, kiscourseid, .keep_all = TRUE)
  })
  
  filtered_joblist <- reactive({
    req(input$universities, input$degrees)
    joblist %>%
      filter(
        legal_name == input$universities,
        title == input$degrees
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
  })
  
  filtered_nssUL = reactive({
    req(input$universities, input$degrees)
    nss  %>%
      filter(legal_name == input$universities) %>%
      summarise(
        meant1 = mean(t1, na.rm=TRUE),
        meant2 = mean(t2, na.rm=TRUE),
        meant3 = mean(t3, na.rm=TRUE),
        meant4 = mean(t4, na.rm=TRUE),
        meant5 = mean(t5, na.rm=TRUE),
        meant6 = mean(t6, na.rm=TRUE),
        meant7 = mean(t7, na.rm=TRUE),
        meanoverall = mean(overall, na.rm=TRUE),
      )
  })
  
  filtered_nssCL = reactive({
    req(input$universities, input$degrees)
    nss %>%
      filter(legal_name == input$universities & 
               title == input$degrees) %>%
      summarise(
        t1,
        t2,
        t3,
        t4,
        t5,
        t6,
        t7,
        overall
      )
  })
  
  output$salaryplot = renderPlot({
    
    df = filtered_salaries()
    
    df2 = df %>% 
      summarise(
        med = weighted.mean(goinstmed, gosalpop, na.rm=TRUE),
        lq = weighted.mean(goinstlq, gosalpop, na.rm=TRUE),
        uq = weighted.mean(goinstuq, gosalpop, na.rm=TRUE),
        pop = sum(gosalpop, na.rm=TRUE)
      ) %>%
      mutate(
        mu = (med),
        sigma = ((uq) - (lq)) / 1.349
      )
    
    simsal = rnorm(
      n=50,
      mean = df2$mu,
      sd = df2$sigma
    )
    
    ggplot(data.frame(simsal), aes(x = simsal)) +
      geom_histogram(
        bins = 5,
        fill = "#1DB954",
        colour = "#121212",
        alpha = 0.9
      ) +
      theme_minimal(base_size = 18) +
      labs(
        x = "Simulated Salary (£)",
        y = NULL
      ) + theme_classic(base_size = 18)  + 
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(colour = "#E0E0E0", family = "sans", size = 7),
        axis.text = element_text(colour = "#B3B3B3", size = 6),
        axis.title = element_text(colour = "#B3B3B3", size = 7, face = "bold"),
        axis.line = element_line(colour = "#333333"),
        plot.margin = margin(12, 16, 12, 12)
      )
    
    
  }, res = 150, bg = "transparent")
  
  output$entryplot = renderPlot({
    df = filtered_tariff()
    
    df3 = df %>%
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
    
    
    # Spotify-inspired green gradient palette
    n_levels <- length(unique(df3$entry_tariff[df3$count != 0]))
    green_pal <- colorRampPalette(c("#0D4A25", "#1DB954", "#6BF09C"))(max(n_levels, 3))
    
    df3 %>%
      filter(count != 0) %>%
      ggplot(aes(x = entry_tariff, y = count, fill = entry_tariff)) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = green_pal) +
      labs(x = NULL, y = "Percentage of Entrants") +
      theme_classic(base_size = 18) +
      theme(legend.position = "none") + 
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(colour = "#E0E0E0", family = "sans", size = 7),
        axis.text = element_text(colour = "#B3B3B3", size = 6),
        axis.title = element_text(colour = "#B3B3B3", size = 7, face = "bold"),
        axis.line = element_line(colour = "#333333"),
        plot.margin = margin(12, 16, 12, 12)
      )
    
    
    
  }, res = 150, bg = "transparent")
  
  output$jobplot = renderPlot({
    
    df = filtered_joblist()
    
    n_jobs <- length(unique(df$job[df$perc != 0]))
    green_pal <- colorRampPalette(c("#0D4A25", "#1DB954", "#6BF09C"))(max(n_jobs, 3))
    
    df %>%
      filter(perc != 0) %>%
      ggplot(aes(x = reorder(job, perc), y = perc, fill=job)) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = green_pal) +
      labs(x = NULL, y = "Percentage of Graduates") +
      theme_classic(base_size = 18) +
      theme(legend.position = "none")  + 
      theme(
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(colour = "#E0E0E0", family = "sans", size = 7),
        axis.text = element_text(colour = "#B3B3B3", size = 6),
        axis.title = element_text(colour = "#B3B3B3", size = 7, face = "bold"),
        axis.line = element_line(colour = "#333333"),
        plot.margin = margin(12, 16, 12, 12)
      )
    
  }, res = 150, bg = "transparent")
  
  output$nssplot = renderPlot({
    
    courseUniLevels = filtered_nssCL()
    uniLevels = filtered_nssUL()
    
    set1 <- sapply(1:ncol(courseUniLevels), function(i) courseUniLevels[[i]])
    mean1 = sapply(1:ncol(uniLevels), function(i) uniLevels[[i]])
    
    df <- tibble(
      category = c(
        "Teaching", 
        "Assessment & Feedback", 
        "Academic Support", 
        "Learning Opportunities", 
        "Organisation & Management", 
        "Learning Resources", 
        "Learning Community", 
        "Overall"
      ),
      setVal = set1,
      meanVal = mean1
    )
    
    # Color segments by above/below mean
    df <- df %>%
      mutate(direction = ifelse(setVal >= meanVal, "above", "below"))
    
    # Set the order you want as factor levels (bottom to top on y-axis)
    df$category <- factor(df$category, levels = rev(c(
      "Teaching", 
      "Assessment & Feedback", 
      "Academic Support", 
      "Learning Opportunities", 
      "Organisation & Management", 
      "Learning Resources", 
      "Learning Community", 
      "Overall"
    )))
    
    ggplot(df, aes(y = category)) +
      
      # Connector segment — colored by direction
      geom_segment(aes(x = meanVal, xend = setVal, yend = category, color = direction),
                   linewidth = 1.5, show.legend = FALSE) +
      
      # Uni mean points
      geom_point(aes(x = meanVal), color = "#535353", size = 4) +
      
      # Course points — green if above, muted red if below
      geom_point(aes(x = setVal, color = direction), size = 5, show.legend = FALSE) +
      
      scale_color_manual(values = c("above" = "#1DB954", "below" = "#E63946")) +
      
      scale_x_continuous(expand = expansion(mult = c(0.08, 0.08))) +
      
      labs(
        x = "% Agree",
        y = NULL
      ) +
      
      theme_classic(base_size = 18) +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.grid.major.x = element_line(color = "#222222", linewidth = 0.3),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(colour = "#E0E0E0", family = "sans", size = 7),
        axis.text.y = element_text(colour = "#B3B3B3", size = 6.5),
        axis.text.x = element_text(colour = "#B3B3B3", size = 6),
        axis.title = element_text(colour = "#B3B3B3", size = 7, face = "bold"),
        axis.line = element_line(colour = "#333333"),
        plot.margin = margin(12, 20, 12, 12)
      )
    
  }, res = 150, bg = "transparent")
}


# Run the application 
shinyApp(ui = ui, server = server)