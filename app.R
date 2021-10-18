library(shiny)
library(shinydashboard)
library(tidyverse)

load("cluster_result.RData")

ui <- dashboardPage(
  title = "Demográfiai klaszterek",
  dashboardHeader(title = "Demográfia klaszterek",
                  tags$li(a(href = 'https://github.com/MarcellGranat/demog-cluster',
                            img(src = 'https://pngimg.com/uploads/github/github_PNG40.png',
                                title = "Company Home", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  dashboardSidebar(
    sliderInput("n_clust", "Klaszterek száma:", min = 2, max = 10,
                value = 3, step = 1),
    sidebarMenu(
      menuItem("Mi ez a projekt?", tabName = "what", icon = icon("question")),
      menuItem("Térkép", tabName = "sf", icon = icon("question")),
      menuItem("Átlagok", tabName = "means", icon = icon("question"))
      
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "what", 
              box(width = 12, title = "Mi ez a projekt",
                  h3("Ide jön majd a rövid leírás, pár ábra a módszertanról") # TODO text
              )
      ),
      tabItem(tabName = "sf",
              fluidRow(
                column(
                  
              sliderInput("sf_time", "Év:", min = 1999, max = 2019, value = 2010, 
                          sep = "", animate = TRUE),
                width = 4),
              column(
                width = 4,
              selectInput("sf_var", "Megjelenít:", choices = c(
                "Klaszter" = "clust",
                "Termékenységi arányszám" = "tfr",
                "Várható élettartam" = "lifeexp",
                "Öregedési index" = "ageing",
                "Szülőkorban lévő nők aránya" = "motherrate",
                "Nők foglalkozatási rátája" = "emp"
              )
              )
              )
              ),
              plotOutput("sf_plot", width = "600px", heigh = "600px")
      ),
      tabItem(tabName = "means", plotOutput("mean_plot"))
    )
  )
)

server <- function(input, output, session) {
  
  dat_filtered <- reactive({
    out <- cluster_df %>%
      filter(n_cluster == input$n_clust) %>%
      left_join(dat) %>%
      transmute(time,
                geo = map(geo, ~ tibble(geo = .)),
                data = map2(data, geo, bind_cols),
                data = map2(data, fit, ~ mutate(.x, clust = as.factor(.y$cluster))),
                gg = map(data, ~ select(., "geo", input$sf_var)),
                gg = map(gg, set_names, "geo", "var"),
                gg = map(gg, full_join, eurostat::get_eurostat_geospatial(nuts_level = "2")),
                gg = map(gg, function(x) {
                  ggplot(x, aes(geometry = geometry, fill = var)) +
                    geom_sf(color = "black") + 
                    scale_x_continuous(limits = c(-10, 40)) + 
                    scale_y_continuous(limits = c(30, 75)) +
                    theme_minimal() +
                    labs(fill = NULL)
                })
      )
    
    if (input$sf_var != "clust") {
      gg_limits <- 
        dat %>% 
        pull(data) %>% 
        bind_rows() %>% 
        pull(input$sf_var) %>% 
        na.omit() %>% 
        {c(min(.), max(.))}
      
      out <- out %>% 
        mutate(
          gg = map(gg, ~ . + scale_fill_viridis_c(limits = gg_limits))
        )
    }
    
    out
    
  })
  
  output$sf_plot <- renderPlot({
    dat_filtered() %>% 
      filter(time == input$sf_time) %>% 
      pull(gg) %>% 
      first()
  })
  
  output$mean_plot <- renderPlot({
    cluster_df %>% 
      filter(n_cluster == input$n_clust) %>% 
      left_join(dat) %>% 
      transmute(time, 
                data = map2(data, geo, ~ mutate(.x, geo = .y)),
                data = map2(data, fit, ~ mutate(.x, clust = .y$cluster))
      ) %>% 
      unnest(data) %>% 
      group_by(time, clust) %>% 
      {
        full_join(
          summarise_if(., is.numeric, mean, na.rm = T), count(.)
        )
      } %>% 
      ungroup() %>% 
      pivot_longer(-c(time, clust)) %>% 
      mutate(clust = as.factor(clust)) %>% 
      ggplot(aes(time, value, color = clust, fill = clust)) + 
      geom_line() +
      geom_point(shape = 21, color = "black") +
      facet_wrap(~ name, scales = "free_y")
  })

}



shinyApp(ui, server)


