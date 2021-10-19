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
    checkboxInput("sf_eq", "Klaszter megfeleltetés", value = TRUE),
    sidebarMenu(
      menuItem("Mi ez a projekt?", tabName = "what", icon = icon("question")),
      menuItem("Térkép", tabName = "sf", icon = icon("question")),
      menuItem("Átlagok", tabName = "means", icon = icon("question")),
      menuItem("Régiók pozicíója", tabName = "region_position")
      
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
                    "Várható élettartam" = "lifexp",
                    "Öregedési index" = "ageing",
                    "Szülőkorban lévő nők aránya" = "motherrate",
                    "Nők foglalkozatási rátája" = "emp"
                  )
                  )
                ),
                column(
                  width = 4
                )
              ),
              plotOutput("sf_plot", width = "600px", heigh = "600px")
      ),
      tabItem(tabName = "means", plotOutput("mean_plot")),
      tabItem(tabName = "region_position", 
              sliderInput("region_position_gnumber", label = "", min = 1, max = 10, step = 1, value = 1),
              plotOutput("region_position_plot"))
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
                clust = map(fit, ~ tibble(clust = .$cluster))
      )
    
    if (input$sf_eq) {
      eq <- eq_clusters() %>% 
        filter(n_cluster == input$n_clust) %>% 
        select(-n_cluster) %>% 
        group_by(time) %>% 
        nest() %>% 
        ungroup() %>% 
        transmute(time, eq = data)
      
      out <- out %>% 
        left_join(eq) %>% 
        mutate(
          clust = map2(clust, eq, left_join),
          clust = map(clust, select, clust = eq_clust)
        )
      
    }
    
    out <- out %>% 
      mutate(
        data = map2(data, clust, bind_cols),
        data = map(data, ~ mutate(., clust = factor(clust))),
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
    dat_filtered() %>% 
      select(time, data) %>% 
      unnest() %>% 
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
  
  output$region_position_plot <- renderPlot({
    dat_filtered() %>% 
      transmute(time, data) %>% 
      unnest() %>% 
      select(time, geo, clust) %>% 
      pivot_wider(names_from = time, values_from = clust) %>% 
      arrange(geo) %>% 
      filter(cut(row_number(), 10, F) == input$region_position_gnumber) %>% 
      pivot_longer(-1, names_to = "time", values_to = "clust") %>% 
      ggplot(aes(time, geo, fill = clust)) +
      geom_tile(color = "black")
  })
  
  eq_clusters <- reactive({
    
    best_clusters <- cluster_df %>%
      mutate(
        r_squared = map_dbl(fit, ~ .$betweenss / .$totss)
      ) %>%
      group_by(n_cluster) %>%
      slice_max(r_squared) %>%
      ungroup() %>%
      left_join(dat) %>%
      transmute(time, n_cluster, data = map2(scaled_data, fit, ~ data.frame(.x, clust = .y$cluster))) %>%
      unnest(data) %>%
      group_by(time, n_cluster, clust) %>%
      summarise_all(mean) %>%
      ungroup() %>%
      pivot_longer(tfr:emp, values_to = "best_value")
    
    fin_eq <- function(x) {
      out <- tibble()
      res <- x
      
      for (i in unique(x$clust)) {
        out <- res %>% 
          filter(clust == i) %>% 
          head(1) %>% 
          bind_rows(out)
        
        res <- res %>% 
          filter(
            !(clust %in% out$clust) &
              !(eq_clust %in% out$eq_clust)
          )
      }
      
      out
    }
    
    cluster_df %>%
      left_join(dat) %>%
      transmute(time, n_cluster, data = map2(scaled_data, fit, ~ data.frame(.x, current_clust = .y$cluster))) %>%
      unnest(data) %>%
      group_by(time, n_cluster, current_clust) %>%
      summarise_all(mean) %>%
      ungroup() %>%
      pivot_longer(tfr:emp) %>%
      left_join(best_clusters, by = c("n_cluster", "name")) %>%
      group_by(time.x, n_cluster, current_clust, clust) %>%
      group_modify(~ tibble(diff = sum((.$value-.$best_value)^2))) %>%
      ungroup() %>%
      arrange(diff) %>%
      transmute(time = time.x, n_cluster, eq_clust = clust, clust = current_clust) %>% 
      group_by(time, n_cluster) %>% 
      group_modify(~ fin_eq(.)) %>% 
      ungroup()
  })
  
}


shinyApp(ui, server)


