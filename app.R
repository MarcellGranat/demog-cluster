library(shiny)
library(shinydashboard)
library(tidyverse)

load("cluster_result.RData")

ui <- dashboardPage(
  title = "stat2",
  dashboardHeader(title = "Statisztika II.",
                  tags$li(a(href = 'https://github.com/MarcellGranat/stat2',
                            img(src = 'corvinus_logo_HU_rgb.png',
                                title = "Company Home", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  dashboardSidebar(
    sliderInput("n_clust", "Klaszterek száma:", min = 2, max = 10,
                value = 3, step = 1),
    sidebarMenu(
      menuItem("Mi ez a projekt?", tabName = "what", icon = icon("question")),
      menuItem("Térkép", tabName = "sf", icon = icon("question"))
      
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "what", 
              box(width = 12, title = "Mi ez a projekt",
                  h3("Ide jön majd a rövid leírás, pár ábra a módszertanról") # TODO text
              )
      ),
      tabItem(tabName = "sf",
              sliderInput("sf_time", "Év:", min = 1999, max = 2019, value = 2010, animate = TRUE),
              selectInput("sf_var", "Megjelenít:", choices = c(
                "Klaszter" = "clust",
                "Termékenységi arányszám" = "tfr",
                "Várható élettartam" = "lifeexp",
                "Öregedési index" = "ageing",
                "Szülőkorban lévő nők aránya" = "motherrate",
                "Nők foglalkozatási rátája" = "emp"
              ),
              box(plotOutput("sf_plot"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$sf_plot <- renderPlot({
    cluster_df %>% 
      filter(time == input$sf_time, n_cluster == input$n_clust) %>% 
      left_join(dat) %>% 
      transmute(data = pmap(list(data, geo, fit), 
                            function(x, y, z) mutate(x, geo = y, clust = z$cluster))) %>% 
      pull() %>% 
      first() %>% 
      left_join(eurostat::get_eurostat_geospatial(nuts_level = "2")) %>% 
      mutate(clust = as.factor(clust)) %>% 
      select(geo, geometry, input$sf_var) %>% 
      set_names("geo", "geometry", "x") %>% 
      ggplot() +
      aes(geometry = geometry, fill = x) + 
      geom_sf(color = "black")
  })
  
}

shinyApp(ui, server)

  
