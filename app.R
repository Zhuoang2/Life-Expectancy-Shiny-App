#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(markdown)
source("R/filter_data.R")

gap_data = read.csv("data/gapminder.csv", stringsAsFactors = FALSE) %>% 
  select(continent, country, everything())

rmarkdown::render("about.Rmd", output_format = "md_document", clean = TRUE)

continents = unique(gap_data$continent)

ui = navbarPage("Life Expectancy App",
                tabPanel("Analysis",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("continent", "Select Continent:",
                                        choices = continents, selected = "Asia"),
                            selectInput("country", "Select Country:",
                                        choices = NULL),
                            sliderInput("yearRange", "Select Year Range:", 
                                        min = min(gap_data$year), max = max(gap_data$year),
                                        value = c(1952, 2007), step = 5)
                          ),
                          
                          mainPanel(
                            fluidRow(
                              plotOutput("lifeExpPlot"),
                              tableOutput("dataTable")
                            )
                          )
                        )
               ),
               
               tabPanel("About",
                        includeMarkdown("about.md")
               )
)

server = function(input, output, session) {
  observeEvent(input$continent, {
    new_choices = gap_data %>%
      filter(continent == input$continent) %>%
      distinct(country) %>%
      pull(country) %>%
      sort()
    
    updateSelectInput(session, "country", choices = new_choices, selected = new_choices[1])
  })
  
  filtered_data = reactive({
    req(input$continent, input$country, input$yearRange)
    filter_gapminder(gap_data, input$continent, input$country, input$yearRange)
  })
  
  output$lifeExpPlot = renderPlot({
    data_to_plot = filtered_data()
    ggplot(data_to_plot, aes(x = year, y = lifeExp)) +
      geom_line(color = "#2c3e50", size = 2) +
      geom_point(color = "#e74c3c", size = 5) +
      labs(title = paste("Life Expectancy in", input$continent, "-", input$country),
           x = "Year", 
           y = "Life Expectancy",
           caption = "Data Sourse: Gapminder") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 20),
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14),
            plot.caption = element_text(size = 10, color = "gray50"))
  })
  
  output$dataTable = renderTable({
    filtered_data() 
  })
}

shinyApp(ui = ui, server = server)

