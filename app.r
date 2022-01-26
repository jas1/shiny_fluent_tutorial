library("shiny") # install.packages("shiny")
# install.packages("remotes")
# remotes::install_github("Appsilon/shiny.react")
# remotes::install_github("Appsilon/shiny.fluent")
library("shiny.fluent") # install.packages("shiny.fluent")
library("dplyr") # install.packages("dplyr")
library("ggplot2") # install.packages("ggplot2")
library("glue") # install.packages("glue")
library("leaflet") # install.packages("leaflet")
library("plotly") # install.packages("plotly")
library("sass") # install.packages("sass")

library("shiny.router") # install.packages("shiny.router")

# definition of columns for the DetailsList
# field name matches the DF names
# name matches the name displayed
# key looks like the "i18n key" 
details_list_columns <- tibble(
  fieldName = c("rep_name", "date", "deal_amount", "client_name", "city", "is_closed"),
  name = c("Sales rep", "Close date", "Amount", "Client", "City", "Is closed?"),
  key = fieldName)

# adding filter component
filters <- tagList(
  DatePicker.shinyInput("fromDate", value = as.Date('2020/01/01'), label = "From date"),
  DatePicker.shinyInput("toDate", value = as.Date('2020/12/31'), label = "To date")
)

ui <- fluentPage(
  filters,
  uiOutput("analysis") # all the UI definition is on the server analysis component
)

server <- function(input, output, session) {
  
  filtered_deals <- reactive({
    req(input$fromDate) # added required filter
    filtered_deals <- fluentSalesDeals %>% filter(
      date >= input$fromDate,
      date <= input$toDate,
      is_closed > 0
    )  
  })
  
  output$analysis <- renderUI({
    items_list <- if(nrow(filtered_deals()) > 0){
      DetailsList(items = filtered_deals(), columns = details_list_columns)
    } else {
      p("No matching transactions.")
    }
    
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", "Sales deals details", block = TRUE), # title
      div(style="max-height: 500px; overflow: auto", items_list) # container with list or no results
    )
  })
}

shinyApp(ui, server)