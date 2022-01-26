# tutorial reference ------------------------------------------------------
# part 1: https://appsilon.github.io/shiny.fluent/articles/shiny-fluent.html
# part 2: https://appsilon.com/shiny-fluent-tutorial/

# libraries ----------------------------------------------------------------
# install.packages("remotes")
# remotes::install_github("Appsilon/shiny.react") # dependency
library("shiny") # install.packages("shiny")
library("shiny.fluent") # remotes::install_github("Appsilon/shiny.fluent")
library("dplyr") # install.packages("dplyr")
library("ggplot2") # install.packages("ggplot2")
library("glue") # install.packages("glue")
library("leaflet") # install.packages("leaflet")
library("plotly") # install.packages("plotly")
library("sass") # install.packages("sass")
library("shiny.router") # install.packages("shiny.router")


#  globals definitions ----------------------------------------------------

# definition of columns for the DetailsList
# field name matches the DF names
# name matches the name displayed
# key looks like the "i18n key" 
details_list_columns <- tibble(
  fieldName = c("rep_name", "date", "deal_amount", "client_name", "city", "is_closed"),
  name = c("Sales rep", "Close date", "Amount", "Client", "City", "Is closed?"),
  key = fieldName)


# functions definitions ---------------------------------------------------

# 
#' makeCard: title + content function
#'
#' @param title no default, text title
#' @param content no default, content ui elements
#' @param size default:12 , size of the container
#' @param style default: "" style css
#'
#' @return a ui element
#' @export
#'
#' @examples
makeCard <- function(title, content, size = 12, style = "") {
  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}

#' makePage for creating a shiny dashboard page.
#'
#' @param title no default, title of page . 
#' @param subtitle no default, subtitle of page
#' @param contents  no default, ui contents of page
#'
#' @return
#' @export
#'
#' @examples
makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}

#' layout: for transformin into a dashboard.
#' warning: there are 3 globals invocations that are characters: header, navigation, footer
#' they define the sections elements.
#' 
#' @param mainUI : the main UI of the app
#'
#' @return 
#' @export
#'
#' @examples
layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      div(class = "footer", footer)
  )
}


# section definition ------------------------------------------------------


# ui: header definition ---------------------------------------------------

header <- tagList(
  img(src = "logo.svg", class = "logo"),
  div(Text(variant = "xLarge", "Sales Reps Analysis"), class = "title"),
  CommandBar(
    items = list(
      CommandBarItem("New", "Add", subitems = list(
        CommandBarItem("Email message", "Mail", key = "emailMessage", href = "mailto:me@example.com"),
        CommandBarItem("Calendar event", "Calendar", key = "calendarEvent")
      )),
      CommandBarItem("Upload sales plan", "Upload"),
      CommandBarItem("Share analysis", "Share"),
      CommandBarItem("Download report", "Download")
    ),
    farItems = list(
      CommandBarItem("Grid view", "Tiles", iconOnly = TRUE),
      CommandBarItem("Info", "Info", iconOnly = TRUE)
    ),
    style = list(width = "100%")))

# ui: navigation definition ---------------------------------------------------

navigation <- Nav(
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'Analysis', url = '#!/other', key = 'analysis', icon = 'AnalyticsReport'),
      list(name = 'shiny.fluent', url = 'http://github.com/Appsilon/shiny.fluent', key = 'repo', icon = 'GitGraph',target="_blank"),
      list(name = 'shiny.react', url = 'http://github.com/Appsilon/shiny.react', key = 'shinyreact', icon = 'GitGraph',target="_blank"),
      list(name = 'Appsilon', url = 'http://appsilon.com', key = 'appsilon', icon = 'WebAppBuilderFragment',target="_blank")
    ))
  ),
  initialSelectedKey = 'home',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)

# ui: footer definition ---------------------------------------------------

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", "Built with ❤ by Appsilon", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "If you'd like to learn more, reach out to us at hello@appsilon.com"),
  Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
)


## filter definition -------------------------------------------------------
# adding  more filter components
filters <- Stack(
  tokens = list(childrenGap = 10),
  Stack(
    horizontal = TRUE,
    tokens = list(childrenGap = 10),
    DatePicker.shinyInput("fromDate", value = as.Date('2020/01/01'), label = "From date"),
    DatePicker.shinyInput("toDate", value = as.Date('2020/12/31'), label = "To date")
  ),
  Label("Filter by sales reps", className = "my_class"),
  NormalPeoplePicker.shinyInput(
    "selectedPeople",
    class = "my_class",
    options = fluentPeople,
    pickerSuggestionsProps = list(
      suggestionsHeaderText = 'Matching people',
      mostRecentlyUsedHeaderText = 'Sales reps',
      noResultsFoundText = 'No results found',
      showRemoveButtons = TRUE
    )
  ),
  Slider.shinyInput("slider",
                    value = 0, min = 0, max = 1000000, step = 100000,
                    label = "Minimum amount",
                    valueFormat = JS("function(x) { return '$' + x}"),
                    snapToStep = TRUE
  ),
  Toggle.shinyInput("closedOnly", value = TRUE, label = "Include closed deals only?")
)


## analysis page definition ------------------------------------------------

## refactored ui to page definition
analysis_page <- makePage(
  "Sales representatives",
  "Best performing reps",
  div(
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      makeCard("Filters", filters, size = 4, style = "max-height: 320px"),
      makeCard("Deals count", plotlyOutput("plot"), size = 8, style = "max-height: 320px")
    ),
    uiOutput("analysis")
  )
)
# ui definition -----------------------------------------------------------
ui <- fluentPage(
  layout(analysis_page),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css")
  ))


#  server definition ------------------------------------------------------
server <- function(input, output, session) {
  
# filtered data ------------------------------------------------------  
  filtered_deals <- reactive({
    req(input$fromDate) # required date
    selectedPeople <- ( # selected or all
      if (length(input$selectedPeople) > 0) input$selectedPeople
      else fluentPeople$key
    )
    
    minClosedVal <- if (isTRUE(input$closedOnly)) 1 else 0 #
    
    filtered_deals <- fluentSalesDeals %>%
      filter(
        rep_id %in% selectedPeople,
        date >= input$fromDate,
        date <= input$toDate,
        deal_amount >= input$slider,
        is_closed >= minClosedVal
      ) %>%
      mutate(is_closed = ifelse(is_closed == 1, "Yes", "No"))
  })

# output map ------------------------------------------------------    
  output$map <- renderLeaflet({
    points <- cbind(filtered_deals()$LONGITUDE, filtered_deals()$LATITUDE)
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = points)
  })

# output plot ------------------------------------------------------  
  output$plot <- renderPlotly({
    p <- ggplot(filtered_deals(), aes(x = rep_name)) +
      geom_bar(fill = unique(filtered_deals()$color)) +
      ylab("Number of deals") +
      xlab("Sales rep") +
      theme_light()
    ggplotly(p, height = 300)
  })  
  

# output analysis UI ------------------------------------------------------
  output$analysis <- renderUI({
    items_list <- if(nrow(filtered_deals()) > 0){
      DetailsList(items = filtered_deals(), columns = details_list_columns)
    } else {
      p("No matching transactions.")
    }

    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard("Top results", div(style="max-height: 500px; overflow: auto", items_list)), # refactored output
      makeCard("Map", leafletOutput("map")) # map definition
    )
  })
}

shinyApp(ui, server)