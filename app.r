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
ui <- fluentPage(
  Text(variant = "xxLarge", "Hello world!")
)

server <- function(input, output, session) {}

shinyApp(ui, server)