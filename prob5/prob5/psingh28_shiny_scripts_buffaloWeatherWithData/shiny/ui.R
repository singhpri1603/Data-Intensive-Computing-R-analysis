
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
shinyUI(fluidPage(

  # Application title
  titlePanel("Buffalo Weather Data"),
### input widgets
  sidebarLayout(
    sidebarPanel(
      ### date range input
      dateRangeInput("date", 
                     label = h3("Date"), 
                     start = "2015-01-01", 
                     end = "2016-02-29", 
                     min = "2015-01-01", 
                     max = "2016-02-29", 
                     format = "yyyy-mm-dd", 
                     startview = "month", 
                     weekstart = 0, 
                     language = "en", 
                     separator = " to ", 
                     width = NULL),
      ### radio button input for selecting location
      radioButtons("location", label = h3("Locations"),
                   choices = list("Tonawanda" = "NORTH TONAWANDA NY US", "Erie Canal" = "FORT ERIE CA", "Niagara International" = "BUFFALO NIAGARA INTERNATIONAL NY US", "Niagara Falls"="NIAGARA FALLS INTERNATIONAL NY US"), 
                   selected ="NORTH TONAWANDA NY US" ),
      ### drop down to select type of information needed
      selectInput("yvar", label = h3("Type of Information"), axis_vars, selected = "tmin")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      
      ggvisOutput("plot1"),
      wellPanel(h4("Units of Measurement:"),
                h5("Temperature = tenths of degrees C"),
                h5("Precipitation = tenths of mm"),
                h5("Snowfall = mm"),
                h5("Average wind speed = tenths of meters per second"))
    
    )
  )
))
