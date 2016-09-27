
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Election Tweets Analysis"),

  # Sidebar with a slider input for number of bins
#   sidebarLayout(
#     sidebarPanel(
# # #       
#      ),
# 
#     # Show a plot of the generated distribution
#     mainPanel(
# fluidRow(
#      column(4,plotOutput("distPlot1")
#             ),
#     
#      column(4, plotOutput("distPlot2")
#     )
#     ),
# fluidRow(
#   column(4,
#       plotOutput("distPlot3"))
#   ,
#   column(4,    plotOutput("distPlot4"))
# #       
#     )
plotOutput("distPlot")

  )
)
