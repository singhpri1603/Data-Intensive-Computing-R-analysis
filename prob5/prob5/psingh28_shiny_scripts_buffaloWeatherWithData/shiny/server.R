
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rjson)
library(ggplot2)
library(ggvis)
library(dplyr)

dframe<-fromJSON("buff_weath.json")
weather<-data.frame(dframe)
weather$date<-as.Date(as.character(weather$date),format="%Y%m%d")

shinyServer(function(input, output, session) {
  
    weath<- reactive({
 #     locations<-c("NORTH TONAWANDA NY US", "FORT ERIE CA","BUFFALO NIAGARA INTERNATIONAL NY US", "NIAGARA FALLS INTERNATIONAL NY US")
      
      ### define reactive filters
      loc<-input$location
      start_date<-input$date[1]
      end_date<-input$date[2]
 #     inf<-infotype[input$info]
      
      ##apply filter
      
    m<-weather %>% filter(station_name== loc,
                       date>=start_date,
                       date<=end_date)
      
    m<-as.data.frame(m)
    
    m
      
    })
    
    vis<-reactive({
      
      ### add label to axes
      xvar_name <-"Date"
      yvar_name <- names(axis_vars)[axis_vars == input$yvar]
     
      ### define axes variables
      xvar<-prop("x", as.symbol("date"))
      yvar <- prop("y", as.symbol(input$yvar))
      
      ### defining plot
      weath %>%
        ggvis(x=xvar, y=yvar) %>%
        layer_lines()%>%
          add_axis("x", title = xvar_name) %>%
          add_axis("y", title = yvar_name) 
      
    })
    
    vis %>% bind_shiny("plot1")
#    ggplot(weather, aes(x=date, y=tmax))+geom_line()
    
    # filtered <- weather %in% filter(station_name==loc)
    # ggplot(weather, aes(x=date, y=inf)) + geom_line()
    
  })
#output$this<- renderText({"You are here now!!!"})
# })
