#R version 4.1.3 is the development env. libraries like nycflights13 and echarts4r.assets might not able to use for other versions of R.
library(nycflights13) 
library(dplyr)
library(shiny)
library(shinythemes)
library(DT)
library(car)
library(tseries)
library(RcmdrMisc)
#install.packages("shinyWidgets")
library(shinyWidgets)
#install.packages("echarts4r")
library(echarts4r)
#remotes::install_github("RinteRface/fullPage")
library(fullPage)
#remotes::install_github("JohnCoene/echarts4r.assets")
library(echarts4r.assets)
#remotes::install_github("JohnCoene/typedjs")
library(typedjs)

#data preprocessing

data <- flights %>%
  mutate(date = as.Date(time_hour)) %>%
  select(date, time_hour, carrier, tailnum, origin, dest,dep_delay,arr_delay) %>%
  na.omit()%>%
  arrange_all()
options <- list(
  loopBottom = TRUE
)
weather_data <- weather %>%
  mutate(date = as.Date(time_hour)) %>%
  select(date, time_hour, origin,temp,dewp,humid,wind_dir,wind_speed,pressure,visib)%>%
  na.omit()%>%
  arrange_all()

airports_data <- airports %>%
  select(faa,lat,lon)
avg <- list(
  type = "average",
  name = "average: "
)
options(dplyr.summarise.inform = FALSE)
##################
# USER INTERFACE #
##################

ui <- pagePiling(
  ##################
  #Global varibales#
  ##################
  sections.color = c('#f9f7f1','#f3f9fe','#eefff8'),
  opts = options,
  menu = c("Home" = "section0",
           "Delay" = "section1",
           "Exploration"= "section2",
           "Rawdata" = "section3"),
  #inline CSS
  tags$style(
    type="text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
    ,HTML("
    * {
        font-family: 'Playfair Display', serif;
    }  
    h1{
        font-size: 38px;
        text-align: center;
    }
    h2{
      font-size:30px;
      text-aligh:center;
    }
    p{
        text-align: left;
    }
   .shiny-input-container {
        color: #474747;
      }")),
  ##################
  #   Home Page    #
  ##################
  pageSection(
    h1(typedOutput("title"), class = "header shadow-dark"),
    tags$hr(),
    h3("Flights departing New York City in 2013."),
    echarts4rOutput('map'),
    menu = "section0"
  ),
  
  ##################
  # delays page    #
  ##################
  pageSection(
    tags$h1('Avg. departure & arrival delays'),
 #   h4("Chart shows flight depart from NYC, data update once per week"),
     echarts4rOutput('delay'),
     echarts4rOutput('arrival'),
    menu = "section1"
  ),
  
  ##################
  #detail page:    #
  ##################
  pageSection(
   
    tags$h1('Potential factor of flight delays'),
    fluidRow(
      #column(12, align="center", selectizeInput("ca","Select carrier:",choices=(c(unique(data$origin))),multiple = TRUE,selected=c("JFK"))),
      #column(2,h2('Showing data from in:')),
      column(10, echarts4rOutput('delay2')),
      column(2, wellPanel("Select date range of the line graph:", dateInput('date',inputId = "start",label = 'Start Date (after 2013-01-01):',value = as.Date("2013-01-01")),
                          dateInput('date',inputId = "end",label = 'End Date (before 2013-12-31):',value = as.Date("2013-01-31"))))
      
    ),
    
    tags$h3(textOutput('filter_title')),
    fluidRow(
     column(3, selectizeInput("wvar","Select weather variable to plot:",
                                             choices=(c("Temperature (in F)","Dewpoint (in F)",
                                                        "Wind direction (in degrees)",
                                                        "Wind speed (in mph)",
                                                        "Precipitation (in inches)",
                                                        "Sea level pressure (in millibars)",
                                                        "Visibility (in miles)")),selected="Visibility (in miles)")),
     
    ),
    menu = "section2",
    #DTOutput('dt')
    column(4,echarts4rOutput('w')),
    column(4,echarts4rOutput('chart2')),
    column(4,echarts4rOutput('chart'))
  ),
   ##################
   #     Raw data   #
   ##################
  pageSection(
    tags$h1('Raw Data'),
    #selectizeInput("table","Select table to see details:",choices=(c("airlines","airports","flights",
    #                                               "planes","weather")),selected="airlines") ,
    DTOutput('RawData'),
    tags$p('Airline on-time data for all flights departing NYC in 2013.'),
    tags$p(
      'Data is from the nycflights13 dataset, available at ',
      tags$a(
        'https://CRAN.R-project.org/package=nycflights13',
        href = 'https://CRAN.R-project.org/package=nycflights13'
      )
    ),
    menu = "section3"
  )
)

################
# SHINY SERVER #
################

server <- function(input, output){
  
  output$title <- renderTyped({
    typed(c( "Exploration on flight delays"), typeSpeed = 25)
  })
  
  filteredData<-reactive({
    if(as.Date(input$start) > as.Date(input$end)){
      show_alert("Start date should not beyond end date, please select again.")
    }
    data     %>%filter(date <= as.Date(input$end) & date >= as.Date(input$start))
  })
  choose_ca <-reactive({
    weather_data %>%
      select(date, time_hour, input$weatherattr) %>%
        filter(date <= as.Date(input$end) & date >= as.Date(input$start)) 
  })
  
  # Track the filter date in a reactive value
  filter_date <- reactiveVal(value = NULL)
  
  # Update the filter date on click on data point
  observeEvent(input$delay2_clicked_data, {
    filter_date(input$delay2_clicked_data$value[1])
  })
  
  # Reset the filter date
  observeEvent(input$reset_filter, {
    filter_date(NULL)
  })
  
  # Display the filter date
  output$filter_title <- renderText({
    if (is.null(filter_date())) return('Click on any data point in the line chart to show detail for that day')
    paste('Showing detail on', filter_date())
  })
  
  # Create an interactive chart
  output$map <- renderEcharts4r({
    flight <- filteredData() %>% group_by(origin,dest) %>% summarise(dep_delay=mean(dep_delay),arr_delay=mean(arr_delay))
    dep<-merge(flight,airports_data,by.x="origin",by.y="faa",no.dups = TRUE)
    all<-merge(dep,airports_data,by.x="dest",by.y="faa",no.dups = TRUE)
    all %>% 
      e_charts() %>% 
      e_globe(
        environment = ea_asset("starfield"),
        base_texture = ea_asset("world topo"), 
        height_texture = ea_asset("world topo"),
        displacementScale = 0.05
      ) %>% 
      e_lines_3d(
        lon.x, lat.x, lon.y, lat.y,
        name = "flights",
        effect = list(show = TRUE)
      ) %>% 
      e_legend(FALSE)
  })
  
  
  # origin/count of flight
  output$chart <- renderEcharts4r({
    if (!is.null(filter_date())){
      data %>%
        filter(date == filter_date())%>%
        group_by(origin)%>%
        count(date)%>% 
        e_charts(origin) %>%
        e_bar(n) %>%
        e_tooltip(formatter = htmlwidgets::JS("
          function(params){
            return('<b>'+params.value[1] + '</b> flights departs at <b>' + params.value[0] +'</b>')
                    }
        ")) %>%
        e_axis_labels(x="Airport",y="Depart flights count")   %>%
        e_legend(show = FALSE)%>%
        e_color(c("#247BA0", "#FF1654", "#70C1B3", "#2f2f2f", "#F3FFBD", "#B2DBBF"))

      }

  })
  # origin/dep,arr delay
  output$chart2 <- renderEcharts4r({
    if (!is.null(filter_date())){
       data %>% 
          filter(date == filter_date())%>%
          group_by(origin) %>% 
          summarise(dep_delay=mean(dep_delay),arr_delay=mean(arr_delay))%>% 
          e_charts(origin) %>% 
          e_bar(dep_delay)%>% 
          e_bar(arr_delay)%>% 
          e_axis_labels(y = 'Delay (minutes)',x="Airport")   %>%
          e_color(c("#247BA0", "#FF1654", "#70C1B3", "#2f2f2f", "#F3FFBD", "#B2DBBF"))  %>%
          e_tooltip(formatter = htmlwidgets::JS("
          function(params){
            return('Total delay of <b> '+ params.value[0]+' </b> is: <b>' + params.value[1] +'</b> minutes.' )
                    }
        ")) 
    }
    
  })
  # Create a table with detailed information

  output$delay2 <- renderEcharts4r({
  
  filteredData() %>%
    group_by(origin,date) %>%
    summarise_at(c("dep_delay"), mean,na.rm=TRUE) %>%
    e_charts(date) %>%
    e_line(dep_delay) %>%
    e_datazoom(
      type = "slider", 
      toolbox = FALSE
    ) %>% 
    e_mark_line(data = avg) %>%
    e_color(c("#247BA0", "#FF1654", "#70C1B3", "#2f2f2f", "#F3FFBD", "#B2DBBF")) %>%
    e_axis_labels( x = 'Date') %>%
    e_tooltip(axisPointer = list(type = 'cross')) %>%
    e_title("Departure delays in minutes","Negative times represent early depatrues")
  
})

  output$w<- renderEcharts4r({
    if (!is.null(filter_date())){
      whe <- weather_data  %>%filter(date == filter_date())%>% group_by(time_hour,origin) %>%
        summarise(temp=mean(temp),dewp=mean(dewp),precip=mean(precip),humid=mean(humid),
                  wind_dir=mean(wind_dir),wind_speed=mean(wind_speed),
                  pressure=mean(pressure),visib=mean(visib))
      avg_dep<-data %>% filter(date == filter_date())
      whedep<-merge(avg_dep,whe,by=c("time_hour","origin"),sort = TRUE)
      
      if(input$wvar ==  "Temperature (in F)"){index<-grep("temp", colnames(whedep))}
      if(input$wvar ==  "Dewpoint (in F)"){index<-grep("dewp", colnames(whedep))}
      if(input$wvar ==  "Wind direction (in degrees)"){index<-grep("wind_dir", colnames(whedep))}
      if(input$wvar ==  "Wind speed (in mph)"){index<-grep("wind_speed", colnames(whedep))}
      if(input$wvar ==  "Precipitation (in inches)"){index<-grep("precip", colnames(whedep))}
      if(input$wvar ==  "Sea level pressure (in millibars)"){index<-grep("pressure", colnames(whedep))}
      if(input$wvar ==  "Visibility (in miles)"){index<-grep("visib", colnames(whedep))}
     
      names(whedep)[index]<-"weather_information"
      whedep%>%
        e_chart(origin)|>
        e_bar(weather_information)|>
        e_color(c("#247BA0", "#FF1654", "#70C1B3", "#2f2f2f", "#F3FFBD", "#B2DBBF")) %>%
        e_legend(show = FALSE)%>%
        #e_labels()%>%
        e_tooltip()%>%
        e_axis_labels(y = input$wvar)
    }
  })
  output$delay <- renderEcharts4r({
   
     flights %>% 
      transmute(week = as.Date(cut(time_hour, "week")), dep_delay, origin) %>% 
      group_by( week) %>% 
      summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
      e_charts(x = week) %>% 
      e_datazoom(
        type = "slider", 
        toolbox = FALSE
        
      ) %>% 
      e_line(dep_delay)%>% 
      e_tooltip(axisPointer = list( type = "cross"),formatter = htmlwidgets::JS("
          function(params){
            return('Average departure delay on ' + params.value[0] + ' is <b> ' + parseFloat(params.value[1]).toFixed(2) +'<b/> minutes')
                    }
        ")) %>% 
      
      e_color(c("#247BA0")) %>%
      e_title("Departure delays(minutes) by airport","Chart shows flight depart from airports in NYC, data update once per week") %>% 
      e_x_axis(week, axisPointer = list(show = TRUE))%>% e_toolbox_feature(feature = "saveAsImage")
  })
  
  
  output$arrival <- renderEcharts4r({
    flights_ts <- flights %>% 
      transmute(week = as.Date(cut(time_hour, "week")), arr_delay, origin) %>% 
      group_by( week) %>% 
      summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>% 
      e_charts(x = week) %>% 
      e_datazoom(
        type = "slider", toolbox = FALSE
      ) %>% 
      e_tooltip(axisPointer = list( type = "cross"),formatter = htmlwidgets::JS("
          function(params){
            return('Average arrival delay on ' + params.value[0] + ' is <b> ' + parseFloat(params.value[1]).toFixed(2) +'<b/> minutes')
                    }
        ")) %>% 
      e_color(c("#FF1654")) %>%
      e_title("Arrival delays(minutes) by airport","Negative times represent early arrivals") %>% 
      e_x_axis(week, axisPointer = list(show = TRUE))%>% 
      e_show_loading() %>%
      e_line(arr_delay)
    flights_ts |> e_toolbox_feature(feature = "saveAsImage")
  })
  
  
  output$RawData <- DT::renderDataTable(
    DT::datatable({
      data
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'powderblue', 'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c(names(data))
    ))
}

shinyApp(ui, server)

