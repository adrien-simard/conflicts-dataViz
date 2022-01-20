
# ===== LIBRARIES========
library(ggplot2)
library(shiny)
library(dplyr)
library(readr)
library(shinythemes)
library(rlist)
library(plotly)
library(shinyWidgets)
library(scales)
library(DT)
library(stringr)
# ======================

#====data Processing====

# specifying the path
# C:/Users/adrie/OneDrive/Documents/App-1/
path <- 'C:/Users/adrie/OneDrive/Documents/App-1/data.csv'
# reading contents of csv file
data <- read.csv(path,sep=",")
#delete the blank in the column ISO3
data$ISO3=str_replace(data$ISO3," ","")
#number of Victims preprocess 
distinct_df = data %>% distinct(region)
dfreg = data %>% group_by(year,region) %>% summarise(Victims = sum(best))

dfregion = data %>% group_by(year,country) %>% summarise(Victims = sum(best))
colnames(dfregion) <- c('year','region','Victims')
dfregion= bind_rows(dfreg,dfregion)
distinct_countries = dfregion %>% distinct(region)
region <- c("Asia","Americas","Europe" ,"Middle East","Africa")

#number of conflict
dfConfliReg = data %>% group_by(year,region) %>% summarise(conflicts = n_distinct(conflict.name))
dfConfliCountry = data %>% group_by(year,country) %>% summarise(conflicts = n_distinct(conflict.name))
colnames(dfConfliCountry) <- c('year','region','conflicts')
dfconfli = bind_rows(dfConfliReg,dfConfliCountry)


dfW = data %>% group_by(year) %>% summarise(conflicts = n_distinct(conflict.name),Victims = sum(best))

#type of violence
dfViolenceReg = data %>% group_by(year,region,type.of.violence) %>% summarise(Victims = sum(best),conflicts = n_distinct(conflict.name))
dfViolenceCountry = data %>% group_by(year,country,type.of.violence) %>% summarise(Victims = sum(best),conflicts = n_distinct(conflict.name))
colnames(dfViolenceCountry) <- c('year','region','type.of.violence','Victims','conflicts')
dfviolence = bind_rows(dfViolenceReg,dfViolenceCountry)

# data for the table
dt_u = data %>% group_by(conflict.name,side_a,side_b,region)%>% summarise(Start.date = min(year),Active = max(active_year),Victims = sum(best),duration=max(year)- min(year))
colnames(dt_u) <- c("Conflict","Side A","Side B" ,"Region","Start Date","Active","Victims","Duration in Year")

#Dataset with conflict and Victims for the map
dfmap = data %>% group_by(year,country,ISO3) %>% summarise(Victims = sum(best), conflicts = n_distinct(conflict.name))


# ======================

# Shiny App with4 pages 
shinyApp(
  shinyUI(
    navbarPage("Violence around the world",
               tabPanel("Presentation", uiOutput('page1')),
               tabPanel("Region plots", uiOutput('page2')),
               tabPanel("Conflicts and victims Maps", uiOutput('page3')),
               tabPanel("Conflicts data table", uiOutput('page4'))
    )
  ),
  shinyServer(function(input, output, session) {
    
    #======collect variables ============
    # Years
    yearValues <- reactive({
      years = input$years
      first = years[1]
      second = years[2]
      c(first:second)
      
    })
    # years2
    yearValues2 <- reactive({
      years2 = input$years2
      first = years2[1]
      second = years2[2]
      c(first:second)
      
    })
    # region
    regionValues <- reactive({
      region = input$region
      first = region[1]
      second = region[2]
      three = region[3]
      fourth = region[4]
      five = region[5]
      c(first)
      print(region)
      })
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(dt_u, con)
      }
    )
    #==================
    
    #========== Page4: Conflicts data table ================
    output$page4 <- renderUI({
      fluidPage(
        fluidRow(
          column(11,
                h3("Search a conflict"),
                p("Here you can search for a particular conflict and also export the data. ")
                ,
                # Button
                downloadButton("downloadData", "Download")
          )
        ),
        fluidRow(
          column(11,
                 DT::dataTableOutput("mytable")
                 )
        )
      )
    })
    #========== Page3: Conflicts and victims Maps ================
    output$page3 <- renderUI({
      fluidPage(
        fluidRow(
          column(10,
                 # Year Slider
                 sliderInput("years2", " Years: ",
                             min = 1989,max = 2022,
                             value = c(1989,2022),
                             sep = ""
                 )
          ),
          column(12,
                 plotlyOutput('map1',width="100%")
          ),
          column(12,
                 plotlyOutput('map2',width="100%")
          )
        )
        
      )
      
      
    })
    #========== Page2: Region plots ================
    output$page2 <- renderUI({
      fluidPage(
        #title and description
        fluidRow(
        #Row with Two Bar charts            
        fluidRow(
          column(3,
                 sidebarPanel(width=12,
                              # Year Slider
                              sliderInput("years", " Years: ",
                                          min = 1989,max = 2022,
                                          value = c(1989,2022),
                                          sep = "",
                              ),
                              #region or continent selection
                              multiInput(
                                inputId = "region",
                                label = " Search and select different region:", 
                                choices = unique(distinct_countries$region),
                                selected = unique(distinct_df$region),
                                choiceNames = unique(distinct_countries$region),
                                choiceValues = unique(distinct_countries$region)
                              )
                 )
          ),
          column(9,
                 tabsetPanel(
                   tabPanel("Total victims and conflicts",
                            column(6,plotlyOutput('barchart1')),
                            column(6,plotlyOutput('barchart2')),
                            br("  "),
                            tags$ul(
                              tags$li(strong("Non-state conflict"),": use of armed force between organized groups, neither of which is the government of a state"), 
                              tags$li(strong("State based conflict"),": use of armed force between government"), 
                              tags$li(strong("One sided conflict"),": The deliberate use of armed force by the government of a state or by a formally organised group against civilians ")
                            )
                            ),
                   
                   tabPanel("Temporal Evolution",
                            column(12,plotlyOutput('linechart1')),
                            column(12,plotlyOutput('linechart2'))),
                   tabPanel("Type of conflicts", plotlyOutput("piechart1") )
                 )
                 )
        )
        )
      )
    })
    #========== Page2: Presentation ================
    output$page1 <- renderUI({
      fluidPage(
        h3("Presentation"),
        fluidRow(
          p("This report is a data Visualisation of the UCDP Georeferenced Event Dataset (UCDP GED), a
project within the Uppsala Conflict Data Program (UCDP), at the Department of Peace
and Conflict Research, Uppsala University.",
            br("The aim of this report is to better understand the different conflicts around the world. Our data covers all conflicts from", strong("1989 to 2020."))),
          p("Several dimensions of the problem will be highlighted. Like the ",strong("evolution over time"),", the ",strong("geographical locations"),", the ",strong("type of violence"),", and some quantitative data like the number of ", strong("victims and conflicts.")),
          p("Our work will allow you to study conflicts in the world on several temporal and geographical scales. To do this you can use the filters on the regions and the sliders on the following pages of the report."),
          h3("Global Situation"),
          fluidRow(
            column(6,
                   plotlyOutput('linechart1W')
            ),
            column(6,
                   plotlyOutput('linechart2W')
            )
          )
        )
      )
    })
    #===============================================
    
    #======== Charts with ggplot and plotly ========
    
    # line chart "Number of Victims"
    output$linechart1 <- renderPlotly({
      p <- ggplot(dfregion %>% filter(year %in% yearValues() & region %in% regionValues() ) ) +
        aes(x = year, y = Victims, colour = region) +
        geom_line(size = 1) +
        theme(plot.title = element_text(size = 12),axis.text=element_text(size=8),legend.title = element_text(size=8),legend.text = element_text(size=8))+
        scale_y_continuous(labels = label_number()) +
        scale_color_hue(direction = 1) +
        labs(x = "Year", y = "Victims", title = "Evolution of the number of victims", color = "Regions")
        
      
        
    })
    # WW line chart "Number of conflict 
    output$linechart2W <- renderPlotly({
      p <- ggplot(dfW) +
        aes(x = year, y = conflicts,colour="World") +
        scale_y_continuous(labels = label_number()) +
        geom_line(size = 1) +
        theme(plot.title = element_text(size = 12),axis.text=element_text(size=8),legend.title = element_text(size=8),legend.text = element_text(size=8))+
        scale_color_hue(direction = 1) +
        labs(x = "Year", y = "conflicts", title = "Evolution of the number of conflicts in the world",color = "Legend")
    })
    
    # WW line chart "Number of conflict 
    output$linechart1W <- renderPlotly({
      p <- ggplot(dfW) +
        aes(x = year, y = Victims,colour="World") +
        scale_y_continuous(labels = label_number()) +
        geom_line(size = 1) +
        theme(plot.title = element_text(size = 12),axis.text=element_text(size=8),legend.title = element_text(size=9),legend.text = element_text(size=8))+
        scale_color_hue(direction = 1) +
        labs(x = "Year", y = "Victims", title = "Evolution of the number of victims in the world",color = "Legend")
    })
      
      
    # line chart "Number of conflict 
    output$linechart2 <- renderPlotly({
      p <- ggplot(dfconfli %>% filter(year %in% yearValues() & region %in% regionValues() ) ) +
        aes(x = year, y = conflicts, colour = region) +
        scale_y_continuous(labels = label_number()) +
        geom_line(size = 1) +
        theme(plot.title = element_text(size = 12),axis.text=element_text(size=8),legend.title = element_text(size=9),legend.text = element_text(size=8))+
        scale_color_hue(direction = 1) +
        labs(x = "Year", y = "conflicts", title = "Evolution of the number of conflicts", color = "Regions")
      
      
    })
    #barchart  Total number of Victimss per region
    output$barchart1 <- renderPlotly({
      
      p <- ggplot(dfviolence %>% filter(year %in% yearValues() & region %in% regionValues() )%>% group_by(region,type.of.violence)   %>% summarise(Victims = sum(Victims)), aes(x=region,y=Victims,fill= type.of.violence)) + 
        scale_y_continuous(labels = label_number())+
        geom_bar(position="stack",stat="identity")+
        theme(plot.title = element_text(size = 12),axis.text=element_text(size=6),legend.title = element_text(size=8),legend.text = element_text(size=8))+
        labs(title = "Total number of victims per region")
      })
    #barchart  Total number of conflicts per region
    output$barchart2 <- renderPlotly({
      
      p <- ggplot(dfviolence %>% filter(year %in% yearValues() & region %in% regionValues() )%>% group_by(region,type.of.violence)   %>% summarise(Conflicts = sum(conflicts)), aes(x=region,y=Conflicts))  +
        scale_y_continuous(labels = label_number())+
        geom_col(aes(fill = type.of.violence))+
        theme(plot.title = element_text(size = 12),axis.text=element_text(size=6),legend.title = element_text(size=8),legend.text = element_text(size=8))+
        labs(title = "Total number of conflicts per region")

    })
    
    output$mytable <- DT::renderDataTable({
      DT::datatable(dt_u,options = list(lengthMenu = c(5, 30, 50), pageLength = 5,orderClasses = TRUE))
    })
    
    # pie chart Victims and type of violence
    output$piechart1 <- renderPlotly({
      
      fig <- plot_ly(dfviolence %>% filter(year %in% yearValues() & region %in% regionValues() ) %>% group_by(type.of.violence)   %>% summarise(Victims = sum(Victims)), labels = ~type.of.violence, values = ~Victims,
                     textposition = 'outside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste('Victims :', Victims),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1)),
                     #The 'pull' attribute can also be used to create space between the sectors
                     showlegend = FALSE)
      fig <- fig %>% add_pie(hole = 0.5)
      fig <- fig %>% layout(title = 'Percent of Victims according to type of violence',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
  #The choropleth map Conflicts
  output$map1 <- renderPlotly({
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    fig <- plot_geo(dfmap %>% filter(year %in% yearValues2()) %>% group_by(country,ISO3) %>% summarise(Victims = sum(Victims),Conflicts = sum(conflicts)))
    fig <- fig %>% add_trace(
      z = ~Conflicts, color = ~Conflicts, colors = 'OrRd', hoverinfo = 'text',
      text =~paste(' Country :', country, "<br />", 'Conflicts :', Conflicts, "<br />", 'Victims :', Victims), locations =~ISO3, marker = list(line = l)
    )
    
    fig <- fig %>% layout(
      title = "World conflicts map"
    )

    
  })
  
  #The choropleth map Victims
  output$map2 <- renderPlotly({
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    fig <- plot_geo(dfmap %>% filter(year %in% yearValues2()) %>% group_by(country,ISO3) %>% summarise(Victims = sum(Victims),Conflicts = sum(conflicts)))
    fig <- fig %>% add_trace(
      z = ~Victims, color = ~Victims, colors = 'YlOrRd', hoverinfo = 'text',
      text =~paste(' Country :', country, "<br />", 'Conflicts :', Conflicts, "<br />", 'Victims :', Victims), locations =~ISO3, marker = list(line = l)
    )
    fig <- fig %>% layout(
      title = "World victims map"
    )
    
  })
  
  })
)

