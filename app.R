
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
# ======================

#====data Processing====

# specifying the path
path <- 'data.csv'
# reading contents of csv file
data <- read.csv(path,sep=";")
#number of death preprocess 
distinct_df = data %>% distinct(region)
dfreg = data %>% group_by(year,region) %>% summarise(death = sum(best))
colnames(dfreg) <- c('year','country','death')
dfcountry = data %>% group_by(year,country) %>% summarise(death = sum(best))
dfcountry = bind_rows(dfreg,dfcountry)
distinct_countries = dfcountry %>% distinct(country)
region <- c("Asia","Americas","Europe" ,"Middle East","Africa")

#number of conflict
dfConfliReg = data %>% group_by(year,region) %>% summarise(conflicts = n_distinct(conflict.name))
colnames(dfConfliReg) <- c('year','country','conflicts')
dfConfliCountry = data %>% group_by(year,country) %>% summarise(conflicts = n_distinct(conflict.name))
dfconfli = bind_rows(dfConfliReg,dfConfliCountry)

#type of violence
dfViolenceReg = data %>% group_by(year,region,type.of.violence) %>% summarise(death = sum(best),conflicts = n_distinct(conflict.name))
colnames(dfViolenceReg) <- c('year','country','type.of.violence','death','conflicts')
dfViolenceCountry = data %>% group_by(year,country,type.of.violence)
dfviolence = bind_rows(dfViolenceReg,dfViolenceCountry)
dfviolence = bind_rows(dfViolenceReg,dfViolenceCountry)

# ======================


# Shiny App with 3 pages 
shinyApp(
  shinyUI(
    navbarPage("Violence around the world",
               tabPanel("Presentation", uiOutput('page1')),
               tabPanel("Analysis", uiOutput('page2')),
               tabPanel("Map", uiOutput('page3'))
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
    # country
    countryValues <- reactive({
      country = input$country
      first = country[1]
      second = country[2]
      three = country[3]
      fourth = country[4]
      five = country[5]
      c(first)
      print(country)
    })
    
    output$page2 <- renderUI({
      
      sidebarLayout(
        sidebarPanel(
          
          # Year Slider
          sliderInput("years", "  Years: ",
                      min = 1989,max = 2022,
                      value = c(1989,2022),
                      sep = "",
          ),
          #country or continent selection
          multiInput(
            inputId = "country",
            label = "  Search and select different region:", 
            choices = unique(distinct_countries$country),
            selected = unique(distinct_df$region),
            choiceNames = unique(distinct_countries$country),
            choiceValues = unique(distinct_countries$country)
          )),
        mainPanel(
          
        )
      )
    })
    #Analysis page with many charts
    output$page2 <- renderUI({
      # title and description
      fluidRow(column(11,
                      h3("Violence around the world"),
                      p("This dataset is UCDP's most disaggregated dataset, covering individual 
                      events of organized violence (phenomena of lethal violence occurring at a 
                      given time and place).
                        ")
                      ),
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
                 #country or continent selection
                 multiInput(
                   inputId = "country",
                   label = " Search and select different region:", 
                   choices = unique(distinct_countries$country),
                   selected = unique(distinct_df$region),
                   choiceNames = unique(distinct_countries$country),
                   choiceValues = unique(distinct_countries$country)
                 )
               )
                 
        ),
        column(5,
               plotlyOutput('barchart1')
               
        ),
        column(4,
               plotlyOutput('barchart2')
               
        )
      ),
      fluidRow(
        column(2,
               plotlyOutput('piechart1')
               ),
        column(5,
               plotlyOutput('linechart1')
        ),
        column(5,
               plotlyOutput('linechart2')
        )
        
      ))
      
     
    })
    
    output$page3 <- renderUI({
      h3("Violence around the world")
      
    })
    # line chart "Number of Death"
    output$linechart1 <- renderPlotly({
      p <- ggplot(dfcountry %>% filter(year %in% yearValues() & country %in% countryValues() ) ) +
        aes(x = year, y = death, colour = country) +
        geom_line(size = 1) +
        scale_y_continuous(labels = label_number()) +
        scale_color_hue(direction = 1) +
        labs(x = "Year", y = "Death", title = "Number of Death", color = "Regions")
      
        
    })
    # line chart "Number of conflict 
    output$linechart2 <- renderPlotly({
      p <- ggplot(dfconfli %>% filter(year %in% yearValues() & country %in% countryValues() ) ) +
        aes(x = year, y = conflicts, colour = country) +
        scale_y_continuous(labels = label_number()) +
        geom_line(size = 1) +
        scale_color_hue(direction = 1) +
        labs(x = "Year", y = "conflicts", title = "Number of conflicts", color = "Regions")
      
      
    })
    #barchart  Total number of deaths per region
    output$barchart1 <- renderPlotly({
      
      p <- ggplot(dfviolence %>% filter(year %in% yearValues() & country %in% countryValues() )%>% group_by(country,type.of.violence)   %>% summarise(Death = sum(death)), aes(x=country,y=Death,fill= type.of.violence)) + 
        scale_y_continuous(labels = label_number())+
        geom_bar(position="stack",stat="identity")+
        labs(title = "Total number of deaths per region")
      
      })
    #barchart  Total number of conflicts per region
    output$barchart2 <- renderPlotly({
      
      p <- ggplot(dfviolence %>% filter(year %in% yearValues() & country %in% countryValues() )%>% group_by(country,type.of.violence)   %>% summarise(Conflicts = sum(conflicts)), aes(x=country,y=Conflicts))  +
        scale_y_continuous(labels = label_number())+
        geom_col(aes(fill = type.of.violence))+
      labs(title = "Total number of conflicts per region")
        

    })
    # pie chart Death and type of violence
    output$piechart1 <- renderPlotly({
      
      fig <- plot_ly(dfviolence %>% filter(year %in% yearValues() & country %in% countryValues() ) %>% group_by(type.of.violence)   %>% summarise(Death = sum(death)), labels = ~type.of.violence, values = ~Death,
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste('death :', Death),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1)),
                     #The 'pull' attribute can also be used to create space between the sectors
                     showlegend = FALSE)
      fig <- fig %>% add_pie(hole = 0.3)
      fig <- fig %>% layout(title = 'Death and type of violence',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
  })
)

