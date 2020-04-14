#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
covid = read.csv('covid.csv')
covid[is.na(covid)] = 0



# Defining UI for application that displays table and bar plots for nCovid-19 data
ui <- fluidPage(

    # Application title
    titlePanel("Coronavirus Data Dashborad"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Continent",
                        "Select Continent",
                        unique(covid$Continent)),
            sliderInput('Cases', 'Minimum Number of Cases', min(covid$Total.cases), max(covid$Total.cases), value = mean(covid$Total.cases))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(
               tabPanel('Table', DT::DTOutput('table')),
               tabPanel('Plot', plotOutput('plot'))
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    table = function(){
        data = covid%>%
            filter(Continent == input$Continent)%>%
            filter(Total.cases >= input$Cases)%>%
            mutate(Recovery_rate = round(Total.Recovered/Total.cases * 100, 2),
                   Death_rate = round(Total.Deaths/Total.cases * 100, 2))%>%
            select(Country, Continent, Total.cases, Active.cases, Total.Deaths, Recovery_rate, Death_rate)
    }

    output$table = DT::renderDT({table()
    })
    output$plot = renderPlot({
        data = covid %>%
            filter(Continent == input$Continent)%>%
            select(Country,Total.cases, Total.Deaths, Total.Recovered, Active.cases)
        
        data = melt(data[, -2], id.vars = 'Country')
        
        ggplot(data, aes(Country, value, fill = variable)) +
            geom_col()+
            geom_bar(stat = 'identity') +
            coord_flip()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
