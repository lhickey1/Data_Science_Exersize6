library(shiny)
library(tidyverse)
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
covid19

ui <- fluidPage(
  selectInput(inputId = "state",
              label = "State:",
              multiple = TRUE, 
              choices = list(
                Alabama = "Alabama", Alaska = "Alaska", Arizona = "Arizona",
                Arkansas = "Arkansas", California = "California", Colorado = "Colorado",
                Connecticut = "Connecticut", Delaware = "Delaware", Florida = "Florida",
                Georgia = "Georgia", Hawaii = "Hawaii", Idaho = "Idaho",
                Illinois = "Illinois", Indiana = "Indiana", Iowa = "Iowa",
                Kansas = "Kansas", Kentucky = "Kentucky", Louisiana = "Louisiana",
                Maine = "Maine", Maryland = "Maryland", Massachusetts = "Massachusetts",
                Michigan = "Michigan", Minnesota = "Minnesota", Mississippi = "Mississippi",
                Missouri = "Missouri", Montana = "Montana", Nebraska = "Nebraska",
                Nevada  = "Nevada", 'New Hampshire' = "New Hampshire", 'New Jersey' = "New Jersey",
                'New Mexico' = "New Mexico", 'New York' = "New York", 'North Carolina' = "North Carolina",
                'North Dakota' = "North Dakota", Ohio = "Ohio", Oklahoma = "Oklahoma",
                Oregon = "Oregon", Pennsylvania = "Pennsylvania", 'Rhode Island' = "Rhode Island",
                'South Carolina' = "South Carolina", 'South Dakota' = "South Dakota", Tennessee = "Tennessee",
                Texas = "Texas", Utah = "Utah", Vermont = "Vermont",
                Virginia = "Virginia", Washington = "Washington", 'West Virginia' = "West Virginia",
                Wisconsin = "Wisconsin", Wyoming = "Wyoming")),
  plotOutput(outputId = "covidplot")
  )

server <- function(input,output){
  output$covidplot <- renderPlot(
    covid19 %>% 
      filter(state == input$state) %>% 
      filter(cases >= 20) %>% 
      group_by(state) %>% 
      mutate(day_count = date - min(date)) %>% 
      mutate(cum_cases = log10(cases)) %>% 
      ungroup() %>% 
      ggplot(aes(x = day_count,
                 y = cum_cases,
                 color = state))+
      geom_line() + 
      labs(x = "Days since 20 cases or more",
           y = "Cases (log10 scale)",
           color = "State") +
      theme_minimal()
  )
}
shinyApp(ui = ui,server = server)




