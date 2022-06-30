library(shiny)
library(tidyverse)

data <- read_csv("https://raw.githubusercontent.com/pranavrajaram/workshop/main/data/shinydata.csv") %>%
  arrange(desc(Year)) %>%
  filter(FantasyPoints >= 50.0) %>%
  select(Player, Year, Tm, Pos, G, PassAtt, PassYards, RushAtt, RushYards, Rec, RecYards, FantasyPoints) %>%
  filter(Pos != "0") %>%
  mutate(TotalYds = RushYards + RecYards + PassYards,
         TotalTouches = RushAtt + Rec + PassAtt)

ui <- fluidPage(
  titlePanel("Fantasy Football Stats and History"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Choose a Year", choices = data$Year),
      selectInput("position", "Choose a Position", choices = data$Pos)
    ), 
    mainPanel(
      plotOutput("myplot"),
      dataTableOutput("mytable")
    )
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    data %>%
      filter(Year == input$year) %>%
      filter(Pos == input$position) %>%
      head(n = 36)
    })
  
  output$myplot <- renderPlot({
    ggplot(data = selectedData()) +
      geom_point(aes(x = TotalTouches,
                     y = FantasyPoints)) +
      geom_text(aes(x = TotalTouches,
                    y = FantasyPoints,
                    label = Player)) +
      labs(title = "Opportunities vs Fantasy Points",
           x = "Total Opportunities",
           y = "Fantasy Points") +
      theme_minimal()
  })
  
  output$mytable <- renderDataTable({
    selectedData()
  })
  
}

shinyApp(ui = ui, server = server)