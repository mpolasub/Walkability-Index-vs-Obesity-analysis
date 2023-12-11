source('finalProj.R')

#import library
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)

obesity_walkability_df <- read.csv("obesity_walkability.csv")


# intro page
intro_ui <- fluidPage(
  tabsetPanel(
    mainPanel(style = 'margin-left: 5%;',
      h1("Introduction"),
      h2("Ian Matthew Lua, Matthew Polasub, Sam Carruthers"),
      h3("Project Overview: Does the walkability of a state affect the population's obesity?"),
      HTML('<center><img src = "https://www.cdc.gov/obesity/data/maps/2022/2022_overall_v4.svg"
         width = "500"></center>'),
      p("In the last 10 years, obesity in the US has shot up by around 10% (2013-2023). Cited as an increasing problem by the CDC,
    our group was intrigued on what factors could lead to this. After some research and thought, we felt that the walkability
    of each state had an effect on whether people would want to walk as a form of exercise, affecting their body weight. 
    This led us to take data from the following sources in order to compare them:
    Obesity: 2015 CDC Behavioral Risk Factor Surveillance System Survey (Lake County Illinois)
    Walkability: Information from each block group in the 2019 census (US Environmental Agency)
    Although slightly outdated, we felt these sources would give us an insight into whether our hypothesis was correct, that 
    a state with a low walkability index would be likely to have a higher obese population."),
      br(),
      h3("Why should you care?"),
      p("According to the CDC, Obesity impacts our nation’s health, economy, and military readiness. 
      We already understand that this medical condition can lead to serious health problems such as heart disease, diabetes, etc, and 
      costs the US healthcare system a lot of money ($173 billion). At the rate the US is going, by 2030, 
      at least half of the population will have obesity (Harvard School of Public Health)"),
      br(),
      p("Since state walkability plays a role in population obesity, by analyzing and comparing how both topics are related, we hope that this project encourages
      public government officials to improve walkability by adding more walkways, enforcing more pedestrian-first policies, and reduce over-reliance on
      cars.")
    ),
  ),
  tags$style(HTML("
    h1 {
      text-align: center;
    }
    h2 {
            text-align: center;
            background-color: #ffd272;
            color: Black;
            }

    h3 {
          text-align: center;
          background-color: #72ffd2;
            color: Black;
            }

    p {
          text-align: center;
            color: Black;
            }")),
  
  
  
)

#page 1
page1ui <- fluidPage(
  titlePanel("State Obesity and State Urban Population Percentage"),
  sidebarLayout(
    sidebarPanel(
      h2("Control Panel"),
      selectInput(
        inputId = "state_name",
        label = "Select a State",
        choices = obesity_walkability_df$NAME
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      h3("Comparing State Obesity to Urban Population Percentage"),
      p("Urban areas often have different lifestyles compared to rural areas. 
        With better urban planning and healthier resources. 
        Urban environments may offer more opportunities for physical activity, such as parks, gyms, and recreational facilities. 
        However, these factors may not always counterbalance the negative influences of urban living on obesity."), 
      p("Here is a comparison between the obesity percentege by state and urban population percentage by state.
        Because 'urban population percentage' is so vague, it will be hard to infer conclusions from just this factor.
        in the next page, we will explore a different metric, derived from urban planning, to look for valuable information.")
    ),
    mainPanel(
      HTML('<center><img src = "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Map_of_USA_with_state_names_2.svg/1200px-Map_of_USA_with_state_names_2.svg.png"
         width = "700"></center>'),
      fluidRow(style = 'margin-left: 5%;',
        column(5,
               plotOutput(outputId = "obesity_bar_plot", width  = "500px",height = "400px")
        ),
        column(5,
               plotOutput(outputId = "urban_bar_plot", width  = "500px",height = "400px")
        )
      )
    )
  )
)

#page 2
page2ui <- fluidPage(
  titlePanel("Walkability by Urban percentage Scatter"),
  br(),
  p("From the scatterplot, we can see that walkable states are more urban than non walkable states.
    This makes sense, as the compact design and large amount of people generally leads to a more person
    catered environment. Public transportation is also more catered towards environments with a large amount of people, while
    being neglected in more rural environments. With this conclusion in mind, we can draw another conclusion about obesity. If people
    live in areas where they are more incentivized to walk, we believe that the obesity rate of that area will generally be lower than that
    of an environment where an indiviudal drives their care everywhere. Due to this reason, however, the level of urbanization is not directly correlated to 
    obesity; walkability is."),
  br(),
  
  mainPanel(
    selectInput(
      inputId = "state",
      label = "Choose a state",
      choices = obesity_walkability_df$NAME
    ),
    plotlyOutput(outputId = "scatter"),
    
  )
)

#page 3
page3ui <- fluidPage(
  titlePanel("Obesity vs Walkability Scatter"),
  br(),
  p("From the scatterplot, there is a negative correlation between obesity and walkability.
    In other words, the more walkable an area is, the less likely people are to be obese. States such as California, Colorado, 
    and Massachusetts are very walkable, and have some of the lowest rates of obesity. Other states, such as Mississipi and Kentucky, are 
    extremely unwalkable, and hence have the highest rates of obesity in the United States. This conclusion makes sense, as the Deep South
    is generally known as the region of the United States with the most problem with obesity."),
  
  mainPanel(
    selectInput(
      inputId = "state",
      label = "Choose a state",
      choices = obesity_walkability_df$NAME
    ),
    
    plotlyOutput(outputId = "scatter2"),
  )
)


# conclusion
conclusion_ui <- fluidPage(
  HTML('<center><img src = "https://uncnews.unc.edu/wp-content/uploads/sites/933/2020/08/shutterstock_688185661-scaled.jpg"
         width = "700"></center>'),
  titlePanel("Conclusion"),
  h3("1. Our hypothesis was right"),
  p("As evidenced in the Obesity vs Walkability scatterplot, states with a lower walkability index typically had a higher population percentage that was obese.
     The only major outlier was West Virginia, which had a relatively high walkability index, but somehow had one of the highest obese population rates.
     This is primarily due to a lack of access to healthy food retailers, as well as lack of population motivation to perform physical activity."),
  h3("2. A state that was more urban than rural typically had a higher walkability index"),
  p("Due to improvements in urban planning and an increase in urbanization over the last century, most states have seen a steady increase in their
    respective walkability index. "),
  h3("Limitations"),
  p("Unfortunately, the data we used was slightly outdated, coming from 2015 and 2019 respectively. If it were more recent, we would have been able
    to see if state walkability had improved, even though obesity had increased in the United States. Also, our dataframes lacked nuance, we weren't able
    to get more info on race/gender, which we could have added to our observations."),
  
  
  tags$style(HTML("
    h3 {
          text-align: center;
          background-color: #72ffd2;
            color: Black;
            }

    p {
          text-align: center;
            color: Black;
            }")),
  
)

server <- function(input, output){

  output$obesity_bar_plot <- renderPlot({
    state_row_df <- filter(obesity_walkability_df, NAME == input$state_name)
    
    ggplot(data = state_row_df, aes(x = NAME, y = Obesity)) + 
      geom_bar(position = "stack", stat = "identity", width=0.2) +
      labs(y="Obesity Percentage", x="State") +
      ylim(0, 50)
  })
  
  output$urban_bar_plot <- renderPlot({
    state_row_df <- filter(obesity_walkability_df, NAME == input$state_name)
    
    ggplot(data = state_row_df, aes(x = NAME, y = urban_pop_perc)) + 
      geom_bar(position = "stack", stat = "identity", width=0.2) +
      labs(y="Urban Population Percentage", x="State") +
      ylim(0, 1.0)
  })
  
  output$scatter <- renderPlotly({
    p <- ggplot(obesity_walkability_df, aes(x = avg_index, y = urban_pop_perc, color = NAME)) +
      geom_point() +
      labs(
        title = "Urban Percentage vs Walkability",
        x = "Walkability Index",
        y = "Urban Percentage",
      ) +
      geom_text(
        data = obesity_walkability_df,
        aes(
          x=filter(obesity_walkability_df, NAME == input$state)$avg_index,
          y=filter(obesity_walkability_df, NAME == input$state)$urban_pop_perc,
          label = input$state))
    return(p)
  })
  
  output$scatter2 <- renderPlotly({
    p <- ggplot(obesity_walkability_df, aes(x = avg_index, y = Obesity, color = NAME)) +
      geom_point() +
      labs(
        title = "Obesity vs Walkability",
        x = "Walkability Index",
        y = "Obesity",
      ) +
      geom_text(
        data = obesity_walkability_df,
        aes(
          x=filter(obesity_walkability_df, NAME == input$state)$avg_index,
          y=filter(obesity_walkability_df, NAME == input$state)$Obesity,
          label = input$state))
    return(p)
  })
  

  
}

shinyApp(ui = navbarPage(
  title = "Obesity vs Walkability",
  tabPanel("Introduction", intro_ui),
  tabPanel("Urban Percentage and Obesity", page1ui),
  tabPanel("Walkability by Urban Percentage", page2ui),
  tabPanel("Walkability by Obesity Percentage", page3ui),
  tabPanel("Conclusion", conclusion_ui)
), 
server = server)