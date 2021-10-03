library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

# data manipulation
df_use <- tibble::rownames_to_column(USArrests, "State")
df_use$urban_pop_interval <- floor(df_use$UrbanPop / 20) * 20
urban_pop_filter_df <- sort(
  unique(df_use$urban_pop_interval), decreasing = FALSE
                            )


# UI
ui <- fluidPage(dashboardPage(
dashboardHeader(title = "Shiny Dashboard demo"),
  dashboardSidebar(
    sidebarMenu(
menuItem("Charts Demo", tabName = "Charts"),
menuItem("Data Tables Demo", tabName = "DataTables")
   )

  ),
dashboardBody(
    tabItems(
      tabItem("Charts", h1("Charts Demo"),
              fluidRow(
                valueBoxOutput("Box_1"),
                valueBoxOutput("Box_2"),
                valueBoxOutput("Box_3")
              ),
# 2nd row showing state wise numbers in a bar chart
fluidRow(box(plotOutput("bar3"), width = 8),
box(selectInput(
"urban_pop_filter", "UrbanPopulation:", urban_pop_filter_df), width = 4)), 
# Scatter plot demonstration
fluidRow(box(plotOutput("scatter1"), width = 8),
         box(selectInput
             ("ArrestIndex_", "Arrest Index:", c("Murder", "Assault", "Rape"))
, width = 4))), 
tabItem("DataTables", h1("Data Tables Demo"),
dataTableOutput("aggTable")
      ))
    )
)
)
#   SERVER
server <- function(input, output) {
# building valuebox for murder arrests
output$Box_1 <- renderValueBox({
    valueBox(
      mean(df_use[["Murder"]])
      , "Avg. Statewise Murder Arrests Index in US (per million)"
      , color = "navy"

    )
  })
# building 2nd box
output$Box_2 <- renderValueBox({
  valueBox(
    mean(df_use[["Assault"]])
    , "Avg. Statewise Assault Arrests Index (per million)"
    , color = "navy"
)
})
# building 3rd box box
output$Box_3 <- renderValueBox({
  valueBox(
    mean(df_use[["Rape"]])
    , "Avg. Statewise Rape Arrests Index (per million)"
    , color = "navy"
)
})
# filtering data for the bar chart
filtered_data <- reactive({
  dplyr::filter(df_use, df_use$urban_pop_interval == input$urban_pop_filter)
})
# building a bar chart
output$bar3 <- renderPlot(
  ggplot(data = filtered_data(), aes(x = State, y = Assault)) +
    geom_bar(stat = "identity")
)
# building a scatter plot with values
output$scatter1 <- renderPlot(plot(
  df_use$UrbanPop, df_use[[input$ArrestIndex_]], main = "Scatterplot Example", 
xlab = "urban populations percentage ", ylab = "arrests per million"
))
output$aggTable <- renderDataTable(df_use)
}

shinyApp(ui, server)
