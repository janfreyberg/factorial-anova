
# This is the user-interface definition of a Shiny web application.

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("2x2 Between-Subjects Factorial ANOVA"),
  # Subtitle
  p(paste("NB: The effects used in this are example effects. ",
          "Not all main effects / interactions look like the ",
          "ones in this visualisation!")),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("effects", "Effects present in data:",
                         c("Main Effect of Factor 1"="mainEffect1",
                           "Main Effect of Factor 2"="mainEffect2",
                           "Interaction between Factors 1 & 2"="interact12")),
      radioButtons("squares", "Choose which Squares to show:",
                   c("None"="none",
                     "Total"="total",
                     "Error (within groups)"="error",
                     "Model"="between",
                     "Effect of Factor 1"="main1",
                     "Effect of Factor 2"="main2",
                     "Interaction between Factors 1 & 2"="interaction"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      hr(),
      textOutput("errorExplanation"),
      hr()
    )
  )
))
