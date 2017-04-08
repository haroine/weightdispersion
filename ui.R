library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Estimates and sampling weights dispersion"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("alpha", 
                "Coefficient alpha:", 
                min = 0,
                max = 1,
                step = seq(0,1,length.out = length(listPlotsArticle))[2],
                value = 0.2),
    checkboxInput("scale", "Scale", FALSE),
    checkboxInput("include_CI_mean", "Include confidence interval for the estimate of the mean  X", TRUE),
    checkboxInput("include_CI_95", "Include confidence interval for the estimate of the 95th percentile of  X", TRUE)
      ),
  

  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("plot_simus"),
    plotOutput("plot_mse")
  )
))