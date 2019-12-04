source("./test_dii_functions.R")

data <- test_data

# Editing Dr. Sullivan's app to repurpose here
shinyApp(
  # Define UI for iris application
  shinyUI(pageWithSidebar(
    # Application title
    headerPanel("DII app"),
    sidebarPanel(
      selectInput("variable", "First variable:",
                  list(
                    "dii", "tx"
                  )),
      selectInput("variable2", "Second variable:",
                  names(data[,2:24]
                  ))
      ),
    mainPanel(
      h3(textOutput("caption")),
      plotOutput("plot")
    )
  )),
# Define server logic required to plot variables
shinyServer(function(input, output) {
    # reactive text
    text <- reactive({
      paste(input$variable, 'versus', input$variable2, 'intake')
    })
    formulaText <- reactive({
      paste(input$variable, "~", input$variable2)
    })
    # Return as text the selected variables
    output$caption <- renderText({
      text()
    })
    # Generate a plot of the requested variables
    output$plot <- renderPlot({
      p <- ggplot(data, aes_string(x=input$variable, y=input$variable2)) + geom_point()
      print(p)
    })
  }),
  options = list(height = 200)
)
