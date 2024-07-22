library(shiny)

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Linear Regression Data"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      
      # Input: Select a file
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Checkbox if file has header
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Select number of rows to display
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # Action button to trigger model update
      actionButton("go", "Plot Linear Model")
    ),
    
    # Show plots and table output
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("lmPlot"),
      tableOutput("contents"),
      verbatimTextOutput("values")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  lmdata <- reactiveValues()
  
  dataInput <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    return(df)
  })
  
  update_lm <- function() {
     lm_model <- lm(y ~ x, data = dataInput())
    lmdata$model <- lm_model
    lmdata$rsq <- summary(lm_model)$r.squared
    lmdata$coef <- summary(lm_model)$coefficients[, 1]
    lmdata$slope <- coef(lm_model)[2]  # Slope
    lmdata$intercept <- coef(lm_model)[1]  # Intercept
    lmdata$cor <- cor(dataInput()$x, dataInput()$y)  # Correlation coefficient
  }
  
  observeEvent(input$go, {
    update_lm()
  })
  
  output$distPlot <- renderPlot({
    plot(dataInput()$x, dataInput()$y,
         xlab = "X",
         ylab = "Y",
         main = "Distribution Plot",
         col = "black",
         border = "white")
  })
  
  output$lmPlot <- renderPlot({
    plot(dataInput()$x, dataInput()$y,
         xlab = "X",
         ylab = "Y",
         main = "Linear Model Plot",
         col = "black",
         border = "white")
    abline(lmdata$model, col = 'red')
  })
  
 output$values <- renderText({
    paste("R-squared = ", round(lmdata$rsq, 3),
          "\nSlope = ", round(lmdata$slope, 3),
          "\nIntercept = ", round(lmdata$intercept, 3),
          "\nCorrelation coefficient = ", round(lmdata$cor, 3))
  })
    
  output$contents <- renderTable({
    if (input$disp == "head") {
      return(head(dataInput()))
    } else {
      return(dataInput())
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
