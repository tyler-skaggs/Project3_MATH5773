library(shiny)
library(ggplot2)
source("Rcode.R")

ui <- fluidPage(
  navbarPage("User Interface:",
             tabPanel("Upload",
                      titlePanel("Uploading Files"),
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose CSV File",
                                    multiple = TRUE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          tags$hr(),
                          checkboxInput("header", "Header", TRUE),
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),
                          tags$hr(),
                          radioButtons("disp", "Display",
                                       choices = c(Head = "head",
                                                   All = "all"),
                                       selected = "head"),
                          radioButtons("quote", "Quote",
                                       choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                                       selected = '"')),
                        mainPanel(
                          verbatimTextOutput("summary"),
                          tableOutput("contents")
                        ))),
             tabPanel("Graphing",
                      sidebarLayout(
                        sidebarPanel( uiOutput("variable_x"),
                                      uiOutput("variable_y")),

                      mainPanel(
                        h3(textOutput("caption")),
                        plotOutput("plot1",
                                 click = "plot_click",
                                 dblclick = "plot_reset",
                                 hover = "plot_hover",
                                 brush = "plot_brush"
                        ),
                        verbatimTextOutput("info")
                      )))
          ))

server <- function(input, output, session) {
  onSessionEnded(stopApp)

  data <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote)
    return(df)
  })

  output$contents <- renderTable({
    if (input$disp == "head") {
      return(head(data()))
    }
    else {
      return(data())
    }
  })

  output$summary <- renderPrint({
    summary(data())
  })

  output$variable_x <- renderUI({
    selectInput("variableNames_x", label = "Variable_X", choices = names(data()))
  })

  output$variable_y <- renderUI({
    selectInput("variableNames_y", label = "Variable_Y", choices = names(data()) )
  })

  dat <- reactive({
    test <- data.frame(data()[[input$variableNames_x]], data()[[input$variableNames_y]], rep(FALSE, nrow(data())))
    colnames(test) <- c("X", "Y", "sel")
    return(test)
  })

  output$plot1 <- renderPlot({
    df <- dat()
    df$sel <- selected()
    ggplot(df, aes(x = X, y = Y)) +
      geom_point(aes(color = sel)) +
      scale_colour_discrete(limits = c("TRUE", "FALSE"))
  }, res = 96)

  selected <- reactiveVal(rep(FALSE))

  observeEvent(input$plot_brush, {
    brushed <- brushedPoints(dat(), input$plot_brush, allRows = TRUE)$selected_

      selected(brushed | selected())

  })

  observeEvent(input$plot_reset, {
    selected(rep(FALSE, nrow(dat())))
  })

  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0(
      "click: ", xy_str(input$plot_click),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
}

shinyApp(ui, server)

