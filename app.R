library(shiny)

data(airquality)
data(mtcars)
maacs <- read.csv("maacs.csv")

ui <- fluidPage(
    titlePanel("Dataset Visualizer"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "dataset",
                        label = "Choose a dataset",
                        choices = c("airquality", "mtcars", "maacs")),
            selectInput(inputId = "variable1",
                        label = "Choose a x variable",
                        choices = NULL),
            selectInput(inputId = "variable2",
                        label = "Choose a y variable",
                        choices = NULL),
            actionButton(inputId = "plot",
                         label = "Plot!")
        ),
        mainPanel(
            plotOutput("summary")
        )
    )
)

server <- function(input, output, session) {
    observeEvent(input$dataset, {
        dat <- get(input$dataset)
        updateSelectInput(inputId = "variable1",
                          choices = names(dat),
                          selected = names(dat)[1])
        updateSelectInput(inputId = "variable2",
                          choices = names(dat)[-1])
    })
    observeEvent(input$variable1, {
        dat <- get(input$dataset)
        updateSelectInput(inputId = "variable2",
                          choices = setdiff(names(dat), input$variable1))
    })
    observeEvent(input$variable2, {
        dat <- get(input$dataset)
        updateSelectInput(inputId = "variable1",
                          choices = setdiff(names(dat), input$variable2),
                          selected = input$variable1)
    })
    dat <- eventReactive(input$plot, {
        get(input$dataset)
    })
    variable1 <- eventReactive(input$plot, {
        input$variable1
    })
    variable2 <- eventReactive(input$plot, {
        input$variable2
    })
    output$summary <- renderPlot({
        d <- dat()
        plot(d[, variable1()], d[, variable2()],
             xlab = variable1(),
             ylab = variable2(),
             main = sprintf("Scatterplot of %s and %s",
                            variable1(), variable2()))
    })
}
shinyApp(ui, server)