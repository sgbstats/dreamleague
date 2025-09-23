library(shiny)
library(reactable)

ui <- fluidPage(
  reactableOutput("table")
)

server <- function(input, output, session) {
  data <- data.frame(
    Name = c("Alice", "Bob", "Charlie"),
    Age = c(25, 30, 35),
    stringsAsFactors = FALSE
  )
  
  details <- list(
    "Alice works in sales and likes hiking.",
    "Bob is a developer and enjoys chess.",
    "Charlie is a designer and travels often."
  )
  
  output$table <- renderReactable({
    reactable(
      data,
      details = function(index) {
        div(style = "padding: 16px;",
            strong("Details: "), details[[index]])
      }
    )
  })
}

shinyApp(ui, server)
