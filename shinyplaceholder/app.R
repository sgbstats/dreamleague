library(shiny)

ui <- fluidPage(
  tags$div(
    style = paste(
      "max-width: 700px;",
      "margin: 10vh auto;",
      "padding: 2rem;",
      "font-family: sans-serif;",
      "text-align: center;"
    ),
    tags$h1("This web app has moved"),
    tags$p(
      "The new Dreamleague web app is now available at ",
      tags$a(
        href = "https://dreamleague.uk",
        "dreamleague.uk",
        target = "_blank"
      ),
      "."
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
