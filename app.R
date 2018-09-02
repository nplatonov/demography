source("./shiny.R")
app <- shinyApp(ui=ui,server=server)
if (interactive()) print(app) else runApp(app,launch.browser=TRUE)
