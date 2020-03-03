
# libs
library(shiny)
library(rdrop2)

# setup
fields <- c("climber", "height", "grade", "style", "takes", "send")
collectDir <- "climbing_records"

entryTime <- function() {
    as.character(Sys.time())
}

make_json <- function(climber, height, grade, 
                      style, takes, send, time_stamp) {
    . <- glue::glue(
    '[
      {{ 
        "climber": "{climber}", 
        "height": "{height}",
        "grade": "{grade}", 
        "style": "{style}",  
        "takes": "{takes}",  
        "send": "{send}", 
        "time_stamp": "{time_stamp}"
      }}
    ]'
    )
    
    fileOut <- sprintf("%s_%s.json",
                       format(Sys.time(), "%Y%m%d-%H%M%OS"), 
                       climber)
    fileOut <- file.path(tempdir(), fileOut)
    write(., fileOut)
    drop_upload(fileOut, path = collectDir)
}

# app
ui <- fluidPage(
    titlePanel("ClimbOn: Collect climbing data"),
    shinyjs::useShinyjs(),
    div(
        id = "dataCollect", 
        selectInput("climber", "Climber", 
                    c("Kat", "Nick")),
        selectInput("height", "Wall height",
                    c("9m", "13m")), 
        selectInput("grade", "Grade", 
                    c("5.5", "5.6", "5.7", "5.8", "5.9", "5.10a", "5.10b", "5.10c", "5.10d", 
                      "5.11a", "5.11b", "5.11c", "5.11d", "5.12a", "5.12b", "5.12c", "5.12d", 
                      "5.13a", "5.13b", "5.13c", "5.13d", "5.14a")), 
        selectInput("style", "Ascent style", 
                    c("Toprope", "Go", "Redpoint", "Onsight", "Flash")), 
        conditionalPanel(condition = "input.style == 'Toprope' || input.style == 'Go'", 
                         selectInput("takes", "Number of takes", 
                                     0:50)),
        selectInput("send", "Did you topout?", 
                    c("Yes", "No")), 
        actionButton("submit", "Submit", class = "btn-primary")
    ), 
    shinyjs::hidden(
        div(
            id = "fuck_ya",
            h3("Rock on!"),
            actionLink("climb_again", "Add another climb")
        )
    )  
)

server <- function(input, output) {
    
    climber <- reactive({ input$climber })
    height <- reactive({ input$height })
    grade <- reactive({ input$grade })
    style <- reactive({ input$style })
    takes <- reactive({
        if (input$style %in% c("Redpoint", "Onsight", "Flash")) { 
            0
        } else {
            input$takes
        }
    })
    send <- reactive({ input$send })
    
    observeEvent(input$submit, {
        make_json(climber(), 
                  height(), 
                  grade(), 
                  style(), 
                  takes(), 
                  send(), 
                  entryTime())
        shinyjs::reset("dataCollect")
        shinyjs::hide("dataCollect")
        shinyjs::show("fuck_ya")
    })
    
    observeEvent(input$climb_again, {
        shinyjs::show("dataCollect")
        shinyjs::hide("fuck_ya")
    }) 
}

shinyApp(ui = ui, server = server)
