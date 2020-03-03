
# libs
library(shiny)
library(shinyMobile)
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
ui <- f7Page(
    shinyjs::useShinyjs(),
    init = f7Init(theme = "dark"),
    f7SingleLayout(
        navbar = f7Navbar(
            title = "Track your climbs", 
            hairline = T, 
            shadow = T
        ), 
        f7Shadow(
            intensity = 16, 
            hover = F, 
            f7Card(
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
                    f7Button(
                        inputId = "submit",
                        color = "red", 
                        label = "Submit"
                    )
                ), 
                div(
                    id = "new_climb", 
                    shinyjs::hidden(
                        f7Button(
                            inputId = "fuck_ya",
                            color = "red", 
                            label = "Add another climb"
                        )
                    )
                ) 
            )
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
        shinyjs::hide("submit")
        shinyjs::show("fuck_ya")
    })
    
    observeEvent(input$fuck_ya, {
        shinyjs::show("dataCollect")
        shinyjs::show("submit")
        shinyjs::hide("new_climb")
    }) 
}

shinyApp(ui = ui, server = server)
