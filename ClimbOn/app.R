
# libs
library(shiny)
library(shinyMobile)

# setup
rdrop2::drop_auth(rdstoken = "droptoken.rds")
authenticators <- readRDS("authenticators.rds")
make_json <- function(climber, height, grade, 
                      style, takes, send) {
    . <- glue::glue(
    '[
      {{ 
        "climber": "{climber}", 
        "height": "{height}",
        "grade": "{grade}", 
        "style": "{style}",  
        "takes": "{takes}",  
        "send": "{send}", 
        "time_stamp": "{as.character(Sys.time())}"
      }}
    ]'
    )
    
    fileOut <- sprintf("%s_%s.json",
                       format(Sys.time(), "%Y%m%d-%H%M%OS"), 
                       climber)
    fileOut <- file.path(tempdir(), fileOut)
    write(., fileOut)
    rdrop2::drop_upload(fileOut, path = "climbing_records")
}

loginPageUI <- function(..., id, title, label = "Sign in") {
    submitBttn <- f7Button(inputId = "login", label = label)
    submitBttn[[2]]$attribs$class <- "item-link list-button f7-action-button"
    submitBttn[[2]]$name <- "a"
    shiny::tagList(
        shinyMobile:::f7InputsDeps(),
        shiny::tags$div(
            id = id,
            `data-start-open` = "[true]",
            class = "login-screen",
            shiny::tags$div(class = "view", shiny::tags$div(
                class = "page",
                shiny::tags$div(
                    class = "page-content login-screen-content",
                    shiny::tags$div(class = "login-screen-title",
                                    title),
                    shiny::tags$form(
                        shiny::tags$div(class = "list",
                                        shiny::tags$ul(
                                            f7Text(
                                                inputId = "login_user",
                                                label = "",
                                                placeholder = "Username"
                                            ),
                                            f7Password(
                                                inputId = "login_password",
                                                label = "",
                                                placeholder = "Password"
                                            ),
                                            ...
                                        )),
                        shiny::tags$div(class = "list", shiny::tags$ul(shiny::tags$li(submitBttn)))
                    )
                )
            ))
        )
    )
}


authenticate <- function(user, password) {
    if (!(user %in% authenticators$users)) {
        return(FALSE)
    }
    pw <- authenticators$passwords[authenticators$users == user]
    if (password != pw) {
        return(FALSE)
    }
    return(TRUE)
}

# app
ui <- shinyMobile::f7Page(
    shinyjs::useShinyjs(),
    shiny::tags$link(rel = "apple-touch-icon", href = "icons/apple-touch-icon.png"),
    init = shinyMobile::f7Init(theme = "light"),
    shinyMobile::f7SingleLayout(
        navbar = shinyMobile::f7Navbar(
            title = "If you don't want to fall, you won't...", 
            hairline = T, 
            shadow = F
        ),
        loginPageUI(id = "loginPage", title = "Get climbing"),
        shinyjs::hidden(
            f7BlockHeader(text = textOutput("authentication"))
        ), 
        shinyMobile::f7Shadow(
            intensity = 5, 
            hover = F, 
            shinyMobile::f7Card(
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
                                     selectInput("takes", "Number of takes", 0:50)),
                    selectInput("send", "Did you topout?", 
                                c("Yes", "No")), 
                    shinyMobile::f7Button(
                        inputId = "submit",
                        color = "red", 
                        label = "Record climb"
                    )
                ), 
                div(
                    id = "new_climb", 
                    shinyMobile::f7Button(
                        inputId = "fuck_ya",
                        color = "red", 
                        label = "Add another climb"
                        )
                    )
                ) 
            )
        )
    )


server <- function(input, output, session) {
    
    check <- reactive({ authenticate(input$login_user, input$login_password) })
    observeEvent(input$login, {
        if (isFALSE(check())) {
            updateF7Login(id = "loginPage")
        } else {
            updateF7Login(id = "loginPage", 
                          user = input$login_user, 
                          password = input$login_password)
        }
    })
    user <- reactive({ input$login_user })
    password <- reactive({ input$login_password })
    output$authentication <- renderText({
        req(user())
        req(password())
        "User authenticated..."
    })
    
    shinyjs::hide("new_climb")
    
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
                  send())
        shinyjs::reset("dataCollect")
        shinyjs::hide("dataCollect")
        shinyjs::hide("submit")
        shinyjs::show("new_climb")
    })
    
    observeEvent(input$fuck_ya, {
        shinyjs::show("dataCollect")
        shinyjs::show("submit")
        shinyjs::hide("new_climb")
    }) 
}

shinyApp(ui = ui, server = server)
