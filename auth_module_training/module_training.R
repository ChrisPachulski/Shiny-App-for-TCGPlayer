# BUSINESS SCIENCE ----
# DS4B 202-R ----
# AUTHENTICATION & MODULE TRAINING -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Render a Login Dialog Page
# - Dynamically render a UI upon authentication
# - Create a module to produce the login
# - Use the shinyauthr package

library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(shinyauthr)

source('~/Desktop/auth_module_training/modules/module_login.R')

ui <- navbarPage(
    title = "Module Training", 
    theme = shinytheme("flatly"),
    collapsible = TRUE,
    
    tabPanel(
        useShinyjs(),
        title = "Login Module",
        
        shinyauthr::loginUI(id = 'login_3'),
        
        uiOutput(outputId = 'web_page')
        
    )
)

server <- function(input, output, session) {
    
    user_base_tbl <- tibble(
        user_name = "user1",
        password  = "pass1"
    )
    
    # NO MODULE ----
    
    validate_password = eventReactive(input$login_button,{
        if(input$user_name %in% user_base_tbl$user_name ){
            
            user_tbl = user_base_tbl %>%
                filter(input$user_name %in% user_name )
            
            if(input$password %in% user_base_tbl$password){
                validate = T
            }else{
                validate = F
            }
            
        }else{
            validate = F
        }
        
        return(validate)
    })
    
    output$display_content = renderUI({
        
        req(validate_password())
        
        div(
            class = 'well',
            id = 'success',
            h1(class='page-header', 'MTG BAN Historical Data', tags$small('by Chris Pachulski')),
            p(class = 'lead', "Page Content")
        )
    })
    
    # MODULE ----
    validate_2 = callModule(
        module   = real_validate_pwd, 
        id       = 'login_2',
        data     = user_base_tbl,
        user_col = 'user_name',
        pwd_col  = 'password')
    
    output$display_content_2 = renderUI({
        
        req(validate_2())
        
        div(
            class = 'well',
            id = 'success',
            h1(class='page-header', 'Validated Module', tags$small('by Chris Pachulski')),
            p(class = 'lead', "Page Content")
        )
    })
    
    # SHINYAUTHR ----
    credentials <- callModule(
        module   = shinyauthr::login,
        id       = "login_3",
        data     = user_base_tbl,
        user_col = user_name,
        pwd_col  = password,
        log_out  = reactive(logout_init()))
    
    user_auth <- reactive({
        credentials()$user_auth
    })
    
    user_data <- reactive({
        credentials()$info
    })
    
    logout_init <- callModule(
        module = shinyauthr::logout, 
        id = "logout",
        active = reactive(user_auth())
    )
    
    output$creds <- renderPrint({
        credentials()
    })
    
    output$web_page = renderUI({
        req(F)
        tagList(
            h2("No Module"),
            
            div(
                id = 'login',
                style = 'width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;',
                div(
                    class = 'well',
                    #style = 'background-color:Azure;',
                    h2(class = 'text-center', "Please Login"),
                    textInput(inputId = 'user_name', 
                              label = tagList(icon('user'), 'User Name'), 
                              placeholder = 'Enter Username Here'),
                    
                    passwordInput(inputId     = 'password',
                                  label       = tagList(icon('unlock'), "Password"),
                                  placeholder = 'Enter Password'
                    ),
                    div(
                        class = 'text-center',
                        actionButton(inputId = 'login_button', "Log in", class = 'btn-primary', style = 'color:white;')
                    )
                )
                
            ),
            
            uiOutput(outputId = 'display_content'),
            
            h2("Using A Module"),
            
            login_ui(id = 'login_2', title = "Modular Login"),
            
            uiOutput(outputId = 'display_content_2'),
            
            # TODO
            
            h2('Using Shiny Auth')
        )
    })
    
}

shinyApp(ui, server)