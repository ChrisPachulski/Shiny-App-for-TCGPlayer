login_ui = function(id, title) {
    
    ns = NS(id)
    
    div(
        id = ns('login'),
        style = 'width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;',
        div(
            class = 'well',
            #style = 'background-color:Azure;',
            h2(class = 'text-center', title),
            textInput(inputId = ns('user_name'), 
                      label = tagList(icon('user'), 'User Name'), 
                      placeholder = 'Enter Username Here'),
            
            passwordInput(inputId     = ns('password'),
                          label       = tagList(icon('unlock'), "Password"),
                          placeholder = 'Enter Password'
            ),
            div(
                class = 'text-center',
                actionButton(inputId = ns('login_button'), "Log in", class = 'btn-primary', style = 'color:white;')
            )
        )
        
    )
}



# Server
# data = user_base_tbl
# input = NULL
# input$user_name = 'user1'
# input$password = 'pass1'
# validate_pwd(data = data ,user_col = user_col,pwd_col=pwd_col)

validate_pwd = function(input, output, session, data, user_col, pwd_col){
    
    user = data %>% pull(!! enquo(user_col))
    pwd = data  %>% pull(!! enquo(pwd_col))
    
    eventReactive(input$login_button,{
        validate = F
        if (input$user_name == user && input$password == pwd){
            validate = T
        }
    })  
    
}


real_validate_pwd = function(input, output, session, data, user_col, pwd_col){
    
    user = data %>% pull(!! enquo(user_col))
    pwd = data  %>% pull(!! enquo(pwd_col))
    
    eventReactive(input$login_button,{
        if(input$user_name %in% user ){
            
            user_tbl = data %>%
                filter( UQ(sym(user_col)) %in% input$user_name )
            
            if(input$password %in% (user_tbl %>% pull(!! enquo(pwd_col)) ) ){
                validate = T
                print(validate)
            }else{
                validate = F
            }
            
        }else{
            validate = F
        }
        
        if (validate) shinyjs::hide(id = 'login')
        
        return(validate)
    })  
    
}

