pacman::p_load(shiny,shinyjs,DT,shinyWidgets,shinythemes,shinyauthr,plotly,tidyquant,tidyverse,lubridate,anytime,bigrquery,plotly,fs,janitor,zoo,tidyquant)
source('00_scripts/tcg_analysis_functions.R')
source('00_scripts/shiny_functions.R')
source('00_scripts/generate_favorite_cards.R')
source('00_scripts/crud_operations_local.R')

gaeas_cradle("wolfoftinstreet@gmail.com")


data_list = NULL

data_list[[1]] = tryCatch({read_csv('/Users/cujo253/Desktop/tcg_edition_data.csv')},error=function(e){''})
data_list[[2]] = tryCatch({read_csv('/Users/cujo253/Desktop/tcg_edition_labels.csv')},error=function(e){''})
if(as_tibble( Sys.Date(), .name_repair='unique') != read_csv('/Users/cujo253/Desktop/tcg_edition_date.csv')){
    data_list = get_data_list('edition');
    write_csv(data_list[[1]] , '/Users/cujo253/Desktop/tcg_edition_data.csv');
    write_csv(data_list[[2]] , '/Users/cujo253/Desktop/tcg_edition_labels.csv');
    write_csv(as_tibble( Sys.Date(), .name_repair='unique'), '/Users/cujo253/Desktop/tcg_edition_date.csv')
}  


field_options = get_field_of_interest_options()

# 
# user_base_tbl = tibble(
#     user = c('cujo253','wolfoftinstreet'),
#     password = c('pass1','pass2'),
#     permissions = c('admin','user'),
#     name = c('George','Brad'),
#     favorites = list(c('Theros | NonFoil','Double Masters 2022 | NonFoil'), c('Amonkhet | NonFoil')),
#     favorite_fields = list(c('revenue','sold_quantity'),c('revenue')),
#     last_ovr_viewpoint = c('Mirrodin | NonFoil','War of the Spark | NonFoil'),
#     last_field_viewpoint = c('revenue','sold_quantity'),
#     user_setting = list(tibble(mavg_short = 7, mavg_long = 60, time_window = 60),
#                      tibble(mavg_short = 3, mavg_long = 25, time_window = 45))
# )
# 

#user_base_tbl %>% write_rds("00_data_local/user_base_tbl.rds")
# UI - FluidPage ----
my_ui = tagList(
    useShinyjs(),
    tags$head(
        tags$link(rel='stylesheet', type = 'text/css', href = shinytheme('superhero') ),
        tags$link(rel='stylesheet', type = 'text/css', href = 'styles.css'),
        login_title = 'Enter Username'
    ),
    
    # User Login
    
    #verbatimTextOutput(outputId = 'creds'),
    shinyauthr::loginUI(
        id = 'login',
        title = tagList(h3(class = 'text-center', "Wolf's Warrens"), p(class = 'text-center',"Please Log In"))
    ),
    
    # User Website
    uiOutput(outputId = 'website')  
    )         

# Server  ----
server = function(input, output, session){
    
    # User Data ----
    read_user_base()

    # 0.0 USER LOGIN ----
    
    # 0.1 Credentials ----
    credentials <- callModule(
        module = shinyauthr::login,
        id       = "login",
        data     = user_base_tbl,
        user_col = user,
        pwd_col  = password,
        log_out  = reactive(logout_init())
    )
    
    logout_init <- callModule(
        module = shinyauthr::logout,
        id     = "logout",
        active = reactive(credentials()$user_auth)
    )
    
    output$creds <- renderPrint({
        credentials()
    })
    
    # 0.2 Instantiating User Information ----
    reactive_values = reactiveValues()

    observe({
        
        if (credentials()$user_auth) {
            
            user_data_tbl = credentials()$info
            #user_data_tbl = user_base_tbl %>% head(1)
            
            reactive_values$permissions = user_data_tbl$permissions
            reactive_values$user_name = user_data_tbl$name
            
            reactive_values$favorites_list = user_data_tbl %>% pull(favorites) %>% pluck(1)
            reactive_values$favorites_field_list = user_data_tbl %>% pull(favorite_fields) %>% pluck(1)
        
            reactive_values$last_ovr_viewpoint = user_data_tbl %>% pull(last_ovr_viewpoint) %>% pluck(1)
            reactive_values$last_field_viewpoint = user_data_tbl %>% pull(last_field_viewpoint) %>% pluck(1)
            
            
            user_setting_tbl = user_data_tbl %>% pull(user_setting) %>% .[[1]]
            
            #reactive_values= NULL
            
            reactive_values$mavg_short = user_setting_tbl$mavg_short
            reactive_values$mavg_long = user_setting_tbl$mavg_long 
            reactive_values$from = as.numeric(user_setting_tbl$time_window)
            reactive_values$to = 1
        }
        
    })
    
    
    # User Input 1 ---- Macro Selection
    observeEvent(input$line_plot,{
        update_and_write_user_base(
            user_name = credentials()$info$user,
            column_name = 'last_ovr_viewpoint',
            assign_input =  paste0(unlist(get_input_from_user_input(data = data_list[[1]], input = input$edition_card_user_input)),collapse=" | "))
        
        update_and_write_user_base(
            user_name = credentials()$info$user,
            column_name = 'last_field_viewpoint',
            assign_input =  input$user_field_of_interest_input)
    })
    
    macro_selection = eventReactive(input$line_plot,{
        paste0(unlist(get_input_from_user_input(data = data_list[[1]], input = input$edition_card_user_input)),collapse=" | ")
    }, ignoreNULL = F)
    
    #input =NULL
    # User Input 2 ---- Micro Selection
    field_selection = eventReactive(input$line_plot,{
        input$user_field_of_interest_input #= 'revenue'
    }, ignoreNULL = F)
    # Apply Saved Settings ----
    mavg_short = eventReactive(input$apply_and_save,{
        input$mavg_short
    }, ignoreNULL = F)
    
    mavg_long = eventReactive(input$apply_and_save,{
        input$mavg_long
    }, ignoreNULL = F)
    
    time_window = eventReactive(input$apply_and_save,{
        input$time_window
    }, ignoreNULL = F)
    
    observeEvent(input$apply_and_save,{
        
        update_and_write_user_base(
            user_name = credentials()$info$user,
            column_name = 'user_setting',
            assign_input =  list(tibble(mavg_short = input$mavg_short,mavg_long = input$mavg_long,time_window = input$time_window))
            )

    })
    
    
    selected_tab = eventReactive(input$apply_and_save,{
        if(is.character(input$tab_panel_all_charts)){
            selected_tab = input$tab_panel_all_charts
        }else{
            selected_tab = 'Last Analysis'
        }
        
        selected_tab
        
    }, ignoreNULL = F)
    
    # Data Acquisition/Formatting
    
    get_data()
    
    data_tbl = reactive({
        
        paste0(unlist(get_input_from_user_input(data = data_list[[1]], input = macro_selection())),collapse=" | ") %>% 
            get_input_from_user_input(data = data_list[[1]], .) %>%
            get_user_targeted_data(data = data_list[[1]], 
                                   needed_input= . , 
                                   from = time_window(), 
                                   to = reactive_values$to ) %>%
            get_user_targeted_field(data = ., 
                                    column = gsub(" ", '_',tolower(field_selection())), 
                                    mavg_short=reactive_values$mavg_short,
                                    mavg_long=reactive_values$mavg_long)
            
    })
    
    
    # Plotting Measures ----
    plot_header = eventReactive(input$line_plot,{
        paste0(gsub(' \\| ',' ~ ',input$edition_card_user_input)," ~ ",input$user_field_of_interest_input)
    }, ignoreNULL = F)
    
    output$plot_header = renderText({
        plot_header()
    })
    
    output$plotly_plot = renderPlotly({
        data_tbl() %>% framing_for_graph(.,needed_input = gsub(" ", '_',tolower(reactive_values$last_field_viewpoint)))
    })
    # ShinyJS ----
    shinyjs::onclick(id = 'settings_toggle', {
        shinyjs::toggle(id = 'slider_inputs', anim = T)
    })
    
    # Add Favorites ----
    observeEvent(input$favorites_add, {
        new_value = paste0(unlist(get_input_from_user_input(data = data_list[[1]], input = input$edition_card_user_input)),collapse=" | ")
        reactive_values$favorites_list <- c(reactive_values$favorites_list, new_value) 
        
        new_symbol = gsub(' ','_',tolower(input$user_field_of_interest_input))
        reactive_values$favorites_field_list <- c(reactive_values$favorites_field_list, new_symbol) 
        
        distinction_tbl = tibble(v1 = reactive_values$favorites_list, v2 = reactive_values$favorites_field_list) %>%
            distinct()
        
        reactive_values$favorites_list = distinction_tbl$v1
        
        reactive_values$favorites_field_list = distinction_tbl$v2
        
        update_and_write_user_base(
            user_name = credentials()$info$user,
            column_name = 'favorites',
            assign_input =  list(reactive_values$favorites_list ))
        
        update_and_write_user_base(
            user_name = credentials()$info$user,
            column_name = 'favorite_fields',
            assign_input =  list(reactive_values$favorites_field_list))
    })
    
    
    # Render Favorites ----
    output$favorite_cards = renderUI({
        
        if( (length(reactive_values$favorites_list) > 0) & (length(reactive_values$favorites_field_list)>0)  ){
        
        generate_favorite_cards(favorites  = reactive_values$favorites_list ,
                                column     = reactive_values$favorites_field_list,
                                mavg_short = mavg_short(),
                                mavg_long  = mavg_long())
        }
    })
    
    observeEvent(input$favorites_clears,{
        modalDialog(
            title = 'Clear Favorites',
            size = 'm',
            easyClose=T,
            p('Are you sure you want to remove favorites?'),
            br(),
            div(
                selectInput(inputId = 'drop_list',
                            label = 'Remove Single Favorites',
                            choices = paste0(reactive_values$favorites_list," ~ ", 
                                             str_to_title(gsub('_',' ',reactive_values$favorites_field_list) ) ) %>% sort()
                            ),
                actionButton(inputId = 'remove_single_favorite',
                             label = 'Clear Single',
                             class = 'btn-warning'),
                actionButton(inputId = 'remove_all_favorite',
                             label = 'Clear All',
                             class = 'btn-danger')
                            
            ),
            footer = modalButton('Exit')
        ) %>% showModal()
    })
    
    observeEvent(input$remove_single_favorite,{
        
        if(which(reactive_values$favorites_list == gsub(' \\~ .*$','',input$drop_list)) == which(reactive_values$favorites_field_list == str_to_lower(gsub(' ','_',gsub('^.* \\~ ','',input$drop_list))))){
            reactive_values$favorites_list = reactive_values$favorites_list[-which(reactive_values$favorites_list == gsub(' \\~ .*$','',input$drop_list))]
            reactive_values$favorites_field_list = reactive_values$favorites_field_list[-which(reactive_values$favorites_field_list == str_to_lower(gsub(' ','_',gsub('^.* \\~ ','',input$drop_list))))]
        }
       
        updateSelectInput(session = session, 
                          inputId = 'drop_list', 
                          choices = paste0(reactive_values$favorites_list," ~ ", 
                                           str_to_title(gsub('_',' ',reactive_values$favorites_field_list) ) ) %>% sort()
                          ) 
        
        update_and_write_user_base(
            user_name = credentials()$info$user,
            column_name = 'favorites',
            assign_input =  list(reactive_values$favorites_list) )
        
        update_and_write_user_base(
            user_name = credentials()$info$user,
            column_name = 'favorite_fields',
            assign_input =  list(reactive_values$favorites_field_list) )
        
    })
    

    
    observeEvent(input$remove_all_favorite,{
        
        
        reactive_values$favorites_list = NULL
        reactive_values$favorites_field_list = NULL

        
        updateSelectInput(session = session, 
                          inputId = 'drop_list', 
                          choices = paste0(reactive_values$favorites_list," ~ ", 
                                           str_to_title(gsub('_',' ',reactive_values$favorites_field_list) ) ) %>% sort()
        ) 
        
    })
    
    observeEvent(input$remove_all_favorite,{
        update_and_write_user_base(
            user_name = credentials()$info$user,
            column_name = 'favorites',
            assign_input =  list(reactive_values$favorites_list) )
        
        update_and_write_user_base(
            user_name = credentials()$info$user,
            column_name = 'favorite_fields',
            assign_input =  list(reactive_values$favorites_field_list) )
    })
    
    observeEvent(input$favorites_toggle,{
        shinyjs::toggle(id='favorite_cards_section', anim = T, animType = 'slide')
    })
    # Session 3.0 Favorite Plotting Tabs ----
    output$all_charts = renderUI({
        
        tab_panel_1 = tabPanel(
            title = "Last Analysis",
            div(
                class = "panel",
                div(class="panel-header",
                    h4(textOutput(outputId = 'plot_header'))
                ),
                div(
                    class = "panel-body",
                    plotlyOutput(outputId = 'plotly_plot')
                    
                )
            )
        )
        
        favorite_tab_panels = NULL
        
        if(length(reactive_values$favorites_list) > 0){
        
            favorite_tab_panels = map2(.x = reactive_values$favorites_list , .y = reactive_values$favorites_field_list,.f = function(x,y){
                    tabPanel(
                        title = paste0(x," ~ ", 
                                       str_to_title(gsub('_',' ',y) )),
                        div(
                            class = "panel",
                            div(class="panel-header",
                                h4(paste0(x," ~ ", 
                                          str_to_title(gsub('_',' ',y) )))
                            ),
                            div(
                                class = "panel-body",
                                
                                x %>% 
                                    get_input_from_user_input(data = data_list[[1]], .) %>%
                                    get_user_targeted_data(data = data_list[[1]], needed_input= ., from = time_window(), to = reactive_values$to ) %>%
                                    get_user_targeted_field(data = ., 
                                                            column = gsub(" ", '_',tolower(y)), 
                                                            mavg_short=mavg_short(),
                                                            mavg_long=mavg_long()) %>%
                                    framing_for_graph(., needed_input = gsub(" ", '_',tolower(y)))
                                
                                
                            )
                        )
                    )
                })

        }
        
        do.call(
            what = tabsetPanel,
            args = list(tab_panel_1) %>% 
                append(favorite_tab_panels) %>%
                append(list(id='tab_panel_all_charts', type = 'pills',selected = selected_tab()))
        )

    })
    # Render Website ----
    
    output$website = renderUI({
        
        req(credentials()$user_auth)
        
        navbarPage(
            #useShinyjs(),
            #icon = img(class = 'img-circle img-responsive', src='wolf.png', style = 'width:100px;') %>% a(href = 'https://twitter.com/WolfOfTinStreet'),
            title="Magic The Gathering Sales",
            inverse = T,
            collapsible = T,
            theme = shinytheme('superhero'),
            
            header = div(
                class = 'pull-right',
                style = 'padding-right: 20px;',
                p("Welcome, ", reactive_values$user_name)
            ),
            
            tabPanel(
                title = "Edition Analysis",
                
                # Theme Selection ----
                # Test Other Themes
                #shinythemes::themeSelector(),
                
                # Custom CSS Inclusion ----
                useShinyjs(),
                tags$head(
                    tags$link(rel='stylesheet', type = 'text/css', href = 'styles.css')
                ),
                #1.0 Header ----
                div(
                    class = "container",
                    id = "header",
                    h1(class =  "page-header","Edition Analyzer"),
                    p(class="lead", "Data Provided by", a("MTG BAN", href = 'https://www.mtgban.com/') )
                ),
                
                # 2.0 Favorites ----
                div(
                    class = "container hidden-xs hidden-sm",
                    id = 'favorite_container',
                    div(
                        class = 'container',
                        column(
                            width = 12,
                            h5("Favorites", class = 'pull-left',),
                            actionButton(inputId = 'favorites_clears',
                                         label = 'Clear Favorites',
                                         class = 'pull-right',),
                            
                            actionButton(inputId = 'favorites_toggle',
                                         label = 'Show/Hide',
                                         class = 'pull-right',)
                            
                        ),
                        div(
                            class = 'row',
                            id = 'favorite_cards_section',
                            
                            uiOutput(outputId = 'favorite_cards', class = 'container')
                            
                            
                        )
                    )
                ),
                #3.0 application ui ----
                div(
                    class = "container",
                    id = "application_ui",
                    column(
                        width=4, 
                        wellPanel(
                            div(
                                id = 'input_main',
                                # Input # 2 ----
                                pickerInput(
                                    inputId = "edition_card_user_input", 
                                    label = strong('Edition:'),
                                    choices = data_list[[2]]$label,
                                    multiple = FALSE,
                                    selected = data_list[[2]] %>% filter(label %>% str_detect(paste0('^',gsub(' \\| ',' \\\\| ',reactive_values$last_ovr_viewpoint)))),
                                    options = pickerOptions(
                                        actionsBox = FALSE,
                                        liveSearch = TRUE,
                                        size = 5
                                    ) 
                                ),
                                # Input # 3
                                pickerInput(
                                    inputId = "user_field_of_interest_input", 
                                    label = strong('Interest:'),
                                    choices = field_options$user_labels,
                                    multiple = FALSE,
                                    selected = field_options %>% filter(user_labels %>% str_detect(str_to_title(gsub("_"," ",reactive_values$last_field_viewpoint)))),
                                    options = pickerOptions(
                                        actionsBox = FALSE,
                                        size = 3
                                    ) 
                                )
                            ),
                            # Followup Input Buttons ----
                            div(
                                id = "input_buttons",
                                actionButton(inputId = 'line_plot',
                                             label = '<strong><big>Plot</big></strong>' %>% HTML(),
                                             class = 'btn-success',
                                             icon = icon(class = "fa-1x",'chart-line')),
                                
                                div(
                                    class = "pull-right",
                                    actionButton(inputId = 'settings_toggle',
                                                 label = NULL,
                                                 class = 'btn-warning',
                                                 icon = icon(class = 'fa-1x', 'gears')),
                                    
                                    actionButton(inputId = 'favorites_add', label = NULL, class = 'btn-danger', icon = icon('heart'))
                                    
                                )
                            ),
                            # Plot Elements ----
                            div(
                                id = "slider_inputs",
                                sliderInput(inputId = 'time_window',
                                            label = 'Days Back',
                                            value = reactive_values$from,
                                            min = 1,
                                            max = as.numeric((today() - data_list[[1]] %>% pull(date) %>% max() %>% pluck(1)) + 365)
                                            ),
                                hr(),
                                sliderInput(inputId = 'mavg_short',
                                            label = 'Short Moving Average',
                                            value = reactive_values$mavg_short,
                                            min = 3,
                                            max = 21),
                                hr(),
                                sliderInput(inputId = 'mavg_long',
                                            label = 'Long Moving Average',
                                            value = reactive_values$mavg_long,
                                            min = 22,
                                            max = 90 ),
                                actionButton(inputId = 'apply_and_save', label = 'Apply & Save', icon = icon("save"))
                            ) %>% shinyjs::hidden()
                        )
                    ),
                    # Plot Panel ----
                    column(width=8, 
                           uiOutput(outputId = "all_charts")
                           
                    )
                )
            )
        )  
    })
}



drop_list = c(paste0('Kaladesh | NonFoil',' ~ ','sold_quantity'))
current_user_favorites = c("Kaladesh | NonFoil","Theros | NonFoil")
current_user_favorites_fields = c("sold_quantity","revenue")
# 
# if(which(current_user_favorites == gsub(' \\~ .*$','',drop_list)) == which(current_user_favorites_fields == str_to_lower(gsub(' ','_',gsub('^.* \\~ ','',drop_list))))){
#     current_user_favorites = current_user_favorites[-which(current_user_favorites == gsub(' \\~ .*$','',drop_list))]
#     current_user_favorites_fields = current_user_favorites_fields[-which(current_user_favorites_fields == str_to_lower(gsub(' ','_',gsub('^.* \\~ ','',drop_list))))]
# }

# Run the damn thing ------------------------------------------------------

shinyApp(ui=my_ui, server=server)

