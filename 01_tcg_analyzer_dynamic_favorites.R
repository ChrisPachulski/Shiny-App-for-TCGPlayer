pacman::p_load(shiny,shinyjs,DT,shinyWidgets,shinythemes,plotly,tidyquant,tidyverse,lubridate,anytime,bigrquery,plotly,fs,janitor,zoo,tidyquant)
source('00_scripts/tcg_analysis_functions.R')
source('00_scripts/shiny_functions.R')
source('00_scripts/generate_favorite_cards.R')

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

current_user_favorites = c("Kaladesh | NonFoil","Kaladesh | NonFoil")
current_user_favorites_fields = c("sold_quantity","revenue")

# UI - FluidPAge
my_ui = navbarPage(
    useShinyjs(),
    #icon = img(class = 'img-circle img-responsive', src='wolf.png', style = 'width:100px;') %>% a(href = 'https://twitter.com/WolfOfTinStreet'),
    title="Magic The Gathering Sales",
    inverse = T,
    collapsible = T,
    theme = shinytheme('superhero'),
    tabPanel(
      title = "Edition Analysis",
      
      # Theme Selection ----
      # Test Other Themes
      #shinythemes::themeSelector(),
      
      # Custom CSS Inclusion ----
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
                             selected = 'Double Masters 2022 | NonFoil',
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
                             selected = gsub(" ", '_',tolower('Sold Quantity')),
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
                         sliderInput(inputId = 'mavg_short',
                                     label = 'Short Moving Average',
                                     value = 7,
                                     min = 3,
                                     max = 30),
                         hr(),
                         sliderInput(inputId = 'mavg_long',
                                     label = 'Long Moving Average',
                                     value = 60,
                                     min = 45,
                                     max = 90 )
                     ) %>% shinyjs::hidden()
                )
            ),
          # Plot Panel ----
          column(width=8, 
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
      )
   )
)            

# Server  ----
server = function(input, output, session){

    # User Input 1 ---- Macro Selection
    macro_selection = eventReactive(input$line_plot,{
        paste0(unlist(get_input_from_user_input(data = data_list[[1]], input = input$edition_card_user_input)),collapse=" | ")
    }, ignoreNULL = F)
    
    
    # User Input 2 ---- Micro Selection
    field_selection = eventReactive(input$line_plot,{
        input$user_field_of_interest_input
    }, ignoreNULL = F)
    
    # Data Acquisition/Formatting
    data_tbl = eventReactive(input$line_plot,{
        
        paste0(unlist(get_input_from_user_input(data = data_list[[1]], input = macro_selection())),collapse=" | ") %>% 
            get_input_from_user_input(data = data_list[[1]], .) %>%
            get_user_targeted_data(data = data_list[[1]], needed_input= . , from = today() - days(365), to = today() ) %>%
            get_user_targeted_field(data = ., 
                                    column = gsub(" ", '_',tolower(field_selection())), 
                                    mavg_short=input$mavg_short,
                                    mavg_long=input$mavg_long)
            
    })
    
    
    # Plotting Measures ----
    plot_header = eventReactive(input$line_plot,{
        paste0(gsub(' \\| ',' ~ ',input$edition_card_user_input)," ~ ",input$user_field_of_interest_input)
    }, ignoreNULL = F)
    
    output$plot_header = renderText({
        plot_header()
    })
    
    output$plotly_plot = renderPlotly({
        data_tbl() %>% framing_for_graph(.,needed_input = gsub(" ", '_',tolower(field_selection())))
    })
    # ShinyJS ----
    shinyjs::onclick(id = 'settings_toggle', {
        shinyjs::toggle(id = 'slider_inputs', anim = T)
    })
    # FAVORITES ----
    reactive_values = reactiveValues()
    
    reactive_values$favorites_list = current_user_favorites
    
    output$favorites_print = renderPrint(reactive_values$favorites_list)
    
    # Add Favorites ----
    observeEvent(input$favorites_add, {
        new_value = paste0(unlist(get_input_from_user_input(data = data_list[[1]], input = input$edition_card_user_input)),collapse=" | ")
        reactive_values$favorites_list <- c(reactive_values$favorites_list, new_value) %>% unique()
    })
    
    reactive_values$favorites_field_list = current_user_favorites_fields
    
    observeEvent(input$favorites_add, {
        new_symbol = gsub(' ','_',tolower(input$user_field_of_interest_input))
        reactive_values$favorites_field_list <- c(reactive_values$favorites_field_list, new_symbol) %>% unique()
    })
    
    # Render Favorites ----
    output$favorite_cards = renderUI({
        
        if(length(reactive_values$favorites_list) > 0){
        
        generate_favorite_cards(favorites  = reactive_values$favorites_list ,
                                column     = reactive_values$favorites_field_list,
                                mavg_short = input$mavg_short,
                                mavg_long  = input$mavg_long)
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
    
    observeEvent(input$favorites_toggle,{
        shinyjs::toggle(id='favorite_cards_section', anim = T, animType = 'slide')
    })
    
}

# drop_list = c(paste0('Kaladesh | NonFoil',' ~ ','sold_quantity'))
# current_user_favorites = c("Kaladesh | NonFoil","Theros | NonFoil")
# current_user_favorites_fields = c("sold_quantity","revenue")
# 
# if(which(current_user_favorites == gsub(' \\~ .*$','',drop_list)) == which(current_user_favorites_fields == str_to_lower(gsub(' ','_',gsub('^.* \\~ ','',drop_list))))){
#     current_user_favorites = current_user_favorites[-which(current_user_favorites == gsub(' \\~ .*$','',drop_list))]
#     current_user_favorites_fields = current_user_favorites_fields[-which(current_user_favorites_fields == str_to_lower(gsub(' ','_',gsub('^.* \\~ ','',drop_list))))]
# }

# Run the damn thing ------------------------------------------------------

shinyApp(ui=my_ui, server=server)

