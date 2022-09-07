pacman::p_load(shiny,shinyjs,DT,shinyWidgets,shinythemes,plotly,tidyquant,tidyverse,lubridate,anytime,bigrquery,plotly,fs,janitor,zoo,tidyquant)
source('00_scripts/tcg_analysis_functions.R')
source('00_scripts/shiny_functions.R')

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
                  h5("Favorites")
              )
          ),
          div(
              class = '',
              id = 'favorite_selections',
              column(
                  width = 4,
                  info_card(title = 'Double Masters 2022',value = HTML('20-Day <small>50-day</small>'),
                            sub_value = '20%')
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
                                      icon = icon(class = "fa-2x",'chart-line')),
                        
                         div(
                             class = "pull-right",
                             actionButton(inputId = 'settings_toggle',
                                          label = NULL,
                                          class = 'btn-warning',
                                          icon = icon(class = 'fa-2x', 'gears'))
                         )
                     ),
                     # Plot Elements ----
                     div(
                         id = "slider_inputs",
                         sliderInput(inputId = 'mavg_short',
                                     label = 'Short Moving Average',
                                     value = 20,
                                     min = 5,
                                     max = 40),
                         hr(),
                         sliderInput(inputId = 'mavg_long',
                                     label = 'Long Moving Average',
                                     value = 60,
                                     min = 40,
                                     max = 180)
                     ) %>% shinyjs::hidden()
                )
            ),
          column(width=8, 
                 div(
                     class = "panel",
                    div(class="panel-header",
                        h4(textOutput(outputId = 'plot_header'))),
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
}


# Run the damn thing ------------------------------------------------------

shinyApp(ui=my_ui, server=server)

