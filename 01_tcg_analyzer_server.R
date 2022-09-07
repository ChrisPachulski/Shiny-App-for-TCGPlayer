pacman::p_load(shiny,shinyjs,DT,shinyWidgets,plotly,tidyquant,tidyverse,lubridate,anytime,bigrquery,plotly,fs,janitor,zoo,tidyquant)
source('00_scripts/tcg_analysis_functions.R')

gaeas_cradle("wolfoftinstreet@gmail.com")

desired = 'edition'
data_list = NULL
if(tolower(desired) == 'card'){
    data_list[[1]] = tryCatch({read_csv('/Users/cujo253/Desktop/tcg_card_data.csv')},error=function(e){''})
    data_list[[2]] = tryCatch({read_csv('/Users/cujo253/Desktop/tcg_card_labels.csv')},error=function(e){''})
    if(as_tibble( Sys.Date(), .name_repair='unique') != read_csv('/Users/cujo253/Desktop/tcg_card_date.csv')){
        data_list = get_data_list('card');
        write_csv(data_list[[1]] , '/Users/cujo253/Desktop/tcg_card_data.csv');
        write_csv(data_list[[2]] , '/Users/cujo253/Desktop/tcg_card_labels.csv');
        write_csv(as_tibble( Sys.Date(), .name_repair='unique'), '/Users/cujo253/Desktop/tcg_card_date.csv')
    }
}else{
    data_list[[1]] = tryCatch({read_csv('/Users/cujo253/Desktop/tcg_edition_data.csv')},error=function(e){''})
    data_list[[2]] = tryCatch({read_csv('/Users/cujo253/Desktop/tcg_edition_labels.csv')},error=function(e){''})
    if(as_tibble( Sys.Date(), .name_repair='unique') != read_csv('/Users/cujo253/Desktop/tcg_edition_date.csv')){
        data_list = get_data_list('edition');
        write_csv(data_list[[1]] , '/Users/cujo253/Desktop/tcg_edition_data.csv');
        write_csv(data_list[[2]] , '/Users/cujo253/Desktop/tcg_edition_labels.csv');
        write_csv(as_tibble( Sys.Date(), .name_repair='unique'), '/Users/cujo253/Desktop/tcg_edition_date.csv')
    }  
}

field_options = get_field_of_interest_options()
# data_list = get_data_list('edition')
# write_csv(data_list[[1]],'/Users/cujo253/Desktop/tcg_edition_data.csv')
# write_csv(data_list[[2]],'/Users/cujo253/Desktop/tcg_edition_labels.csv')
# write_csv(as_tibble( Sys.Date(), .name_repair='unique'), '/Users/cujo253/Desktop/tcg_edition_date.csv')

# UI - FluidPAge
my_ui = fluidPage(
    title="TCG Analyzer",
                  #1.0 Header ----
  div(
    class = "container", 
    column(
        width = 10,
        h1(class="page-header", 'TCGplayer Analyzer',  tags$small("by WolfOfTinStreet") ),
    ),
    column(
        width = 2,
        img(class = 'img-circle img-responsive', src='wolf.png', style = 'width:100px;') %>% a(href = 'https://twitter.com/WolfOfTinStreet')
    )
  ),#2.0 application
  div(
      column(
          width=10, 
             wellPanel(
                 pickerInput(
                     inputId = "edition_card_user_input", 
                     label = 'Select An Option To Review',
                     choices = data_list[[2]]$label,
                     multiple = FALSE,
                     selected = 'Double Masters 2022 | NonFoil',
                     options = pickerOptions(
                         actionsBox = FALSE,
                         liveSearch = TRUE,
                         size = 5
                     ) 
                ),
                pickerInput(
                    inputId = "user_field_of_interest_input", 
                    label = 'Pick A Numeric Field To Review',
                    choices = field_options$user_labels,
                    multiple = FALSE,
                    selected = gsub(" ", '_',tolower('Sold Quantity')),
                    options = pickerOptions(
                        actionsBox = FALSE,
                        size = 3
                    ) 
                ),
            actionButton(inputId = 'peruse',
                         label = 'Peruse',
                         icon = icon('book-open')),
            hr(),
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
            )
        ),
      column(width=12, 
             div(
                div(h4(textOutput(outputId = 'plot_header'))),
                div(
                    #verbatimTextOutput(outputId='data_tbl')
                    plotlyOutput(outputId = 'plotly_plot')
                        
                )
             )
        )
  )
  
)            

# Server 
server = function(input, output, session){
    
    # User Input 1 ---- Macro Selection
    macro_selection = eventReactive(input$peruse,{
        paste0(unlist(get_input_from_user_input(data = data_list[[1]], input = input$edition_card_user_input)),collapse=" | ")
    }, ignoreNULL = F)
    
    
    # User Input 2 ---- Micro Selection
    field_selection = eventReactive(input$peruse,{
        input$user_field_of_interest_input
    }, ignoreNULL = F)
    
    # Data Acquisition/Formatting
    data_tbl = eventReactive(input$peruse,{
        
        paste0(unlist(get_input_from_user_input(data = data_list[[1]], input = macro_selection())),collapse=" | ") %>% 
            get_input_from_user_input(data = data_list[[1]], .) %>%
            get_user_targeted_data(data = data_list[[1]], needed_input= . , from = today() - days(365), to = today() ) %>%
            get_user_targeted_field(data = ., 
                                    column = gsub(" ", '_',tolower(field_selection())), 
                                    mavg_short=input$mavg_short,
                                    mavg_long=input$mavg_long)
            
    })
    
    
    # Plotting Measures ----
    plot_header = eventReactive(input$peruse,{
        paste0(gsub(' \\| ',' ~ ',input$edition_card_user_input)," ~ ",input$user_field_of_interest_input)
    }, ignoreNULL = F)
    
    output$plot_header = renderText({
        plot_header()
    })
    
    output$plotly_plot = renderPlotly({
        data_tbl() %>% framing_for_graph(.,needed_input = gsub(" ", '_',tolower(field_selection())))
    })

}


# Run the damn thing ------------------------------------------------------

shinyApp(ui=my_ui, server=server)

