pacman::p_load(shiny,shinyWidgets,plotly,tidyquant,tidyverse)
source('00_scripts/tcg_analysis_functions.R')

gaeas_cradle("wolfoftinstreet@gmail.com")


data_list[[1]] = tryCatch({read_csv('/Users/cujo253/Desktop/tcg_edition_data.csv')},error=function(e){''})
data_list[[2]] = tryCatch({read_csv('/Users/cujo253/Desktop/tcg_edition_labels.csv')},error=function(e){''})
if(as_tibble( Sys.Date(), .name_repair='unique') != read_csv('/Users/cujo253/Desktop/tcg_edition_date.csv')){
    data_list = get_data_list('edition');
    write_csv(as_tibble( Sys.Date(), .name_repair='unique'), '/Users/cujo253/Desktop/tcg_edition_date.csv')
}


# data_list = get_data_list('edition')
# write_csv(data_list[[1]],'/Users/cujo253/Desktop/tcg_edition_data.csv')
# write_csv(data_list[[2]],'/Users/cujo253/Desktop/tcg_edition_labels.csv')
# write_csv(as_tibble( Sys.Date(), .name_repair='unique'), '/Users/cujo253/Desktop/tcg_edition_date.csv')

# UI - FluidPAge
my_ui = fluidPage(
    title="TCG Analyzer",
                  #1.0 Header ----
  div(
    h1('TCGplayer Analyzer',),
    p("by WolfOfTinStreet")
  ),#2.0 application
  div(
      column(
          width=5, 
             wellPanel(
                 pickerInput(
                     inputId = "edition_card_user_input", 
                     label = 'Pick An Option To Review',
                     choices = data_list[[2]]$label,
                     multiple = FALSE,
                     selected = 'Double Masters 2022 | NonFoil',
                     options = pickerOptions(
                         actionsBox = FALSE,
                         liveSearch = TRUE,
                         size = 5
                     ) 
                ),
            actionButton(inputId = 'analyze',
                         label = 'Peruse',
                         icon = icon('book-open'))
            )
        ),
      column(width=7, 
             div(
                div(h4("Placeholder")),
                div(
                    get_user_targeted_field(data = get_user_targeted_data(data = data_list[[1]],
                                                                          needed_input = get_input_from_user_input(data = data_list[[1]],
                                                                                                                   input = 'Double Masters 2022 | NonFoil') ), 
                                            column = 'sold_quantity') %>%
                        framing_for_graph(.,needed_input = 'sold_quantity')
                        
                )
             )
        )
  )
  
)            

# Server 
server = function(input, output, session){
    
}


# Run the damn thing ------------------------------------------------------

shinyApp(ui=my_ui, server=server)

