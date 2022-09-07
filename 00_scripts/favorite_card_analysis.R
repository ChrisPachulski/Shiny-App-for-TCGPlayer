# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - FAVORITE CARD ANALYSIS -----
# Version 1

# APPLICATION DESCRIPTION ----
# - The user will select 1 stock from the SP 500 stock index
# - [DONE] The functionality is designed to pull the past 180 days of stock data 
# - We will conver the historic data to 2 moving averages - short (fast) and long (slow)
# - We will make a function to generate the moving average cards

# SETUP ----
library(tidyquant)
library(tidyverse)
library(bigrquery)
library(shiny)

source("00_scripts/tcg_analysis_functions.R")
source("00_scripts/shiny_functions.R")

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




favorites <- c("Double Masters 2022 | NonFoil", "Kaldheim | NonFoil", "Dominaria | NonFoil")


# 2.0 Get Moving Average Data for Each Stock History ----

get_favorites_mavg_info_tbl = function(data){
    n_short = data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
    n_long= data %>% pull(mavg_long) %>% is.na() %>% sum() + 1
    
    data_sub_tbl = data %>%
        tail(1) %>%
        mutate(mavg_warning_flag = mavg_short < mavg_long) %>%
        mutate( n_short = n_short,
                n_long = n_long,
                pct_chg = (mavg_short-mavg_long)/mavg_short
                )
    
    return(data_sub_tbl)
}

generate_favorite_card = function(data, seen_column){
    seen_column = str_to_title(gsub('_',' ',seen_column))
    column(
        width = 3,
        info_card(
            title = str_glue('{data$id_column} ~ {seen_column}'),
            value = str_glue("{data$n_short}-Day <small>vs {data$n_long}-Day</small>") %>% HTML(),
            sub_value = data$pct_chg %>% scales::percent(),
            sub_text_color = ifelse(data$mavg_warning_flag, "danger","success" ),
            sub_icon = ifelse(data$mavg_warning_flag, "arrow-down","arrow-up" )
        )
    )
}

favorites =  c("Kaladesh | NonFoil","Kaladesh | NonFoil")
column =  c("sold_quantity","revenue")

favorites =  c("Double Masters 2022 | NonFoil")
column =  c("sold_quantity")

generate_favorite_cards = function(favorites,
                                   column = 'sold_quantity',
                                   from = NULL,
                                   to = NULL,
                                   mavg_short=15,
                                   mavg_long = 30){
    if(!is.null(column)){
        if(length(column) > 1){
            seen_column = syms(column)
        }else{
            seen_column = sym(column)
        }
    }
    
    selection_favorites_tbl = NULL
    
    if(length(column) == 1){
    selection_favorites_tbl = favorites %>%
        get_input_from_user_input(data = data_list[[1]]) %>%
        get_user_targeted_data(data = data_list[[1]], needed_input = ., from = from, to = to) %>%
        get_user_targeted_field(.,column = seen_column, mavg_short = mavg_short, mavg_long = mavg_long) 
    
    data_sub_tbl = selection_favorites_tbl %>%
        filter(id_column ==  favorites) %>%
        filter(id_field ==  seen_column) %>%
        fill(mavg_short,.direction=c('down')) %>%
        fill(mavg_long,.direction=c('down'))
    
    favorites_mavg_info_tbl = get_favorites_mavg_info_tbl(data = data_sub_tbl)
    
    }else{
        suppressWarnings(for(i in 1:length(column)){
            selection_favorites_binding_tbl = favorites %>%
                get_input_from_user_input(data = data_list[[1]]) %>%
                get_user_targeted_data(data = data_list[[1]], needed_input = ., from = from, to = to) %>%
                get_user_targeted_field(.,column = seen_column[i][[1]], mavg_short = mavg_short, mavg_long = mavg_long) 
            
            selection_favorites_tbl = suppressMessages(selection_favorites_tbl %>% bind_rows(selection_favorites_binding_tbl))
        })
        
        favorites_mavg_info_tbl = NULL

        for(v in 1:length(favorites)){
            
            data_sub_tbl = selection_favorites_tbl %>%
                filter(id_column ==  favorites[v]) %>%
                filter(id_field ==  seen_column[v]) %>%
                fill(mavg_short,.direction=c('down')) %>%
                fill(mavg_long,.direction=c('down'))
            
            binding_df = get_favorites_mavg_info_tbl(data = data_sub_tbl)
            
            favorites_mavg_info_tbl = suppressMessages(favorites_mavg_info_tbl %>% bind_rows(binding_df)) %>% distinct()
            
        }
    }
    
    
    favorites_mavg_info_tbl %>% 
        mutate(id_composite = paste0(id_column,' | ',id_field)) %>%
               split(., f =.$id_composite) %>%
               map(.f = function(data){
                   data %>%
                       generate_favorite_card(.,seen_column = .$id_field )
               }) %>%
               tagList()
    
    
}

generate_favorite_cards(favorites = favorites, column = column, mavg_short = 9)



# 4.0 Generate All Favorite Cards in a TagList ----



# 5.0 Save Functions ----
dump(c("get_favorites_mavg_info_tbl", "generate_favorite_card", 'generate_favorite_cards'), 
     file = "00_scripts/generate_favorite_cards.R", append = FALSE)
