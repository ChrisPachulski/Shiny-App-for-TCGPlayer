get_favorites_mavg_info_tbl <-
function(data){
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
generate_favorite_card <-
function(data, seen_column){
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
generate_favorite_cards <-
function(favorites,
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
