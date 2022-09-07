pacman::p_load(tidyverse,lubridate,anytime,bigrquery,plotly,fs,janitor,zoo,tidyquant)
gaeas_cradle <- function(email){
    con <- dbConnect(
        bigrquery::bigquery(),
        project = "gaeas-cradle",
        dataset = "premiums",
        billing = "gaeas-cradle"
    )
    bq_auth(email = email, use_oob = TRUE)
    options(scipen = 20)
    con
}
get_data_list = function(value){
    if(value == 'edition'){
        
        con = gaeas_cradle("wolfoftinstreet@gmail.com")
        
        statement = 'SELECT rdate, date,a.Set,	CASE WHEN version = 1 THEN "Foil" ELSE "NonFoil" END as version, sum(sold_quantity) sold_quantity,	count(dop) orders,	sum(sell_price) revenue,
                    FROM `gaeas-cradle.mtg_basket.*` a 
                    LEFT JOIN (SELECT CAST(tcg_id as FLOAT64) tcg_id, rdate FROM`gaeas-cradle.roster.mtgjson` ) b on a.tcg_id=b.tcg_id
                    WHERE _TABLE_SUFFIX BETWEEN
                        FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 365 DAY)) AND
                        FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 1 DAY)) 
                    GROUP BY 1,2,3,4'
        
        tcgplayer_edition_sales_data <- dbSendQuery(con, statement = statement,page_size=50000) %>% dbFetch(., n = -1) %>% clean_names()
        
        
        list_options = tcgplayer_edition_sales_data %>%
            mutate(label = paste0(set," | ",version)) %>%
            select(label) %>%
            distinct()   %>%
            arrange(label)
        
        return(list(tcgplayer_edition_sales_data,list_options))
        
    }else if(value == 'card'){
 
        con = gaeas_cradle("wolfoftinstreet@gmail.com")
        
        statement = 'SELECT rdate, date,Card_name,a.Set,number,	CASE WHEN version = 1 THEN "Foil" ELSE "NonFoil" END as version, sum(sold_quantity) sold_quantity,	count(dop) orders,	sum(sell_price) revenue,
                    FROM `gaeas-cradle.mtg_basket.*` a 
                    LEFT JOIN (SELECT CAST(tcg_id as FLOAT64) tcg_id, rdate FROM`gaeas-cradle.roster.mtgjson` ) b on a.tcg_id=b.tcg_id
                    WHERE _TABLE_SUFFIX BETWEEN
                        FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 180 DAY)) AND
                        FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 1 DAY)) 
                    GROUP BY 1,2,3,4,5,6'
        
        tcgplayer_card_sales_data <- dbSendQuery(con, statement = statement,page_size=50000) %>% dbFetch(., n = -1) %>% clean_names()
        
        statement = 'SELECT rdate, date,Card_name,a.Set,number,	CASE WHEN version = 1 THEN "Foil" ELSE "NonFoil" END as version, sum(sold_quantity) sold_quantity,	count(dop) orders,	sum(sell_price) revenue,
                    FROM `gaeas-cradle.mtg_basket.*` a 
                    LEFT JOIN (SELECT CAST(tcg_id as FLOAT64) tcg_id, rdate FROM`gaeas-cradle.roster.mtgjson` ) b on a.tcg_id=b.tcg_id
                    WHERE _TABLE_SUFFIX BETWEEN
                        FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 365 DAY)) AND
                        FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 181 DAY)) 
                    GROUP BY 1,2,3,4,5,6'
        
        tcgplayer_card_sales_data <- tcgplayer_card_sales_data %>% bind_rows(dbSendQuery(con, statement = statement,page_size=50000) %>% dbFetch(., n = -1) %>% clean_names())
        
        
        
        list_options = tcgplayer_card_sales_data %>%
            mutate(label = paste0(card_name," | ",version," | ",number)) %>%
            select(label) %>%
            distinct()   %>%
            arrange(label)
        
        return(list(tcgplayer_card_sales_data,list_options))
        
    }else{
        print('Please choose either "edition" or "card"')
    }
}    

get_data = function() {
    data_list <- NULL
    
    data_list[[1]] <- tryCatch({read_csv('/Users/cujo253/Desktop/tcg_edition_data.csv')},error=function(e){''})
    data_list[[2]] <- tryCatch({read_csv('/Users/cujo253/Desktop/tcg_edition_labels.csv')},error=function(e){''})
    if(as_tibble( Sys.Date(), .name_repair='unique') != read_csv('/Users/cujo253/Desktop/tcg_edition_date.csv')){
        data_list <<- get_data_list('edition');
        write_csv(data_list[[1]] , '/Users/cujo253/Desktop/tcg_edition_data.csv');
        write_csv(data_list[[2]] , '/Users/cujo253/Desktop/tcg_edition_labels.csv');
        write_csv(as_tibble( Sys.Date(), .name_repair='unique'), '/Users/cujo253/Desktop/tcg_edition_date.csv')
    }  
}

data_pull = get_data_list(value='edition')

selection_info = data_list[[2]]

historical_data = data_list[[1]]

get_field_of_interest_options = function(){
    resulting_df = data.frame(
        user_labels = c('Sold Quantity', 'Orders', 'Revenue'),
        data_names = c('sold_quantity','orders','revenue')) %>%
        as_tibble()
    
    return(resulting_df)
}

user_input = trimws("Mirrodin | NonFoil")
#user_input = c("Double Masters 2022 | NonFoil",'Kaldheim | NonFoil', 'Dominaria | NonFoil')
length(user_input)
get_input_from_user_input = function(data= data_list[[1]], input = user_input){
    
    if( (str_detect('card',data %>% colnames()) %>% unique() %>% length())  > 1){
        if(length(input) == 1){
            card_name = input %>% str_split(pattern=' \\| ') %>% pluck(1,1)
            version = input %>% str_split(pattern=' \\| ') %>% pluck(1,2)
            number = input %>% str_split(pattern=' \\| ') %>% pluck(1,3)
            return(list(card_name,version,number))
        }else{
            card_name = NULL
            version = NULL
            number = NULL
            for( v in 1:length(input)){
                card_name = append(card_name, input[v] %>% str_split(pattern=' \\| ') %>% pluck(1,1))
                version =append(card_name, input[v] %>% str_split(pattern=' \\| ') %>% pluck(1,2))
                number = append(card_name, input[v] %>% str_split(pattern=' \\| ') %>% pluck(1,3))
            }
            return(list(card_name,version,number))    
        }
        
    }else if ((str_detect('card',data %>% colnames()) %>% unique() %>% length()) == 1){
        if(length(input) == 1){
            edition = input %>% str_split(pattern=' \\| ') %>% pluck(1,1)
            version = input %>% str_split(pattern=' \\| ') %>% pluck(1,2)
            return(list(edition,version))
        }else{
            edition = NULL
            version = NULL

            for( v in 1:length(input)){
                edition = append(edition, input[v] %>% str_split(pattern=' \\| ') %>% pluck(1,1))
                version =append(version, input[v] %>% str_split(pattern=' \\| ') %>% pluck(1,2))
            }
            return(list(edition,version))    
        }
        
    }else{
        print("Call Doctor Who (The Tenth, Preferably), We Have To Go Back And Save The Analysis!")
    }
    
    
}


acquired_user_input = get_input_from_user_input(input=user_input)

paste0(unlist(acquired_user_input),collapse=" | ")

# Alright Matt, I get to re-create your tq_get a bit, not going to lie I hate these {{}} things...
# I already love pluck, to hell with do.call!!!!
oldest = data_list[[1]] %>% select(date) %>% filter(date==min(date)) %>% distinct() %>% pluck(1)
start = data_list[[1]] %>% select(date) %>% filter(date==max(date)) %>% distinct() %>% pluck(1)

get_user_targeted_data = function(data=data_list[[1]],needed_input=acquired_user_input,  from = NULL,to = NULL){
    if(is.null(from) ){
        from = data_list[[1]] %>% select(date) %>% filter(date==min(date)) %>% distinct() %>% pluck(1)
    }else{
        from = (data_list[[1]] %>% pull(date) %>% max() %>% pluck(1)) - from
    }
    
    
    if(is.null(to) ){
        to = data_list[[1]] %>% select(date) %>% filter(date==max(date)) %>% distinct() %>% pluck(1)
    }else{
        to = (today() - ((today() - (data_list[[1]] %>% pull(date) %>% max() %>% pluck(1)))+to)  )
    }
    
    # I don't know why rdate only half joins and Im too ashamed to actually go be a data engineer on my own db
    input_list_length = (do.call(rbind, lapply(needed_input, lengths)) %>% as_tibble() %>% ncol())
    if( input_list_length == 1){
        if(length(needed_input)<=3){
        resulting_df = data %>%
            filter(set == needed_input[[1]][1]) %>%
            filter(version == needed_input[[2]][1]) %>%
            filter(date >= {{from}}) %>%
            filter(date <= {{to}}) %>%
            arrange(desc(date)) %>%
            fill(rdate, .direction = "updown") %>%
            group_by(rdate, date, set, version) %>%
            summarize(sold_quantity = sum(sold_quantity),
                      orders= sum(orders),
                      revenue = sum(revenue)) %>%
            ungroup()
        }else{
            resulting_df = data %>%
                filter(card_name == needed_input[[1]][1]) %>%
                filter(version == needed_input[[2]][1]) %>%
                filter(number == needed_input[[3]][1]) %>%
                filter(date >= {{from}}) %>%
                filter(date <= {{to}})  %>%
                arrange(desc(date)) %>%
                fill(rdate, .direction = "updown") %>%
                group_by(rdate, date, card_name,set,number, version) %>%
                summarize(sold_quantity = sum(sold_quantity),
                          orders= sum(orders),
                          revenue = sum(revenue)) %>%
                ungroup()
        }
    }else{
        resulting_df = NULL
        if(length(needed_input)<=3){
            for(v in 1:input_list_length){
                binding_df = data %>%
                    filter(set == needed_input[[1]][v]) %>%
                    filter(version == needed_input[[2]][v]) %>%
                    filter(date >= {{from}}) %>%
                    filter(date <= {{to}})  %>%
                    arrange(desc(date)) %>%
                    fill(rdate, .direction = "updown") %>%
                    group_by(rdate, date, set, version) %>%
                    summarize(sold_quantity = sum(sold_quantity),
                              orders= sum(orders),
                              revenue = sum(revenue)) %>%
                    ungroup()
                resulting_df = suppressMessages(resulting_df %>% bind_rows(binding_df))
            }
        }else{
            for(v in 1:input_list_length){
                binding_df = data %>%
                filter(card_name == needed_input[[1]][v]) %>%
                filter(version == needed_input[[2]][v]) %>%
                filter(number == needed_input[[3]][v]) %>%
                filter(date >= {{from}}) %>%
                filter(date <= {{to}})  %>%
                arrange(desc(date)) %>%
                fill(rdate, .direction = "updown") %>%
                group_by(rdate, date, card_name,set,number, version) %>%
                summarize(sold_quantity = sum(sold_quantity),
                          orders= sum(orders),
                          revenue = sum(revenue)) %>%
                ungroup()
            
            resulting_df = suppressMessages(resulting_df %>% bind_rows(binding_df))
            }
            #return(resulting_df)
        } 
    }
    
    return(resulting_df)
}

user_selected_base_data = get_user_targeted_data(data = data_list[[1]])

user_selected_base_data %>% mutate_if(is.character,as.factor) %>% summary()

user_input_field_of_interest = "revenue"

# BY FAR ~ The hardest function to write

get_user_targeted_field=function(data = user_selected_base_data, column=user_input_field_of_interest, mavg_short=7, mavg_long=30){
    
    seen_column_value = sym(column)
    
    if( (str_detect('card',data %>% colnames()) %>% unique() %>% length())  == 1){
        rolodex = data %>% select(set,version) %>% distinct()
        test_for_multiple = data %>% mutate(id_column = paste0(set, ' | ',version)) %>% select(id_column) %>% distinct() %>% nrow()
    }else{
        rolodex = data %>% select(card_name,version,number) %>% distinct()
        test_for_multiple = data %>% mutate(id_column = paste0(card_name, ' | ',version,' | ',number))%>% select(id_column) %>% distinct() %>% nrow()
    }
    
    if(test_for_multiple == 1){
        subset_data = data %>% 
            select(date,seen_column_value) 
        
        # You made me read documentation. Did I learn something? Yes. Am I happy about it?! Not right now.
        # Zoo feels like choose your own adventure python style...
        zoo_numeric_values = subset_data %>% select(seen_column_value) %>% as.vector() %>% unlist(use.names = F)
        
        x.Date <- subset_data$date %>% as.vector()
        x <- zoo(zoo_numeric_values,x.Date)
        
        # I am god damn doctor strange.
        short_df = cbind(subset_data,rollmean(x = x,k=mavg_short,fill=T,align="right") %>% as_tibble() %>% rename("mavg_short"="value"))
        long_df = cbind(short_df,rollmean(x = x,k=mavg_long,fill=T,align="right") %>% as_tibble() %>% rename("mavg_long"="value"))
    
        resulting_df = long_df %>% as_tibble() %>%
            mutate(mavg_short = round(ifelse(mavg_short==1,NA,mavg_short),0),
                   mavg_long = round(ifelse(mavg_long==1,NA,mavg_long),0)) %>%
            rename(original=seen_column_value)%>%
            mutate(id_column = paste0(rolodex$set, ' | ',rolodex$version),
                   id_field =  as.character(seen_column_value)) %>%
            select(date,id_column,id_field,original,mavg_short,mavg_long)
        
    }else{
        resulting_df = NULL
        for(v in 1:test_for_multiple){
            if( (str_detect('card',data %>% colnames()) %>% unique() %>% length())  == 1){
                subset_data = data %>% 
                    filter(set == rolodex$set[v])%>% 
                    filter(version == rolodex$version[v]) %>% 
                    arrange(date) %>%
                    select(date,seen_column_value) 
                
                # You made me read documentation. Did I learn something? Yes. Am I happy about it?! Not right now.
                # Zoo feels like choose your own adventure python style...
                zoo_numeric_values = subset_data %>% select(seen_column_value) %>% as.vector() %>% unlist(use.names = F)
                
                x.Date <- subset_data$date %>% as.vector()
                x <- zoo(zoo_numeric_values,x.Date)
                
                # I am god damn doctor strange.
                short_df = cbind(subset_data,rollmean(x = x,k=mavg_short,fill=T,align="right") %>% as_tibble() %>% rename("mavg_short"="value"))
                long_df = cbind(short_df,rollmean(x = x,k=mavg_long,fill=T,align="right") %>% as_tibble() %>% rename("mavg_long"="value"))
                
                binding_df = long_df %>% as_tibble() %>%
                    mutate(mavg_short = round(ifelse(mavg_short==1,NA,mavg_short),0),
                           mavg_long = round(ifelse(mavg_long==1,NA,mavg_long),0)) %>%
                    rename(original=seen_column_value) %>%
                    mutate(id_column = paste0(rolodex$set[v], ' | ',rolodex$version[v]),
                           id_field =  as.character(seen_column_value)) %>%
                    select(date,id_column,id_field,original,mavg_short,mavg_long)
                
                resulting_df = suppressMessages(resulting_df %>% bind_rows(binding_df))
            }else{
                subset_data = data %>% 
                    filter(card_name == rolodex$card_name[v])%>% 
                    filter(version == rolodex$version[v]) %>% 
                    filter(version == rolodex$number[v]) %>% 
                    arrange(date) %>%
                    select(date,seen_column_value) 
                
                # You made me read documentation. Did I learn something? Yes. Am I happy about it?! Not right now.
                # Zoo feels like choose your own adventure python style...
                zoo_numeric_values = subset_data %>% select(seen_column_value) %>% as.vector() %>% unlist(use.names = F)
                
                x.Date <- subset_data$date %>% as.vector()
                x <- zoo(zoo_numeric_values,x.Date)
                
                # I am god damn doctor strange.
                short_df = cbind(subset_data,rollmean(x = x,k=mavg_short,fill=T,align="right") %>% as_tibble() %>% rename("mavg_short"="value"))
                long_df = cbind(short_df,rollmean(x = x,k=mavg_long,fill=T,align="right") %>% as_tibble() %>% rename("mavg_long"="value"))
                
                binding_df = long_df %>% as_tibble() %>%
                    mutate(mavg_short = round(ifelse(mavg_short==1,NA,mavg_short),0),
                           mavg_long = round(ifelse(mavg_long==1,NA,mavg_long),0)) %>%
                    rename(original=seen_column_value) %>%
                    mutate(id_column = paste0(rolodex$card_name[v], ' | ',rolodex$version[v],' | ',rolodex$number[v]),
                           id_field =  as.character(seen_column_value)) %>%
                    select(date,id_column,id_field,original,mavg_short,mavg_long)
                
                resulting_df = suppressMessages(resulting_df %>% bind_rows(binding_df))
            }
        }
    }
    
    
    return(resulting_df)
}



fundamental_input_selected_tbl = get_user_targeted_field(data=user_selected_base_data, user_input_field_of_interest)

framing_for_graph = function(data = fundamental_input_selected_tbl, needed_input = user_input_field_of_interest){
    # Worth every second of extra processing.
    haskellers_ire = needed_input
    
    staging_graph = data %>%
        gather(key = 'legend', value = 'value', original:mavg_long, factor_key=T) %>%
        ggplot(aes(date,value,color=legend,group=legend)) +
        geom_line(aes(linetype=legend)) +
        theme_tq()
    
    # Come at me you haskell folks. You're god damn right I reassigned it with a purpose. 
    # We'll end the conversation there, for reasons.
    if(haskellers_ire == 'revenue'){
        staging_graph = staging_graph + scale_y_continuous(labels = scales::dollar_format())
    }
    
    staging_graph = staging_graph +
        scale_color_tq() +
        labs(y = str_to_title(gsub("_"," ",haskellers_ire)), x = "" ) +
        theme(axis.title.y = element_text(angle=0,vjust = 0.5),)
    
    interactive_plot = ggplotly(staging_graph)
    
    return(interactive_plot)
    
}

framing_for_graph(data = fundamental_input_selected_tbl, needed_input = user_input_field_of_interest)


generate_commentary = function(data = fundamental_input_selected_tbl){
    warning_signal = data %>%
        tail(1) %>%
        mutate(mavg_warning_flag = mavg_long < mavg_short) %>%
        pull(mavg_warning_flag)
    
    n_short = fundamental_input_selected_tbl %>% pull(mavg_short) %>% is.na() %>% sum() + 1
    n_long = fundamental_input_selected_tbl %>% pull(mavg_long) %>% is.na() %>% sum() + 1
    
    if(warning_signal == T){
        str_glue("In reviewing the {user_input_field_of_interest} for {user_input}, 
             the {n_short}-day moving average has surpassed the {n_long}-day moving average. 
             The hype as worn off, and the set is entering more consistent territory for long term gains.")
    }else{
        str_glue("In reviewing the {user_input_field_of_interest} for {user_input}, 
             the {n_short}-moving average has is lower than the {n_long}-day moving average. 
             The hype as worn off, and the set is entering more consistent territory for long term gains.")
    }

}


generate_commentary(fundamental_input_selected_tbl)



    

#fs::dir_create("00_scripts")
dump(list = c('gaeas_cradle','get_data','get_data_list','get_field_of_interest_options','get_input_from_user_input','get_user_targeted_data','get_user_targeted_field','framing_for_graph','generate_commentary'),
     file = '00_scripts/tcg_analysis_functions.R',append=F)
