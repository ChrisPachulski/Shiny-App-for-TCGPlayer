library(tidyverse)

read_user_base <- function() {
    user_base_tbl <<- read_rds("00_data_local/user_base_tbl.rds")
}

update_and_write_user_base = function(user_name, column_name, assign_input ) {
    user_base_tbl[user_base_tbl$user == user_name,][[column_name]] <<- assign_input
    user_base_tbl %>% write_rds("00_data_local/user_base_tbl.rds")
}


dump(c('read_user_base','update_and_write_user_base'), file = '00_scripts/crud_operations_local.R')


list(tibble(mavg_short = 7,mavg_long = 22,time_window = 30))
update_and_write_user_base(user_name = 'cujo253',column_name = 'favorites', assign_input=c('Theros | NonFoil') )
