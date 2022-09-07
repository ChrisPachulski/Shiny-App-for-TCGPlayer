pacman::p_load(mongolite,jsonlite,config,tidyverse,lubridate,anytime)



# Connection to Remote NOSQL ----------------------------------------------

Sys.setenv(R_CONFIG_ACTIVE= 'default')

config <- config::get(file = 'config.yaml')


mongo_connect = function(collection, database, 
              host = config$host,
              username = config$username,
              password = config$password) {
    mongo(
        collection = collection,
        url = str_glue('mongodb+srv://{username}:{password}@{host}/{database}')
    )
    
}

mongo_connection = mongo_connect(
    database = 'tolarian_academy',
    collection = 'user_base'
)

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
#                      tibble(mavg_short = 3, mavg_long = 25, time_window = 45)),
#     account_created = c(ymd_hms('2020-04-01 16:42:12'), ymd_hms('2022-09-01 20:10:12'))
# )
# 
# 
# user_base_tbl %>% toJSON(POSIXt = 'mongo') %>% prettify()
# 
# mongo_connection$insert(user_base_tbl)

mongo_read_user_base <- function(database = 'tolarian_academy', collection = 'user_base_dev', 
                                 host = config$host,
                                 username = config$username,
                                 password = config$password) {
    
    mongo_connection <- mongo_connect(
        database   = database,
        collection = collection,
        host       = host,
        username   = username,
        password   = password
        )
    
    user_base_tbl <<- mongo_connection$find() %>% as_tibble()
    
    mongo_connection$disconnect()
}

mongo_read_user_base()


mongo_update_user_base <- function(user_name, column_name, assign_input,
                                   database   = 'tolarian_academy',
                                   collection = 'user_base_dev', 
                                   host       = config$host,
                                   username   = config$username,
                                   password   = config$password) {
    
    user_base_tbl[user_base_tbl$user == user_name,][[column_name]] <<- assign_input
    
    mongo_connection <- mongo_connect(
        database   = database,
        collection = collection,
        host       = config$host,
        username   = config$username,
        password   = config$password
    )
    
    query_string = str_c('{"user": "',user_name, '"}')
    
    update_string = user_base_tbl %>%
        filter( user == user_name) %>%
        select(-user,-password,-permissions) %>%
        toJSON(POSIXt = 'mongo') %>%
        str_remove_all(pattern = "^\\[|\\]$")
    
    mongo_connection$update(
        query = query_string,
        update = str_c('{"$set" : ', update_string, ' }')
    )
    
    print('Update Success!')
    
    mongo_connection$disconnect()
    
    
}





mongo_connection$find()

user_base_tbl$user_setting

# 
# mongo_update_and_write_user_base(
#     user_name = 'cujo253',
#     column_name = 'user_setting',
#     assign_input = list(tibble(
#         mavg_short  = 9,
#         mavg_long   = 40,
#         time_window = 30
#     ))
# )


dump(c('mongo_connect', 'mongo_read_user_base', 'mongo_update_user_base'),
     file = '00_scripts//crud_operations_mongodb.R', append = FALSE)
