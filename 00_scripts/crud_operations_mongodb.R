mongo_connect <-
function(collection, database, 
              host = config$host,
              username = config$username,
              password = config$password) {
    mongo(
        collection = collection,
        url = str_glue('mongodb+srv://{username}:{password}@{host}/{database}')
    )
    
}
mongo_read_user_base <-
function(database = 'tolarian_academy', collection = 'user_base_dev', 
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
mongo_update_user_base <-
function(user_name, column_name, assign_input,
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
