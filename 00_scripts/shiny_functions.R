info_card = function(title, value, sub_value, 
                     main_icon = 'chart-line', sub_icon = 'arrow-up',
                     bg_color='default', text_color = 'default',
                     sub_text_color = 'success'){
    div(
        class = 'panel panel-default',
        style = "padding: 0px;",
        div(
            class = str_glue("panel-body bg-{bg_color} text-{text_color}"),
            p(class = 'pull right', icon(class = 'fa-3x', main_icon)),
            h4(title),
            h5(value),
            p(
                class = str_glue('text-{sub_text_color}'),
                icon(sub_icon),
                tags$small(sub_value)
            )
        )
    )
}


panel_card = function(title, ..., footer){
    ftr = NULL
    if( !is.null(footer) ) ftr <- div(class='panel-footer', footer)
        div(
            class = "panel",
            div(class="panel-header",
                h4(title)
            ),
            div(
                class = "panel-body",
                ...
                
            ),
           ftr
    )
}