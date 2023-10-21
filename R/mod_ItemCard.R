#' ItemCard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ItemCard_ui <- function(id){
  ns <- NS(id)
  tagList(
  uiOutput(ns("itemcard"))
  )
}

#' ItemCard Server Functions
#'
#' @noRd
mod_ItemCard_server <- function(id,content,selectchoices){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    info<-ns

      output$itemcard<-renderUI({
        tagList(
          f7Card(
            h4(content),
            f7Select(id, NULL , choices = selectchoices, selected = NULL),
            footer = NULL,
            hairlines = F, strong = T, inset = F, tablet = FALSE),
          hr()

)

      })
  }) # Closes moduleServer
}


## To be copied in the UI
# mod_ItemCard_ui("ItemCard_1")

## To be copied in the server
# mod_ItemCard_server("ItemCard_1")
