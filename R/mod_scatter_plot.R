#' scatter_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scatter_plot_ui <- function(id){
  ns <- NS(id)
  # get the column names
  column_names<-c("connectivity",
                  "environment",
                  "infrastructure",
                  "vehicle_cyclist_interaction",
                  "perception",
                  "bikeability")
  tagList(
    sidebarLayout(
      sidebarPanel(
        # select variables to show
        selectInput(ns("variable1"),
                    "Select the variable #1",
                    choices = column_names,
                    selected = column_names[1]),
      selectInput(ns("variable2"),
                  "Select the variable #2",
                  choices = column_names,
                  selected = column_names[2])
      ),
      mainPanel(
        # singapore 
        tags$h1("Singapore",align="middle"),
        plotOutput(ns("plot_singapore")),
        # tokyo 
        tags$h1("Tokyo",align="middle"),
        plotOutput(ns("plot_tokyo")),
        )
      )
  )
}
    
#' scatter_plot Server Functions
#'
#' @noRd 
mod_scatter_plot_server <- function(id){
  moduleServer( id, function(input, output, session){
    load(here::here("data/point.rda"))
    ns <- session$ns
    
    # singapore
    plot_singapore<-reactive({
      point %>% 
        magrittr::extract("singapore") %>% # extract the city
        magrittr::extract2(1) %>% 
        {
          point_temp<-.
          var1<-point_temp %>% 
            dplyr::pull(input[["variable1"]])
          var2<-point_temp %>% 
            dplyr::pull(input[["variable2"]])
          point_temp<-point_temp %>% 
            dplyr::mutate(var1=var1,
                   var2=var2)
          point_temp
        } %>% 
        ggplot2::ggplot(.,mapping=ggplot2::aes(x=var1,y=var2)) +
        ggplot2::geom_point()
    })
    output$plot_singapore <- renderPlot({
      plot_singapore()
    })
    
    # tokyo
    plot_tokyo<-reactive({
      point %>% 
        magrittr::extract("tokyo") %>% # extract the city
        magrittr::extract2(1) %>% 
        {
          point_temp<-.
          var1<-point_temp %>% 
            dplyr::pull(input[["variable1"]])
          var2<-point_temp %>% 
            dplyr::pull(input[["variable2"]])
          point_temp<-point_temp %>% 
            dplyr::mutate(var1=var1,
                   var2=var2)
          point_temp
        } %>% 
        ggplot2::ggplot(.,mapping=ggplot2::aes(x=var1,y=var2)) +
        ggplot2::geom_point()
    })
    output$plot_tokyo <- renderPlot({
      plot_tokyo()
    })
  })
}
    
## To be copied in the UI
# mod_scatter_plot_ui("scatter_plot_ui_1")
    
## To be copied in the server
# mod_scatter_plot_server("scatter_plot_ui_1")
