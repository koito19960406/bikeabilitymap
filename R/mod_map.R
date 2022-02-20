#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id){
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
        # put the link to the paper and description
        tags$p("This dashboard is an extension of the research conducted by
               Koichi Ito and Filip Biljecki, so read", 
               tags$a(href="https://www.researchgate.net/publication/354710278_Assessing_bikeability_with_street_view_imagery_and_computer_vision",
                      "this paper on bikeability",), 
               "to learn more!"),
        
        # select variables to show
        selectInput(ns("variable"),
                    "Select the variable to map",
                    choices = column_names,
                    selected = column_names[1]),
        # download button
        downloadButton(ns("download"), 
                       "Download CSV data"),
        # add my twitter & linkedin
        tags$p(tags$br(),
               "If you want to ask questions, contact me via:",
               tags$br(),
               a(img(src="www/LinkedIn_logo.png",width=30,height=30), href="https://www.linkedin.com/in/koichi-ito-651464161/"),
               a(img(src="www/twitter-logo.png",width=30,height=30), href="https://twitter.com/KoichiIto12")
               )
      ),
      mainPanel(    
        # add Singapore map
        tags$h1(textOutput(ns("title_singapore")),align="middle"),
        leaflet::leafletOutput(ns("map_singapore")) %>%
          shinycssloaders::withSpinner(),
        # add Tokyo map
        tags$h1(textOutput(ns("title_tokyo")),align="middle"),
        leaflet::leafletOutput(ns("map_tokyo")) %>%
          shinycssloaders::withSpinner()
        )
    )
  )
}
    
#' map Server Functions
#'
#' @noRd 
mod_map_server <- function(id){
  moduleServer( id, function(input, output, session){
    load(here::here("data/point.rda"))
    ns <- session$ns
    

    # create title ------------------------------------------------------------
    # tokyo
    title_tokyo<-reactive({
      paste0(stringr::str_to_title(stringr::str_replace_all(input[["variable"]],"_"," "))," in Tokyo" )
    })
    output[["title_tokyo"]]<-renderText(title_tokyo())
    # singapore
    title_singapore<-reactive({
      paste0(stringr::str_to_title(stringr::str_replace_all(input[["variable"]],"_"," "))," in Singapore" )
    })
    output[["title_singapore"]]<-renderText(title_singapore())
    
    

    # create a common color scale --------------------------------------------------
    # tokyo
    point_data_tokyo <- point %>% 
        magrittr::extract("tokyo") %>% # extract the city
        magrittr::extract2(1)
    
    # extract respective variable
    point_data_no_geom_tokyo <- reactive({
      point_data_tokyo %>% 
        dplyr::pull(tolower(input[["variable"]]))  # extract the variable
    })
    
    # singapore
    point_data_singapore <- point %>% 
        magrittr::extract("singapore") %>% # extract the city
        magrittr::extract2(1)
    
    # extract respective variable
    point_data_no_geom_singapore <- reactive({
      point_data_singapore %>% 
        dplyr::pull(tolower(input[["variable"]]))  # extract the variable
    })
    
    # concatenate two point data
    point_combined<-reactive({
      c(point_data_no_geom_tokyo(),point_data_no_geom_singapore())
    })
    # tokyo -------------------------------------------------------------------
    #Labelling for the Map    
    labels_tokyo <- reactive({
      paste0(glue::glue("<b>Pano ID</b>: { point_data_tokyo$pano_id } </br>"), 
             glue::glue("<b>{input[['variable']]}: </b>"), 
             " ", 
             glue::glue("{round(point_data_no_geom_tokyo(),2)} <br>")
             ) %>% 
        lapply(htmltools::HTML)                                                                             
    })
    
    # create color bin
    pal_tokyo <- reactive(leaflet::colorNumeric(palette = viridis::magma(256), domain= point_combined()))
    
    map_tokyo<-reactive({
      # map
      point %>% 
        magrittr::extract("tokyo") %>% # extract the city
        magrittr::extract2(1) %>%
        leaflet::leaflet() %>% 
        leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>% 
        leaflet::clearBounds() %>% 
        leaflet::addCircles(label=~labels_tokyo(),
                            color  =  ~pal_tokyo()(point_data_no_geom_tokyo())) %>% 
        leaflet::addLegend("bottomright",
                           pal= pal_tokyo() ,
                           values= ~point_data_no_geom_tokyo(),
                           title = "Legend",
                           opacity= 0.7)
    })
    # render map
    output[["map_tokyo"]] <- leaflet::renderLeaflet({
      map_tokyo()
    })
    

    # singapore ---------------------------------------------------------------
    #Labelling for the Map       
    labels_singapore <- reactive({
      paste0(glue::glue("<b>Pano ID</b>: { point_data_singapore$pano_id } </br>"), 
             glue::glue("<b>{input[['variable']]}: </b>"), 
             " ", 
             glue::glue("{round(point_data_no_geom_singapore(),2)} <br>")
             ) %>% 
        lapply(htmltools::HTML)                                                                             
    })
    
    # create color bin
    pal_singapore <- reactive(leaflet::colorNumeric(palette = viridis::magma(256), domain= point_combined()))
    
    map_singapore<-reactive({
      # map
      point %>% 
        magrittr::extract("singapore") %>% # extract the city
        magrittr::extract2(1) %>%
        leaflet::leaflet() %>% 
        leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>% 
        leaflet::clearBounds() %>% 
        leaflet::addCircles(label=labels_singapore(),
                            color  =  ~pal_singapore()(point_data_no_geom_singapore())) %>% 
        leaflet::addLegend("bottomright",
                           pal= pal_singapore() ,
                           values= ~point_data_no_geom_singapore(),
                           title = "Legend",
                           opacity= 0.7)
    })
    # render map
    output[["map_singapore"]] <- leaflet::renderLeaflet({
      map_singapore()
    })
    

    # download ----------------------------------------------------------------
    # prepare the data
    download_data<-point %>% 
      {
        point_temp<-.
        # concatenate singapore and tokyo points
        point_singapore<-point_temp %>%
          magrittr::extract("singapore") %>% # extract the city
          magrittr::extract2(1) %>% 
          dplyr::mutate(city="Singapore")
        point_tokyo<-point_temp %>%
          magrittr::extract("tokyo") %>% # extract the city
          magrittr::extract2(1) %>% 
          dplyr::mutate(city="Tokyo")
        point_combined<-rbind(point_singapore,point_tokyo)
        # convert geometry to longitude latitude
        lat_lon_df<-sf::st_coordinates(point_combined$geometry) %>% 
          as.data.frame()
        point_combined<-point_combined %>% 
          dplyr::mutate(longitude=lat_lon_df$X,
                        latitude=lat_lon_df$Y) %>% 
          dplyr::select(-c(X,distDiff)) %>% 
          sf::st_drop_geometry()
      }
    
    output[["download"]] <- downloadHandler(
      filename = function(){
        paste0("bikeability", ".csv")
      },
      content = function(file){
        write.csv(download_data, file)
      })
  })
}
## To be copied in the UI
# mod_map_ui("map_ui_1")
    
## To be copied in the server
# mod_map_server("map_ui_1")
