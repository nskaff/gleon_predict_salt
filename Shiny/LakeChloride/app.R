#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(leafgl)
library(viridisLite)
library(colourvalues)

obs = read_csv('../../LAGOS_prediction/output_data_datout.csv') %>% 
    group_by(lagoslakeid) %>% 
    summarise(gnis_name = first(gnis_name), min = min(Chloride, na.rm = T), max = max(Chloride, na.rm = T), 
              LakeArea = first(LakeArea), lat = first(nhd_lat), long = first(nhd_long)) %>% 
    ungroup() %>% 
    st_as_sf(coords = c("long", "lat"), 
             crs = 4326, agr = "constant") %>% 
    mutate(label = paste0("<b>",gnis_name,"</b>", '<br>',
                       'Lake Area = ', round(LakeArea,1), ' ha', '<br>',
                       'Observed Chloride = ',round(min,2), ' - ',round(max,2),' mg/L'))

df = read_csv('../../LAGOS_prediction/output_data_allLagosPredictions.csv') %>% 
    filter(!lagoslakeid %in% obs$lagoslakeid)
df.sf = df %>% st_as_sf(coords = c("nhd_long", "nhd_lat"), 
          crs = 4326, agr = "constant") %>% 
    mutate(label = paste0("<b>",gnis_name, "</b>",'<br>',
                          'Lake Area = ', round(LakeArea,1), ' ha', '<br>',
                          'Predicted Chloride = ',round(exp(prediction.50),2),' mg/L'))

cols = colour_values_rgb(df.sf$prediction.50, include_alpha = FALSE) / 255


    
    
# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Lake Chloride Predictions"),

    leafletOutput('mymap'),
    
    hr(),
    print("Dugan et al (TBD) Lakes at risk of chloride contamination")
)

# Define server logic required to map
server <- function(input, output) {
    
    output$mymap <- renderLeaflet(({
        leaflet() %>% 
            addProviderTiles(providers$Esri.WorldImagery,
                             options = providerTileOptions(noWrap = TRUE)) %>% 
            
            addGlPoints(data = df.sf, group = 'Predicted', color = cols,
                        popup = 'label') %>% 
            
            addGlPoints(data = obs, group = 'Observed', color = cbind(1,1,1),
                        popup = 'label') %>% 
        
            setView(lng = -80.37, lat = 44.16, zoom = 4) %>% 
     
            addLegend("topright", colors = c('white'), labels = c('Observed'),
                      title = "Observed Lake Chloride (mg/L)",
                      opacity = 1) %>% 
            
            addLegend("topright", pal = colorBin("viridis", exp(df.sf$prediction.50)), 
                      values = df.sf$prediction.50,
                      title = "Predicted Lake Chloride (mg/L)"
            )
        
    }))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
