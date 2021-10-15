library(tidyverse)
library(geobr)
library(sf)
library(rgdal)

alv <- read_csv2("base_alvara.CSV")
alv$ATIVIDADE_PRINCIPAL = as.factor(alv$ATIVIDADE_PRINCIPAL)

alv = alv %>% 
    group_by(ATIVIDADE_PRINCIPAL) %>% 
    mutate(count = n())

alv = subset(alv, alv$count > 100)

cep = read.table("cep_curitiba.txt", header = TRUE, sep = " ", dec = ".")
cep$CEP = cep$cep
cep$cep = NULL

bairros = geobr::read_neighborhood()
bairros = subset(bairros, bairros$name_muni == "Curitiba")

alv = merge(alv, cep, by = "CEP", all.x = TRUE)

mun_cwb <- read_municipality(code_muni=4106902, year=2017)
cwb = st_as_sf(mun_cwb)

no_axis <- theme(axis.title=element_blank(),
                 axis.ticks=element_blank())

server <- function(input, output) {

        subset_sector <- function() {
            
            licenses = alv[alv$ATIVIDADE_PRINCIPAL==input$Setor,]
            licenses = licenses[!is.na(licenses$long),]
            licenses_sf <- st_as_sf(licenses, coords = c("long", "lat"), crs =4674)
            
            licenses_coord  <- licenses_sf %>%
                cbind(., st_coordinates(licenses_sf))
            
            return(list(licenses_sf, licenses_coord))
        }

        
        plot1 <- reactive({
            
            obj_list <- subset_sector()
            
            ggplot() +
                geom_sf(data=mun_cwb, color= NA, size=.15, fill = NA)+
                geom_sf(data=bairros, size=.15, fill = NA) + 
                geom_sf(data=obj_list[[1]]) + 
                stat_density_2d(data = obj_list[[2]], aes(X, Y, alpha= ..level.., fill= ..level..), colour=FALSE,
                                geom="polygon", bins=20)  +
                geom_density_2d(aes(X, Y), data=obj_list[[2]], colour = "white", alpha = .4) +
                scale_fill_distiller(palette = "Spectral") + scale_alpha(range=c(0,.4)) +
                theme_minimal() + no_axis
        })
        
        plot2 <- reactive({
           
            obj_list <- subset_sector()
            
            bairros$licenses = lengths(st_intersects(bairros, obj_list[[1]]))
            
            ggplot() +
                geom_sf(data=mun_cwb, color= NA, size=.15, fill = NA)+
                geom_sf(data=bairros, size=.15, aes(fill = licenses)) +
                theme_minimal() + no_axis
        })
        
        plot3 <-  reactive({
            
            obj_list <- subset_sector()
            
            hex_points <- spsample(as_Spatial(cwb), type = "hexagonal", cellsize = 0.008)
            
            hex_polygons <- HexPoints2SpatialPolygons(hex_points) 
            hex_polygons = as(hex_polygons , "sf")
            
            
            hex_polygons = hex_polygons %>%
                st_as_sf() %>%
                st_transform(4674) 
            
            
            hex_polygons$licenses = lengths(st_intersects(hex_polygons, obj_list[[1]]))
            
            
            ggplot() +
                geom_sf(data=hex_polygons, size=.1, aes(fill = licenses)) +
                scale_alpha(range=c(0,.4)) +
                scale_fill_viridis_c(option = "magma") +
                theme_minimal() + no_axis +
                theme(legend.title = element_blank())
        })
        
        # Return the requested graph
        graphInput <- reactive({
            switch(input$graph,
                   "Heatmap" = plot1(),
                   "Cloropleth" = plot2(),
                   "Hexagonal" = plot3()
            )
        })
        
        output$selected_graph <- renderPlot({ 
            graphInput()
        }, height = 1100, width = 800)
        
    }


ui <- fluidPage(
    titlePanel("Company Licenses Curitiba"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput("Setor",
                           "Economic sector",
                           choices = unique(alv$ATIVIDADE_PRINCIPAL)),
            selectInput("graph", "Plot type:", 
                        choices = c("Heatmap", "Cloropleth", "Hexagonal"))
        ), # end of sidebar Panel 
        mainPanel(
            plotOutput("selected_graph", width = "100%")
        )  # end of mainPanel
    )
)

# Run the application 
shinyApp(ui = ui, server = server)