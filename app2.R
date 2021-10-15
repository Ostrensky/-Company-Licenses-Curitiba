library(tidyverse)
library(geobr)
library(sf)
library(rgdal)
library(walrasthetics2)

alv <- read.csv2("base_alvara.CSV")
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
no_axis <- theme(axis.title=element_blank(),
                 axis.ticks=element_blank())


server <- function(input, output) {

        

        
        plot1 <- reactive({
            pizzaria = alv[alv$ATIVIDADE_PRINCIPAL==input$Setor,]
            pizzaria = pizzaria[!is.na(pizzaria$long),]
            pizza <- st_as_sf(pizzaria, coords = c("long", "lat"), crs =4674)
            
            piz  <- pizza %>%
                cbind(., st_coordinates(pizza))
            ggplot() +
                geom_sf(data=mun_cwb, color= NA, size=.15, fill = NA)+
                geom_sf(data=bairros, size=.15, fill = NA) + 
                geom_sf(data=pizza) + 
                stat_density_2d(data = piz, aes(X, Y, alpha= ..level.., fill= ..level..), colour=FALSE,
                                geom="polygon", bins=input$bins)  +
                geom_density_2d(aes(X, Y), data=piz, colour = "white", alpha = .4) +
                scale_fill_distiller(palette = "Spectral") + scale_alpha(range=c(0,.4)) +
                theme_minimal() + no_axis
        })
        
        plot2 <- reactive({
            pizzaria = alv[alv$ATIVIDADE_PRINCIPAL==input$Setor,]
            pizzaria = pizzaria[!is.na(pizzaria$long),]
            pizza <- st_as_sf(pizzaria, coords = c("long", "lat"), crs =4674)
            
            piz  <- pizza %>%
                cbind(., st_coordinates(pizza))
            
            bairros$pizzarias = lengths(st_intersects(bairros, pizza))
            
            ggplot() +
                geom_sf(data=mun_cwb, color= NA, size=.15, fill = NA)+
                geom_sf(data=bairros, size=.15, aes(fill = pizzarias)) +
                scale_fill_walras(palette = "cool", discrete = FALSE) +
                theme_minimal() + no_axis
        })
        
        plot3 <-  reactive({
            
            pizzaria = alv[alv$ATIVIDADE_PRINCIPAL==input$Setor,]
            pizzaria = pizzaria[!is.na(pizzaria$long),]
            pizza <- st_as_sf(pizzaria, coords = c("long", "lat"), crs =4674)
            
            piz  <- pizza %>%
                cbind(., st_coordinates(pizza))
            
            cwb = st_as_sf(mun_cwb)
            
            hex_points <- spsample(as_Spatial(cwb), type = "hexagonal", cellsize = 0.008)
            
            hex_polygons <- HexPoints2SpatialPolygons(hex_points) 
            hex_polygons = as(hex_polygons , "sf")
            
            
            hex_polygons = hex_polygons %>%
                st_as_sf() %>%
                st_transform(4674) 
            
            
            hex_polygons$pizzarias = lengths(st_intersects(hex_polygons, pizza))
            
            
            ggplot() +
                geom_sf(data=hex_polygons, size=.1, aes(fill = pizzarias)) +
                scale_alpha(range=c(0,.4)) +
                scale_fill_viridis_c(option = "magma") +
                theme_minimal() + no_axis +
                theme(legend.title = element_blank())
        })
        
        # Return the requested graph
        graphInput <- reactive({
            switch(input$graph,
                   "Heatmap" = plot1(),
                   "Coropletico" = plot2(),
                   "Hexagonal" = plot3()
            )
        })
        
        output$selected_graph <- renderPlot({ 
            graphInput()
        }, height = 1100, width = 800)
        
    }


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectizeInput("Setor",
                           "Selecione o setor",
                           choices = unique(alv$ATIVIDADE_PRINCIPAL)),
            selectInput("graph", "Escolha um tipo de gráfico:", 
                        choices = c("Heatmap", "Coropletico", "Hexagonal")),
            numericInput("bins", "Bins do Heatmap:", 10, min = 1, max = 100)
        ), # end of sidebar Panel 
        mainPanel(
            plotOutput("selected_graph", width = "150%")
        )  # end of mainPanel
    )
)

# Run the application 
shinyApp(ui = ui, server = server)