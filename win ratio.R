library(sp)
library(tigris)
library(rgdal)
library(readr)

vd = voting_districts(state = 'NC')
win_prect <- read_csv("~/Dropbox/College/2017 Spring/STA 160/Gerrymandering/win_prect.csv", 
                      col_types = cols(X1 = col_skip()))

prect_merged = geo_join(vd, win_prect,
                           "VTDST10", "Precinct")

NC_distr_12 = cds[(cds@data$STATEFP == NC_code) 
                  & (cds@data$NAMELSAD =="Congressional District 12") , ] #2015
NC_distr_12_2010 = readOGR("/Users/chenjingqi/Dropbox/College/2017 Spring/STA 160/Gerrymandering/Congressional Districts/NC_Shapefiles/NC_2010_CD12.shp")


pal <- colorQuantile(c('#67a9cf','#f7f7f7', '#ef8a62'), NULL, n=5)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = prect_merged, 
              fillColor = ~pal(prect_merged$Ratio_win), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = prect_merged$VTDST10) %>%
  addPolygons(data = NC_distr_12, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=1,
              weight = 1.5, color="black",
              smoothFactor = 0.5, group="NC_distr_12") %>%
  addPolygons(data =  NC_distr_12_2010, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=1,
              weight = 1.5, color="red",
              smoothFactor = 0.5, group=" NC_distr_12_2010") %>%
  addLegend(pal = pal, 
            values = prect_merged$Ratio_win, 
            position = "bottomright", 
            title = "Ratio Win") %>%
  addLayersControl(
    overlayGroups = c("Ratio Win"),
    options = layersControlOptions(collapsed = FALSE)
  )
