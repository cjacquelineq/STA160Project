library(sp)
library(tigris)
library(rgdal)
library(readr)

# convert into binary values based on party: D = 0(Blue), R = 1 (Red)
district = c('01','02','03','04','05','06','07','08','09','10','11','12','13')
NC_CD08_party_bi = c(0,0,1,0,1,1,0,1,1,1,0,0,0)
NC_CD14_party_bi = c(0,1,1,0,1,1,1,1,1,1,1,0,1)


NC_2008 = data.frame('district'=district,'party' = NC_CD08_party_bi )
NC_2014 = data.frame('district'=district,'party' = NC_CD14_party_bi )



NC_2008_shp = readOGR("/Users/chenjingqi/Dropbox/College/2017 Spring/STA 160/Gerrymandering/Congressional Districts/NC_Shapefiles/cd37_110.shp")
US_2014_shp = readOGR('/Users/chenjingqi/Dropbox/College/2017 Spring/STA 160/Gerrymandering/Congressional Districts/Shapefiles/cb_2013_us_cd113_500k/cb_2013_us_cd113_500k.shp')
NC_2014_shp = US_2014[US_2014$STATEFP == 37,]

cds = congressional_districts()
#cds2011 = congressional_districts(year = 2013)


NC_08_merged = geo_join(NC_2008_shp, NC_2008,
                        "CD", "district")
NC_14_merged = geo_join(NC_2014_shp, NC_2014,
                        "CD113FP", "district")


popup_08 <- paste0("<span style='color:#7f0000'><strong>
                Percentage low income: </strong></span>", 
                   paste('Congressional District',as.character(NC_08_merged$CD)))

pal <- colorNumeric(c('#67a9cf','#f7f7f7', '#ef8a62'), NULL, n=5)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = NC_08_merged, 
              fillColor = ~pal(NC_08_merged$party), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup_08) %>%
  addPolygons(data = NC_2008_shp, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=1,
              weight = 1.5, color="black",
              smoothFactor = 0.5, group="NC_2008_shp") %>%
  addLegend(pal = pal, 
            values = NC_08_merged$party, 
            position = "bottomright", 
            title = "Party") %>%
  addLayersControl(
    overlayGroups = c("Party 2008"),
    options = layersControlOptions(collapsed = FALSE)
  )






pal <- colorNumeric(c('#67a9cf','#f7f7f7', '#ef8a62'), NULL, n=5)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = NC_14_merged, 
              fillColor = ~pal(NC_14_merged$party), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = NC_14_merged$CD) %>%
  addPolygons(data = NC_2014_shp, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=1,
              weight = 1.5, color="black",
              smoothFactor = 0.5, group="NC_2014_shp") %>%
  addLegend(pal = pal, 
            values = NC_14_merged$party, 
            position = "bottomright", 
            title = "Party") %>%
  addLayersControl(
    overlayGroups = c("Party 2014"),
    options = layersControlOptions(collapsed = FALSE)
  )

