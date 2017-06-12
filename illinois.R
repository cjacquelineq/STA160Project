
## Illinois
library(sp)
library(tigris)
# Counties that overlap with district 4
county_list = c("Cook", "DuPage", "Will County")
ill_tracts <- tracts(state = 'IL', 
                     county = county_list)

library(acs)
# This only need to be done once!
api.key.install(key = "")

pop_hisp = acs.fetch(geo=geo.make(state="IL",
                                  county=county_list, tract="*"),
                     endyear = 2010, dataset="sf1",
                     variable=c("P0010001", "P0040003"))

col1 = pop_hisp@geography$NAME
col2 = (pop_hisp@estimate[,"P0040003"] 
        / pop_hisp@estimate[,"P0010001"])
hispanic_df <- data.frame(col1, col2)
colnames(hispanic_df) <- c("NAMELSAD", "perc_hispanic")

hispanic_merged = geo_join(ill_tracts, hispanic_df,
                           "NAMELSAD", "NAMELSAD")

cds = congressional_districts()

#Keep District Four from Illinois
ill_code = "17"
distr_four = cds[(cds@data$STATEFP == ill_code) 
                 & (cds@data$NAMELSAD =="Congressional District 4") , ]


library(leaflet)

pal <- colorQuantile("Oranges", NULL, n=5)

popup <- paste0("<span style='color:#7f0000'><strong>
                Percentage Hispanic: </strong></span>", 
                as.character(round(hispanic_merged$perc_hispanic,
                                   digit=3)))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = hispanic_merged, 
              fillColor = ~pal(hispanic_merged$perc_hispanic), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup) %>%
  addPolygons(data = distr_four, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=1,
              weight = 1.5, color="black",
              smoothFactor = 0.5, group="distr_four") %>%
  addLegend(pal = pal, 
            values = hispanic_merged$perc_hispanic, 
            position = "bottomright", 
            title = "Percentage Hispanic") %>%
  addLayersControl(
    overlayGroups = c("distr_four"),
    options = layersControlOptions(collapsed = FALSE)
  )

