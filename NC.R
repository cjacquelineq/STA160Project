##NC 12 Black

library(sp)
library(tigris)
#https://github.com/walkerke/tigris
library(rgdal)
# Counties that overlap with district 12

county_list = c('Mecklenburg','Cabarrus','Rowan','Davidson','Forsyth','Guilford')

NC_tracts <- tracts(state = 'NC', 
                     county = county_list)
NC_all_tracts <- tracts(state = 'NC')



vd = voting_districts(state = 'NC')

#plot(vd)
#head(vd,1)
#length(vd)

library(acs)
#https://cran.r-project.org/web/packages/acs/acs.pdf
#http://eglenn.scripts.mit.edu/citystate/wp-content/uploads/2013/06/wpid-working_with_acs_R3.pdf 
# This only need to be done once!

#acs variables: http://api.census.gov/data/2015/acs5/variables.html
api.key.install(key="72df351a17d37225f8248b61ae15ce2c69162d6c")

NC_pop_bl = acs.fetch(geo=geo.make(state="NC",
                                   county=county_list,tract="*"),
                     endyear = 2010, dataset="sf1",
                     variable=c("P0010001", "P0060003"))
NC_all_pop_bl = acs.fetch(geo=geo.make(state="NC",county = "*",tract="*"),
                      endyear = 2010, dataset="sf1",
                      variable=c("P0010001", "P0060003"))

# P0010001 total populatin
# P0060003 Black or African American alone or in combination with one or more other races

NC_pop_low_inc = acs.fetch(geo=geo.make(state="NC",
                                   county=county_list,tract="*"),
                      endyear = 2010, dataset="acs",span=5,
                      variable=c("B17025_001","B17025_002"))

NC_all_low_inc = acs.fetch(geo=geo.make(state="NC",county = "*",tract="*"),
                          endyear = 2010, dataset="acs",span=5,
                          variable=c("B17025_001","B17025_002"))
# B17025_001 Total:
# B17025_002 Income in the past 12 months below poverty level

NC_pop_high_inc = acs.fetch(geo=geo.make(state="NC",
                                        county=county_list,tract="*"),
                           endyear = 2010, dataset="acs",span=5,
                           variable=c("B17025_001","B17025_009"))
NC_all_high_inc = acs.fetch(geo=geo.make(state="NC",
                                         county="*",tract="*"),
                            endyear = 2010, dataset="acs",span=5,
                            variable=c("B17025_001","B17025_009"))

# B17025_001 Total
# B17025_009 Income in the past 12 months at or above poverty level	


NC_pop_ba_edu = acs.fetch(geo=geo.make(state="NC",
                                         county=county_list,tract="*"),
                            endyear = 2010, dataset="acs",span=5,
                            variable=c("B23006_001","B23006_023"))

NC_all_ba_edu = acs.fetch(geo=geo.make(state="NC",
                                       county="*",tract="*"),
                          endyear = 2010, dataset="acs",span=5,
                          variable=c("B23006_001","B23006_023"))
#B23006_001 Total
#B23006_023 Bachelor's degree or higher

col1 = NC_pop_bl@geography$NAME
col2 = (NC_pop_bl@estimate[,"P0060003"] 
        / NC_pop_bl@estimate[,"P0010001"])
colloinc = (NC_pop_low_inc@estimate[,"B17025_002"] 
        / NC_pop_low_inc@estimate[,"B17025_001"])
colhiinc = (NC_pop_high_inc@estimate[,"B17025_009"] 
          / NC_pop_high_inc@estimate[,"B17025_001"])
colbaedu = (NC_pop_ba_edu@estimate[,"B23006_023"] 
            / NC_pop_ba_edu@estimate[,"B23006_001"])

colblkall = (NC_all_pop_bl@estimate[,"P0060003"] 
        / NC_all_pop_bl@estimate[,"P0010001"])
colloincall = (NC_all_low_inc@estimate[,"B17025_002"] 
            / NC_all_low_inc@estimate[,"B17025_001"])
colhiincall = (NC_all_high_inc@estimate[,"B17025_009"] 
            / NC_all_high_inc@estimate[,"B17025_001"])
colbaeduall = (NC_all_ba_edu@estimate[,"B23006_023"] 
               / NC_all_ba_edu@estimate[,"B23006_001"])




NC_black_df <- data.frame(col1, col2)
NC_lowinc_df <- data.frame(col1, colloinc)
NC_highinc_df <- data.frame(col1, colhiinc)
NC_baedu_df <- data.frame(col1, colbaedu)
NC_blkall_df <- data.frame(NC_all_pop_bl@geography$NAME, colblkall)
NC_lowincall_df <- data.frame(NC_all_pop_bl@geography$NAME, colloincall)
NC_highincall_df <- data.frame(NC_all_pop_bl@geography$NAME, colhiincall)
NC_baeduall_df <- data.frame(NC_all_pop_bl@geography$NAME, colbaeduall)


colnames(NC_black_df) <- c("NAMELSAD", "perc_black")
colnames(NC_lowinc_df) <- c("NAMELSAD", "perc_low_income")
colnames(NC_highinc_df) <- c("NAMELSAD", "perc_high_income")
colnames(NC_baedu_df) <- c("NAMELSAD", "perc_ba_edu")
colnames(NC_blkall_df) <- c("NAMELSAD", "perc_black")
colnames(NC_lowincall_df) <- c("NAMELSAD", "perc_low_income")
colnames(NC_highincall_df) <- c("NAMELSAD", "perc_high_income")
colnames(NC_baeduall_df) <- c("NAMELSAD", "perc_ba_edu")



NC_black_merged = geo_join(NC_tracts, NC_black_df,
                           "NAMELSAD", "NAMELSAD")
NC_lowinc_merged = geo_join(NC_tracts, NC_lowinc_df,
                           "NAMELSAD", "NAMELSAD")
NC_highinc_merged = geo_join(NC_tracts, NC_highinc_df,
                            "NAMELSAD", "NAMELSAD")
NC_baedu_merged = geo_join(NC_tracts, NC_baedu_df,
                             "NAMELSAD", "NAMELSAD")
NC_blkall_merged = geo_join(NC_all_tracts, NC_blkall_df,
                           "NAMELSAD", "NAMELSAD")
NC_lowincall_merged = geo_join(NC_all_tracts, NC_lowincall_df,
                            "NAMELSAD", "NAMELSAD")
NC_highincall_merged = geo_join(NC_all_tracts, NC_highincall_df,
                             "NAMELSAD", "NAMELSAD")
NC_baeduall_merged = geo_join(NC_all_tracts, NC_baeduall_df,
                           "NAMELSAD", "NAMELSAD")

#length(NC_lowinc_merged)
#length(NC_blkall_merged)

#NC_lowincall_df

#head(NC_blkall_merged$perc_black,1)

cds = congressional_districts()
#cds2011 = congressional_districts(year = 2013)

NC_code = "37"
NC_shp_2015 = cds[(cds@data$STATEFP == NC_code), ]
NC_distr_12 = cds[(cds@data$STATEFP == NC_code) 
                 & (cds@data$NAMELSAD =="Congressional District 12") , ] #2015
NC_distr_12_2010 = readOGR("/Users/chenjingqi/Dropbox/College/2017 Spring/STA 160/Gerrymandering/Congressional Districts/NC_Shapefiles/NC_2010_CD12.shp")
NC_shp_2010 = readOGR("/Users/chenjingqi/Dropbox/College/2017 Spring/STA 160/STA160Project/Congressional Districts/Shapefiles/gz_2010_37_500_11_500k/gz_2010_37_500_11_500k.shp")
#cds11 = congressional_districts(year = 2011)
#NC_distr_12_2008 = cds[(cds@data$STATEFP == NC_code) 
#                  & (cds@data$NAMELSAD =="Congressional District 12") , ]

library(leaflet)



popup_black <- paste0("<span style='color:#7f0000'><strong>
                Percentage black or african american: </strong></span>", 
                as.character(round(NC_black_merged$perc_black,
                                   digit=3)))

popup_low_inc <- paste0("<span style='color:#7f0000'><strong>
                Percentage low income: </strong></span>", 
                as.character(round(NC_lowinc_merged$perc_low_income,
                                   digit=3)))

popup_high_inc <- paste0("<span style='color:#7f0000'><strong>
                Percentage high income: </strong></span>", 
                    as.character(round(NC_highinc_merged$perc_high_income,
                                       digit=3)))

popup_baedu <- paste0("<span style='color:#7f0000'><strong>
                Percentage Bachelor's degree or higher: </strong></span>", 
                         as.character(round(NC_baedu_merged$perc_ba_edu,
                                            digit=3)))

popup_blkall <- paste0("<span style='color:#7f0000'><strong>
                Percentage black or african american: </strong></span>", 
                      as.character(round(NC_blkall_merged$perc_black,
                                         digit=3)))
popup_lowincall <- paste0("<span style='color:#7f0000'><strong>
                Percentage low income: </strong></span>", 
                       as.character(round(NC_lowincall_merged$perc_low_income,
                                          digit=3)))
popup_highincall <- paste0("<span style='color:#7f0000'><strong>
                Percentage low income: </strong></span>", 
                          as.character(round(NC_highincall_merged$perc_high_income,
                                             digit=3)))
popup_baeduall <- paste0("<span style='color:#7f0000'><strong>
                Percentage Bachelor's degree or higher: </strong></span>", 
                      as.character(round(NC_baeduall_merged$perc_ba_edu,
                                         digit=3)))

###### Black
pal <- colorQuantile("Greens", NULL, n=5)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = NC_black_merged, 
              fillColor = ~pal(NC_black_merged$perc_black), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup_black) %>%
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
  addPolygons(data =  vd, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=0.2,
              weight = 1.5, color="orange",
              smoothFactor = 0.5, group=" vd",popup = vd$VTDST10) %>%
  addLegend(pal = pal, 
            values = NC_black_merged$perc_black, 
            position = "bottomright", 
            title = "Percentage Black or African American") %>%
  addLayersControl(
    overlayGroups = c("Black Dist 12"),
    options = layersControlOptions(collapsed = FALSE)
  )


###### Black All
pal <- colorQuantile("Greens", NULL, n=5)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = NC_blkall_merged, 
              fillColor = ~pal(NC_blkall_merged$perc_black), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup_blkall) %>%
  addPolygons(data = NC_shp_2015, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=1,
              weight = 1.5, color="black",
              smoothFactor = 0.5, group="NC_shp_2015") %>%
  addPolygons(data =  NC_shp_2010, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=0.5,
              weight = 1.5, color="red",
              smoothFactor = 0.5, group=" NC_shp_2010") %>%
  addLegend(pal = pal, 
            values = NC_blkall_merged$perc_black, 
            position = "bottomright", 
            title = "Percentage Black or African American") %>%
  addLayersControl(
    overlayGroups = c("Black All"),
    options = layersControlOptions(collapsed = FALSE)
  )






###### Low Income
pal <- colorQuantile("Blues", NULL, n=5)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = NC_lowinc_merged, 
              fillColor = ~pal(NC_lowinc_merged$perc_low_income), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup_low_inc) %>%
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
            values = NC_lowinc_merged$perc_low_income, 
            position = "bottomright", 
            title = "Percentage low income") %>%
  addLayersControl(
    overlayGroups = c("NC_distr_12"),
    options = layersControlOptions(collapsed = FALSE)
  )


###### Low Income All
pal <- colorQuantile("Blues", NULL, n=5)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = NC_lowincall_merged, 
              fillColor = ~pal(NC_lowincall_merged$perc_low_income), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup_lowincall) %>%
  addPolygons(data = NC_shp_2015, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=1,
              weight = 1.5, color="black",
              smoothFactor = 0.5, group="NC_shp_2015") %>%
  
  addPolygons(data =  NC_shp_2010, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=0.5,
              weight = 1.5, color="red",
              smoothFactor = 0.5, group=" NC_shp_2010") %>%
  addLegend(pal = pal, 
            values = NC_lowincall_merged$perc_low_income, 
            position = "bottomright", 
            title = "Percentage low income") %>%
  addLayersControl(
    overlayGroups = c("NC_shp_2015"),
    options = layersControlOptions(collapsed = FALSE)
  )





###### Highincome
pal <- colorQuantile("Purples", NULL, n=5)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = NC_highinc_merged, 
              fillColor = ~pal(NC_highinc_merged$perc_high_income), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup_high_inc) %>%
  addPolygons(data = NC_distr_12, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=1,
              weight = 1.5, color="black",
              smoothFactor = 0.5, group="NC_distr_12") %>%
  addPolygons(data =  NC_distr_12_2010, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=0.5,
              weight = 1.5, color="red",
              smoothFactor = 0.5, group=" NC_distr_12_2010") %>%
  addLegend(pal = pal, 
            values = NC_highinc_merged$perc_high_income, 
            position = "bottomright", 
            title = "Percentage High Income") %>%
  addLayersControl(
    overlayGroups = c("NC_distr_12"),
    options = layersControlOptions(collapsed = FALSE)
  )

###### Highincome All
pal <- colorQuantile("Purples", NULL, n=5)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = NC_highincall_merged, 
              fillColor = ~pal(NC_highincall_merged$perc_high_income), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup_highincall) %>%
  addPolygons(data = NC_shp_2015, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=1,
              weight = 1.5, color="black",
              smoothFactor = 0.5, group="NC_shp_2015") %>%
  addPolygons(data =  NC_shp_2010, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=0.5,
              weight = 1.5, color="red",
              smoothFactor = 0.5, group="NC_shp_2010") %>%
  addLegend(pal = pal, 
            values = NC_highincall_merged$perc_high_income, 
            position = "bottomright", 
            title = "Percentage High Income") %>%
  addLayersControl(
    overlayGroups = c("NC_shp_2015"),
    options = layersControlOptions(collapsed = FALSE)
  )


####### Education

pal <- colorQuantile("Reds", NULL, n=5)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = NC_baedu_merged, 
              fillColor = ~pal(NC_baedu_merged$perc_ba_edu), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup_baedu) %>%
  addPolygons(data = NC_distr_12, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=1,
              weight = 1.5, color="black",
              smoothFactor = 0.5, group="NC_distr_12") %>%
  addPolygons(data =  NC_distr_12_2010, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=0.5,
              weight = 1.5, color="green",
              smoothFactor = 0.5, group=" NC_distr_12_2010") %>%
  addLegend(pal = pal, 
            values = NC_baedu_merged$perc_ba_edu, 
            position = "bottomright", 
            title = "Percentage Bachelor's degree or higher") %>%
  addLayersControl(
    overlayGroups = c("NC_distr_12"),
    options = layersControlOptions(collapsed = FALSE)
  )

####### Education All

pal <- colorQuantile("Reds", NULL, n=5)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = NC_baeduall_merged, 
              fillColor = ~pal(NC_baeduall_merged$perc_ba_edu), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = popup_baeduall) %>%
  addPolygons(data = NC_shp_2015, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=1,
              weight = 1.5, color="black",
              smoothFactor = 0.5, group="NC_shp_2015") %>%
  addPolygons(data =  NC_shp_2010, 
              fillOpacity = 0.,
              stroke=TRUE, opacity=0.5,
              weight = 1.5, color="green",
              smoothFactor = 0.5, group=" NC_shp_2010") %>%
  addLegend(pal = pal, 
            values = NC_baeduall_merged$perc_ba_edu, 
            position = "bottomright", 
            title = "Percentage Bachelor's degree or higher") %>%
  addLayersControl(
    overlayGroups = c("NC_shp_2015"),
    options = layersControlOptions(collapsed = FALSE)
  )



