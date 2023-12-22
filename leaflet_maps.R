require(stringr)
require(shiny)
require(dplyr)
require(DT)
require(leaflet)
require(grDevices)
require(htmlwidgets)

#load csv with locality records, decimal latitude and longitude etc
museum_data<-read.csv("geographic_data.csv")

head(museum_data)

#remove any records with no geographic coordinates
museum_data<-museum_data[!is.na(museum_data$lat_dec),]
museum_data<-museum_data[!is.na(museum_data$lon_dec),]

#and do any other necessary filtering to clean the data
museum_data$ano[is.na(museum_data$ano)]<-""
museum_data$revised_accuracy[which(is.na(museum_data$revised_accuracy))]<-0
museum_data<-museum_data[museum_data$revised_accuracy<=41,]

#Create a unique list of species by concatenating the genus and species columns
museum_data$genus_species<-paste0(museum_data$genus,"_",museum_data$species)
taxon_list<-unique(museum_data$genus_species)

#create output directory
dir.create(paste("HTML_maps/",sep=""))

#This loop will create a HTML leaflet map for each species in taxon_list
for (sp in taxon_list){
	print(sp)
	filtered_map<-museum_data[museum_data$genus_species==sp,]
	filtered_map_sp<-filtered_map
	filtered_map_sp$subspecies<-sp
	filtered_map<-rbind(filtered_map_sp,filtered_map)
	map<-leaflet()
	#Choose which baselayers you want to make available on the map
	map<-addProviderTiles(map,providers$Esri.NatGeoWorldMap,group = "National Geographic")
	map<-addProviderTiles(map,providers$Esri.WorldImagery, group = "Satellite")
	map<-addProviderTiles(map,providers$OpenTopoMap, group = "Topography")
	map<-addProviderTiles(map,providers$OpenStreetMap.Mapnik, group = "OSM")
	subspecies<-unique(filtered_map$subspecies)
	subspecies<-c(subspecies[1],sort(subspecies[2:length(subspecies)]))
	subspecies<-subspecies[subspecies!=""]
	marker_colours<-rainbow(length(subspecies))
	for (subsp in 1:length(subspecies)){		
		filtered_map_subsp<-filtered_map[filtered_map$subspecies==subspecies[subsp],]
		col_as_RGB<-paste(c(as.vector(col2rgb(marker_colours[subsp])),1), collapse = ",")		
		map<-addMarkers(map,filtered_map_subsp$lon_dec, filtered_map_subsp$lat_dec, group = subspecies[subsp],
			#Choose which values from the CSV file you want to include in the pop-up bubbles
			popup=paste("genus", filtered_map_subsp$genus, "<br>",
				"Species:", filtered_map_subsp$species, "<br>",
				"Subspecies:", filtered_map_subsp$subspecies, "<br>",
				"If hybrid:", filtered_map_subsp$if_hybrid, "<br>",
				"Locality:", filtered_map_subsp$locality_code, "<br>",
				"Day:", filtered_map_subsp$day, "<br>",
				"Month:", filtered_map_subsp$month, "<br>",
				"Year:", filtered_map_subsp$year, "<br>",
				"Date as string:", filtered_map_subsp$date_if_string, "<br>",
				"Collection:", filtered_map_subsp$coleccion, "<br>",
				paste0("<img src='",filtered_map_subsp$image_link, "' width='200' />")
			),
			popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE),
			clusterOptions = markerClusterOptions(maxClusterRadius = 5,transparent=TRUE,singleMarkerMode=TRUE,zoomToBoundsOnClick=FALSE,
				#Edit map icons as desired.
				iconCreateFunction=JS(paste0("function(cluster) {
					var c = ' marker-cluster-small';
					var html = '<div style=\"background-color:rgba(",col_as_RGB,")\"><span>' + cluster.getChildCount() + '</div><span>'
					return new L.DivIcon({html: html, className: 'marker-cluster' + c,iconSize: new L.Point(40, 40) });
				}"))
			)
		)	
	}
	#Add the legend	
	map<-addLayersControl(map,
			baseGroups = c("National Geographic","Satellite", "Topography","OSM"),
			overlayGroups = subspecies,
			options = layersControlOptions(collapsed = FALSE),
		)%>%
      
	#list groups to hide on startup (i.e. have subspecies unchecked when map is opened)
    hideGroup(subspecies[2:length(subspecies)])		
	#save the output map for the species
	dir.create(paste("HTML_maps/",sp,"/",sep=""))
	saveWidget(map, paste("HTML_maps/",sp,"/","index.html",sep=""), selfcontained = FALSE)
}


