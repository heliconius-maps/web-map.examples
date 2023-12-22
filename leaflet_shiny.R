require(stringr)
require(shiny)
require(dplyr)
require(DT)
require(leaflet)

#load csv with locality records, decimal latitude and longitude etc
museum_data<-read.csv("geographic_data.csv")

head(museum_data)

#remove any records with no geographic coordinates
museum_data<-museum_data[!is.na(museum_data$lat_dec),]
museum_data<-museum_data[!is.na(museum_data$lon_dec),]

#and do any other necessary filtering to clean the data
museum_data$year[is.na(museum_data$year)]<-""
museum_data$revised_accuracy[which(is.na(museum_data$revised_accuracy))]<-0
museum_data<-museum_data[museum_data$revised_accuracy<=41,]


### Define the Shiny Dashboard ###
ui <- shinyUI(
		fluidPage(
			includeCSS("MarkerCluster.Default.css"), #Optional line - manually ajusting CSS can be easier than dealing with java script
			sidebarLayout(
				sidebarPanel(
					#Add drop-down menu choices for the sidebar
					htmlOutput("genus_selector"),
					htmlOutput("species_selector"),				
					htmlOutput("subspecies_selector"),
					div(style="display:inline-block;float:right",downloadButton('download', 'Download the data')),
				),
				mainPanel(
					#Some details for main panel 
					leafletOutput("mymap",height=850),
					fluidRow(dataTableOutput('table'))
				)
			)
		)
	)

#Define server logic required for an interactive dashboard ----
server <- function(input, output, session) {  
	filtered_map <- reactive({
		rows <- (input$genus == "All" | museum_data$genus==input$genus) &
				(input$species == "All" | museum_data$species==input$species) &
				(input$subspecies == "All" | museum_data$subspecies==input$subspecies)
				(input$localidad_codigo == "All" | museum_data$locality_code==input$locality_code)
				(input$image_link == "All" | museum_data$image_link==input$image_link)
				(input$day == "All" | museum_data$day==input$day)
				(input$month == "All" | museum_data$month==input$month)
				(input$year == "All" | museum_data$year==input$year)
				(input$coleccion == "All" | museum_data$coleccion==input$coleccion)
		museum_data[rows,,drop = FALSE]  
	}) 
 
	output$genus_selector = renderUI({
		selectInput(inputId = "genus",
		label = "genus:",
		choices = c("All",sort(as.character(unique(museum_data$genus)))),
		selected = "Heliconius"
		)
	})
 
	output$species_selector = renderUI({
		data_available = museum_data[museum_data$genus == input$genus, "species"]
		selectInput(inputId = "species",
		label = "species:",
		choices = c("All",sort(as.character(unique(data_available)))),
		selected = "sapho"
		)
	})

	output$subspecies_selector = renderUI({
		data_available = museum_data[museum_data$species == input$species, "subspecies"]
		selectInput(inputId = "subspecies",
		label = "subspecies:",
		choices = c("All",sort(as.character(unique(data_available)))),
		)
	 })  
	 
	observe({
		output$table <- renderDataTable(select(filtered_map(),genus,species,subspecies,sex,locality_code,day,month,year,fecha,colector,coleccion), 
			colnamonth=c("Genus","Species","Subspecies","Sex","Locality","Day","Month","Year","Date_if_string","Collector","Collection"),
			options = list(pageLength = 5, scrollX = TRUE, bFilter=0, columnDefs = NULL), callback = JS('table.page(3).draw(false);')
			#options = list(pageLength = 5, width="100%", scrollX = TRUE)
			, rownamonth= FALSE
		)
	})	 
	 
 	output$mymap <- renderLeaflet({
		leaflet() %>%
			#Choose which baselayers you want to make available on the map
			addProviderTiles(
			"Esri.NatGeoWorldMap",
			group = "National Geographic"
			) %>%		
			addProviderTiles(
			"Esri.WorldImagery",
			group = "Satellite"
			) %>%
			addProviderTiles(
			"OpenTopoMap",
			group = "Topography"
			) %>%	
			addProviderTiles(
			"OpenStreetMap.Mapnik",
			group = "OSM"
			) %>%			
			addLayersControl(
			baseGroups = c("National Geographic","Satellite","Topography","OSM"),
			position = "topleft"
			)%>%	
			addMarkers(lng=filtered_map()$lon_dec, lat=filtered_map()$lat_dec,
				#Fields to appear in pop-up bubbles
				popup=paste("Genus:", filtered_map()$genus, "<br>",
					"Species:", filtered_map()$species, "<br>",
					 "Subspecies:", filtered_map()$subspecies, "<br>",
					 "If hybrid:", filtered_map()$if_hybrid, "<br>",
					 "Sex:", filtered_map()$sex, "<br>",
					 "Locality:", filtered_map()$locality_code, "<br>",
					 "Day:", filtered_map()$day, "<br>",
					 "Month:", filtered_map()$month, "<br>",
					 "Year:", filtered_map()$year, "<br>",
					 "Date_if_string:", filtered_map()$fecha, "<br>",
					 "Collection:", filtered_map()$coleccion, "<br>",
					 paste0("<img src='",filtered_map()$image_link, "' width='200' />")
					 ),
				popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE),	 
				#Icon options
				clusterOptions = markerClusterOptions(maxClusterRadius = 5,transparent=TRUE,singleMarkerMode=TRUE,zoomToBoundsOnClick=FALSE,
				iconCreateFunction=JS("function(cluster) {
					var c = ' marker-cluster-small';
					var html = '<div style=\"background-color:rgba(255,0,0,1)\"><span>' + cluster.getChildCount() + '</div><span>'
					return new L.DivIcon({html: html, className: 'marker-cluster' + c,iconSize: new L.Point(40, 40) });
					}")
				)			 	 
			)
	})

	output$download <- downloadHandler(
		filename = function(){"output.csv"}, 
		content = function(fname){
		  write.table(filtered_map()[,c(1:20)], fname,row.namonth = FALSE,
		  col.namonth = c("unique_registros_ID","voucher","genus","species","subspecies","if_hybrid","sex","locality","lat_dec","lon_dec","revised_accuracy","elev_min","elev_max","elev_unit","day","month","year","date_if_string","collector","collection"),
		  sep=",")
		}
	)
}

# Create Shiny object
shinyApp(ui = ui, server = server)




