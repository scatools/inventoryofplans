function(input, output, session) {
#Default popup window when landing
  showModal(div(id="ModalDiv123", modalDialog(
    title = "Welcome to the Conservation Projects & Plans category Visualization Tool",
    HTML(c("<div align='left'>
             &nbsp; &nbsp; This visualization tool is intended for users to explore the plans inventory that 
             has been collected during the development of SCA Tool.</br>
             <b>Intended Use</b></br>
             &nbsp; &nbsp; All the plans are categorized based on their relevance to achieving  the goals of the RESTORE Council.Under each RESTORE goal exists several priorities that were 
           identified by stakeholders and are reflected as GIS data layers.</div>")),
    footer = tagList(
      modalButton("Explore the Tool")),
    size = "m"
  )))
#initially hide the table on top right  
  shinyjs::hide(id="Sidebar")
#reactive value for holding the click event when user click on map shape 
data_of_click <- reactiveValues(clickedMarker=NULL)
#default map
output$map <- renderLeaflet({
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(-88.222778, 30.120, zoom = 6)
    
})

#R function for opening documents when clicking on learn more
shinyInput <- function(FUN, len, id, label, url) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i),label="Learn more",onclick = paste0(' window.open("',url[i],'","_blank")')))
  }
  inputs
}
#R function for redirecting to detail datatable when click
shinyInput_short <- function(FUN, len, id, label) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i),label="Learn more",onclick = 'Shiny.onInputChange(\"select_button\",  this.id)'))
  }
  inputs
}

shinyInput_long <- function(FUN, len, id, label) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i),label="Learn more"))
  }
  inputs
}
#table to render in detailed data table page
cleandata_MS<-subset(tabledata_MS, select=-Plan.URL)
df_MS<-reactiveValues(data = cleandata_MS%>%
                     mutate(Action=paste(shinyInput(actionButton, 67, 'button_',url = tabledata_MS$Plan.URL))))
output$Mississippi = DT::renderDataTable(
  df_MS$data, server = TRUE, escape = FALSE, selection = list(mode = 'single', selected = c(1))
)

cleandata_AL<-subset(tabledata_AL, select=-Plan.URL)
df_AL<-reactiveValues(data = cleandata_AL%>%
                     mutate(Action=paste(shinyInput(actionButton, 45, 'button_',url = tabledata_AL$Plan.URL))))
output$Alabama = DT::renderDataTable(
  df_AL$data, server = TRUE, escape = FALSE, selection = list(mode = 'single', selected = c(1))
)

cleandata_FL<-subset(tabledata_FL, select=-c(Plan.URL))
df_FL<-reactiveValues(data = cleandata_FL%>%
                     mutate(Action=paste(shinyInput(actionButton, 72, 'button_',url = tabledata_FL$Plan.URL))))
output$Florida = DT::renderDataTable(
  df_FL$data, server = TRUE, escape = FALSE, selection = list(mode = 'single', selected = c(1)), 
  options = list( scrollX=TRUE,
                 columnDefs = list(list(width = '50px', targets = c(0)),
                                   list(width = '10px', targets = c(1)),
                                   list(width = '10px', targets = c(2)),
                                   list(width = '10px', targets = c(3)),
                                   list(width = '10px', targets = c(4)),
                                   list(width = '10px', targets = c(5)),
                                   list(width = '100px', targets = c(6)),
                                   list(width = '100px', targets = c(7)),
                                   list(width = '100px', targets = c(8)),
                                   list(width = '100px', targets = c(9)),
                                   list(width = '100px', targets = c(10)),
                                   list(width = '20px', targets = c(11)),
                                   list(width = '20px', targets = c(12)),
                                   list(width = '20px', targets = c(13)),
                                   list(width = '20px', targets = c(14)),
                                   list(width = '20px', targets = c(15))
                                   
                                   ))
)

cleandata_TX<-subset(tabledata_TX, select=-c(Plan.URL))
df_TX<-reactiveValues(data = cleandata_TX%>%
                     mutate(Action=paste(shinyInput(actionButton, 69, 'button_',url = tabledata_TX$Plan.URL))))
output$Texas = DT::renderDataTable(
  df_TX$data, server = TRUE, escape = FALSE, selection = list(mode = 'single', selected = c(1))
)

cleandata_LA<-subset(tabledata_LA, select=-Plan.URL)
df_LA<-reactiveValues(data = cleandata_LA%>%
                        mutate(Action=paste(shinyInput(actionButton, 42, 'button_',url = tabledata_LA$Plan.URL))))
output$Louisiana = DT::renderDataTable(
  df_LA$data, server = TRUE, escape = FALSE, selection = list(mode = 'single', selected = c(1))
)

cleandata_Regional<-subset(tabledata_Regional, select=-Plan.URL)
df_Regional<-reactiveValues(data = cleandata_Regional%>%
                        mutate(Action=paste(shinyInput(actionButton, 45, 'button_',url = tabledata_Regional$Plan.URL))))
output$Regional = DT::renderDataTable(
  df_Regional$data, server = TRUE, escape = FALSE, selection = list(mode = 'single', selected = c(1)),
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '50px', targets = "_all"))
  )
)
#event handling selection within the map
observe({
  priorityby<-input$priority
  timeby<-input$timeframe
  Scaleby<-input$scale
  
  if(Scaleby == "All"){
    if(timeby == "2017")
    {
      Point_city_ms_toshow<-Points_city_ms
      Point_city_tx_toshow<-Points_city_tx
      Point_county_al_toshow<-Points_county_al
      Point_county_tx_toshow<-Points_county_tx
      Point_county_fl_toshow<-Points_county_fl
      Point_county_ms_toshow<-Points_county_ms
      Point_perish_la_toshow<-Points_perish_la
      Points_state_toshow<-Points_state
    }
    else
    {
      Point_city_ms_toshow<-Points_city_tx[Points_city_ms@data[[timeby]]==1,]
      Point_city_tx_toshow<-Points_city_tx[Points_city_tx@data[[timeby]]==1,]
      Point_county_al_toshow<-Points_county_al[Points_county_al@data[[timeby]]==1,]
      Point_county_tx_toshow<-Points_county_tx[Points_county_tx@data[[timeby]]==1,]
      Point_county_fl_toshow<-Points_county_fl[Points_county_fl@data[[timeby]]==1,]
      Point_county_ms_toshow<-Points_county_ms[Points_county_ms@data[[timeby]]==1,]
      Point_perish_la_toshow<-Points_perish_la[Points_perish_la@data[[timeby]]==1,]
      Points_state_toshow<-Points_state[Points_state@data[[timeby]]==1,]
    }
    if(priorityby!="All"){
      Point_city_ms_toshow<-Point_city_ms_toshow[Point_city_ms_toshow[[priorityby]]==1,]
      Point_city_tx_toshow<-Point_city_tx_toshow[Point_city_tx_toshow@data[[priorityby]]==1,]
      Point_county_al_toshow<-Point_county_al_toshow[Point_county_al_toshow@data[[priorityby]]==1,]
      Point_county_tx_toshow<-Point_county_tx_toshow[Point_county_tx_toshow@data[[priorityby]]==1,]
      Point_county_fl_toshow<-Point_county_fl_toshow[Point_county_fl_toshow@data[[priorityby]]==1,]
      Point_county_ms_toshow<-Point_county_ms_toshow[Point_county_ms_toshow@data[[priorityby]]==1,]
      Point_perish_la_toshow<-Point_perish_la_toshow[Point_perish_la_toshow@data[[priorityby]]==1,]
      Points_state_toshow<-Points_state_toshow[Points_state_toshow@data[[priorityby]]==1,]
    }
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls()%>%
      addCircles(data = Point_city_ms_toshow, fillOpacity = 0.5, 
                 stroke = TRUE, weight = 5, radius = 4000,
                 color = "salmon",layerId=Point_city_ms_toshow@data$NAME10,popup=Point_city_ms_toshow@data$NAME10,
                 highlightOptions = highlightOptions(
                   color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                   bringToFront = TRUE, sendToBack = TRUE))%>%
      addCircles(data = Point_city_tx_toshow, fillOpacity = 0.5, 
                 stroke = TRUE, weight = 5, radius = 4000,
                 color = "salmon",layerId=Point_city_tx_toshow@data$CITY_NM,popup=Point_city_tx_toshow@data$CITY_NM,
                 highlightOptions = highlightOptions(
                   color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                   bringToFront = TRUE, sendToBack = TRUE))%>%
      addCircles(data = Point_county_al_toshow, fillOpacity = 0.5, 
                 stroke = TRUE, weight = 5, radius = 10000,
                 color = "Purple",layerId=Point_county_al_toshow@data$NAME,popup=Point_county_al_toshow@data$NAME,
                 highlightOptions = highlightOptions(
                   color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                   bringToFront = TRUE, sendToBack = TRUE))%>%
      addCircles(data = Point_county_tx_toshow, fillOpacity = 0.5, 
                 stroke = TRUE, weight = 5, radius = 10000,
                 color = "Purple",layerId=Point_county_tx_toshow@data$NAME,popup=Point_county_tx_toshow@data$NAME,
                 highlightOptions = highlightOptions(
                   color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                   bringToFront = TRUE, sendToBack = TRUE))%>%
      addCircles(data = Point_county_fl_toshow, fillOpacity = 0.5, 
                 stroke = TRUE, weight = 5, radius = 10000,
                 color = "Purple",layerId=Point_county_fl_toshow@data$NAME,popup=Point_county_fl_toshow@data$NAME,
                 highlightOptions = highlightOptions(
                   color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                   bringToFront = TRUE, sendToBack = TRUE))%>%
      addCircles(data = Point_county_ms_toshow, fillOpacity = 0.5, 
                 stroke = TRUE, weight = 5, radius = 10000,
                 color = "Purple",layerId=Point_county_ms_toshow@data$NAME,popup=Point_county_ms_toshow@data$NAME,
                 highlightOptions = highlightOptions(
                   color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                   bringToFront = TRUE, sendToBack = TRUE))%>%
      addCircles(data = Point_perish_la_toshow, fillOpacity = 0.5, 
                 stroke = TRUE, weight = 5, radius = 10000,
                 color = "Purple",layerId=Point_perish_la_toshow@data$NAME,popup=Point_perish_la_toshow@data$NAME,
                 highlightOptions = highlightOptions(
                   color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                   bringToFront = TRUE, sendToBack = TRUE))%>%
      addCircles(data = Points_state_toshow, fillOpacity = 0.5, 
                 stroke = TRUE, weight = 5, radius = 30000,
                 color = "Green",layerId=Points_state_toshow@data$Name,popup=Points_state_toshow@data$Name,
                 highlightOptions = highlightOptions(
                   color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                   bringToFront = TRUE, sendToBack = TRUE))%>%
      addLegend(position="bottomleft", color=c('salmon','purple','green'),label=c('City Level Plan','County Level Plan','State Level Plan'))
  }
  else if(Scaleby=="State"){
    if(timeby == "2017")
    {
      State_toshow<-State
    }
    else
    {
      State_toshow<-State[State@data[[timeby]]==1,]
    }
    if(priorityby!="All"){
      State_toshow<-State_toshow[State_toshow[[priorityby]]==1,]
    }
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls()%>%
      addPolygons(data=State_toshow, fillOpacity = 0.5,weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillColor = "Orange",layerId= State_toshow@data$Name, popup=State_toshow@data$Name,
                  highlightOptions = highlightOptions(
                    color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                    bringToFront = TRUE, sendToBack = TRUE)
                  )
  }
  else if(Scaleby=="County"){
    if(timeby == "2017")
    {
      County_All_toshow<-County_All
      City_ms_toshow<-City_ms
      City_tx_toshow<-City_tx
    }
    else
    {
      County_All_toshow<- County_All[County_All@data[[timeby]]==1,]
      City_ms_toshow<-City_ms[City_ms@data[[timeby]]==1,]
      City_tx_toshow<-City_tx[City_tx@data[[timeby]]==1,]
      
    }
    if(priorityby!="All"){
      County_All_toshow<- County_All_toshow[County_All_toshow@data[[priorityby]]==1,]
      City_ms_toshow<-City_ms_toshow[City_ms_toshow[[priorityby]]==1,]
      City_tx_toshow<-City_tx_toshow[City_tx_toshow[[priorityby]]==1,]
    }
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls()%>%
      addPolygons(data=County_All_toshow, fillOpacity = 0.5,weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillColor = "salmon",layerId= County_All_toshow@data$NAME, popup=County_All_toshow@data$Name,group='County',
                  highlightOptions = highlightOptions(
                    color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                    bringToFront = TRUE, sendToBack = TRUE)) %>%
      addPolygons(data=City_ms_toshow, fillOpacity = 0.5,weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillColor = "purple",layerId= City_ms_toshow@data$NAME10, popup=City_ms_toshow@data$NAME10,group='City',
                  highlightOptions = highlightOptions(
                    color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                    bringToFront = TRUE, sendToBack = TRUE))%>%
      addPolygons(data=City_tx_toshow, fillOpacity = 0.5,weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillColor = "purple",layerId= City_tx_toshow@data$CITY_NM, popup=City_tx_toshow@data$CITY_NM,group='City',
                  highlightOptions = highlightOptions(
                    color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                    bringToFront = TRUE, sendToBack = TRUE))%>%
      addLayersControl(overlayGroups=c('County','City'),position='bottomright')%>%
      addLegend(position="bottomleft", color=c('salmon','purple'),label=c('County Level Plans','City Level Plans'))
  }
  else if(Scaleby=="CA"){
    if(timeby == "2017")
    {
      Watershed_toshow<-Watershed
      Costalzone_toshow<-Costalzone
    }
    else{
      Watershed_toshow<- Watershed[Watershed@data[[timeby]]==1,]
      Costalzone_toshow<- Costalzone[Costalzone@data[[timeby]]==1,]
    }
    if(priorityby!="All"){
      Watershed_toshow<- Watershed_toshow[Watershed_toshow@data[[priorityby]]==1,]
      Costalzone_toshow<- Costalzone_toshow[Costalzone_toshow@data[[priorityby]]==1,]
    }
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls()%>%
      addPolygons(data=Costalzone_toshow, fillOpacity = 0.3,weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillColor = "blue",layerId= Costalzone@data$Name, popup=Costalzone@data$Name,group='Coastal Zone') %>%
      addPolygons(data=Watershed_toshow, fillOpacity = 0.3,weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillColor = "purple",layerId= Watershed_toshow@data$NAME, popup=Watershed_toshow@data$NAME,
                  highlightOptions = highlightOptions(
                    color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                    bringToFront = TRUE, sendToBack = TRUE),group='watershed') %>%
      addPolylines(data= Watershed1,color='black',layerId= Watershed1@data$NAME,weight = 1,group='watershed')%>%
      addLayersControl(overlayGroups=c('Coastal Zone','watershed'),position='bottomright')%>%
      addLegend(position="bottomleft", color=c('blue','purple','black'),label=c('Costal Zone','Watersheds','Rivers & Streams'))
  }
  else if(Scaleby=="Regional"){
    if(timeby == "2017")
    {
      Regional_toshow<-Regional
    }
    else
    {
      Regional_toshow<-Regional[Regional@data[[timeby]]==1,]
    }
    if(priorityby!="All"){
      Regional_toshow<-Regional_toshow[Regional_toshow[[priorityby]]==1,]
    }
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data=Regional_toshow, fillOpacity = 0.5,weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillColor = "Orange",layerId= Regional_toshow@data$Name, popup=Regional_toshow@data$Name,
                  highlightOptions = highlightOptions(
                    color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                    bringToFront = TRUE, sendToBack = TRUE)
      )
  }
})

df_Rshortdatage<-reactiveValues(data = shortdatage%>%
                                  mutate(Action=paste(shinyInput_short(actionButton, 340, "short_",label="Learn more"))))

#map click event
observeEvent(input$map_shape_click,{
  data_of_click$clickedMarker <- input$map_shape_click
})

#render short datatable overlay on map
output$docs <- DT::renderDataTable({
  Region_name<-data_of_click$clickedMarker$id
  df_Rshortdatage$data[df_Rshortdatage$data$Geo.Extent==Region_name,]
},options = list(
  pageLength = 5
),server = FALSE, escape = FALSE, selection = 'none')

#when click learn more on overlay table, redirect to the selected one and highlight it
observeEvent(input$select_button,{
  num<-as.numeric(strsplit(input$select_button, "_")[[1]][2])
  trail1tb<-reactive({
    if(num<=45){return("Alabama")}
    else if(num>45 & num<=117){return("Florida")}
    else if(num>117 & num<=184){return("Mississippi")}
    else if(num>184 & num<=229){return("Regional")}
    else if(num>229 & num<=298){return("Texas")}
    else {return("Louisiana")}
    })
  updateTabsetPanel(session, "nav",
                      selected = "Data Tables")
  updateTabsetPanel(session,"nav1",selected = trail1tb())
  proxyal = dataTableProxy("Alabama")
  selectRows(proxyal, which(tabledata_AL$ref_num == num))
  selectPage(proxyal, which(tabledata_AL$ref_num == num) %/% 10 + 1)
  proxyfl = dataTableProxy("Florida")
  selectRows(proxyfl, which(tabledata_FL$ref_num == num))
  selectPage(proxyfl, which(tabledata_FL$ref_num == num) %/% 10 + 1)
  proxyms = dataTableProxy("Mississippi")
  selectRows(proxyms, which(tabledata_MS$ref_num == num))
  selectPage(proxyms, which(tabledata_MS$ref_num == num) %/% 10 + 1)
  proxyre = dataTableProxy("Regional")
  selectRows(proxyre, which(tabledata_Regional$ref_num == num))
  selectPage(proxyre, which(tabledata_Regional$ref_num == num) %/% 10 + 1)
  proxytx = dataTableProxy("Texas")
  selectRows(proxytx, which(tabledata_TX$ref_num == num))
  selectPage(proxytx, which(tabledata_TX$ref_num == num) %/% 10 + 1)
  proxyla = dataTableProxy("Louisiana")
  selectRows(proxyla, which(tabledata_LA$ref_num == num))
  selectPage(proxyla, which(tabledata_LA$ref_num == num) %/% 10 + 1)
})
#redirect to newlly added plan when click
observeEvent(input$form2, {
  updateTabsetPanel(session, "nav",
                    selected = "Recently Added Plans")
})
cleandata_new<-subset(tabledata_newly, select=-Url)

df_newdatage<-reactiveValues(data = cleandata_new%>%
                                  mutate(Action=paste(shinyInput(actionButton, 294, "button_",label="Learn more",url =tabledata_newly$Url))))

output$Newly = renderDataTable(df_newdatage$data, server = TRUE, escape = FALSE, selection = 'single')
#event handle show and hide datatable
observeEvent(input$showpanel,{
  if(input$showpanel == F){
    show(id="Sidebar")
    enable(id="Sidebar")
  }
  else{
    hide(id="Sidebar")
  }
})



output$goalsTable<-renderImage({
  
  filename <- normalizePath(file.path('./images',
                                      paste('RESTORE_goals', '.png', sep='')))
  return(list(
    src= filename, width=550))
}, deleteFile = F) 



}

