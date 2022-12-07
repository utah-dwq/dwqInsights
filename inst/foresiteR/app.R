library(shiny)
library(shinyjs)
library(shinyBS)
library(wqTools)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(plotly)
library(RColorBrewer)

load(system.file("extdata", "latest_asmnts_hist_impairments_app.Rdata", package = "dwqInsights"))
load(system.file("extdata", "tribal_poly.Rdata", package = "dwqInsights"))
#####
ui <- fluidPage(
    headerPanel(
        title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo.png', height = 50, width = 143), target="_blank"),
        tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="4siteR:Assessment Planning Tool")
    ),
    titlePanel("foresiteR:Assessment Planning Tool"),
    fluidRow(column(4, selectInput("basin", "Basin",choices=c("All",unique(wmu_poly$Mgmt_Unit)))),
             column(4, selectInput("cat", "AU Category",choices=c("All","Fully Supporting"="1",
                                                                           "No Evidence of Impairment"="2",
                                                                           "Insufficient Data"="3",
                                                                           "Approved TMDL"="4A",
                                                                           "Not Supporting"="5")))),
    fluidRow(column(2, actionButton("apply","Apply filter(s)")),
             column(1, actionButton("reset","Reset Map"))),
    br(),
    fluidRow(shinycssloaders::withSpinner(leaflet::leafletOutput("mappo", height="400px", width="100%"),size=2, color="#0080b7")),
    br(),
    fluidRow(column(6, shinycssloaders::withSpinner(leaflet::leafletOutput("mappino", height="600px", width="100%"),size=2, color="#0080b7")),
             column(6,fluidRow(column(4,uiOutput("param")),
                               column(4,uiOutput("fraction")),
                               column(4, uiOutput("unit"))),
                    fluidRow(column(2, uiOutput("submit"))),
                    bsTooltip("submit",title="All non-detects and over-detects in the plot data will appear at their detection limit if it was reported in the dataset."),
                    fluidRow(plotlyOutput("timeseries")),
                    fluidRow(column(2,uiOutput("getData"))),
                    br()))
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    reactive_objects=reactiveValues()

    # Map generation
    output$mappo=leaflet::renderLeaflet({
        mappo=leaflet()%>%
            setView(lat=39.723030, lng=-111.554213, zoom=6)%>%
            addProviderTiles("Esri.WorldImagery", group = "Satellite", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
            addProviderTiles("Esri.WorldTopoMap", group = "World topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
            addMapPane("underlay_polygons", zIndex = 400) %>%
            addMapPane("au_poly", zIndex = 410)  %>%
            addMapPane("markers", zIndex = 420)  %>%
            addMapPane("highlight", zIndex = 415)%>%
            addPolygons(data=wqTools::wmu_poly,group="Watershed management units",fillOpacity = 0.1,weight=2,color="#0b86a3", options = pathOptions(pane = "underlay_polygons"),
                        popup=wqTools::wmu_poly$Mgmt_Unit
            )%>%
            addPolygons(data=wqTools::bu_poly,group="Beneficial uses",fillOpacity = 0.1,weight=2,color="purple", options = pathOptions(pane = "underlay_polygons"),
                        popup=paste0(
                            "Description: ", wqTools::bu_poly$R317Descrp,
                            "<br> Uses: ", wqTools::bu_poly$bu_class)
            )%>%
            addPolygons(data=wqTools::au_poly, group="All AUs", fillOpacity = 0.1,weight=2,color="#034963", options = pathOptions(pane = "underlay_polygons"),
                        popup=paste0(
                          "Assessment unit ID: ",wqTools::au_poly$ASSESS_ID,
                          "<br> Assessment unit Name: ",wqTools::au_poly$AU_NAME
                        ))%>%
            addPolygons(data=tribal_poly, group="Tribal lands", fillOpacity = 0.1,weight=2,color="orange", options = pathOptions(pane = "underlay_polygons"),
                        popup=paste0(
                          "Tribe: ",tribal_poly$Tribe,
                          "<br> Name: ",tribal_poly$Label_Fede))%>%
            addCircleMarkers(data=ns_sites_paramwide, lng=~IR_Long, lat=~IR_Lat,group="Impaired sites",color="red", radius=5,options = pathOptions(pane = "markers"),
                             popup=paste0(
                                 "MLID: ",ns_sites_paramwide$IR_MLID,
                                 "<br> Name: ",ns_sites_paramwide$IR_MLNAME,
                                 "<br> Parameter (First Year Listed):",ns_sites_paramwide$`Impaired Parameters`
                             ))%>%
            addCircles(data = centroids, group = "AUID",stroke=F, fill=F, label=~ASSESS_ID,
                     popup = centroids$ASSESS_ID) %>%
            addCircles(data = centroids, group = "AUName",stroke=F, fill=F, label=~AU_NAME,
                     popup = centroids$AU_NAME)%>%
          leaflet.extras::addSearchFeatures(
            targetGroups = c('AUID','AUName'),
            options = leaflet.extras::searchFeaturesOptions(
              zoom=12, openPopup = FALSE, firstTipSubmit = TRUE,
              autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))%>%
            addLayersControl(position ="topright",
                                      baseGroups = c("World topo", "Satellite"),overlayGroups = c("All AUs", "Tribal lands","Beneficial uses", "Watershed management units", "Impaired sites"),
                                      options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))%>%
            hideGroup("Beneficial uses")%>%
            hideGroup("All AUs")%>%
            hideGroup("Tribal lands")%>%
            hideGroup("Impaired sites")%>%
            addControl(dateInput("mindate","Start Date",startview = "month"))%>%
            addControl(dateInput("maxdate","End Date",startview = "month"))%>%
            addControl(actionButton("select","Let's go!"))
          })
    mappo=leafletProxy('mappo')

    # Apply filters to map
    observeEvent(input$apply,{
        mappo%>%
            clearGroup("AU Category")
        au_sel = au_poly
        if(!input$basin=="All"){
            au_sel = subset(au_sel, au_sel$Mgmt_Unit==input$basin)}
        if(!input$cat=="All"){
            au_sel = subset(au_sel, au_sel$EPA_IR_CATEGORY_ID==input$cat)}
        view=sf::st_bbox(au_sel)
        mappo%>%
            addPolygons(data=au_sel,group="AU Category",fillOpacity = 0.3,weight=2,color=~factpal(au_sel$EPA_IR_CATEGORY_ID), layerId=~polyID, options = pathOptions(pane = "au_poly"),
                        popup=paste0(
                            "AU name: ", au_sel$AU_NAME,
                            "<br> AU ID: ", au_sel$ASSESSMENT_UNIT_ID,
                            "<br> EPA Category: ", au_sel$EPA_IR_CATEGORY_ID,
                            "<br> Parameter (Year Listed): ", au_sel$`Impaired Parameters`,
                            "<br> Delisted Parameters: ", au_sel$`Delisted Parameters`))%>%
            fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4]))%>%
            hideGroup("Watershed management units")

    })

    # Reset overall map and selections
    observeEvent(input$reset,{
        reactive_objects$focal_au = NULL
        mappo%>%
            setView(lat=39.723030, lng=-111.554213, zoom=6)%>%
            clearGroup("AU Category")%>%
            clearGroup("Selected AU")%>%
            showGroup("Watershed management units")
    })

    # Map polygon click to select AUs
    observeEvent(input$mappo_shape_click,{
        au_id=as.character(unique(au_poly$ASSESSMENT_UNIT_ID[au_poly$polyID==input$mappo_shape_click$id]))
        single_au_poly = subset(wqTools::au_poly, wqTools::au_poly$ASSESS_ID==au_id)
        reactive_objects$focal_au = single_au_poly
        mappo%>%
            clearGroup("Selected AU")%>%
            addPolygons(data=single_au_poly, group="Selected AU",fillOpacity = 0.5,weight=2,color="#26F0F1", layerId=~polyID, options = pathOptions(pane = "highlight"),
                        popup=paste0("Assessment unit ID: ",single_au_poly$ASSESS_ID,
                        "<br> Assessment unit Name: ",single_au_poly$AU_NAME))
    })

    # Small map for site review
    output$mappino=leaflet::renderLeaflet({
        mappino=leaflet()%>%
            setView(lat=39.723030, lng=-111.554213, zoom=6)%>%
            addWMSTiles("https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer", group = "USGS topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE), layers = "0") %>%
            addWMSTiles("https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", group = "Hydrography", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE), layers = "0") %>%
            addProviderTiles("Esri.WorldImagery", group = "Satellite", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
            addProviderTiles("Esri.WorldTopoMap", group = "World topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
            addMapPane("au_poly", zIndex = 410)  %>%
            addMapPane("imps", zIndex = 420)  %>%
            addMapPane("markers", zIndex = 415)%>%
            addLayersControl(position ="topleft",
                             baseGroups = c("World topo","USGS topo", "Hydrography", "Satellite"),
                             overlayGroups = c("Selected AU", "All sites","Parameter sites","Impaired sites"),
                             options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))
    })
    mappino=leafletProxy('mappino')

    observeEvent(input$select,{
      if(is.null(reactive_objects$focal_au)){showModal(modalDialog(title="No AU selected","Please select an assessment unit and date range before clicking 'Let's go!'"))}else{
        mindate = as.character(format(input$mindate,"%m/%d/%Y"))
        maxdate = as.character(format(input$maxdate,"%m/%d/%Y"))
        reactive_objects$daterange = paste0(mindate,"to",maxdate)
        focal_au = reactive_objects$focal_au
        auid = focal_au$ASSESS_ID
        sites = wqTools::readWQP(type="sites",siteType=c("Lake, Reservoir, Impoundment","Spring","Stream"),auid=auid, start_date = mindate, end_date = maxdate)
        if(dim(sites)[1]==0){showModal(modalDialog(title="No sites","There are no sites in this assessment unit for the date range specified."))}else{
        sites = wqTools::assignAUs(sites, lat = "LatitudeMeasure",long="LongitudeMeasure")
        sites = subset(sites, sites$ASSESS_ID%in%auid)
        sites = wqTools::assignUses(sites, lat = "LatitudeMeasure",long="LongitudeMeasure")
        dat = wqTools::readWQP(type="result", siteid = c(sites$MonitoringLocationIdentifier), start_date = mindate, end_date = maxdate)
        dat$ResultSampleFractionText[is.na(dat$ResultSampleFractionText)]="NA"
        dat$ResultMeasureValue_plot = ifelse(!is.na(dat$ResultDetectionConditionText)&is.na(dat$ResultMeasureValue),dat$DetectionQuantitationLimitMeasure.MeasureValue, dat$ResultMeasureValue)
        dat$ResultMeasure.MeasureUnitCode_plot = ifelse(!is.na(dat$ResultDetectionConditionText)&is.na(dat$ResultMeasure.MeasureUnitCode),dat$DetectionQuantitationLimitMeasure.MeasureUnitCode, dat$ResultMeasure.MeasureUnitCode)
        dat$ResultMeasure.MeasureUnitCode_plot[is.na(dat$ResultMeasure.MeasureUnitCode_plot)]="NA"
        site_colorz = data.frame(MonitoringLocationIdentifier=unique(sort(sites$MonitoringLocationIdentifier)), mapcolor = colorRampPalette(brewer.pal(7,"Set2"))(length(unique(sites$MonitoringLocationIdentifier))))
        dat = merge(dat, site_colorz, all.x = TRUE)
        sites = merge(sites, site_colorz, all.x = TRUE)
        reactive_objects$sites = sites
        reactive_objects$data = dat
        ns_sites_au = subset(ns_sites_paramwide, ns_sites_paramwide$ASSESSMENT_UNIT_ID==auid)
        view=sf::st_bbox(focal_au)
        mappino%>%
            showGroup("All sites")%>%
            clearGroup("Selected AU")%>%
            clearGroup("Parameter sites")%>%
            clearGroup("All sites")%>%
            addPolygons(data=focal_au, group="Selected AU",stroke = TRUE, fillOpacity = 0,weight=3,color="#26F0F1", layerId=~polyID, options = pathOptions(pane = "au_poly"),
                        popup=paste0(
                            "AU name: ", focal_au$AU_NAME,
                            "<br> AU ID: ", focal_au$ASSESS_ID))%>%
            addCircleMarkers(data = sites,lng =~LongitudeMeasure, lat=~LatitudeMeasure,group="All sites",color=~mapcolor, radius=8,options = pathOptions(pane = "markers"),
                             popup=paste0(
                                 "Organization: ",sites$OrganizationFormalName,
                                 "<br> WQX ID: ",sites$MonitoringLocationIdentifier,
                                 "<br> WQX Name: ",sites$MonitoringLocationName,
                                 "<br> Type: ",sites$MonitoringLocationTypeName,
                                 "<br> Beneficial Uses:",sites$BeneficialUse
                             ))%>%
            addCircleMarkers(data=ns_sites_au, lng=~IR_Long, lat=~IR_Lat,group="Impaired sites",color="red", fillOpacity=0, radius=5,options = pathOptions(pane = "imps"),
                             popup=paste0(
                                 "MLID: ",ns_sites_au$IR_MLID,
                                 "<br> Name: ",ns_sites_au$IR_MLNAME,
                                 "<br> Parameter (First Year Listed):",ns_sites_au$`Impaired Parameters`
                             ))%>%
            fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4]))
        }}})
    output$param <- renderUI({
        req(reactive_objects$data)
        params = sort(unique(reactive_objects$data$CharacteristicName))
        selectInput("param",label="Parameter",choices = params)
    })
    output$fraction <- renderUI({
        req(input$param)
        fracs = unique(subset(reactive_objects$data,reactive_objects$data$CharacteristicName==input$param)$ResultSampleFractionText)
        selectInput("fraction",label = "Fraction", choices=fracs)
    })
    output$unit <- renderUI({
        req(input$fraction)
        if(input$fraction=="NA"){
            units = unique(subset(reactive_objects$data,reactive_objects$data$CharacteristicName==input$param)$ResultMeasure.MeasureUnitCode_plot)
        }else{units = unique(subset(reactive_objects$data,reactive_objects$data$CharacteristicName==input$param&reactive_objects$data$ResultSampleFractionText==input$fraction)$ResultMeasure.MeasureUnitCode_plot)}
        selectInput("unit",label="Units", choices = units)
    })
    output$submit <- renderUI({
        req(input$unit)
        actionButton("submit",label="Submit")
    })

    observeEvent(input$submit,{
        dat = reactive_objects$data
        p_data = subset(dat, dat$CharacteristicName==input$param&dat$ResultSampleFractionText==input$fraction&dat$ResultMeasure.MeasureUnitCode_plot==input$unit)
        reactive_objects$p_data = p_data
        site_ncount = p_data%>%dplyr::group_by(MonitoringLocationIdentifier)%>%dplyr::summarise(ncount = length(unique(ActivityStartDate)))
        site_ncount$radius = scales::rescale(site_ncount$ncount,to=c(5,20))
        sites = reactive_objects$sites
        sites$mapcolor[!sites$MonitoringLocationIdentifier%in%unique(p_data$MonitoringLocationIdentifier)]="#646464"
        sites = merge(sites, site_ncount, all.x = TRUE)
        sites$radius[is.na(sites$radius)]=3
        sites$ncount[is.na(sites$ncount)]=0
        mappino%>%
            clearGroup("Parameter sites")%>%
            hideGroup("All sites")%>%
            addCircleMarkers(data = sites,lng =~LongitudeMeasure, lat=~LatitudeMeasure,group="Parameter sites",color=~mapcolor, radius=~radius,options = pathOptions(pane = "markers"),
                             popup=paste0(
                                 "Organization: ",sites$OrganizationFormalName,
                                 "<br> WQX ID: ",sites$MonitoringLocationIdentifier,
                                 "<br> WQX Name: ",sites$MonitoringLocationName,
                                 "<br> Type: ",sites$MonitoringLocationTypeName,
                                 "<br> Beneficial Uses: ",sites$BeneficialUse,
                                 "<br> NCount: ",sites$ncount
                             ))
        })

    output$timeseries <- renderPlotly({
        req(reactive_objects$p_data)
        dat = reactive_objects$p_data
        dat$ResultMeasureValue_plot = as.numeric(dat$ResultMeasureValue_plot)
        dat = subset(dat, !is.na(dat$ResultMeasureValue_plot))
        dat = dat[order(dat$ActivityStartDate),]
        sites = unique(dat$MonitoringLocationIdentifier)
        # fig <- plot_ly(data=dat, x=~ActivityStartDate, y=~ResultMeasureValue, color=~MonitoringLocationIdentifier, line=list(color=dat$mapcolor))%>%
        #     layout(title=unique(dat$CharacteristicName),font=list(family="Arial"),xaxis=list(title="Date"),yaxis=list(title=unique(dat$ResultMeasure.MeasureUnitCode)))
        fig <- plot_ly(type="scatter")
        for(i in 1:length(sites)){
            d = subset(dat, dat$MonitoringLocationIdentifier==sites[i])
            fig <- fig%>%
                add_trace(data = d, x=~ActivityStartDate, y=~ResultMeasureValue_plot, color=~MonitoringLocationIdentifier,line=list(color=unique(d$mapcolor)), marker=list(color=unique(d$mapcolor)), mode="lines+markers")
        }
        fig <- fig%>%
            layout(title=unique(dat$CharacteristicName),font=list(family="Arial"),xaxis=list(title="Date"),yaxis=list(title=unique(dat$ResultMeasure.MeasureUnitCode_plot)))

    })

    output$getData <- renderUI({
      req(reactive_objects$p_data)
      downloadButton("dwn","Download Plot Data")
    })

    output$dwn <- downloadHandler(
      filename = function(){
        paste0(unique(reactive_objects$p_data$CharacteristicName),"_",reactive_objects$focal_au$ASSESS_ID,".csv")
      },
      content = function(file){
        write.csv(reactive_objects$p_data,file,row.names = FALSE)
      }
    )

}



# Run the application
shinyApp(ui = ui, server = server)
