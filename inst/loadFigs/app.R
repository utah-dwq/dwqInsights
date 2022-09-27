library(shiny)
library(shinyjs)
library(dplyr)
library(wqTools)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(plotly)
library(ggplot2)
library(sortable)
library(RColorBrewer)

template = data.frame("MonitoringLocationIdentifier"=c("MLID_1234"),
                      "ActivityStartDate"=c("01/01/2022"),"CharacteristicName"=c("Pollutant_A"),"ResultMeasureValue"=c(50),"ResultMeasure.MeasureUnitCode"=c("mg/L"),"BeneficialUse"=c("2A"),"NumericCriterion"=c(25),"OtherColumnsOK"=c("blank"))
colorz = c("#034963","#0b86a3","#00a1c6","#cde3e2", "#BFD7B5","#D1CAA1","#D1D2F9","#77625C","#EFA9AE")
gmean = function(x){exp(mean(log(x)))}
# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel(
    title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo.png', height = 50, width = 143), target="_blank"),
    tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="4siteR:Assessment Planning Tool")
  ),
  titlePanel("loadFigs: Build Pretty Pollutant Loading Figures"),
  fluidRow(column(4, fileInput("uplode","Upload data",accept=".csv")),
           column(2, downloadButton("template","Download template"), offset=6)),
  fluidRow(column(3,radioButtons("aggfun", "Daily aggregating function",choiceValues=c("mean","gmean"),choiceNames=c("Arithmetic mean", "Geometric mean"), selected="Arithmetic mean")),
           column(3, numericInput("mos","Margin of safety", value=0, min=0, max=1)),
           column(3, numericInput("cf","Loading correction factor", value=0)),
           column(3, textInput("loadunit","Loading units", value=""))),
  fluidRow(column(2, uiOutput("calcs"))),
  br(),
  navlistPanel("Output Menu",id="outmenu",
               tabPanel("Summary Table",
                        fluidRow(column(12, uiOutput("summ_sites"))),
                        fluidRow(column(4, uiOutput("summ_go"))),
                        br(),
                        fluidRow(column(4, uiOutput("summ_dwn"))),
                        fluidRow(column(12, DT::dataTableOutput("summ")))),
               tabPanel("Upstream-Downstream",
                        fluidRow(column(12, uiOutput("ud_sites"))),
                        fluidRow(column(4, uiOutput("ud_agg"))),
                        fluidRow(column(4, uiOutput("ud_go"))),
                        br(),
                        fluidRow(column(4, uiOutput("ud_dwn"))),
                        fluidRow(column(12, plotOutput("ud_plot")))
                        ),
               tabPanel("Timeseries"),
               tabPanel("Monthly Concentration"),
               tabPanel("Monthly Loading"),
               tabPanel("Load Duration Curve")
               )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # create object to hold reactive values
  reactives = reactiveValues()
  # allows user to download the data template
  output$template <- downloadHandler(
    filename = function() {
      "loadFigs_data_template.csv"
    },
    content = function(file) {
      write.csv(template, file, row.names = FALSE)
    }
  )
  # Upload dataset
  observeEvent(input$uplode,{
    file=input$uplode$datapath
    dat=read.csv(file)
    # check to ensure all required column names present
    ds_names = names(dat)
    req_names = c("MonitoringLocationIdentifier","ActivityStartDate","CharacteristicName","ResultMeasureValue","ResultMeasure.MeasureUnitCode","BeneficialUse","NumericCriterion")
    missing = req_names[!req_names%in%ds_names]
    if(length(missing)>0){
      text = paste(missing,collapse=", ")
      showModal(modalDialog(title="Whoops!",paste0(text," column(s) missing from input dataset. Refresh this app and add required column(s) before running.")))
    }else{
      # check to ensure date is in the right format
      date_test = as.Date(dat$ActivityStartDate[1],format="%m/%d/%Y")
      if(is.na(date_test)){
        showModal(modalDialog(title="Whoops!","The ActivityStartDate column needs to be in the format mm/dd/YYYY. Please convert dates and try again."))
      }else{
        # data is now reactive
        reactives$dat = dat
        params = paste(unique(dat$CharacteristicName), collapse = "and ")
        showModal(modalDialog(title="Check",paste0("This dataset contains ",params," data. If Flow is missing, you may still create summary tables and concentration-based plots by populating the margin of safety and correction factor values as zero before clicking run. Otherwise, populate the margin of safety and correction factor values needed to calculate loading in amount/day." )))
        }
      }
  })
  # Once data loaded, allow user to populate fields and then calculate summaries/aggregations.
  output$calcs <- renderUI({
    req(reactives$dat)
    actionButton("calcs",label="Run summary and loading calculations")
  })
  # Runs TMDL calc function when button is pressed
  observeEvent(input$calcs,{
    calcdat = tmdlCalcs(idata=reactives$dat, aggFun=input$aggfun, input$cf, input$mos, rec_ssn=c(121,304), irg_ssn=c(135,288), exportfromfunc = FALSE)
    reactives$calcdat = calcdat
    # summary values for all data
    gmean = function(x){exp(mean(log(x)))}
    dat_agg = calcdat%>%dplyr::group_by(MonitoringLocationIdentifier, CharacteristicName,ResultMeasure.MeasureUnitCode)%>%dplyr::summarise(daterange = paste0(min(Date)," to ",max(Date)),samplesize=length(DailyResultMeasureValue),minconc = min(DailyResultMeasureValue),arithmean=mean(DailyResultMeasureValue),maxconc = max(DailyResultMeasureValue), perc_exc = sum(Exceeds)/length(Exceeds)*100)
    rec_mean = calcdat%>%dplyr::filter(Rec_Season=="rec")%>%dplyr::group_by(MonitoringLocationIdentifier, CharacteristicName,ResultMeasure.MeasureUnitCode)%>%dplyr::summarise(rec_ssn_arithmean=mean(DailyResultMeasureValue),rec_ssn_perc_exc = sum(Exceeds)/length(Exceeds)*100)
    dat_agg = merge(dat_agg, rec_mean, all = TRUE)
    if(input$aggfun=="gmean"){
      dat_agg_geo = calcdat%>%dplyr::group_by(MonitoringLocationIdentifier, CharacteristicName, ResultMeasure.MeasureUnitCode)%>%dplyr::summarise(geomean = gmean(DailyResultMeasureValue))
      rec_geo = calcdat%>%dplyr::filter(Rec_Season=="rec")%>%dplyr::group_by(MonitoringLocationIdentifier, CharacteristicName, ResultMeasure.MeasureUnitCode)%>%dplyr::summarise(rec_ssn_geomean=gmean(DailyResultMeasureValue))
      dat_agg_geo = merge(dat_agg_geo, rec_geo, all = TRUE)
      dat_agg = merge(dat_agg, dat_agg_geo, all = TRUE)
    }
    if("DailyFlowValue"%in%colnames(calcdat)){
      flo_agg = calcdat%>%dplyr::filter(!is.na(DailyFlowValue))%>%group_by(MonitoringLocationIdentifier, ResultMeasure.MeasureUnitCode)%>%summarise(daterange = paste0(min(Date)," to ",max(Date)),samplesize=length(DailyFlowValue),minconc = min(DailyFlowValue),arithmean=mean(DailyFlowValue),maxconc = max(DailyFlowValue))
      dat_agg = plyr::rbind.fill(dat_agg, flo_agg)
      dat_agg$CharacteristicName[is.na(dat_agg$CharacteristicName)] = "Flow"
    }
    dat_agg = dat_agg[order(dat_agg$MonitoringLocationIdentifier),]
    reactives$summ <- dat_agg
    })

  # Makes site list appear when loading and summary calcs finish
  output$summ_sites <- renderUI({
    req(reactives$summ)
    sites = unique(reactives$calcdat$MonitoringLocationIdentifier)
    sites = sites[order(sites)]
    checkboxGroupInput("summ_sites","Select sites of interest", choices=c(sites), selected=c(sites),inline=TRUE)
  })

  # Go button to create table
  output$summ_go <- renderUI({
    req(reactives$summ)
    actionButton("summ_go","Create Table")
  })
  # Filter dataset to selected sites
  observeEvent(input$summ_go,{
    reactives$summ_filter = subset(reactives$summ, reactives$summ$MonitoringLocationIdentifier%in%input$summ_sites)
  })

  # Downloader
  output$summ_dwn <- renderUI({
    req(reactives$summ_filter)
    downloadButton("summ_dwn1",label="Download Summary Table")
  })

  # Download Handler
  output$summ_dwn1 <- downloadHandler(
    filename = function() {
      "loadFigs_summary_table.csv"
    },
    content = function(file) {
      write.csv(reactives$summ_filter, file, row.names = FALSE)
    }
  )

  # Create data table
  output$summ <- DT::renderDataTable({
    req(reactives$summ_filter)
    DT::datatable(reactives$summ_filter)
  })

  # Makes site list appear when loading and summary calcs finish
  output$ud_sites <- renderUI({
    req(reactives$summ)
    sites = unique(reactives$calcdat$MonitoringLocationIdentifier)
    sites = sites[order(sites)]
    bucket_list(header="Drag sites to right column in the order you'd like displayed in the plot",
                group_name = "ud_group",
                orientation = "horizontal",
                add_rank_list(text = "Drag from here",
                              labels=as.list(sites), input_id = "ud_list_1"),
                add_rank_list(text="...to here (MAX 8 SITES)",
                              labels=NULL,
                              input_id = "ud_list_2"))
    })


  # Upstream-downstream aggregating function
  output$ud_agg <- renderUI({
    req(input$ud_list_2)
    radioButtons("ud_agg", label="Choose site-level aggregating function",choiceValues=c("mean","gmean"),choiceNames=c("Arithmetic mean", "Geometric mean"), selected="Arithmetic mean")
  })

  # Go button to create plot
  output$ud_go <- renderUI({
    req(input$ud_agg)
    actionButton("ud_go","Create upstream-downstream plot")
  })

  # Filter dataset to selected sites
  observeEvent(input$ud_go,{
    dat = subset(reactives$calcdat, reactives$calcdat$MonitoringLocationIdentifier%in%input$ud_list_2&!reactives$calcdat$CharacteristicName%in%c("Flow"))
    sites_num = length(unique(dat$MonitoringLocationIdentifier))
    if(sites_num>8){
      showModal(modalDialog(title="Whoops!","Please select 8 or fewer sites to plot."))
    }else{
      reactives$ud = dat}
      })

  udplot <- reactive({
    ud = reactives$ud
    ggplot(data=ud, aes(x=MonitoringLocationIdentifier,y=DailyResultMeasureValue))+geom_jitter(aes(fill=factor(MonitoringLocationIdentifier)), position=position_jitter(0.1), shape=21, size=2, color="#646464")+scale_fill_manual(values=colorz)+theme_classic()+labs(x="Monitoring Location ID", y=unique(ud$ResultMeasure.MeasureUnitCode))+stat_summary(fun=input$ud_agg, geom="point", fill="#FFB800",color="black", size=2.5, shape=23)+geom_hline(yintercept=unique(ud$NumericCriterion), color="#cb181d",size=0.5)+theme(legend.position = "none", axis.text.x=element_text(angle=45, hjust=1)) # NOTE, should adjust numeric criterion line to accommodate variable criteria (e.g. hardness-dependent)
  })

  # Create US-DS plot in app
  output$ud_plot <- renderPlot({
    req(reactives$ud)
    udplot()
      })

  # download US-DS plot
  output$ud_dwn <- renderUI({
    req(reactives$ud)
    downloadButton("ud_dwn1",label="Download plot")
  })

  # Download Handler
  output$ud_dwn1 <- downloadHandler(
    filename = function() {
      "loadFigs_upstream_downstream.png"
    },
    content = function(file) {
      ggsave(file, udplot())
    },
    contentType='image/png'
  )


}

# Run the application
shinyApp(ui = ui, server = server)
