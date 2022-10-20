library(shiny)
library(shinyjs)
library(shinyBS)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(sortable)
library(RColorBrewer)

template = data.frame("MonitoringLocationIdentifier"=c("MLID_1234"),
                      "ActivityStartDate"=c("01/01/2022"),"CharacteristicName"=c("Pollutant_A"),"ResultMeasureValue"=c(50),"ResultMeasure.MeasureUnitCode"=c("mg/L"),"BeneficialUse"=c("2A"),"NumericCriterion"=c(25),"OtherColumnsOK"=c("NOTE: Make sure your dates are in the mm/dd/yyyy format and all of your ResultMeasureValue and Numeric Criterion values are numeric."))
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
  fluidRow(column(3,radioButtons("aggfun", "Daily aggregating function",choiceValues=c("mean","gmean"),choiceNames=c("Arithmetic mean", "Geometric mean"), selected="mean")),
           bsTooltip("aggfun",title="The geometric mean is typically used to aggregate E. coli to a daily mean, while the standard arithmetic mean is used for all other pollutants."),
           column(3, numericInput("mos","Margin of safety", value=0, min=0, max=1)),
           bsTooltip("mos",title="This field adds an explicit margin of safety to the TMDL calculation and is expressed as a proportion. For example, a value of 0.1 (10% margin of safety) is subtracted from 1 and then multiplied by the numeric criterion and correction factor to obtain the TMDL less a margin of safety. In this example, the calculated TMDL is 90% of the TMDL calculation without a margin of safety (or a proportion of 0)."),
           column(3, numericInput("cf","Loading correction factor", value=24465715)),
           bsTooltip("cf",title="The correction factor acts as a link between the pollutant unit, flow unit, and desired loading unit. The loading unit should resemble [amount]/[time]. The value provided is the correction factor for E. coli expressed in MPN/100 mL and flow expressed in cubic feet per second to obtain the unit MPN/day."),
           column(3, textInput("loadunit","Loading units", value="MPN/day")),
           bsTooltip("loadunit",title="The loading unit should resemble [amount]/[time]. This value is used to correctly label loading figures.")),
  fluidRow(column(3, uiOutput("calcs")),
           column(3, uiOutput("calcs_dwn"))),
  br(),
  tabsetPanel(id="tabs",
    tabPanel("Summary Table",
             br(),
             p("Click the check boxes for the sites you'd like displayed in the table. Then click Create Table. A download button will appear above the table, which will save an Excel spreadsheet to your computer."),
             br(),
             sidebarPanel(fluidRow(column(12,uiOutput("summ_sites"))),
                          fluidRow(column(4, uiOutput("summ_go")))),
             mainPanel(fluidRow(column(4, uiOutput("summ_dwn"))),
                       fluidRow(column(12, DT::dataTableOutput("summ"))))),
    tabPanel("Concentration Plots",
             br(),
             fluidRow(column(12, uiOutput("conc_sites"))),
             fluidRow(column(4, uiOutput("conc_go"))),
             br(),
             fluidRow(column(4, uiOutput("conc_sel")),
                      column(4, uiOutput("conc_agg")),
                      column(4, uiOutput("conc_std"))),
             br(),
             fluidRow(column(4, uiOutput("conc_dwn"))),
             fluidRow(column(12, plotOutput("conc_plot", width="100%", height = "600px")))),
    tabPanel("Loading Plots",
             br(),
             p("The load duration curve plot shows how observed loading and TMDL loading change with flow regime/flow percentile. Please note that all flow values collected at this site are used to create the TMDL curve. However, the percent exceedance value in parentheses only shows the percentage of observed loading values that exceed the TMDL given the total number of observed loading values. The monthly loading bar plot shows how the mean observed loading compares to TMDL loading by month. In all cases, the TMDL represents the TMDL less the margin of safety, entered above."),
             br(),
             sidebarPanel(fluidRow(column(12, uiOutput("load_site"))),
                          fluidRow(column(12, uiOutput("load_sel"))),
                          fluidRow(column(12, uiOutput("load_agg")))),
             mainPanel(fluidRow(column(4, uiOutput(("load_dwn")))),
                       fluidRow(column(12, plotOutput("load_plot",width="100%", height = "400px"))))),
    tabPanel("Flow Plots",
             br(),
             p("The timeseries plot shows all of the flow data for a given site across time. The monthly bar plot shows the average flow by month using all the flow data provided for that site."),
             br(),
             sidebarPanel(fluidRow(column(12, uiOutput("flow_site"))),
                          fluidRow(column(12, uiOutput("flow_sel")))),
             mainPanel(fluidRow(column(4, uiOutput("flow_dwn"))),
                       fluidRow(column(12, plotOutput("flow_plot",width="100%", height = "400px"))))
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Hide tabs until worksheet uploaded
  observe({
    if(is.null(reactives$calcdat)){
      hideTab(inputId="tabs",target="Summary Table")
      hideTab(inputId="tabs",target="Concentration Plots")
      hideTab(inputId="tabs",target="Loading Plots")
      hideTab(inputId="tabs",target="Flow Plots")
    }
  })

  # Welcome!
  observe({
    showNotification("Welcome! Upload your data and select an aggregating function to get started. If you are unsure of the correct data format, please download the template using the button at the top right. Hover over application inputs for more information.", duration = 20)
  })
  # create object to hold reactive values
  reactives = reactiveValues()

  # geometric mean
  gmean = function(x){exp(mean(log(x)))}

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
      dat$ResultMeasureValue = suppressWarnings(as.numeric(as.character(dat$ResultMeasureValue)))
      dat = subset(dat, !is.na(dat$ResultMeasureValue))
      # data is now reactive
      reactives$dat = dat
      params = paste(unique(dat$CharacteristicName), collapse = " and ")
      showModal(modalDialog(title="Check",paste0("This dataset contains ",params," data. If Flow is missing, you may still create summary tables and concentration-based plots by populating the margin of safety and correction factor values as zero before clicking run. Otherwise, populate the margin of safety and correction factor values needed to calculate loading in amount/day." )))
    }
  })
  # Once data loaded, allow user to populate fields and then calculate summaries/aggregations.
  output$calcs <- renderUI({
    req(reactives$dat)
    actionButton("calcs",label="Run summary and loading calculations")
  })
  # Runs TMDL calc function when button is pressed
  observeEvent(input$calcs,{
    calcdat = dwqInsights::tmdlCalcs(idata=reactives$dat, aggFun=input$aggfun, cf = input$cf, mos = input$mos, rec_ssn=c(121,304), irg_ssn=c(135,288), exportfromfunc = FALSE)
    reactives$calcdat = calcdat
    calcdat_out = calcdat
    calcdat_out$Margin_of_Safety = input$mos
    calcdat_out$Correction_Factor = input$cf
    calcdat_out$Loading_Unit = input$loadunit
    reactives$calcdat_out = calcdat_out
    if("Observed_Loading"%in%names(calcdat)){
      perc = unique(calcdat$Flow_Percentile)
      reactives$flow = subset(reactives$dat, reactives$dat$CharacteristicName=="Flow")
      reactives$flow$Date = as.Date(reactives$flow$ActivityStartDate, format="%m/%d/%Y")}
      if(length(perc)==1&any(is.na(perc))){
        showModal(modalDialog(title="Warning","This dataset contains flow data that is not associated with a monitoring location that has pollutant data. Loadings will not be calculated."))
      }else{
        reactives$loading = subset(calcdat, !is.na(calcdat$Flow_Percentile))
        }
    # summary values for all data
    dat_agg = calcdat%>%dplyr::group_by(MonitoringLocationIdentifier, CharacteristicName,ResultMeasure.MeasureUnitCode)%>%dplyr::summarise(Date_Range = paste0(min(Date)," to ",max(Date)),Sample_Size=length(DailyResultMeasureValue),Minimum_Concentration = min(DailyResultMeasureValue),Arithmetic_Mean_Concentration=mean(DailyResultMeasureValue),Maximum_Concentration = max(DailyResultMeasureValue), Percent_Exceeding_Criterion = sum(Exceeds)/length(Exceeds)*100)
    rec_mean = calcdat%>%dplyr::filter(Rec_Season=="rec")%>%dplyr::group_by(MonitoringLocationIdentifier, CharacteristicName,ResultMeasure.MeasureUnitCode)%>%dplyr::summarise(Rec_Season_Arithmetic_Mean_Concentration=mean(DailyResultMeasureValue),Rec_Season_Percent_Exceeding_Criterion = sum(Exceeds)/length(Exceeds)*100)
    dat_agg = merge(dat_agg, rec_mean, all = TRUE)
    if(input$aggfun=="gmean"){
      dat_agg_geo = calcdat%>%dplyr::group_by(MonitoringLocationIdentifier, CharacteristicName, ResultMeasure.MeasureUnitCode)%>%dplyr::summarise(Geometric_Mean_Concentration = gmean(DailyResultMeasureValue))
      rec_geo = calcdat%>%dplyr::filter(Rec_Season=="rec")%>%dplyr::group_by(MonitoringLocationIdentifier, CharacteristicName, ResultMeasure.MeasureUnitCode)%>%dplyr::summarise(Rec_Season_Geometric_Mean_Concentration=gmean(DailyResultMeasureValue))
      dat_agg_geo = merge(dat_agg_geo, rec_geo, all = TRUE)
      dat_agg = merge(dat_agg, dat_agg_geo, all = TRUE)
    }
    dat_agg = dat_agg[order(dat_agg$MonitoringLocationIdentifier),]
    reactives$summ <- dat_agg
    })

  # Download data from tmdl calcs function.
  output$calcs_dwn <- renderUI({
    req(reactives$calcdat)
    downloadButton("calcs_dwn1","Download Calculation Spreadsheet")
  })

  output$calcs_dwn1 <- downloadHandler(
    filename = function() {
    "loadCalcs_output_spreadsheet.csv"
  },
  content = function(file) {
    write.csv(reactives$calcdat_out, file, row.names = FALSE)
  })

  # Show tabs after worksheet uploaded based on data provided.
  observe({
    if(!is.null(reactives$calcdat)){
      showTab(inputId="tabs",target="Summary Table")
      showTab(inputId="tabs",target="Concentration Plots")
    }
    if(!is.null(reactives$loading)){
      showTab(inputId="tabs",target="Loading Plots")
    }
    if(!is.null(reactives$flow)){
      showTab(inputId="tabs",target="Flow Plots")
    }
  })

  # Makes site list appear when loading and summary calcs finish
  output$summ_sites <- renderUI({
    req(reactives$summ)
    sites = unique(reactives$calcdat$MonitoringLocationIdentifier)
    sites = sites[order(sites)]
    checkboxGroupInput("summ_sites","Select sites of interest", choices=c(sites), selected=c(sites))
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
    DT::datatable(reactives$summ_filter, rownames=FALSE)
  })

  # Makes site list appear when loading and summary calcs finish
  output$conc_sites <- renderUI({
    req(reactives$summ)
    pollsites = subset(reactives$calcdat, !is.na(reactives$calcdat$CharacteristicName))
    sites = unique(pollsites$MonitoringLocationIdentifier)
    sites = sites[order(sites)]
    bucket_list(header="Drag sites to right column in the order you'd like displayed in the plot. Top to bottom will become left to right in downstream-upstream plot.",
                group_name = "conc_group",
                orientation = "horizontal",
                add_rank_list(text = "Drag from here",
                              labels=as.list(sites), input_id = "conc_list_1"),
                add_rank_list(text="...to here (MAX 8 SITES)",
                              labels=NULL,
                              input_id = "conc_list_2"))
    })

  # Go button to create plot
  output$conc_go <- renderUI({
    req(input$conc_list_2)
    actionButton("conc_go","Confirm site list and order")
  })

  # Filter dataset to selected sites after clicking 'go'
  observeEvent(input$conc_go,{
    dat = subset(reactives$calcdat, reactives$calcdat$MonitoringLocationIdentifier%in%input$conc_list_2&!reactives$calcdat$CharacteristicName%in%c("Flow"))
    sites_num = length(unique(dat$MonitoringLocationIdentifier))
    if(sites_num>8){
      showModal(modalDialog(title="Whoops!","Please select 8 or fewer sites to plot."))
    }else{
      reactives$conc = dat}
  })

  # Radio button to select plot to make
  output$conc_sel <- renderUI({
    req(reactives$conc)
    radioButtons("conc_sel","Select plot type",choices = c("Dot Plot","Monthly Means","Timeseries"))
  })

  # Upstream-downstream aggregating function
  output$conc_agg <- renderUI({
    req(input$conc_sel)
    if(input$conc_sel%in%c("Dot Plot","Monthly Means")){
      radioButtons("conc_agg", label="Choose site-level aggregating function (doesn't matter for timeseries)",choiceValues=c("mean","gmean"),choiceNames=c("Arithmetic mean", "Geometric mean"))
    }
      })

  output$conc_std <- renderUI({
    req(input$conc_sel)
    numericInput("conc_std","Add additional standard line",value=0)
  })

  concplot <- reactive({
    req(input$conc_agg)
    conc = reactives$conc
    conc = subset(conc, !is.na(conc$DailyResultMeasureValue))
    conc$month = lubridate::month(conc$Date, label=TRUE)
    conc = conc[order(conc$Date),]
    conc$MonitoringLocationIdentifier = factor(as.character(conc$MonitoringLocationIdentifier), levels = input$conc_list_2)
    conc <<- conc
    num_crit = unique(conc$NumericCriterion[!is.na(conc$NumericCriterion)])
    if(input$conc_agg=="gmean"){func=gmean}else{func="mean"}
    if(input$conc_sel=="Dot Plot"){
      reactives$width=7
      reactives$height=7
      if(length(num_crit)>1){
        g = ggplot(data=conc, aes(x=MonitoringLocationIdentifier,y=DailyResultMeasureValue))+geom_jitter(aes(fill=factor(MonitoringLocationIdentifier)), position=position_jitter(0.1), shape=21, size=2, color="#646464")+scale_fill_manual(values=colorz)+theme_classic()+labs(x="Monitoring Location ID", y=unique(conc$ResultMeasure.MeasureUnitCode))+stat_summary(fun=func, geom="point", fill="#FFB800",color="black", size=2.5, shape=23)+theme(legend.position = "none", axis.text.x=element_text(angle=45, hjust=1)) # NOTE, should adjust numeric criterion line to accommodate variable criteria (e.g. hardness-dependent)
      }else{g = ggplot(data=conc, aes(x=MonitoringLocationIdentifier,y=DailyResultMeasureValue))+geom_jitter(aes(fill=factor(MonitoringLocationIdentifier)), position=position_jitter(0.1), shape=21, size=2, color="#646464")+scale_fill_manual(values=colorz)+theme_classic()+labs(x="Monitoring Location ID", y=unique(conc$ResultMeasure.MeasureUnitCode))+stat_summary(fun=func, geom="point", fill="#FFB800",color="black", size=2.5, shape=23)+geom_hline(yintercept=unique(conc$NumericCriterion), color="#cb181d",size=0.5)+theme(legend.position = "none", axis.text.x=element_text(angle=45, hjust=1)) # NOTE, should adjust numeric criterion line to accommodate variable criteria (e.g. hardness-dependent)
            }
          }
    if(input$conc_sel=="Timeseries"){
      reactives$width=8
      reactives$height=10
      if(length(num_crit)>1){
        g = ggplot(conc, aes(Date,DailyResultMeasureValue, fill=as.factor(MonitoringLocationIdentifier)))+scale_x_date(limits=c(min(conc$Date), max(conc$Date)))+geom_point(shape=21,color="#646464", size=2)+facet_wrap(vars(MonitoringLocationIdentifier), scales="free", ncol=1)+theme_classic()+labs(x="Date", y=unique(conc$ResultMeasure.MeasureUnitCode))+geom_line(aes(y=NumericCriterion), color="#cb181d",size=0.5)+theme(legend.position = "none")+scale_fill_manual(values=colorz)
      }else{
        g = ggplot(conc, aes(Date,DailyResultMeasureValue, fill=as.factor(MonitoringLocationIdentifier)))+scale_x_date(limits=c(min(conc$Date), max(conc$Date)))+geom_point(shape=21,color="#646464", size=2)+facet_wrap(vars(MonitoringLocationIdentifier), scales="free", ncol=1)+theme_classic()+labs(x="Date", y=unique(conc$ResultMeasure.MeasureUnitCode))+geom_hline(yintercept=unique(conc$NumericCriterion), color="#cb181d",size=0.5)+theme(legend.position = "none")+scale_fill_manual(values=colorz)
      }
      }
    if(input$conc_sel=="Monthly Means"){
      reactives$width=8
      reactives$height=10
      conc = conc[order(conc$month),]
      if(length(num_crit)>1){
        g = ggplot(conc, aes(month, DailyResultMeasureValue, fill=factor(MonitoringLocationIdentifier)))+geom_blank()+geom_rect(aes(xmin=4.5,ymin=0,xmax=10.5,ymax=max(DailyResultMeasureValue)), fill="#D3D3D3")+scale_x_discrete(limits=month.abb)+geom_jitter(position=position_jitter(0), size=3, shape=21, color="#646464", alpha=0.65)+facet_wrap(vars(MonitoringLocationIdentifier), scales="free", ncol=1)+theme_classic()+labs(x="Month", y=unique(conc$ResultMeasure.MeasureUnitCode))+theme(legend.position = "none")+scale_fill_manual(values=colorz)+stat_summary(fun=func, geom="point", fill="#FFB800",color="black", size=3, shape=23)
      }else{
        g = ggplot(conc, aes(month, DailyResultMeasureValue, fill=factor(MonitoringLocationIdentifier)))+geom_blank()+geom_rect(aes(xmin=4.5,ymin=0,xmax=10.5,ymax=max(DailyResultMeasureValue)), fill="#D3D3D3")+scale_x_discrete(limits=month.abb)+geom_jitter(position=position_jitter(0), size=3, shape=21, color="#646464", alpha=0.65)+facet_wrap(vars(MonitoringLocationIdentifier), scales="free", ncol=1)+theme_classic()+labs(x="Month", y=unique(conc$ResultMeasure.MeasureUnitCode))+geom_hline(yintercept=unique(conc$NumericCriterion), color="#cb181d",size=0.5)+theme(legend.position = "none")+scale_fill_manual(values=colorz)+stat_summary(fun=func, geom="point", fill="#FFB800",color="black", size=3, shape=23)
      }
    }
    if(input$conc_std>0){
      g = g+geom_hline(yintercept=unique(input$conc_std), color="#ff781f",size=0.5)
    }
    print(g)
      })

  # Create US-DS plot in app
  output$conc_plot <- renderPlot({
    req(input$conc_agg)
    concplot()
      })

  # download US-DS plot
  output$conc_dwn <- renderUI({
    req(input$conc_agg)
    downloadButton("conc_dwn1",label="Download plot")
  })

  # Download Handler
  output$conc_dwn1 <- downloadHandler(
    filename = function() {
      "loadFigs_concplot.png"
    },
    content = function(file) {
      ggsave(file, concplot(), width=reactives$width, height=reactives$height, units="in")
    },
    contentType='image/png'
  )

  # Loading plots
  output$load_site <- renderUI({
    req(reactives$loading)
    sites = unique(subset(reactives$loading, !is.na(reactives$loading$Observed_Loading))$MonitoringLocationIdentifier)
    selectInput("load_site","Select Site", choices=sites)
  })

  output$load_sel <- renderUI({
    req(reactives$loading)
    radioButtons("load_sel","Select Plot Type", choices=c("Load Duration Curve","Monthly Loading Bar Plot"))
  })

  # Loading aggregating function
  output$load_agg <- renderUI({
    req(reactives$loading)
    radioButtons("load_agg", label="Choose month-level aggregating function (doesn't matter for load duration curve)",choiceValues=c("mean","gmean"),choiceNames=c("Arithmetic mean", "Geometric mean"))
  })

  output$load_dwn <- renderUI({
    req(input$load_sel)
    downloadButton("load_dwn1","Download Plot")
  })

  output$load_dwn1 <- downloadHandler(
    filename = function() {
      "loadFigs_loadplot.png"
    },
    content = function(file) {
      ggsave(file, loadplot(), width=reactives$width, height=reactives$height, units="in")
    },
    contentType='image/png'
  )

  loadplot <- reactive({
    req(input$load_sel)
    reactives$width=8
    reactives$height=6
    loading = reactives$loading
    loading = subset(loading, loading$MonitoringLocationIdentifier%in%c(input$load_site))
    if(input$load_sel=="Load Duration Curve"){
      flow_rej = data.frame(regime=c("High \nFlows","Moist \nConditions","Mid-Range \nFlows","Dry \nConditions","Low \nFlows"),place=c(5,25,50,75,95))
      loading = loading[order(loading$Flow_Percentile),]
      exceed = subset(loading, !is.na(loading$Observed_Loading))
      exceed = within(exceed,{
        regime=NA
        regime[Flow_Percentile<10] = "High \nFlows"
        regime[Flow_Percentile>9&Flow_Percentile<40] = "Moist \nConditions"
        regime[Flow_Percentile>39&Flow_Percentile<60] = "Mid-Range \nFlows"
        regime[Flow_Percentile>59&Flow_Percentile<90] = "Dry \nConditions"
        regime[Flow_Percentile>89&Flow_Percentile<101] = "Low \nFlows"
      })
      exceed = exceed%>%group_by(MonitoringLocationIdentifier, regime)%>%dplyr::summarise(perc_exceed=round(length(Exceeds[Exceeds==1])/length(Exceeds)*100,digits=0))
      exceed = merge(exceed, flow_rej, all = TRUE)
      exceed$perc_exceed[is.na(exceed$perc_exceed)] = 0
      exceed$label = paste0(exceed$regime,"\n(",exceed$perc_exceed,"%)")
      why = max(c(loading$TMDL,loading$Observed_Loading), na.rm = TRUE)*0.8
      g = ggplot(loading, aes(x=Flow_Percentile))+geom_blank()+geom_vline(xintercept=c(10,40,60,90),linetype=2)+geom_line(aes(y=TMDL, color="TMDL"),color="#034963",size=1.5)+geom_point(aes(y=Observed_Loading, color="Observed_Loading"),shape=21, color="#464646",fill="#00a1c6",size=3)+theme_classic()+labs(x="Flow Percentile",y=input$loadunit)+annotate("text",x=exceed$place,y=why, label=exceed$label)+scale_color_manual(name = "",values = c( "TMDL" = "#034963", "Observed_Loading" = "#00a1c6"),labels = c("TMDL", "Observed Loading"))
     }
    if(input$load_sel=="Monthly Loading Bar Plot"){
      loading1 = subset(loading, !is.na(loading$Observed_Loading))
      ldc_month = loading1%>%dplyr::select(Date,MonitoringLocationIdentifier,TMDL,Observed_Loading)%>%tidyr::pivot_longer(cols=c(TMDL,Observed_Loading),names_to="Type",values_to="Loading",values_drop_na=TRUE)
      ldc_month$month = lubridate::month(ldc_month$Date, label=TRUE, abbr=TRUE)
      if(input$load_agg=="mean"){
        ldc_month1 = ldc_month%>%dplyr::group_by(MonitoringLocationIdentifier,Type,month)%>%dplyr::summarise(mean_Load = mean(Loading))
      }else{
        ldc_month1 = ldc_month%>%dplyr::group_by(MonitoringLocationIdentifier,Type,month)%>%dplyr::summarise(mean_Load = gmean(Loading))
      }
        g = ggplot(ldc_month1, aes(x=month, y=mean_Load, fill=Type))+geom_blank()+theme_classic()+geom_rect(aes(xmin=4.5,ymin=0,xmax=10.5,ymax=max(ldc_month1$mean_Load*1.1)), fill="#D3D3D3")+scale_x_discrete(limits=month.abb)+labs(x="Month",y=input$loadunit)+geom_col(position="dodge", color="#646464")+scale_fill_manual(values=c("#00a1c6","#034963"),name="",breaks=c("Observed_Loading","TMDL"),labels=c("Observed","TMDL"))+guides(color="none")
        }
    print(g)
  })

  # Create Loading plot in app
  output$load_plot <- renderPlot({
    req(input$load_sel)
    loadplot()
  })

  # Flow plots
  output$flow_site <- renderUI({
    req(reactives$flow)
    selectInput("flow_site","Select Site", choices=unique(reactives$flow$MonitoringLocationIdentifier))
  })

  output$flow_sel <- renderUI({
    req(reactives$flow)
    radioButtons("flow_sel","Select Plot Type", choices=c("Timeseries","Monthly Flow Bar Plot"))
  })

  output$flow_dwn <- renderUI({
    req(input$flow_sel)
    downloadButton("flow_dwn1","Download Plot")
  })

  output$flow_dwn1 <- downloadHandler(
    filename = function() {
      "loadFigs_flowplot.png"
    },
    content = function(file) {
      ggsave(file, flowplot(), width=reactives$width, height=reactives$height, units="in")
    },
    contentType='image/png'
  )

  flowplot <- reactive({
    req(input$flow_sel)
    reactives$width=8
    reactives$height=6
    flow = reactives$flow
    flow = subset(flow, flow$MonitoringLocationIdentifier%in%c(input$flow_site))
    unit = unique(flow$ResultMeasure.MeasureUnitCode)
    if(input$flow_sel=="Timeseries"){
      g=ggplot(flow, aes(x=Date,y=ResultMeasureValue))+geom_area(color="#646464",fill="#0b86a3")+labs(x="Date",y=unit)+theme_classic()
        }
    if(input$flow_sel=="Monthly Flow Bar Plot"){
      flow$month = lubridate::month(flow$Date)
      flo_month = flow%>%dplyr::group_by(month)%>%dplyr::summarise(mean_monthly = mean(ResultMeasureValue, na.rm=TRUE))
      g = ggplot(flo_month, aes(x=month, y=mean_monthly))+geom_col(color="#646464",fill="#0b86a3")+labs(x="Month",y=unit)+theme_classic()+scale_x_discrete(limits=month.abb)
        }
    print(g)
  })

  # Create US-DS plot in app
  output$flow_plot <- renderPlot({
    req(input$flow_sel)
    flowplot()
  })

}

# Run the application
shinyApp(ui = ui, server = server)
