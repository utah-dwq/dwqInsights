#' Prep data for TMDL analysis and plotting
#'
#' This function takes parameter concentration and flow data (if applicable) to calculate mean concentrations and loadings on a daily basis, with additional information provided for other aggregation exercises. Produces outputs that may be fed into plotting functions within the dwqInsights package.
#' @param idata A dataframe object containing parameter and flow data for sites of interest. Object has same column names as an EPA Water Quality Portal narrowresult object, with NumericCriterion and BeneficialUse columns added. Note that this function does not handle detection limits. These must be handled prior to calculating loading.
#' @param cf Numeric. The correction factor to be applied to the loading calculation. Ensure this correction factor is in the correct units to accommodate flow and parameter units.
#' @param mos Numeric. A proportion between 0 and 1 to be used as the margin of safety applied to the TMDL calculations. In other words, this proportion describes the % reduction applied to the straight TMDL loading value based on the standard.
#' @param rec_ssn Numeric. A two-object vector of year days signifying the start and end to the rec season.
#' @param irg_ssn Numeric. A two-object vector of year days signifying the start and end to the irrigation season.
#' @param aggFun String. A character object describing the function used to aggregate to daily/monthly/rec season/irrigation season values. Most typically will be one of the following: gmean, mean, max, min.
#' @param exportfromfunc Logical. Indicates whether workbook should be exported from tmdlCalcs function, or skipped if using Shiny interface. Default is FALSE to accommodate Shiny use.
#' @return The output includes a new Excel workbook with the name of the original file plus today's date.xlsx, as well as the following dataframes, composed within a list: ecoli concentrations, flow data, ldc data, monthly means, rec/non rec means, and irg/non irg means.
#' @export
#' @import lubridate
#' @import plyr
#' @import dplyr

# test = tmdlCalcs(wb_path = wb_path, aggFun = "gmean", cf=24465715, mos=0, exportfromfunc = TRUE)

tmdlCalcs <- function(idata, aggFun="mean", cf, mos= 0, rec_ssn=c(121,304), irg_ssn=c(135,288), exportfromfunc = FALSE){
  warning("This function does not handle special characters, detection limits or fractions and does not calculate correction-factor dependent criteria. Please make these adjustments prior to using tmdlCalcs.")

  ## Calculation functions needed for plotting and assessment ##
  perc.red <- function(x,y){100-x/y*100} # percent reduction equation where x = capacity and y = observed
  flow_perc <- function(x){(1-percent_rank(x))*100} # gives each flow measurement a percent rank (what percentage of flows are higher than value?)

  if(aggFun=="gmean"){
    aggFun = function(x){exp(mean(log(x)))}
  }

  # Determine calendar season - taken from https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
  getSeason <- function(DATES) {
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))
    ifelse (d >= WS | d < SE, "Winter",
            ifelse (d >= SE & d < SS, "Spring",
                    ifelse (d >= SS & d < FE, "Summer", "Fall")))}
  param.dat = idata

  # Load csv
  ds_names = names(param.dat)
  req_names = c("MonitoringLocationIdentifier","ActivityStartDate","CharacteristicName","ResultMeasureValue","ResultMeasure.MeasureUnitCode","BeneficialUse","NumericCriterion")
  missing = req_names[!req_names%in%ds_names]
  if(length(missing)>0){
    text = paste(missing,collapse=", ")
    stop(paste0(text," column(s) missing from input dataset. Add required column(s) before running."))
    }
  start = dim(param.dat)[1]
  param.dat$Date <- as.Date(param.dat$ActivityStartDate, format="%m/%d/%Y")
  param.dat$ResultMeasureValue = suppressWarnings(as.numeric(as.character(param.dat$ResultMeasureValue)))
  param.dat = subset(param.dat, !is.na(param.dat$ResultMeasureValue))
  end = dim(param.dat)[1]

  if(!(start==end)){
    n = start-end
    warning(paste0(n," records removed because result value was non-numeric or NA"))
  }

  # Show parameters and units to inform user composition of dataset
  tbl = unique(param.dat[,c("CharacteristicName","ResultMeasure.MeasureUnitCode")])
  tbl$concat = apply(tbl, 1 , paste, collapse = "-" )
  # paste0("The following parameters are in the dataset: ",paste(tbl$concat, collapse=", "),". Press [enter] to continue or [esc] to exit.")

  # Aggregate to daily values
  param.agg = subset(param.dat, !param.dat$CharacteristicName%in%c("Flow"))
  param.agg.dv = aggregate(ResultMeasureValue~BeneficialUse+MonitoringLocationIdentifier+Date+CharacteristicName+ResultMeasure.MeasureUnitCode+NumericCriterion, data=param.agg, FUN=aggFun)
  names(param.agg.dv)[names(param.agg.dv)=="ResultMeasureValue"] = "DailyResultMeasureValue"
  param.agg.dv$Exceeds = ifelse(param.agg.dv$DailyResultMeasureValue>param.agg.dv$NumericCriterion,1,0)
  param.agg.dv$Season = getSeason(param.agg.dv$Date)

  # Aggregate to monthly means
  # month.agg = param.agg.dv
  # month.agg$month = lubridate::month(month.agg$Date)
  # month.agg = aggregate(DailyResultMeasureValue~MonitoringLocationIdentifier+month+CharacteristicName+ResultMeasure.MeasureUnitCode+NumericCriterion, data=month.agg, FUN=aggFun)
  # names(month.agg)[names(month.agg)=="DailyResultMeasureValue"] = "MonthResultMeasureValue"
  # month.agg$Percent_Reduction = perc.red(month.agg$NumericCriterion, month.agg$MonthResultMeasureValue)
  # month.agg$Percent_Reduction[month.agg$Percent_Reduction<0] = 0

  # Rec season and Irrigation season
  param.agg.dv$Rec_Season = ifelse(lubridate::yday(param.agg.dv$Date)>=rec_ssn[1]&lubridate::yday(param.agg.dv$Date)<=rec_ssn[2],"rec","not rec")
  param.agg.dv$Irg_Season = ifelse(lubridate::yday(param.agg.dv$Date)>=irg_ssn[1]&lubridate::yday(param.agg.dv$Date)<=irg_ssn[2],"irrigation","not irrigation")

  if("Flow"%in%unique(tbl$CharacteristicName)){
    flow.agg = subset(param.dat, param.dat$CharacteristicName%in%c("Flow"))
    flow.agg = aggregate(ResultMeasureValue~MonitoringLocationIdentifier+Date+ResultMeasure.MeasureUnitCode, data=flow.agg, FUN="mean")
    names(flow.agg)[names(flow.agg)=="ResultMeasureValue"] = "DailyFlowValue"
    names(flow.agg)[names(flow.agg)=="ResultMeasure.MeasureUnitCode"] = "FlowUnit"
    flow.agg$Flow_Percentile = flow_perc(flow.agg$DailyFlowValue)
    param.agg.dv = merge(param.agg.dv, flow.agg, all.x = TRUE)
    param.agg.dv$Observed_Loading = param.agg.dv$DailyResultMeasureValue*cf*param.agg.dv$DailyFlowValue
    param.agg.dv$TMDL = as.numeric(param.agg.dv$NumericCriterion)*cf*param.agg.dv$DailyFlowValue*(1-mos)
  }
  ############################ SAVE WORKBOOK FILE WITH NEW SHEETS #########################
  if(exportfromfunc){
    write.csv(param.agg.dv,paste0("tmdlCalc_output_",Sys.Date(),".csv"), row.names = FALSE)
  }
  return(param.agg.dv)
}
