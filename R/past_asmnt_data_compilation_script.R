
# ## Add historical data files
# load("inst/extdata/historical_IR_impairedsites_delistings.Rdata")
# load("inst/extdata/au_wmu_22.Rdata")
# names(au_wmu_22)[names(au_wmu_22)=="ASSESS_ID"] = "ASSESSMENT_UNIT_ID"
# ## Prep polygons and sites
# asmts = readxl::read_xlsx("inst/extdata/latest_ir_categories.xlsx", sheet = 1)
# params = readxl::read_xlsx("inst/extdata/latest_ir_categories.xlsx", sheet = 2) # not supporting only
# params$param_year = paste0(params$PARAMETER_CODE_NAME," (",params$FIRST_LISTED_CYCLE,")")
# params =params%>%select(ASSESSMENT_UNIT_ID,param_year)%>%distinct()
# len = length(unique(params$param_year))+1
# params_wide = params%>%pivot_wider(id_cols=c(ASSESSMENT_UNIT_ID),names_from=param_year,values_from = param_year)%>%unite(col="Impaired Parameters",2:len,sep=", ", na.rm=TRUE, remove=TRUE)
#
# asmts_pw = merge(asmts, params_wide, all.x = TRUE)
# asmts_pw$EPA_IR_CATEGORY_ID = factor(asmts_pw$EPA_IR_CATEGORY_ID, levels=c("1","2","3","4A","5"))
# asmts_pw$`Impaired Parameters`[is.na(asmts_pw$`Impaired Parameters`)] = "None"
# asmts_pw = merge(asmts_pw,delistings_wide, all.x = TRUE)
# asmts_pw$`Delisted Parameters`[is.na(asmts_pw$`Delisted Parameters`)] = "None"
#
# au_p = wqTools::au_poly
# au_p = au_p%>%rename(ASSESSMENT_UNIT_ID=ASSESS_ID)
# au_p = merge(au_p, asmts_pw, all.x = TRUE)
# au_p = merge(au_p, au_wmu_22, all.x = TRUE)
# factpal = colorFactor(c("#118a11","#255d8a","#a6a6a6","#984ea3","#e41a1c"),levels(au_p$EPA_IR_CATEGORY_ID))
#
# ns_sites = ns_all%>%select(IR_MLID, IR_MLNAME,IR_Lat,IR_Long)%>%distinct()%>%group_by(IR_MLID, IR_Lat,IR_Long)%>%slice_head()
# ns_sites_paramwide = ns_all%>%select(IR_MLID, IR_Lat, IR_Long, ATTAINS_PARAM_NAME, IR_Year)%>%distinct()
# ns_sites_paramwide = ns_sites_paramwide%>%group_by(IR_MLID,IR_Lat,IR_Long,ATTAINS_PARAM_NAME)%>%summarise(MinYear=min(IR_Year))
# ns_sites_paramwide$Parameter_Year = paste0(ns_sites_paramwide$ATTAINS_PARAM_NAME," (",ns_sites_paramwide$MinYear,")")
# num = length(unique(ns_sites_paramwide$Parameter_Year))+3
# ns_sites_paramwide = ns_sites_paramwide%>%pivot_wider(id_cols=c(IR_MLID,IR_Lat,IR_Long),names_from = Parameter_Year,values_from = Parameter_Year)%>%unite("Impaired Parameters",4:all_of(num),sep=", ",remove=TRUE, na.rm=TRUE)
#
# ns_sites_paramwide = merge(ns_sites_paramwide,ns_sites, all.x = TRUE)
#
# ns_sites_paramwide$IR_Lat=as.numeric(ns_sites_paramwide$IR_Lat)
# ns_sites_paramwide$IR_Long=as.numeric(ns_sites_paramwide$IR_Long)
# ns_sites_paramwide = assignAUs(ns_sites_paramwide, lat="IR_Lat", long="IR_Long")
# names(ns_sites_paramwide)[names(ns_sites_paramwide)=="ASSESS_ID"] = "ASSESSMENT_UNIT_ID"
# ns_sites_paramwide = merge(ns_sites_paramwide,au_wmu_22, all.x = TRUE)
