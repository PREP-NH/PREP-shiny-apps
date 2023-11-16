
if (!require("pacman",character.only = TRUE))
{
  install.packages("pacman",dep=TRUE)
  if(!require("pacman",character.only = TRUE)) stop("Package not found")
}
# Keeping below source for github package. Ask Easton whether pacman works for github packages or not.
#devtools::install_github("rensa/stickylabeller")
pacman::p_load(dplyr, tidyr, ggplot2, ggmap,lubridate, patchwork, png,jsonlite,httr,svglite, dygraphs, xts)

theme_prep <- function(){
  theme_classic() + theme(legend.position = 'none',
                          panel.border = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          axis.text=element_text(size=14),
                          axis.title=element_text(size=20)) 
}
library(ggmap)
library(jsonlite)
library(scales)
library(cowplot)
library(httr)
library(svglite)
library("png")
library("patchwork")
library(xts)
source('./prep_colors.R')


DO_under5mglSummer <- function(DO_df){
  print(head(DO_df))
  print(unique(DO_df$qualitycodecv))
  DO_df <- DO_df %>%
    filter(!qualitycodecv %in% c('BAD', 'Bad'))
  DO_df <- DO_df %>% 
    mutate(month = month(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))), 
           year = year(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))))
  
  DO_df <- DO_df %>% filter(month %in% c(6,7,8))
  
  DO_df <- DO_df %>% group_by(lowDO = (datavalue<=5), goodDO=(datavalue>5))
  
  DO_dfsm <- DO_df %>% summarise(month,year, vals=n(), lowDOvals=sum(lowDO), goodDOvals=sum(goodDO))
  DO_dfsm <- DO_dfsm  %>% mutate(lowdoprc = round(lowDOvals/vals, 3), gooddoprc = round(goodDOvals/vals, 3))
  
  DO_dfsm <- DO_dfsm  %>%    
    group_by(year, month) %>%  summarize(lowdoprcmean = mean(lowdoprc), gooddoprcmean= mean(gooddoprc))
  DO_dfsm$YearMonth <- paste(DO_dfsm$year, '-', DO_dfsm$month)
  # DO_dfsm$YearMonth <- ifelse((is.na(DO_dfsm$lowdoprcmean)), NA, DO_dfsm$YearMonth)
  # DO_dfsm$YearMonth <- ifelse((DO_dfsm$lowdoprcmean==0), NA, DO_dfsm$YearMonth)
  DO_dfsm <- DO_dfsm[!is.na(DO_dfsm$YearMonth),]
  return (DO_dfsm)
}


DO_under5mglSummerCDMO <- function(DO_df){
  
  DO_df <- DO_df %>%
    filter(qualitycodecv!='BAD')
  DO_df <- DO_df %>% 
    mutate(month = month(as.POSIXct(strptime(valuedatetime, format = "%m/%d/%Y %H:%M", tz = "Etc/GMT-4"))), 
           year = year(as.POSIXct(strptime(valuedatetime, format = "%m/%d/%Y %H:%M", tz = "Etc/GMT-4"))))
  
  DO_df <- DO_df %>% filter(month %in% c(6,7,8))
  
  DO_df <- DO_df %>% group_by(lowDO = (datavalue<=5), goodDO=(datavalue>5))
  
  DO_dfsm <- DO_df %>% summarise(month,year, vals=n(), lowDOvals=sum(lowDO), goodDOvals=sum(goodDO))
  DO_dfsm <- DO_dfsm  %>% mutate(lowdoprc = round(lowDOvals/vals, 3), gooddoprc = round(goodDOvals/vals, 3))
  
  DO_dfsm <- DO_dfsm  %>%    
    group_by(year, month) %>%  summarize(lowdoprcmean = mean(lowdoprc), gooddoprcmean= mean(gooddoprc))
  DO_dfsm$YearMonth <- paste(DO_dfsm$year, '-', DO_dfsm$month)
  # DO_dfsm$YearMonth <- ifelse((is.na(DO_dfsm$lowdoprcmean)), NA, DO_dfsm$YearMonth)
  # DO_dfsm$YearMonth <- ifelse((DO_dfsm$lowdoprcmean==0), NA, DO_dfsm$YearMonth)
  DO_dfsm <- DO_dfsm[!is.na(DO_dfsm$YearMonth),]
  return (DO_dfsm)
}

DO_under5mgFig <- function(DO_dfsm, site_name){
  title <- paste(site_name) #  ' % below 5 mg/l'
  psm1 <- ggplot(data = DO_dfsm, aes(x=year,y=lowdoprcmean, group=1,)) +
    geom_bar(stat="identity", fill="#F15A29",color="#F15A29")  +
    ggtitle(paste(title, "")) +theme_prep()  +  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
    scale_color_prep() +scale_x_continuous(breaks= pretty_breaks())+
    theme( plot.title=element_text( hjust=0.5, vjust=-0.6, size=36,face="bold"),
           axis.text.x = element_text(size=24, face="bold"),
           axis.text.y = element_text(size=24, face="bold"),
           axis.title = element_blank()) 
  #  ylab(' % below 5 mg/l') + xlab(' Year') +   
  # axis.title.y = element_blank()
  return (psm1)
  
}



DOfig <- function(DO_df, months, years, site){
  DO_df <- DO_df %>% 
    mutate(valuedatetime = as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4")))
  
  DO_df <- DO_df %>%
    filter(qualitycodecv!="BAD") 
  DO_df <- DO_df %>% 
    mutate(month = month(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4"))), 
           year = year(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4"))))
  
  DO_df2020 <- DO_df %>% filter(month %in% months) %>% filter(year %in% years)
  print(count(DO_df2020))
  
  
  hresp1 <- ggplot(data = DO_df2020, aes(x=valuedatetime,y=datavalue)) +  ggtitle(c(site, " DO ", str(year))) +
    ylab('Dissolved oxygen (mg/L)') + xlab('2021')  + theme_prep() +
    geom_line(color="#F15A29", size=1.2) + scale_color_prep() + theme(title=element_text(size=20,face="bold"), axis.title=element_text(size=20,face="bold")) +
    # ylim(2,10) +
    scale_y_continuous(limits = c(0,10), breaks = seq(1,10, by = 1))  +
    geom_hline(yintercept=5, linetype="dashed") # + geom_hline(yintercept=3, linetype="dashed") + geom_hline(yintercept=1, linetype="dashed")
  return(hresp1)
  
}
DOfigdates <- function(DO_df, site, startdate, enddate){
  DO_df <- DO_df %>% 
    mutate(valuedatetime = as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4")))
  
  DO_df <- DO_df %>%
    filter(qualitycodecv!="BAD") 
  DO_df <- DO_df %>% 
    mutate(startdate >= as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4")), 
           enddate <= as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4")))
  
  # DO_df2020 <- DO_df %>% filter(month %in% months) %>% filter(year %in% years)
  
  
  
  hresp1 <- ggplot(data = DO_df, aes(x=valuedatetime,y=datavalue)) +  ggtitle(c(site, " DO ")) +
    ylab('Dissolved oxygen (mg/L)') + xlab(paste0(startdate, " - ", enddate))  + theme_prep() +
    geom_line(color="#F15A29", size=1.2) + scale_color_prep() + theme(title=element_text(size=20,face="bold"), axis.title=element_text(size=20,face="bold")) +
    # ylim(2,10) +
    scale_y_continuous(limits = c(0,10), breaks = seq(1,10, by = 1))  +
    geom_hline(yintercept=5, linetype="dashed") # + geom_hline(yintercept=3, linetype="dashed") + geom_hline(yintercept=1, linetype="dashed")
  return(hresp1)
  
}
DOfigdy <- function(DO_df, months, years, site){
  DO_df <- DO_df %>% 
    mutate(valuedatetime = as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4")))
  
  DO_df <- DO_df %>%
    filter(qualitycodecv!="BAD") 
  DO_df <- DO_df %>% 
    mutate(month = month(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4"))), 
           year = year(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4"))))
  
  DO_df2020 <- DO_df %>% filter(month %in% months) %>% filter(year %in% years)
  print(count(DO_df2020))
  
  DO_df$datetimestr <- ymd_hms(DO_df$valuedatetime)
  DO_don <- xts(x= DO_df$datavalue, order.by=DO_df$datetimestr)
  
  hresp1 <- dygraph(DO_don, main=paste0(site, " DO "), 
                    ylab = 'Dissolved oxygen (mg/L)', xlab="Date", ) %>%
    dySeries("V1", label="DO (mg/L)", color="#F15A29", strokeWidth = 0, drawPoints=TRUE, pointSize = 1) %>%
    dyLimit(limit=5, label="Low DO Event") %>%
    dyAxis(name="y", valueRange=c(0,10)) %>% 
    dyLegend(show='follow')
                                      
  # hresp1.updateOptions({valueRange:[0, 10]})  
  # hresp1 <- ggplot(data = DO_df2020, aes(x=valuedatetime,y=datavalue)) +  ggtitle) +
  #   ylab('Dissolved oxygen (mg/L)') + xlab('2021')  + theme_prep() +
  #  geom_line(color="#F15A29", size=1.2) + scale_color_prep() + theme(title=element_text(size=20,face="bold"), axis.title=element_text(size=20,face="bold")) +
    # ylim(2,10) +
  #   scale_y_continuous(limits = c(0,10), breaks = seq(1,10, by = 1))  +
  #   geom_hline(yintercept=5, linetype="dashed") # + geom_hline(yintercept=3, linetype="dashed") + geom_hline(yintercept=1, linetype="dashed")
  return(hresp1)
  
}

DOfigdatesdy <- function(DO_df, site, startdate, enddate){
  DO_df <- DO_df %>% 
    mutate(valuedatetime = as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4")))
  
  DO_df <- DO_df %>%
    filter(qualitycodecv!="BAD") 
  DO_df <- DO_df %>% 
    mutate(startdate >= as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4")), 
           enddate <= as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4")))
  
  # DO_df2020 <- DO_df %>% filter(month %in% months) %>% filter(year %in% years)
  
  DO_df$datetimestr <- ymd_hms(DO_df$valuedatetime)
  
  hresp1 <- ggplot(data = DO_df, aes(x=valuedatetime,y=datetimestr)) +  ggtitle(c(site, " DO ")) +
    ylab('Dissolved oxygen (mg/L)') + xlab(paste0(startdate, " - ", enddate))  + theme_prep() +
    geom_line(color="#F15A29", size=1.2) + scale_color_prep() + theme(title=element_text(size=20,face="bold"), axis.title=element_text(size=20,face="bold")) +
    # ylim(2,10) +
    scale_y_continuous(limits = c(0,10), breaks = seq(1,10, by = 1))  +
    geom_hline(yintercept=5, linetype="dashed") # + geom_hline(yintercept=3, linetype="dashed") + geom_hline(yintercept=1, linetype="dashed")
  DO_don <- xts(x= DO_df$datavalue, order.by=DO_df$datetimestr)
  # print(head(DO_don))
  hresp1 <- dygraph(DO_don, main=paste0(site, " DO "), 
                    ylab = 'Dissolved oxygen (mg/L)', xlab="Date", ) %>%
    dySeries("V1", label="DO (mg/L)", color="#F15A29", strokeWidth = 0, drawPoints=TRUE, pointSize = 1) %>%
    dyLimit(limit=5, label="Low DO Event") %>%
    dyAxis(name="y", valueRange=c(0,10))  %>% 
    dyLegend(show='follow')
    # dyAxis("x", valueFormatter = )
  return(hresp1)
  
}
# http://data.prepestuaries.org:3001/timeseriesresultvalues?resultid=in.(134939,80010,83220,85542,93026,102921,109693)
prepdbdata <- function(resultids, startdate, enddate){
  if(missing(startdate)){
    prepdbhtml <- GET(url = paste0(
      'http://data.prepestuaries.org:3001/timeseriesresultvalues?resultid=in.(', resultids, ')'))
    
  }
  else{
  prepdbhtml <- GET(url = paste0('http://data.prepestuaries.org:3001/timeseriesresultvalues?',
                                   'and=(valuedatetime.gt.%22', startdate, 
                                 '%22,valuedatetime.lt.%22', enddate, 
                                 '%22,resultid.in.(', resultids, '))'
  ))
  }
  prepdb_text <- content(prepdbhtml,
                           "text", encoding = "UTF-8")
  prepdbjson <- fromJSON(prepdb_text,
                           flatten = TRUE)
  
  prepdb <- as.data.frame(prepdbjson)
  
  
  return(prepdb)
}  

prepcatdbdata <- function(resultids, startdate=NULL, enddate=NULL){
  if(is.null(startdate)){
    prepdbhtml <- GET(url = paste0(
      'http://data.prepestuaries.org:3001/categoricalresultvalues?resultid=in.(', resultids, ')'))
    
  }
  else{
    prepdbhtml <- GET(url = paste0('http://data.prepestuaries.org:3001/categoricalresultvalues?',
                                   'and=(valuedatetime.gt.%22', startdate, 
                                   '%22,valuedatetime.lt.%22', enddate, 
                                   '%22,resultid.in.(', resultids, '))'
    ))
  }
  prepdb_text <- content(prepdbhtml,
                         "text", encoding = "UTF-8")
  prepdbjson <- fromJSON(prepdb_text,
                         flatten = TRUE)
  
  prepdb <- as.data.frame(prepdbjson)
  
  
  return(prepdb)
}  


prepsfdbdata <- function(samplingfeatureids){
  prepdbhtml <- GET(url = paste0(
      'http://data.prepestuaries.org:3001/samplingfeatures?samplingfeatureid=in.(', samplingfeatureids, ')'))
  prepdb_text <- content(prepdbhtml,
                         "text", encoding = "UTF-8")
  prepdbjson <- fromJSON(prepdb_text,
                         flatten = TRUE)
  
  prepdb <- as.data.frame(prepdbjson)

  
  return(prepdb)
}  

prepsitesummariesdbdata <- function(samplingfeatureids, variableids){
  # print(samplingfeatureids)
  # print(variableids)
  # print(geturl)
  prepdbhtml <- GET(url = paste0(
    'http://data.prepestuaries.org:3001/sitesummaries?and=(samplingfeatureid.in.(', samplingfeatureids, '),',
    'variableid.in.(', variableids, '))'))
  prepdb_text <- content(prepdbhtml,
                         "text", encoding = "UTF-8")
  prepdbjson <- fromJSON(prepdb_text,
                         flatten = TRUE)
  
  prepdb <- as.data.frame(prepdbjson)
  # print(head(prepdb))
  
  return(prepdb)
}  

sfdataformap <- function(samplingfeatureids){
  
  sfs <- prepsfdbdata(samplingfeatureids)
  
  #access coordinates
  sfs$featuregeometry.coordinates[[1]][1] # long
  sfs$featuregeometry.coordinates[[8]][2] #lat
  sfs$long <- NA
  sfs$lat <- NA
  
  for (i in 1:nrow(sfs)){
    sfs$long[i] <- sfs$featuregeometry.coordinates[[i]][1]
    sfs$lat[i] <- sfs$featuregeometry.coordinates[[i]][2]
  }
  return(sfs)
}

turbfig <- function(resultids, site, var, lmfile, trendline=TRUE, highfilter=40){

  ChloroAPvals = prepdbdata(resultids)
  ChloroAPvals <- ChloroAPvals %>%
    mutate(month = month(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))),
           year = year(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))),
           time = time(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))))

  ChloroAPvals$yearchar <- as.character(ChloroAPvals$year)
  # remove points over 40 ug/l

  ChloroAPvals <- ChloroAPvals %>%
    mutate(valuedatetime = as.POSIXct(strptime(valuedatetime, format =  "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))) %>%
    arrange(valuedatetime)

  ChloroAPvals <- ChloroAPvals %>%
    mutate(diff = valuedatetime - lag(valuedatetime),
           diff_secs = as.numeric(diff, units = 'secs'))



  ChloroAPvals <- ChloroAPvals[!is.na(ChloroAPvals$datavalue),]
  ChloroAPvals$date <- as.Date(ChloroAPvals$valuedatetime, "%m/%d/%Y", tz = "Etc/GMT-4")

  ChloroAPvalsrep <- ChloroAPvals %>%
    group_by(year, date, diff_secs <90) %>%
    summarize(datavalue = mean(datavalue))

  ChloroAPvalsrep$logChloro <- log(ChloroAPvalsrep$datavalue)
  ChloroAPlm <- lm(ChloroAPvalsrep$logChloro ~date, data= ChloroAPvalsrep) # + time
  pval <- summary(ChloroAPlm)$coefficients['date','Pr(>|t|)']
  sink(lmfile)
  print(summary(ChloroAPlm))
  sink()
  print(summary(ChloroAPlm))
  ChloroAPvalsrep <- ChloroAPvalsrep %>%
    filter(datavalue < highfilter & datavalue > 0)
  print(highfilter)
  print('PVAL-----')
  print(pval)
  ChloroAP_median <- ChloroAPvalsrep %>%
    group_by(year) %>%
    summarize(median = median(datavalue, na.rm=TRUE))
  # ChloroAP_median_shrt <- ChloroAP_median %>% filter(year>=2016)
  print('ANNUAL MEDIANS -------')
  print(summary(ChloroAP_median))
  print('ANNUAL MEDIANS -------')
  chloro_ap_medianlm <- lm(median ~year, data=ChloroAP_median)

  print(summary(chloro_ap_medianlm))

  coefs_AP_chloro <- coef(lm(median ~year, data=ChloroAP_median))
  print(summary(ChloroAPvalsrep))

  AP_chlorofig <- ggplot(subset(ChloroAPvalsrep), aes(year, datavalue, group=year)) +
    geom_boxplot() + ggtitle(site)
  #geom_hline(yintercept = 0.5, color="#f5d482", size=1, alpha = 0.5) +
  #geom_hline(yintercept = 0.1, color="#c1e1c1", size=1, alpha=0.5) +
    if(pval<0.05){
      AP_chlorofig <- AP_chlorofig +
        geom_abline(intercept = coefs_AP_chloro [1], slope = coefs_AP_chloro [2], color="#0B88CC", size=2)
      }
  AP_chlorofig <- AP_chlorofig + scale_x_continuous(breaks=seq(from=1988, to=2022, by=1)) +
    # geom_hline(yintercept=5, linetype="dashed", colour="#c1e1c1", size=2)  +
    # geom_hline(yintercept=20, linetype="dashed", colour="#F15A29", size=2)  +
    # scale_y_continuous(limits=c(0, 1.2), breaks=seq(from=0,to=1.2,by=0.2)) +
    theme_prep() + ylab(var)  + xlab("Year") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
  return(AP_chlorofig)
}


turbfigv2 <- function(resultids, site, var, lmfile, trendline=TRUE, highfilter=40){

  ChloroAPvals = prepdbdata(resultids)
  ChloroAPvals <- ChloroAPvals %>%
    mutate(month = month(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))),
           year = year(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))),
           time = time(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))))

  ChloroAPvals$yearchar <- as.character(ChloroAPvals$year)
  # remove points over 40 ug/l

  ChloroAPvals <- ChloroAPvals %>%
    mutate(valuedatetime = as.POSIXct(strptime(valuedatetime, format =  "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))) %>%
    arrange(valuedatetime)

  ChloroAPvals <- ChloroAPvals[!is.na(ChloroAPvals$datavalue),]

  ChloroAPvalsrep <- ChloroAPvals %>%
    filter(datavalue < highfilter & datavalue > 0)
  ChloroAPvalsrep$logChloro <- log(ChloroAPvalsrep$datavalue)
  ChloroAPlm <- lm(ChloroAPvalsrep$logChloro ~valuedatetime, data= ChloroAPvalsrep) # + time
  coefs_AP_chloro_log <- coef(lm(ChloroAPvalsrep$logChloro ~valuedatetime, data= ChloroAPvalsrep))
  pval <- summary(ChloroAPlm)$coefficients['valuedatetime','Pr(>|t|)']
  sink(lmfile)
  print(summary(ChloroAPlm))
  sink()
  print(summary(ChloroAPlm))

  print(highfilter)
  print('PVAL-----')
  print(pval)
  ChloroAP_median <- ChloroAPvalsrep %>%
    group_by(year) %>%
    summarize(median = median(datavalue, na.rm=TRUE))
  # ChloroAP_median_shrt <- ChloroAP_median %>% filter(year>=2016)
  print('ANNUAL MEDIANS -------')
  print(summary(ChloroAP_median))
  print('ANNUAL MEDIANS -------')
  chloro_ap_medianlm <- lm(median ~year, data=ChloroAP_median)

  print(summary(chloro_ap_medianlm))

  coefs_AP_chloro <- coef(lm(median ~year, data=ChloroAP_median))
  print(summary(ChloroAPvalsrep))

  AP_chlorofig <- ggplot(subset(ChloroAPvalsrep), aes(year, datavalue, group=year)) +
    geom_boxplot() + ggtitle(site)
  AP_chlorofig <- AP_chlorofig + scale_x_continuous(breaks=seq(from=1988, to=2022, by=1)) +
    theme_prep() + ylab(paste(" ", var))  + xlab("Year") +
    # scale_y_continuous(breaks = c(0.1, 1, 10, 100, 1000, 10000), labels = c("0.1", "1", "10", "100", "1000", "10000")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
  return(AP_chlorofig)
}

DO_under5mglSummer <- function(DO_df){

  DO_df <- DO_df %>%
    filter(qualitycodecv!='BAD')
  DO_df <- DO_df %>%
    mutate(month = month(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))),
           year = year(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))))

  DO_df <- DO_df %>% filter(month %in% c(6,7,8))

  DO_df <- DO_df %>% group_by(lowDO = (datavalue<=5), goodDO=(datavalue>5))

  DO_dfsm <- DO_df %>% summarise(month,year, vals=n(), lowDOvals=sum(lowDO), goodDOvals=sum(goodDO))
  DO_dfsm <- DO_dfsm  %>% mutate(lowdoprc = round(lowDOvals/vals, 3), gooddoprc = round(goodDOvals/vals, 3))

  DO_dfsm <- DO_dfsm  %>%
    group_by(year, month) %>%  summarize(lowdoprcmean = mean(lowdoprc), gooddoprcmean= mean(gooddoprc))
  DO_dfsm$YearMonth <- paste(DO_dfsm$year, '-', DO_dfsm$month)
  # DO_dfsm$YearMonth <- ifelse((is.na(DO_dfsm$lowdoprcmean)), NA, DO_dfsm$YearMonth)
  # DO_dfsm$YearMonth <- ifelse((DO_dfsm$lowdoprcmean==0), NA, DO_dfsm$YearMonth)
  DO_dfsm <- DO_dfsm[!is.na(DO_dfsm$YearMonth),]
  return (DO_dfsm)
}


TurbSummer <- function(DO_df){

  DO_df <- DO_df %>%
    filter(qualitycodecv!='BAD')
  DO_df <- DO_df %>%
    mutate(month = month(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))),
           year = year(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))))

  DO_df <- DO_df %>% filter(month %in% c(4,5,6,7,8))

  # DO_df <- DO_df %>% group_by(lowDO = (datavalue<=5), goodDO=(datavalue>5))

  DO_dfsm <- DO_df %>% summarise(month,year, vals=n())
  # DO_dfsm <- DO_dfsm  %>% mutate(lowdoprc = round(lowDOvals/vals, 3), gooddoprc = round(goodDOvals/vals, 3))

  # DO_dfsm <- DO_dfsm  %>%
  #  group_by(year, month) %>%  summarize(lowdoprcmean = mean(lowdoprc), gooddoprcmean= mean(gooddoprc))
  DO_dfsm$YearMonth <- paste(DO_dfsm$year, '-', DO_dfsm$month)
  # DO_dfsm$YearMonth <- ifelse((is.na(DO_dfsm$lowdoprcmean)), NA, DO_dfsm$YearMonth)
  # DO_dfsm$YearMonth <- ifelse((DO_dfsm$lowdoprcmean==0), NA, DO_dfsm$YearMonth)
  DO_dfsm <- DO_dfsm[!is.na(DO_dfsm$YearMonth),]
  return (DO_dfsm)
}

TurbTSfig <- function(DO_df, months, sites, var){
  print(sites)
  DO_df2 <- prepdbdata(DO_df)
  DO_df2 <- DO_df2 %>%
    mutate(valuedatetime = as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4")))
  DO_df2 <- DO_df2 %>%
    filter(datavalue<5000)
  DO_df2 <- DO_df2 %>%
    filter(qualitycodecv!="BAD")
  DO_df2 <- DO_df2 %>%
    mutate(month = month(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4"))),
           year = year(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4"))))

  DO_df2020 <- DO_df2 %>% filter(month %in% months)

  hresp1 <- ggplot(data = DO_df2020, aes(x=valuedatetime,y=datavalue)) +  ggtitle(c(sites, var)) +
    ylab(paste(sites, var)) + xlab('date')  + theme_prep() +
    geom_point(color="#F15A29", size=1.2) + scale_color_prep() + theme(title=element_text(size=20,face="bold"), axis.title=element_text(size=20,face="bold")) # +
    # ylim(2,10) +
   #  scale_y_continuous(limits = c(0,10), breaks = seq(1,10, by = 1))
  # geom_hline(yintercept=5, linetype="dashed") # + geom_hline(yintercept=3, linetype="dashed") + geom_hline(yintercept=1, linetype="dashed")
  return(hresp1)

}


turblogfig <- function(resultids, site, var, lmfile, trendline=TRUE, highfilter=40){

  ChloroAPvals = prepdbdata(resultids)
  ChloroAPvals <- ChloroAPvals %>%
    mutate(month = month(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))),
           year = year(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))),
           time = time(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))))

  ChloroAPvals$yearchar <- as.character(ChloroAPvals$year)

  ChloroAPvals <- ChloroAPvals %>%
    mutate(valuedatetime = as.POSIXct(strptime(valuedatetime, format =  "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT-4"))) %>%
    arrange(valuedatetime)

  ChloroAPvals <- ChloroAPvals[!is.na(ChloroAPvals$datavalue),]

  ChloroAPvalsrep <- ChloroAPvals %>%
    filter(datavalue < highfilter & datavalue > 0)
  ChloroAPvalsrep$logChloro <- log(ChloroAPvalsrep$datavalue)
  ChloroAPlm <- lm(ChloroAPvalsrep$logChloro ~valuedatetime, data= ChloroAPvalsrep) # + time
  coefs_AP_chloro_log <- coef(lm(ChloroAPvalsrep$logChloro ~valuedatetime, data= ChloroAPvalsrep))
  pval <- summary(ChloroAPlm)$coefficients['valuedatetime','Pr(>|t|)']
  sink(lmfile)
  print(summary(ChloroAPlm))
  sink()
  print(summary(ChloroAPlm))

  print(highfilter)
  print('PVAL-----')
  print(pval)
  ChloroAP_median <- ChloroAPvalsrep %>%
    group_by(year) %>%
    summarize(median = median(datavalue, na.rm=TRUE))
  # ChloroAP_median_shrt <- ChloroAP_median %>% filter(year>=2016)
  print('ANNUAL MEDIANS -------')
  print(summary(ChloroAP_median))
  print('ANNUAL MEDIANS -------')
  chloro_ap_medianlm <- lm(median ~year, data=ChloroAP_median)

  print(summary(chloro_ap_medianlm))

  coefs_AP_chloro <- coef(lm(median ~year, data=ChloroAP_median))
  print(summary(ChloroAPvalsrep))

  AP_chlorofig <- ggplot(subset(ChloroAPvalsrep), aes(year, logChloro, group=year)) +
    geom_boxplot() + ggtitle(site)
  AP_chlorofig <- AP_chlorofig + scale_x_continuous(breaks=seq(from=1988, to=2022, by=1)) +
    theme_prep() + ylab(paste(" log of ", var))  + xlab("Year") +
    # scale_y_continuous(breaks = c(0.1, 1, 10, 100, 1000, 10000), labels = c("0.1", "1", "10", "100", "1000", "10000")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
  return(AP_chlorofig)
}
getprepdbdata <- function(resultids){
  dbhtml <- GET(url = paste0('http://data.prepestuaries.org:3001/timeseriesresultvalues?resultid=in.(', resultids, ')'))
  db_text <- content(dbhtml,
                     "text", encoding = "UTF-8")
  dbjson <- fromJSON(db_text,
                     flatten = TRUE)

  db <- as.data.frame(dbjson)


  return(db)
}

lafigdf <- function(ffdf, site){
  # GRBUPRla1html <- GET(url = paste0('http://data.prepestuaries.org:3001/timeseriesresultvalues?resultid=in.(', resultids, ')'))
  # GRBUPRla1_text <- content(GRBUPRla1html,
  #                            "text", encoding = "UTF-8")
  # GRBUPRla1json <- fromJSON(GRBUPRla1_text,
  #                            flatten = TRUE)

  # GRBUPRla1 <- as.data.frame(GRBUPRla1json)
  # summary(GRBUPRla1)
  # 2018-04-10T23:15:00
  GRBUPRla1 <- ffdf

  GRBUPRla1 <- GRBUPRla1 %>%
    mutate(month = month(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4"))),
           year = year(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4"))),
           time = time(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4"))))
  GRBUPRla1 <- GRBUPRla1 %>%
    mutate(valuedatetime = as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4")))  %>%
    arrange(valuedatetime)
  GRBUPRla1$date <- as.Date(GRBUPRla1$valuedatetime, "%Y-%m-%d" , tz = "Etc/GMT-4")


  # months <- c(4,5,6,7,8)
  GRBUPRlasummer <- GRBUPRla1 #  %>% filter(month %in% months)

  # hresla1 <- ggplot(data = GRBUPRlasummer, aes(x=valuedatetime,y=datavalue)) +  ggtitle(c(site, " Light Attenuation ", str(year))) +
  #   ylab('Light Attenuation (1/m)') + xlab('summer')  + theme_prep() +
  #   geom_point(color="#F15A29", size=1.2) + scale_color_prep() + theme(title=element_text(size=20,face="bold"), axis.title=element_text(size=20,face="bold"))
  #
  # hresla1
  print(summary(GRBUPRlasummer))
  # have the lm go through the median per year
  GRBUPRlasummerlm <- lm(GRBUPRlasummer$datavalue ~valuedatetime, data= GRBUPRlasummer)
  print(summary(GRBUPRlasummerlm))
  GRBUPRlasummerpred <- predict(GRBUPRlasummerlm, GRBUPRlasummer)
  GRBUPRlasummer['predla'] <-GRBUPRlasummerpred

  UPRLAFig <- ggplot(data = GRBUPRlasummer, aes(x=date,y=datavalue, group = 1)) +  ggtitle(paste('Light Attenuation', site))  + theme_prep() +
    geom_point(color="#F15A29", size=1.2) + scale_x_date(date_breaks = "1 month", date_labels =  "%Y-%m") + ylab('Light Attenuation (1/m)') +
    #  geom_line( aes(x=date,y=predla, group = 1,  color="#F15A29"), size=1.5, alpha=.5) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12, face="bold"))  # theme(axis.text.x = year)
  # scale_x_discrete(labels=APwtempHigh$year) +

  return(UPRLAFig)
}


loglafigWDf <- function(ffdf, site, lmfile='./lmfile.txt', drawLine=TRUE){

  GRBUPRla1 <- ffdf
  print(summary(GRBUPRla1))
  # 2018-04-10T23:15:00


  GRBUPRla1 <- GRBUPRla1 %>%
    mutate(month = month(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4"))),
           year = year(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4"))),
           time = time(as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4"))),
           yearmonth = paste0(year, "-", month))
  GRBUPRla1 <- GRBUPRla1 %>%
    mutate(valuedatetime = as.POSIXct(strptime(valuedatetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4")))  %>%
    arrange(valuedatetime)
  GRBUPRla1$date <- as.Date(GRBUPRla1$valuedatetime, "%Y-%m-%d" , tz = "Etc/GMT-4")


  # months <- c(4,5,6,7,8)
  GRBUPRlasummer <- GRBUPRla1 # GRBUPRla1 %>% filter(month %in% months)

  # hresla1 <- ggplot(data = GRBUPRlasummer, aes(x=valuedatetime,y=datavalue)) +  ggtitle(c(site, " Light Attenuation ", str(year))) +
  #   ylab('Light Attenuation (1/m)') + xlab('summer')  + theme_prep() +
  #   geom_point(color="#F15A29", size=1.2) + scale_color_prep() + theme(title=element_text(size=20,face="bold"), axis.title=element_text(size=20,face="bold"))
  #
  # hresla1
  GRBUPRlasummer$logdatavalue <- log(GRBUPRlasummer$datavalue)
  print("number of values in set")
  print(nrow(GRBUPRlasummer))
  GRBUPRlasummerlm <- lm(GRBUPRlasummer$logdatavalue ~valuedatetime, data= GRBUPRlasummer)
  print(summary(GRBUPRlasummerlm))
  pval <- summary(GRBUPRlasummerlm)$coefficients['valuedatetime','Pr(>|t|)']
  # print(shapiro.test(GRBUPRlasummer$logdatavalue))
  # print(summary(GRBUPRlasummer))
  GRBUPRla_median <- GRBUPRlasummer %>%
    group_by(yearmonth) %>%
    summarize(median = median(datavalue))
  GRBUPRla_median$logmedian <- log(GRBUPRla_median$median)

  # chloro_ap_medianlm <- lm(median ~year, data=ChloroAP_median)

  GRBUPRlasummerlm <- lm(GRBUPRla_median$logmedian ~yearmonth, data= GRBUPRla_median)

  print(lmfile)

  print(pval)
  sink(lmfile)
  print(summary(GRBUPRlasummerlm))
  sink()
  GRBUPRlasummerpred <- predict(GRBUPRlasummerlm, GRBUPRla_median)
  #  GRBUPRlasummer['predla'] <-GRBUPRlasummerpred
  GRBUPRla_median['predlalog'] <-GRBUPRlasummerpred
  GRBUPRla_median['predlaexplog'] <- exp(GRBUPRla_median['predlalog'])
  print(summary(GRBUPRla_median))
  # scale_x_discrete(labels=APwtempHigh$year) +
  # GRBUPRla_median$yearchar <- as.character(GRBUPRla_median$year)
  coefs_GRBUPRla <- coef(lm(median ~yearmonth, data=GRBUPRla_median))

  GRBUPRlasummerpred <- predict(GRBUPRlasummerlm, GRBUPRlasummer)


  GRBUPRlasummer['predlaexplog'] <-   GRBUPRlasummerpred


  # GRBUPRlasummer <- GRBUPRlasummer %>%
  #   filter(datavalue < 5)
  UPRLAFig <- ggplot(data = GRBUPRlasummer, mapping=aes(x=yearmonth,y=datavalue, group=yearmonth) ) +
    # geom_line(data = GRBAPlasummer,mapping=aes(x=yearchar,y=median(datavalue)), group=yearchar) +
    ggtitle(paste('Light Attenuation at', site))  + theme_prep() +
    ylab('Light Attenuation Kd') + xlab('Year') + # scale_x_continuous(breaks=seq(from=2002, to=2022, by=1)) +
    scale_y_continuous(n.breaks = 30) +
    geom_boxplot(mapping=aes(x=yearmonth,y=datavalue, group=yearmonth), outlier.shape = NA) +
    # geom_line( aes(x=valuedatetime,y=predlaexplog, group = 1,  color="#F15A29"), size=1.5, alpha=.5) +
    geom_point(data = GRBUPRlasummer, mapping=aes(x=yearmonth,y=datavalue, group=yearmonth)) + # , color= "#F15A29"
    # scale_color_manual(values="#F15A29") +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_hline(yintercept = 0.75, linetype = "dashed") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12, face="bold"))
  #  geom_line(data = GRBAPlasummer, aes(x=valuedatetime,y=predlaexplog))
  # geom_boxplot(data = GRBAPlasummer, mapping=aes(x=year,y=datavalue, group = 1))
  # if(pval<0.05){
  #   UPRLAFig <- UPRLAFig + geom_abline(intercept = coefs_GRBUPRla[1],
  #             slope = coefs_GRBUPRla[2], colour="#F15A29", size=1.5, alpha=.5)
  #  }
  return(UPRLAFig)
}
