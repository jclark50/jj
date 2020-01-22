################################################################################################
## JJ PACKAGE V1_0
## AUTHOR: JORDAN J. CLARK
## DATE: 2018-12-01

# 
# uninstall("jj")
# library(devtools)
# library(roxygen2)
# setwd("C:/Users/jclar/Documents/R/win-library/3.5/") #library path
# setwd("E:/OneDrive/R Codes/Package/") #library path
# getwd()
# create("jj")
# setwd("./jj/R/")
# theupdate <- "E:/OneDrive/R Codes/jj.R"
# file.copy(theupdate, "C:/Users/jclar/Documents/R/win-library/3.5/jj/R")
# setwd("..")
# devtools::document()
# setwd("..")
# install("jj")
# 
# # flist <- list.files("patha", "^filea.+[.]csv$", full.names = TRUE)
# flist <- "E:/OneDrive/R Codes/jj.R"
# file.copy(flist, "E:/OneDrive/")

################################################################################################
#' detachAllPackages
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' detachAllPackages()
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}


################################################################################################
#' listfuncs
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' listfuncs()
listfuncs = function(){
  print(ls("package:jj"))
}

################################################################################################
#' df_t
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' df_t()
df_t <- function(x,Y) 
{round(((range(x)[2]-range(x)[1])/Y),digits=0)}

################################################################################################
#' plotCI
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' plotCI()
plotCI <- function (x, y = NULL, uiw, liw = uiw, ..., sfrac = 0.01, vertLineLWD, confBoundsLWD) { 
  if (is.list(x)) { 
    y <- x$y 
    x <- x$x 
  } 
  if (is.null(y)) { 
    if (is.null(x)) 
      stop("both x and y NULL") 
    y <- as.numeric(x) 
    x <- seq(along = x) 
  } 
  ui <- y + uiw 
  li <- y - liw 
  plot(x, y, ...) 
  smidge <- diff(par("usr")[1:2]) * sfrac 
  segments(x, li, x, ui, lwd=vertLineLWD) 
  x2 <- c(x, x) 
  ul <- c(li, ui) 
  segments(x2 - smidge, ul, x2 + smidge, ul, lwd=confBoundsLWD) 
  invisible(list(x = x, y = y)) }

################################################################################################
#' add_rects
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' add_rects()
add_rects <- function(threshyellow, threshred, transp){
  rect(58, 0.93, threshyellow, 1.24, col=add.alpha("green", transp), lwd=0.5)
  rect(threshyellow, 0.93, threshred, 1.24, col=add.alpha("yellow", transp), lwd=0.5)
  rect(threshred, 0.93, 82, 1.24, col=add.alpha("red", transp), lwd=0.5)
  
}
################################################################################################
#' add.alpha
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' add.alpha()
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

################################################################################################
#' heat.index2
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' heat.index2()
heat.index2 <- function (t = NA, dp = c(), rh = c(), temperature.metric = "fahrenheit", 
                         output.metric = NULL, round = 0) 
{
  if (length(output.metric) == 0) {
    output.metric <- temperature.metric
  }
  if (length(dp) == 0 & length(rh) == 0) {
    stop("You must give values for either dew point temperature ('dp') or relative humidity ('rh').")
  }
  else if (length(dp) > 0 & length(rh) > 0) {
    stop("You can give values for either dew point temperature ('dp') or relative humidity ('rh'), but you cannot specify both to this function.")
  }
  if (length(dp) != length(t) & length(rh) != length(t)) {
    stop("The vectors for temperature ('t') and moisture (either relative humidity, 'rh', or dew point temperature, 'dp') must be the same length.")
  }
  if (length(dp) > length(rh)) {
    rh <- dewpoint.to.humidity(t = t, dp = dp, temperature.metric = temperature.metric)
  }
  else if (length(rh[!is.na(rh) & (rh > 100 | rh < 0)]) > 0) {
    rh[!is.na(rh) & (rh > 100 | rh < 0)] <- NA
    warning("There were observations with an impossible values for relative humidity (below 0% or above 100%). For these observations, heat index was set to NA.")
  }
  if (temperature.metric == "celsius") {
    t <- celsius.to.fahrenheit(t)
  }
  hi <- mapply(heat_index_2, t = t, rh = rh)
  if (output.metric == "celsius") {
    hi <- fahrenheit.to.celsius(hi)
  }
  hi <- round(hi, digits = round)
  return(hi)
}
################################################################################################
#' heat_index_2
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' heat_index_2()
heat_index_2 <- function (t = NA, rh = NA) 
  ### FROM WEATHER METRICS PACKAGE
  ### MODIFIED TO INCLUDE HI FORMULA FOR T < 40
{
  if (is.na(rh) | is.na(t)) {
    hi <- NA
  }
  else if (t <= 40) {
    #hi <- t
    hi <- (((0.5*(t + 61.0 + ((t-68.0)*1.2) + (rh*0.094)))+t)/2)
    # hi <- round(hi, digits=round)
    
  }
  else {
    alpha <- 61 + ((t - 68) * 1.2) + (rh * 0.094)
    hi <- 0.5 * (alpha + t)
    if (hi > 79) {
      hi <- -42.379 + 2.04901523 * t + 10.14333127 * rh - 
        0.22475541 * t * rh - 6.83783 * 10^-3 * t^2 - 
        5.481717 * 10^-2 * rh^2 + 1.22874 * 10^-3 * t^2 * 
        rh + 8.5282 * 10^-4 * t * rh^2 - 1.99 * 10^-6 * 
        t^2 * rh^2
      if (rh <= 13 & t >= 80 & t <= 112) {
        adjustment1 <- (13 - rh)/4
        adjustment2 <- sqrt((17 - abs(t - 95))/17)
        total.adjustment <- adjustment1 * adjustment2
        hi <- hi - total.adjustment
        # hi <- round(hi, digits=round)
        
      }
      else if (rh > 85 & t >= 80 & t <= 87) {
        adjustment1 <- (rh - 85)/10
        adjustment2 <- (87 - t)/5
        total.adjustment <- adjustment1 * adjustment2
        hi <- hi + total.adjustment
        # hi <- round(hi, digits=round)
        
      }
    }
  }
  #hi <- round(hi, digits=roundTo)
  return(hi)
}
################################################################################################
#' solar_bras
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' add.alpha()
max_solar <- function(df, lat, lon){
  "Calculate maximum solar radiation
  Ryan-Stolzenbach, MIT 1972
  http://www.ecy.wa.gov/programs/eap/models.html"
  library(lubridate)
  
  library(NISTunits) 
  library(fishmethods)
  
  doy <- yday(df$date)
  
  # minute <- as.integer(format(df$date,"%M"))
  # hour <- as.integer(format(df$date,"%H"))
  decimal_hour <- (((df$hour*60)+df$minute)/60)
  
  altitude_m <- (df$elevation * 0.3048)
  
  gmt <- rep(-4, length=nrow(summer_eco))
  
  zenith <- astrocalc4r(day=df$day, month=df$month, year=df$year, hour=decimal_hour,
                        timezone=gmt,
                        lat=df$lat, lon=df$lon, withinput=T)$zenith
  
  el <- 90-zenith  # solar elevation degrees from horizon
  R <- (1.0 + (0.033*cos(2.0*3.14159265358979323844*doy/365.0))) # earth sun distance
  z <- altitude_m
  nrel = 1367.0
  # radiation on horizontal surface at top of atmosphere (bras eqn 2.9)
  sinal <- sin(NISTdegTOradian((el)))
  io = sinal * nrel / (R^2)
  #nfac - atmospheric turbidity (2=clear, 4-5=smoggy)
  nfac <- 2
  
  m = 1.0 / (sinal + 0.15*((el + 3.885)^-1.253))
  # molecular scattering coefficient (bras eqn 2.26)
  a1 = 0.128 - 0.054 * log(m) / log(10.0)
  
  sr <- ifelse(sinal > 0, (io * exp(-nfac * a1 * m)), 0)
  max_possible <- sr
  return(max_possible)
  
}

################################################################################################
#' add.alpha
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' solar_bras()
solar_bras <- function(df, lat, lon){
  # Calculate maximum solar radiation using Bras method
  #  http://www.ecy.wa.gov/programs/eap/models.html
  # FROM weewx
  doy <- yday(df$date)
  
  df$gmt <- as.integer(with(df, paste(-4)))
  altitude_m <- (df$elevation * 0.3048)
  df$decimal_hour <- (((df$hour*60)+df$minute)/60)
  
  zenith <- astrocalc4r(day=df$day, month=df$month, year=df$year, hour=as.integer(df$decimal_hour),
                        timezone=(df$gmt),
                        lat=lat, lon=lon, withinput=T)$zenith 
  zenith_radians <- NISTdegTOradian(zenith)
  
  
  sr = 0.0
  #  alm = Almanac(ts, lat, lon, altitude_m)
  
  solar_elev <- (90-zenith)
  el <- 90-zenith  # solar elevation degrees from horizon
  
  R <- (1.0 + (0.033*cos(2.0*3.14159265358979323844*doy/365.0))) # earth sun distance
  nrel = 1367.0
  
  sinel = math.sin(math.radians(el))
  #sinal <- sin(NISTdegTOradian((el)))
  
  io = sinel * nrel / (R * R)
  #io = sinal * nrel / (R^2)
  
  if (sinel >= 0){
    # optical air mass (bras eqn 2.22)
    m = (1.0 / (sinel + 0.15 * ((el + 3.885)^-1.253)))
  }
  
  a1 = 0.128 - 0.054 * log (m) / log (10.0)
  # clear-sky radiation at earth surface W / m^2 (bras eqn 2.25)
  df$max_bras = io * exp(-nfac * a1 * m)
  
  return(df)
  
  # solar_elev_radians <- NISTdegTOradian(solar_elev)
  
  # el = alm.sun.alt  # solar elevation degrees from horizon
  # R = alm.sun.earth_distance
  # # NREL solar constant W/m^2
  # nrel = 1367.0
  # # radiation on horizontal surface at top of atmosphere (bras eqn 2.9)
  # sinel = math.sin(math.radians(el))
  # io = sinel * nrel / (R * R)
  # if sinel >= 0:
  #   # optical air mass (bras eqn 2.22)
  #   m = 1.0 / (sinel + 0.15 * math.pow(el + 3.885, -1.253))
  # molecular scattering coefficient (bras eqn 2.26)
  
}
################################################################################################
#' tosort
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' tosort()
tosort = function(datatosort, col){
  datatosort = datatosort[order(datatosort[,col]), ]
  return(datatosort)
}


################################################################################################
#' completed
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' completed()
completed = function(data, onCOLS){
  if (nchar(onCOLS) == 0){
    #colINDEX = which(colnames(data) == col)
    data = data[complete.cases(data), ]
    
    return(data)
    
  } else {
    data = data[complete.cases(data[ , onCOLS]),]
    return(data)
  }
}

################################################################################################
#' duplicates
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' duplicates()
duplicates = function(data, oncols){
  #shelby_o_f = duplicates(shelby_o_f, c("date", "hour", "minute", "forecast_hr"))
  
  data2 = data[order(data[,oncols]), ]
  
  require(data.table) # v1.9.5+
  datadt = setDT(data2)
  datadt = datadt[!duplicated(rleidv(datadt, cols = c(oncols))), ]
  datadt = as.data.frame(datadt)
  return(datadt)
}

################################################################################################
#' subsetNA
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' subsetNA()
subsetNA = function(data){
  data2 <- data[rowSums(is.na(data)) > 0,]
  return(data2)
}

################################################################################################
#' subsetNA_DT
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' subsetNA_DT()
subsetNA_DT = function(data, col){
  newdata = data[is.na(data[[col]]),]
  print(cat(nrow(newdata), "are NA"))
  return(newdata)
}

################################################################################################
#' timeVARS
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' timeVARS()
timeVARS <- function(df, dateCol)
{   df$WEEKDAY<-as.integer(factor((weekdays(as.Date(df[,dateCol]))),
                                  levels =c("Monday", "Tuesday", "Wednesday", "Thursday","Friday",
                                            "Saturday", "Sunday"), ordered = TRUE))
    library(lubridate)
    df$doy <- as.integer(yday(df[,dateCol]))
    df$date_int <-as.numeric(df[,dateCol])
    df$year <- as.integer(format(as.Date(df[,dateCol]),"%Y"))
    df$month <- as.integer(format(as.Date(df[,dateCol]),"%m"))
    df$day <- as.integer(format(as.Date(df[,dateCol]),"%d"))
    
    df$weeknum <- as.integer(strftime(df[,dateCol], format = "%V"))
    
    return(df)
}

################################################################################################
#' timeVARS_DT
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' timeVARS_DT()
timeVARS_DT <- function(df, dateCol)
{   df$WEEKDAY<-as.integer(factor((weekdays(as.Date(df[[dateCol]]))),
                                  levels =c("Monday", "Tuesday", "Wednesday", "Thursday","Friday",
                                            "Saturday", "Sunday"), ordered = TRUE))
library(lubridate)
df$doy <- as.integer(yday(df[[dateCol]]))
df$date_int <-as.numeric(df[[dateCol]])
df$year <- as.integer(format(as.Date(df[[dateCol]]),"%Y"))
df$month <- as.integer(format(as.Date(df[[dateCol]]),"%m"))
df$day <- as.integer(format(as.Date(df[[dateCol]]),"%d"))

df$weeknum <- as.integer(strftime(df[[dateCol]], format = "%V"))

return(df)
}

################################################################################################
#' readfilesindirectory
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' readfilesindirectory()
readfilesindirectory = function(){
  filenames <- list.files("PATH", pattern="*.txt", full.names=TRUE)
  ldf <- lapply(filenames, read.table, skip=41, sep="\t", fill = TRUE, na.strings=c("","NA"))
  names(ldf) <- substr(filenames, 55,58)
}

################################################################################################
#' mav
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' mav()
mav <- function(x,n){
  result <- stats::filter(x,rep(1/n,n), sides=1)
  result <- as.numeric(result)
  return(result)
}

# rachel2 <- rachel2 %>%
#   group_by_(.dots=c("Date","Type")) %>%
#   summarize(count=sum(count),
#             sum_char = sum(total))

################################################################################################
#' group_boxplots
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' group_boxplots()
group_boxplots = function(dataog, VAR, groupVAR, horiz = TRUE, ...){
  #dat_m <- melt(dat,measure.vars = c("V1","V2","V3"))
  library(reshape2)
  library(plyr)
  library(dplyr)

  data = dataog
  data = as.data.frame(data)
  
  data[,groupVAR] = as.factor(data[,groupVAR])

  df.m <- melt(data, measure.vars = c(VAR))
  
  x = groupVAR
  y = "value"
  z = "variable"
  
  ###########
  ## save current par settings and return after finished
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  zz <- unique(df.m[, z])
  
  ## sets up the layout to cascade horizontally or vertically
  ## and sets xlim and ylim appropriately
  if (horiz) {
    par(mfrow = c(1, length(zz)), ...)
    ylim <- range(df.m[, y])
    xlim <- NULL
  } else {
    par(mfrow = c(length(zz), 1), ...)
    xlim <- range(df.m[, x])
    ylim <- NULL
  }
  
  ## make a subset of data for each unique by variable
  ## and draw a basic plot for each one
  for (ii in zz) {
    tmp <- df.m[df.m[, z] %in% ii, ]
    plot(tmp[, x], tmp[, y], xlim = xlim, ylim = ylim)
  }
}
################################################################################################
#' getmeans
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' getmeans()
getmeans <- function(data){
  library(data.table)
  data$hour = as.integer(data$hour)
 # data$minute = as.integer(data$minute)
  #data$second = as.integer(data$second)
  
  #setDT(data)
  
  data_mean <- data[, lapply(.SD, mean, na.rm=TRUE), by=list(date, hour) ]
  
  #data_mean <- as.data.frame(data_mean)
  
  return(data_mean)
}
################################################################################################
#' wboxplot
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' wboxplot()
wboxplot = function(data){
  boxplot(data)
  axis(side=2, at=seq(0, 100, by=10), labels=seq(0, 100, by=10))
  abline(h=seq(0, 100, by=10), col=add.alpha("grey30", 0.75), lty=2)
  
  abline(h=92, col=add.alpha("black", 0.75), lty=1, lwd=2)
  abline(h=90, col=add.alpha("red", 0.75), lty=1, lwd=2)
  abline(h=87, col=add.alpha("yellow", 0.75), lty=1, lwd=2)
  abline(h=82, col=add.alpha("green", 0.75), lty=1, lwd=2)
  
  abline(h=c(82,85,87,90,92,94,100), col=add.alpha("grey30", 0.75), lty=2)
  
  cat("Black flag %: ", round(length(which(data>=92))/length(data)*100,2), "\n")
  cat("Red flag %: ", round(length(which(data>=90))/length(data)*100,2), "\n")
  cat("Yellow flag %: ", round(length(which(data>=87))/length(data)*100,2), "\n")
  cat("Green flag %: ", round(length(which(data>=82))/length(data)*100,2), "\n")
  cat("QUANTILE:", "\n")
  quantile(data, c(.5, .75, .8, .85, .9, .95, .98, .99, 1))
  
  #abline(v=c(0.5, 1, 1.5, 2, 2.5, 3), col=add.alpha("grey30", 0.75), lty=2)
}
# 
# changetz = function(datetime, tzc){
#   
# attr(datetime, "tzone") <- tzc
# 
# }
################################################################################################
#' calc_dewpoint
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' calc_dewpoint()
calc_dewpoint = function(df, Tair_f_col, relhumCOL){
  Tair_c = NISTdegFtOdegC(df[[Tair_f_col]])
  relhum = df[[relhumCOL]]
  relhum = ifelse(relhum >= 100, 100, relhum)
  dewpoint = (243.04*(log(relhum/100)+((17.625*Tair_c)/(243.04+Tair_c)))/(17.625-log(relhum/100)-((17.625*Tair_c)/(243.04+Tair_c)))) 
  dewpoint = NISTdegCtOdegF(dewpoint)
  return(dewpoint)
}
################################################################################################
#' mergeDTlist
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' mergeDTlist()
mergeDTlist = function(alist, byvars){
  r = Reduce(function(...) merge(..., all = TRUE, by = byvars),
             alist)
  # Reduce(function(...) merge(..., all = TRUE, by = c('date','station')),
  #        list(mean, min, max))
  return(r)
}
################################################################################################
#' changecolnames
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' changecolnames()
changecolnames = function(froms, tos, what){
  # froms = c("Tair_", "heat_index_", "wbgt_r_", "wbgt_u_")
  # tos = c("t", "h", "wr", "wu")
  # what = colnames(eco_daily_stats_all)
  
  for (f in c(1:length(froms))){
    what = gsub(froms[f], tos[f], what)
  }
  
  #n = gsub(from, to, what)
  return(what)
}
################################################################################################
#' get3days
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' get3days()
get3days = function(df, whichCOLS, splitVAR){
  #df = eco_daily_stats_all
  # whichCOLS = colnames(eco_daily_stats_all)[3:14]
  # splitVAR = "station"
  dfnew = df
  #newlist = list()
  if (class(dfnew) == "list"){
    for (i in c(1:length(dfnew))){
      for (d in whichCOLS){
        #paste(d, "_3", sep="")
        new3col = paste(d, "_3", sep="")
        dfnew[[i]][[new3col]] = mav( dfnew[[i]][[d]], 3)
      }
    }
  } else if (class(dfnew) != "list"){
    dfnew = split(dfnew, f = dfnew[[splitVAR]]) 
    for (i in c(1:length(dfnew))){
      for (d in whichCOLS){
        #paste(d, "_3", sep="")
        new3col = paste(d, "_3", sep="")
        dfnew[[i]][[new3col]] <- mav( dfnew[[i]][[d]], 3)
      }
    }
  } else {
    cat("data is not a list or data.table", "\n")
  }
  
  for (i in c(1:length(dfnew))){
    dfnew[[i]] = tail(dfnew[[i]], -2)
  }
  
  result = rbindlist(dfnew)
  return(result)
}
################################################################################################
#' ipak
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' ipak()
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
################################################################################################
#' createdatesdt
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' createdatesdt()
createdatesdt = function(firstday, lastday){
  d = seq(as.Date(firstday), as.Date(lastday), by=1)
  d <- data.table(date = d)
  return(d)
}

################################################################################################
#' asplit
#'
#' Splits a data into lists based on var input as split variable
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' asplit()
asplit = function(data, var){
  nd = split(data, f=data[[var]])
  return(nd)
}

################################################################################################
#' pausecode
#'
#' Pause code for x number of seconds
#' @keywords cats
#' @export
#' @examples
#' pausecode()
pausecode <- function(x)
{ ##pause code for x number of seconds
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

################################################################################################
#' factorCOL
#'
#' This creates an input factor and remaps levels to integer sequence
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' factorCOL()
factorCOL = function(dataTOfactor, newLEVELS=NULL){
  dataTOfactor = dataTOfactor
  new_factor = as.factor(dataTOfactor)
  
  if (is.null(newLEVELS) == "TRUE"){
    levels(new_factor) = seq(1, length(unique(dataTOfactor)), by=1)
    new_factor = as.integer(as.character.factor(new_factor))
  } else {
    levels(new_factor) = newLEVELS
  }
  return(new_factor)
}

################################################################################################
#' as.integerf
#'
#' This function takes a factor and coverts it to integer.
#' @keywords cats
#' @export
#' @examples
#' as.integerf()
as.integerf = function(factor){
  as.integer(as.character.factor(factor))
}
################################################################################################
#' as.numericf
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' as.numericf()
as.numericf = function(factor){
  as.numeric(as.character.factor(factor))
}

################################################################################################
#' mygrep
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' mygrep()
mygrep = function(cols, lookfor){
  #cols[grep(lookfor, cols)]
  #diag_cols = colns(ldf[[1]])[grep("Char", colns(ldf[[1]]))-1]
  colslist=list()
  for (lf in c(1:length(lookfor))){
    colslist[[lf]] = cols[grep(lookfor[lf], cols)]
  }
  return(unlist(colslist))
}


# f_andrie = function(dt) remove_na(dt)
# 
# f_gdata = function(dt, un = 0) gdata::NAToUnknown(dt, un)
################################################################################################
#' na0
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' na0()
na0 = function(DT) {
  ## https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
  ### original name  f_dowle3
  ## set all NA to 0 in data table
  # either of the following for loops
  
  # by name :
  for (j in names(DT))
    set(DT,which(is.na(DT[[j]])),j,0)
  
  # or by number (slightly faster than by name) :
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}

################################################################################################
#' completeOnCol
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' completeOnCol()
completeOnCol <- function(data, desiredCols) {
  completedOnCol = x2[!is.na(x2$tmax_3_lag4),]
  return(completedOnCol)
}

################################################################################################
#' colclass
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' colclass()
colclass = function(x){
  ## return class of each column
  sapply(x,typeof)
}
################################################################################################
#' coln
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' coln()
coln = function(x){
  colnames(x)
} 
################################################################################################
#' colns
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' colns()
colns = function(x){
  colnames(x)
} 

# cnms = function(x){
#   colnames(x)
# }
################################################################################################
#' stationpressure
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' stationpressure()
stationpressure <- function(df, presCOL, tairFcol, elevation_ft){
  #slp = ((df[,presCOL]/33.8639)/(((288-0.0065*(df[,elevCOL]*0.3048))/288)^5.2561)/(0.0295300))
  altitude_m = rep(elevation_ft*0.3048, length(df[,1]))
  Tair_c = NISTdegFtOdegC(df[,tairFcol])
  slp = df[,presCOL] * (( 1 - ((0.0065*altitude_m)/(Tair_c + 0.0065*altitude_m + 273.15)))^-5.257)
  return(slp)
}



################################################################################################
#' mergej
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' mergej()
mergej = function(ex, why, byVARS, ...){
  # ex = shelbymean
  # why = envoy_raw_mean
  # byVARS = c("date", "hour", "minute")
  xy = merge(ex, why, by = byVARS, ...)
  # na_y = nrow(na.omit(xy, cols=colnames(why)))
  if (nrow(ex) < nrow(xy)){
    cat(nrow(xy), "rows in new DF, which is ", round(nrow(ex)/nrow(xy)*100,2), "% of x and", round(nrow(why)/nrow(xy)*100,2), "% of y", "\n")
  } else {
    cat(nrow(xy), "rows in new DF, which is ", round(nrow(xy)/nrow(ex)*100,2), "% of x and", round(nrow(xy)/nrow(why)*100,2), "% of y", "\n")
    
  }
  return(xy)
  
  
}



