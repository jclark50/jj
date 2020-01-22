################################################################################################
## JJ PACKAGE V1_0
## AUTHOR: JORDAN J. CLARK
## DATE: 2018-12-01

# install.packages("digest")
# install.packages("devtools")
library("devtools")
library(roxygen2)
library(usethis)
## setwd("C:/Users/jclar/Documents/R/win-library/3.4") #library path for laptop
## setwd("/usr/lib/R/site-library") #library path for linux server
## setwd("/home/jordan/R/x86_64-pc-linux-gnu-library/3.5") #library path for linux server user jordan
ogwd = getwd()
setwd("C:/Users/jordan/Documents/R-3.6.2/library/") #library path
uninstall("jj")
getwd()
usethis::create_package("jj", rstudio=FALSE, open=FALSE)
setwd("./jj/R/")
theupdate <- "C:/Users/jordan/OneDrive/R Codes/Package/jj.R"
# theupdate <- "/home/jordan/R/jj.R" # for linux server
# theupdate <- "C:/Users/jclar/OneDrive/R Codes/Package/jj.R" #for laptop
file.copy(theupdate, "C:/Users/jordan/Documents/R-3.6.2/library/jj/R")
# file.copy(theupdate, "C:/Users/jclar/Documents/R/win-library/3.4/jj/R") #for laptop
# file.copy(theupdate, "/usr/lib/R/site-library/jj/R") #for linux server
# file.copy(theupdate, "/home/jordan/R/x86_64-pc-linux-gnu-library/3.5/jj/R") #for linux server user jordan
setwd("..")
devtools::document()
setwd("..")
install("jj")
setwd(ogwd)
#
# ### JJ DATA
#

dir.create("C:/Users/jordan/Documents/R-3.6.2/library/jj/data/")
file.copy("C:/Users/jordan/OneDrive/R Codes/Package/data/jj_data.RData", 
          "C:/Users/jordan/Documents/R-3.6.2/library/jj/data/")

# system("\"C:\path\to\exe.exe\" args", intern = T)
# 
# Sys.getenv('PATH')
# 
# Sys.getenv('R_HOME')
# shell("C:/Users")
# system2(command="mkdir", args = ("C:/Users/jclar/Documents/R/win-library/3.6/jj/"))
# 
# system("cmd.exe /c mkdir C:/Users/jclar/Documents/R/library/jj", intern = T)
# 
# dir.create("C:/Users/jclar/Documents/R/win-library/3.6/jj/")
# system("mkdir -p :/Users/jclar/Documents/R/win-library/3.6/jj/")
# 
# command <- function(command, intern = TRUE, wait = FALSE){
#   system(paste("cmd.exe /c", command), intern = T, wait = wait)
# }
# 
# command(command="mkdir C:/Users/jclar/Documents/test")
# 
# system("cmd.exe mkdir -p C:/Users/jclar/Documents/R/library/jj/data/")
# system("cmd.exe", input = "mkdir -p C:/Users/jclar/Documents/R/library/jj/data/")

# C:\Windows\System32\cmd.exe
# Sys.setenv(PATH = '')
# 
# system("start notepad") 
# ## FOR LAPTOP
# file.copy("C:/Users/jclar/OneDrive/R Codes/Package/data/jj_data.RData", 
#           "C:/Users/jclar/Documents/R/win-library/3.4/jj/data/")


# flist <- list.files("patha", "^filea.+[.]csv$", full.names = TRUE)
# flist <- "E:/OneDrive/R Codes/jj.R"
# file.copy(flist, "E:/OneDrive/")

################################################################################################
#' detachallpackages
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' detachallpackages()
detachallpackages <- function() {
  
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
df_t = function (x, Y, changeunits=NULL) 
{
  if (is.null(changeunits)==TRUE){
    round(((range(x)[2] - range(x)[1])/Y), digits = 0)
  } else if (changeunits == "FtoC"){
    varc = (x - 32)/1.8
    round(((range(varc)[2] - range(varc)[1])/Y), digits = 0)
  } else if (changeunits == "CtoF"){
    varc = (x * 1.8 + 32)
    round(((range(varc)[2] - range(varc)[1])/Y), digits = 0)
  }
  
}
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
    t <- ctof(t)
  }
  hi <- mapply(heat_index_2, t = t, rh = rh)
  if (output.metric == "celsius") {
    hi <- ftoc(hi)
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
#' subsetna
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' subsetna()
subsetna = function(data, onCOL=NULL){
  
  if (is.null(onCOL) == FALSE){
    new_DF <- data[is.na(data[[onCOL]]),]
  } else {
    new_DF <- data[rowSums(is.na(data)) > 0,]
  }
  return(new_DF)
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
# subsetNA_DT = function(data, col){
#   newdata = data[is.na(data[[col]]),]
#   print(cat(nrow(newdata), "are NA"))
#   return(newdata)
# }

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
# library(lubridate)
# all[,year:= as.integer(format(as.Date(validTime),"%Y", tz="GMT"))]
# all[,month:= as.integer(format(as.Date(validTime),"%m", tz="GMT"))]
# all[,day:= as.integer(format(as.Date(validTime),"%d", tz="GMT"))]

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
# group_boxplots = function(dataog, VAR, groupVAR, horiz = TRUE, ...){
#   #dat_m <- melt(dat,measure.vars = c("V1","V2","V3"))
#   library(reshape2)
#   library(plyr)
#   library(dplyr)
# 
#   data = dataog
#   data = as.data.frame(data)
#   
#   data[,groupVAR] = as.factor(data[,groupVAR])
# 
#   df.m <- melt(data, measure.vars = c(VAR))
#   
#   x = groupVAR
#   y = "value"
#   z = "variable"
#   
#   ###########
#   ## save current par settings and return after finished
#   op <- par(no.readonly = TRUE)
#   on.exit(par(op))
#   zz <- unique(df.m[, z])
#   
#   ## sets up the layout to cascade horizontally or vertically
#   ## and sets xlim and ylim appropriately
#   if (horiz) {
#     par(mfrow = c(1, length(zz)), ...)
#     ylim <- range(df.m[, y])
#     xlim <- NULL
#   } else {
#     par(mfrow = c(length(zz), 1), ...)
#     xlim <- range(df.m[, x])
#     ylim <- NULL
#   }
#   
#   ## make a subset of data for each unique by variable
#   ## and draw a basic plot for each one
#   for (ii in zz) {
#     tmp <- df.m[df.m[, z] %in% ii, ]
#     plot(tmp[, x], tmp[, y], xlim = xlim, ylim = ylim)
#   }
# }
# 
group_boxplots = function (dataog, VAR, groupVAR, horiz = TRUE, ylimit = NULL, 
                            
                           ablinesh = NULL, ablinesv = NULL, xlimit=NULL, ...) 
{

  # dataog = shelbywoods_shade_sun
  # var = shelbywoods_shade_sun$wbgt_kestrel-shelbywoods_shade_sun$wbgt_kestrel_woods
  # 
  # dataog = dt
  # # dataog = dataog[,c("speed_rtma_diff", "hour")]
  # VAR = var1-var2
  # groupVAR = "hour"
  
  if (groupVAR=="hour" & is.null(dataog$hour)){
    dataog$hour = hour(dataog$datetime)
  }
  
  if (is.character(VAR)==FALSE){
    dataog$VAR=VAR
    VAR="VAR"
  }
  
  data = data.table(dataog[[VAR]], dataog[[groupVAR]])
  library(reshape2)
  library(plyr)
  library(dplyr)
  # data = dataog
  data = as.data.frame(data)
  data[,2] = as.factor(data[,2])
  df.m <- melt(data, measure.vars = 1)
  
  
  # setDT(df.m)
  # df.m = df.m[order(hour)]
  
  if (is.null(ylimit)==TRUE){
    ylimit=c(min(df.m[,3], na.rm=TRUE), max(df.m[,3], na.rm=TRUE))
  }
  # if (is.null(xlimit)==TRUE){
  #   xlimit=c(df.m[,1][1], tail(df.m[,1],1))
  # }
  
  df.m = as.data.frame(df.m)
  # df.m1 <- melt(data, measure.vars = c(VAR))
  boxplot(split(df.m[,3], df.m[,1]), ylim=ylimit, ...)
  
  
  # x = groupVAR
  # y = "value"
  # z = "variable"
  # op <- par(no.readonly = TRUE)
  # on.exit(par(op))
  # zz <- unique(df.m[, z])
  # if (is.null(ylim = TRUE)) {
  #   if (horiz) {
  #     par(mfrow = c(1, length(zz)), ...)
  #     ylim <- range(df.m[, y])
  #     xlim <- NULL
  #   }
  #   else {
  #     par(mfrow = c(length(zz), 1), ...)
  #     xlim <- range(df.m[, x])
  #     ylim <- NULL
  #   }
  #   for (ii in zz) {
  #     tmp <- df.m[df.m[, z] %in% ii, ]
  #     plot(tmp[, x], tmp[, y], xlim = xlim, ylim = ylim)
  #     abline(h = 0, col = add.alpha("red", 1), lwd = 1, 
  #            lty = 2)
  #   }
  # }
  # else {
  #   if (horiz) {
  #     par(mfrow = c(1, length(zz)), ...)
  #     ylim <- ylimit
  #     xlim <- NULL
  #   }
  #   else {
  #     par(mfrow = c(length(zz), 1), ...)
  #     xlim <- range(df.m[, x])
  #     ylim <- ylimit
  #   }
  #   for (ii in zz) {
  #     tmp <- df.m[df.m[, z] %in% ii, ]
  #     plot(tmp[, x], tmp[, y], xlim = xlim, ylim = ylimit)
  #     abline(h = 0, col = add.alpha("red", 1), lwd = 1, 
  #            lty = 2)
  #   }
  # }
  # if (is.null(ablinesh) == FALSE) {
  #   abline(ablinesh)
  #   # for (i in 1:length(ablinesh)) {
  #   #   abline(h = ablinesh[[i]])
  #   # }
  # }
  # if (is.null(ablinesv) == FALSE) {
  #   abline(ablinesv)
  #   # for (i in 1:length(ablinesv)) {
  #   #   abline(v = ablinesv[[i]])
  #   # }
  # }
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
calc_dewpoint = function(TairCOL, Tair_units, relhumCOL){
  if (Tair_units=="F"){
    Tair_c = ftoc(TairCOL)
  } else if (Tair_units == "C"){
    Tair_c = TairCOL
  } else {
    cat("PLEASE ENTER THE UNITS TAIR IS IN, F OR C", "\n")
  }
 # Tair_c = NISTdegFtOdegC(df[[Tair_col]])
  
  #Tair_c = NISTdegFtOdegC(df[[Tair_f_col]])
  relhum = relhumCOL
  relhum = ifelse(relhum >= 100, 100, relhum)
  dewpoint = (243.04*(log(relhum/100)+((17.625*Tair_c)/(243.04+Tair_c)))/(17.625-log(relhum/100)-((17.625*Tair_c)/(243.04+Tair_c)))) 
  dewpoint = ctof(dewpoint)
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
factorCOL = function(dataTOfactor, newLEVELS=NULL, aswhat=NULL){
  dataTOfactor = dataTOfactor
  new_factor = as.factor(dataTOfactor)
  
  if (is.null(newLEVELS) == "TRUE"){
    levels(new_factor) = seq(1, length(unique(dataTOfactor)), by=1)
    if (aswhat=="integer"){
      cat(levels(new_factor), "\n")
      new_factor = as.integer(as.character.factor(new_factor))
    } else if (aswhat=="character"){
      cat(levels(new_factor), "\n")
      new_factor = as.character.factor(new_factor)
    } else if (aswhat=="factor"){
      cat(levels(new_factor), "\n")
      new_factor = new_factor
    } else if (is.null(aswhat) == TRUE){
      cat(levels(new_factor), "\n")
      new_factor = new_factor
    }
  } else {
    levels(new_factor) = newLEVELS
    if (aswhat=="integer"){
      cat(levels(new_factor), "\n")
      new_factor = as.integer(as.character.factor(new_factor))
    } else if (aswhat=="character"){
      cat(levels(new_factor), "\n")
      new_factor = as.character.factor(new_factor)
    } else if (aswhat=="factor"){
      cat(levels(new_factor), "\n")
      new_factor = new_factor
    } else if (is.null(aswhat) == TRUE){
      cat(levels(new_factor), "\n")
      new_factor = new_factor
    }
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
#' 
mergej = function (ex, why, returnnonmatched=NULL, ...) 
{
  xy = merge(ex, why, ...)
  if (nrow(ex) < nrow(xy)) {
    cat(nrow(xy), "rows in new DF, which is ", round(nrow(ex)/nrow(xy) * 
                                                       100, 2), "% of x and", round(nrow(why)/nrow(xy) * 
                                                                                      100, 2), "% of y", "\n")
  }
  else {
    cat(nrow(xy), "rows in new DF, which is ", round(nrow(xy)/nrow(ex) * 
                                                       100, 2), "% of x and", round(nrow(xy)/nrow(why) * 
                                                                                      100, 2), "% of y", "\n")
  }
  
  nonmatched <- dplyr::anti_join(ex, why, ...)
  cat("nonmerged rows: ", nrow(nonmatched), "\n")
  
  if (nrow(nonmatched)!=0){
    print(head(nonmatched))
    #return(xy)
  } else {
   # return(xy)
  }
  
  if (is.null(returnnonmatched)==FALSE){
    return(nonmatched)
  } else {
    return(xy)
  }
}
# mergej = function(ex, why, byVARS, ...){
#   # ex = shelbymean
#   # why = envoy_raw_mean
#   # byVARS = c("date", "hour", "minute")
#  # if (is.null(newbyxCOLname)==TRUE){
#     xy = merge(ex, why, by = byVARS, ...)
#     
#   # } else {
#   #   xy = merge(ex, why, by = byVARS, ...)
#   #   xy[[]]
#   # }
#   # na_y = nrow(na.omit(xy, cols=colnames(why)))
#   if (nrow(ex) < nrow(xy)){
#     cat(nrow(xy), "rows in new DF, which is ", round(nrow(ex)/nrow(xy)*100,2), "% of x and", round(nrow(why)/nrow(xy)*100,2), "% of y", "\n")
#   } else {
#     cat(nrow(xy), "rows in new DF, which is ", round(nrow(xy)/nrow(ex)*100,2), "% of x and", round(nrow(xy)/nrow(why)*100,2), "% of y", "\n")
#     
#   }
#   return(xy)
#   
#   
# }

################################################################################################
#' trim.trailing
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' trim.trailing()
#trim.trailing <- function (x) sub("\\s+$", "", x)


################################################################################################
#' trim
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' trim()
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


################################################################################################
#' timed
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' timed()
timed = function(what){
  if (what == "start"){
    start.time <<- Sys.time()
  } else if (what == "end"){
    end.time = Sys.time()
    time.taken = end.time - start.time
    theunits = units(time.taken)
    
    cat(round(time.taken,2), theunits,"\n")
  }  else if (what == "stop"){
    end.time = Sys.time()
    time.taken = end.time - start.time
    theunits = units(time.taken)
    
    cat(round(time.taken,2), theunits,"\n")
  } else {
    cat("enter 'start' or either 'end' or 'stop'", "\n")
  }
}


################################################################################################
#' na.omitj
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' na.omitj()
na.omitj = function (dataNA, oncols) 
{
  # dataNA2 = as.data.frame(dataNA)
  # setDT(dataNA2)
  # 
  # noNA = na.omit(dataNA2, ...)
  # cat("Number of rows omitted: ", nrow(dataNA) - nrow(noNA), 
  #     "\n", "% of obs omitted: ", round((1 - nrow(noNA)/nrow(dataNA)) * 
  #                                         100, 2), "\n")
  
  asos[complete.cases(asos[, ..desiredCols])]
  
  
  return(noNA)
}

################################################################################################
#' summarizeDT
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' summarizeDT()
summarizedt = function(){
  cat("all3 = all3[,list(count=sum(count),
                        sum_char=sum(total)),
                  by=list(Name)]", "\n")
    
  cat("dt[, lapply(.SD, sum, na.rm=TRUE), by=category ]", "\n")
  cat('dt[, lapply(.SD, sum, na.rm=TRUE), by=category, .SDcols=c("a", "c", "z") ]', "\n")
    
}

################################################################################################
#' cbindlist
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' cbindlist()
cbindlist = function(alist){
  #do.call(cbind, list(A,B,C))
  result = do.call(cbind, alist)
  return(result)
}


################################################################################################
#' timechar
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' timechar()
timechar = function(datat, addmins=NULL, addsecs=NULL){
  newdatat = ifelse(datat<10, paste0("0", datat), datat)
  
  if (is.null(addmins)==FALSE){
    newdatat = paste(newdatat, "00", sep=":")
  } else {
    newdatat = newdatat
  } 
  
  if (is.null(addsecs)==FALSE){
    newdatat = paste(newdatat, "00", sep=":")
  } else {
    newdatat = newdatat
  } 

  return(newdatat)
}

################################################################################################
#' ni
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' ni()

# ni = function(x,y)!('%in%'(x,y))

ni = Negate('%in%')
#c(1,3,11) %ni% 1:10

################################################################################################
#' '%!in%'
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' '%!in%'
'%!in%' <- function(x,y)!('%in%'(x,y))

################################################################################################
#' '%likein%'
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' '%likein%'
'%likein%' <- function(vector, alistofin){
  for (i in 1:length(alistofin)){
    vector = vector[vector %like% alistofin[[i]]]
  }
  return(vector)
  
  #modelname %like% "rtma" & modelname %like% "rtma"
}

################################################################################################
#' '%!likein%'
#'
#' This function takes a factor and coverts it to numeric
#' @keywords cats
#' @export
#' @examples
#' '%!likein%'
'%!likein%' <- function(vector, alistofin){
  for (i in 1:length(alistofin)){
    vector = vector[!vector %like% alistofin[[i]]]
  }
  return(vector)
  
  modelname %like% "rtma" & modelname %like% "rtma"
}

################################################################################################
#' wb
#'
#' Calculate wetbulb temperature from air temperature and humidity - really for winter wetbulb
#' @keywords cats
#' @export
#' @examples
#' wb()
wb = function(Tair, relhum){
  wetb <- (Tair*(atan(0.151977*((relhum+8.313659)^0.5)))+atan(Tair+relhum)-
           atan(relhum-1.676331)+((0.00391838*(relhum^1.5))*atan(0.023101*relhum))-4.686035)
  # wetb = (-5.806+0.672*Tair-0.006*Tair*Tair+(0.061+0.004*Tair+0.000099*Tair*Tair)*relhum+
  #           (-0.000033-0.000005*Tair-0.0000001*Tair*Tair)*relhum*relhum)
  # 
  return(wetb)
}

################################################################################################
#' 'calcRH'
#'
#' This function calculates relative humidity from dewpoint and air temperature
#' @keywords cats
#' @export
#' @examples
#' calcRH()
calcRH = function(Tair, dewpoint, vpd=NULL, inputdeg){
  if (is.null(vpd)){
    if (inputdeg == "F") {
      Tair2 = (Tair - 32)/1.8
      dewpoint2 = (dewpoint - 32)/1.8
    } else if (inputdeg == "C") {
      Tair2 = Tair
      dewpoint2 = dewpoint
    } else {
      cat("ENTER F OR C FOR INPUT UNITS", "\n")
    }
    rh = 100*(exp((17.625*dewpoint2)/(243.04+dewpoint2))/exp((17.625*Tair2)/(243.04+Tair2))) 
  } else {
    if (inputdeg == "F") {
      Tair2 = (Tair - 32)/1.8
      # dewpoint2 = (dewpoint - 32)/1.8
    } else if (inputdeg == "C") {
      Tair2 = Tair
      # dewpoint2 = dewpoint
    } else {
      cat("ENTER F OR C FOR INPUT UNITS", "\n")
    }
    svp = 610.78*(2.71828^(Tair2/(Tair2+238.3) * 17.2694)) #result is in PA3
    svp = svp/1000
    es = 6.11 * 10^((7.5*Tair2)/(273.5+Tair2))
    # vpd2 = (es-(Rh2*es/100))/10
    rh = ((vpd/svp)-1)*-100 
  }

  return(rh)
  
}

# calcRH = function(Tair, vpd){
#   svp = 610.78*(2.71828^(Tair/(Tair+238.3) * 17.2694)) #result is in PA3
#   svp = svp/1000
#   es = 6.11 * 10^((7.5*Tair)/(273.5+Tair))
#   # vpd2 = (es-(Rh2*es/100))/10
#   RH3 = ((vpd/svp)-1)*-100 
#   return(RH3)
# }


################################################################################################
#' 'solar_ryan'
#'
#' This function calculates relative humidity from dewpoint and air temperature
#' @keywords cats
#' @export
#' @examples
#' solar_ryan()
solar_ryan = function (df, lat, lon, elevation, GMT, date, hour, minute = NULL, 
                       cloudcover = NULL, atc = NULL, TZ = NULL, year=NULL, month=NULL, day=NULL) 
{
  "Calculate maximum solar radiation\n  Ryan-Stolzenbach, MIT 1972\n  http://www.ecy.wa.gov/programs/eap/models.html"
  library(NISTunits)
  doy <- yday(date)
  df$gmt <- as.integer(with(df, paste(GMT)))
  altitude_m <- (elevation * 0.3048)
  if (is.null(minute) == TRUE) {
    df$decimal_hour <- df$hour
  }
  else {
    df$decimal_hour <- (((hour * 60) + minute)/60)
  }
  if (is.null(TZ) == TRUE) {
    # month = as.integer(format(as.Date(date), "%m", tz = "America/New_York"))
    # year = as.integer(format(as.Date(date), "%Y", tz = "America/New_York"))
    # day = as.integer(format(as.Date(date), "%d", tz = "America/New_York"))
    cat("Enter a timezone, TZ=", "\n")
  }
  else {
    if (is.null(df$month)==TRUE){
      month = as.integer(format(as.Date(date), "%m", tz = TZ))
    } else {
      month=month
    }
    if (is.null(df$year)==TRUE){
      year = as.integer(format(as.Date(date), "%Y", tz = TZ))
    } else {
      year=year
    }
    if (is.null(df$day)==TRUE){
      day = as.integer(format(as.Date(date), "%d", tz = TZ))
    } else {
      day=day
    }
  }
  zenith <- fishmethods::astrocalc4r(day = day, month = month, 
                                     year = year, hour = as.numeric(df$decimal_hour), timezone = df$gmt, 
                                     lat = lat, lon = lon, withinput = T)$zenith
  sr = 0
  solar_elev <- (90 - zenith)
  el <- 90 - zenith
  R <- (1 + (0.033 * cos(2 * 3.14159265358979 * doy/365)))
  nrel = 1367
  sinal <- sin(NISTdegTOradian((el)))
  rm = ((((288 - 0.0065 * altitude_m)/288)^5.256)/(sinal + 
                                                     0.15 * ((el + 3.885)^-1.253)))
  toa = nrel * sinal/(R * R)
  if (is.null(atc) == TRUE) {
    atc = 0.8
  }
  else {
    atc = atc
  }
  max_ryan <- ifelse(sinal > 0, (toa * (atc^rm)), 0)
  if (is.null(cloudcover) == TRUE) {
    result = max_ryan
    return(result)
  }
  else {
    result = max_ryan * (1 - (0.75 * ((cloudcover)^3.4)))
    return(result)
  }
  return(result)
}


################################################################################################
#' 'solar_basic'
#'
#' This function calculates relative humidity from dewpoint and air temperature
#' @keywords cats
#' @export
#' @examples
#' solar_basic()
solar_basic = function (df, lat, lon, elevation, GMT, date, hour, minute = NULL, 
                        cloudcover = NULL, atc = NULL, TZ = NULL, year=NULL, month=NULL, day=NULL) 
{
  "Calculate maximum solar radiation\n  Ryan-Stolzenbach, MIT 1972\n  http://www.ecy.wa.gov/programs/eap/models.html"
  library(NISTunits)
  doy <- yday(date)
  df$gmt <- as.integer(with(df, paste(GMT)))
  altitude_m <- (elevation * 0.3048)
  if (is.null(minute) == TRUE) {
    df$decimal_hour <- df$hour
  }
  else {
    df$decimal_hour <- (((hour * 60) + minute)/60)
  }
  if (is.null(TZ) == TRUE) {
    # month = as.integer(format(as.Date(date), "%m", tz = "America/New_York"))
    # year = as.integer(format(as.Date(date), "%Y", tz = "America/New_York"))
    # day = as.integer(format(as.Date(date), "%d", tz = "America/New_York"))
    cat("Enter a timezone, TZ=", "\n")
  }
  else {
    if (is.null(df$month)==TRUE){
      month = as.integer(format(as.Date(date), "%m", tz = TZ))
    } else {
      month=month
    }
    if (is.null(df$year)==TRUE){
      year = as.integer(format(as.Date(date), "%Y", tz = TZ))
    } else {
      year=year
    }
    if (is.null(df$day)==TRUE){
      day = as.integer(format(as.Date(date), "%d", tz = TZ))
    } else {
      day=day
    }
  }
  zenith <- fishmethods::astrocalc4r(day = day, month = month, 
                                     year = year, hour = as.integer(df$decimal_hour), timezone = df$gmt, 
                                     lat = lat, lon = lon, withinput = T)$zenith
  sr = 0
  solar_elev <- (90 - zenith)
  solar_elev_radians <- ((solar_elev*pi)/180)
  
  
  R0 = 990*sin(solar_elev_radians)-30
  R = R0*(1-(0.75*((cloudcover)^3.4)))
  if (is.null(cloudcover) == TRUE) {
    result = R0
    result[result<=0] <- 0
    return(round(result,0))
  }
  else {
    result = R0*(1-(0.75*((cloudcover)^3.4)))
    result[result<=0] <- 0
    return(round(result,0))
  }
  return(round(result,0))
}

################################################################################################
#' 'countna'
#'
#' This function calculates relative humidity from dewpoint and air temperature
#' @keywords cats
#' @export
#' @examples
#' countna()
countna = function(datana){
  nam = matrix(nrow=ncol(datana), ncol=3)
  for (i in 1:length(colnames(datana))){
    nam[i,][1] = colnames(datana)[i]
    nam[i,][2] = sum(is.na(datana[[colnames(datana)[i]]]))
    nam[i,][3] = round((sum(is.na(datana[[colnames(datana)[i]]]))/nrow(datana))*100,2)
    
  }
  nam = as.data.table(nam)
  colnames(nam) = c("variable", "na_count", "perc_na")
  nam$na_count = as.numeric(nam$na_count)
  
  nam = nam[with(nam, order(na_count)), ]
  
 # nam = (nam[order(na_count)])
  
  return(nam)
}


#' 'datetime_from_timevars'
#'
#' This function calculates relative humidity from dewpoint and air temperature
#' @keywords cats
#' @export
#' @examples
#' datetime_from_timevars()
datetime_from_timevars = function(dataDT, dateCOL, hourCOL, minuteCOL, secondCOL=NULL){
  hour = ifelse(dataDT[[hourCOL]] < 10, paste0("0", dataDT[[hourCOL]]), dataDT[[hourCOL]])
  minute = ifelse(dataDT[[minuteCOL]] < 10, paste0("0", dataDT[[minuteCOL]]), dataDT[[minuteCOL]])
  
  if (is.null(secondCOL) == "TRUE"){
    time = paste(hour, minute, "00", sep=":")
    datetime = paste(dataDT[[dateCOL]], time, sep=" ")
  } else {
    second = ifelse(dataDT[[secondCOL]] < 10, paste0("0", dataDT[[secondCOL]]), dataDT[[secondCOL]])
    time = paste(hour, minute, second, sep=":")
    datetime = paste(dataDT[[dateCOL]], time, sep=" ")
  }
  
  return(datetime)
}

################################################################################################
#' 'readclip'
#'
#' This function shows you how to read in data that is copied to clipboard.
#' @keywords cats
#' @export
#' @examples
#' readclip()
readclip = function(){
  cat('read.table(file = "clipboard", sep = "\t", header=FALSE)', "\n")
}


################################################################################################
#' 'summardata'
#'
#' This function shows you how to read in data that is copied to clipboard.
#' @keywords cats
#' @export
#' @examples
#' summardata()
summardata = function(sdata, barplotxnames){
  
  dmean = mean(sdata)
  dmedian = median(sdata)
  dmode = getmode(sdata)
  dquant = quantile(sdata)
  
  cat("\n", "MEAN: ", round(dmean, 2), "\n",
      "MEDIAN: ", round(dmedian, 2), "\n",
      "MODE: ", dmode, "\n",
      "QUANTILES: ", dquant, "\n")
  
  par(mfrow=c(1,2))
  barplot(sdata, names.arg=barplotxnames)
  boxplot(sdata)
  
}

################################################################################################
#' 'getmode'
#'
#' This function shows you how to read in data that is copied to clipboard.
#' @keywords cats
#' @export
#' @examples
#' getmode()
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

################################################################################################
#' 'datecols'
#'
#' This function shows you how to read in data that is copied to clipboard.
#' @keywords cats
#' @export
#' @examples
#' datecols()
datecols = function(){
cat(
  
'eco[,datetime := as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz="America/New_York")]
eco[,date := as.Date(datetime, tz="America/New_York")]
eco[,year:= as.integer(format(as.Date(datetime),"%Y", tz="America/New_York"))]
eco[,month:= as.integer(format(as.Date(datetime),"%m", tz="America/New_York"))]
eco[,day:= as.integer(format(as.Date(datetime),"%d", tz="America/New_York"))]
eco[,hour:=  as.integer(strftime(datetime, format="%H", tz="America/New_York"))]'

)
}


################################################################################################
#' 'na0'
#'
#' This function shows you how to read in data that is copied to clipboard.
#' @keywords cats
#' @export
#' @examples
#' na0()
na0 = function(datatona){
  datatona2 = datatona
  for (i in seq_along(datatona2)) set(datatona2, i=which(is.na(datatona2[[i]])), j=i, value=0)
  return(datatona2)
}


################################################################################################
#' 'winterwb'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' winterwb()
winterwb = function (Tair, relhum) 
{
  wetb = (-5.806 + 0.672 * Tair - 0.006 * Tair * Tair + (0.061 + 
                                                           0.004 * Tair + 9.9e-05 * Tair * Tair) * relhum + (-3.3e-05 - 
                                                                                                               5e-06 * Tair - 1e-07 * Tair * Tair) * relhum * relhum)
  return(wetb)
}


################################################################################################
#' 'basicSOL'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' basicSOL()

basicSOL <- function(df, cloudcover, lat, lon, elevation, GMT, date, hour, minute=NULL){
  library(NISTunits)
  
  doy <- yday(date)
  
  df$gmt <- as.integer(with(df, paste(GMT)))
  altitude_m <- (elevation * 0.3048)
  
  if (is.null(minute)==TRUE){
    df$decimal_hour <- df$hour
  } else {
    df$decimal_hour <- (((hour*60)+minute)/60)
  }
  
  zenith <- fishmethods::astrocalc4r(day=df$day, month=df$month, year=df$year, hour=as.integer(df$decimal_hour),
                                     timezone=df$gmt,
                                     lat=lat, lon=lon, withinput=T)$zenith 
  
  zenith_radians <- NISTunits::NISTdegTOradian(zenith)
  solar_elev <- (90-zenith)
  solar_elev_radians <- ((solar_elev*pi)/180)
  
  R0 = 990*sin(solar_elev_radians)-30
  # cloudcover=0
  R = R0*(1-(0.75*((cloudcover)^3.4)))
  return(R)
}

 
# 
# basicSOL = function(df, cloudcover, lat, lon, elevation, GMT, date, hour, minute=NULL){
#   
#   doy <- yday(date)
#   
#   gmt <- as.integer(with(df, paste(GMT)))
#   altitude_m <- (elevation * 0.3048)
#   
#   if (is.null(minute)==TRUE){
#     decimal_hour <- df$hour
#   } else {
#     decimal_hour <- (((hour*60)+minute)/60)
#   }
# 
#   zenith <- fishmethods::astrocalc4r(day=df$day, month=df$month, year=df$year, hour=(decimal_hour),
#                         timezone=(gmt),
#                         lat=lat, lon=lon, withinput=T)$zenith
#   zenith_radians <- NISTunits::NISTdegTOradian(zenith)
#   solar_elev <- (90-zenith)
#   solar_elev_radians <- ((solar_elev*pi)/180)
#   
#   R0 = 990*sin(solar_elev_radians)-30
#   # cloudcover=0
#   R = R0*(1-(0.75*((cloudcover)^3.4)))
#   return(R)
# }

################################################################################################
#' 'wbgtflags'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' wbgtflags()
# wbgtflags = function (datab, xaxisvar=NULL, xaxisvar1=NULL, xaxisvar2=NULL, alpha=0.2, extend=TRUE) 
# {
#   if (!is.null(extend)){
#     if (is.null(xaxisvar1) == TRUE & is.null(xaxisvar2) == TRUE){
#       xvar = na.omit(datab[[xaxisvar]])
#       xvar2 = length(xvar)
#       rect(datab[[xaxisvar]][1]-5000, 90, datab[[xaxisvar]][xvar2]+5000, 
#            105, col = add.alpha("black", alpha), lwd = 0.5)
#       rect(datab[[xaxisvar]][1]-5000, 88, datab[[xaxisvar]][xvar2]+5000, 
#            90, col = add.alpha("red", alpha), lwd = 0.5)
#       rect(datab[[xaxisvar]][1]-5000, 85, datab[[xaxisvar]][xvar2]+5000, 
#            88, col = add.alpha("yellow", alpha), lwd = 0.5)
#       rect(datab[[xaxisvar]][1]-5000, 80, datab[[xaxisvar]][xvar2]+5000, 
#            85, col = add.alpha("green", alpha), lwd = 0.5)
#     }
#   } else {
#     if (is.null(xaxisvar1) == TRUE & is.null(xaxisvar2) == TRUE){
#       xvar = na.omit(datab[[xaxisvar]])
#       xvar2 = length(xvar)
#       rect(datab[[xaxisvar]][1], 90, datab[[xaxisvar]][xvar2], 
#            105, col = add.alpha("black", alpha), lwd = 0.5)
#       rect(datab[[xaxisvar]][1], 88, datab[[xaxisvar]][xvar2], 
#            90, col = add.alpha("red", alpha), lwd = 0.5)
#       rect(datab[[xaxisvar]][1], 85, datab[[xaxisvar]][xvar2], 
#            88, col = add.alpha("yellow", alpha), lwd = 0.5)
#       rect(datab[[xaxisvar]][1], 80, datab[[xaxisvar]][xvar2], 
#            85, col = add.alpha("green", alpha), lwd = 0.5)
#     } else {
#       # xvar = na.omit(datab[[xaxisvar]])
#       rect(xaxisvar1, 90, xaxisvar2, 
#            105, col = add.alpha("black", alpha), lwd = 0.5)
#       rect(xaxisvar1, 88, xaxisvar2, 
#            90, col = add.alpha("red", alpha), lwd = 0.5)
#       rect(xaxisvar1, 85, xaxisvar2, 
#            88, col = add.alpha("yellow", alpha), lwd = 0.5)
#       rect(xaxisvar1, 80, xaxisvar2, 
#            85, col = add.alpha("green", alpha), lwd = 0.5)
#     }
#   }
#   
#  
# }
wbgtflags = function (alpha=0.2) 
{
  # xvar = na.omit(datab[[xaxisvar]])
  # xvar2 = length(xvar)
  rect(par("usr")[1] - 5000, 90, par("usr")[2] + 
         5000, 105, col = add.alpha("black", alpha), lwd = 0.5)
  rect(par("usr")[1]- 5000, 88, par("usr")[2] + 
         5000, 90, col = add.alpha("red", alpha), lwd = 0.5)
  rect(par("usr")[1]- 5000, 85, par("usr")[2] + 
         5000, 88, col = add.alpha("yellow", alpha), lwd = 0.5)
  rect(par("usr")[1]- 5000, 80, par("usr")[2] + 
         5000, 85, col = add.alpha("green", alpha), lwd = 0.5)
  
}




################################################################################################
#' 'getholidays'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' getholidays()
getholidays = function(datah, datecol, fromyear, toyear){
  
  library(timeDate)
  holidayLIST = listHolidays()
  USholidays = grep("US",holidayLIST, value=T)
  
  datalist = list()
  for (i in USholidays){
    hol =  holiday(fromyear:toyear, i)
    hol = as.character(c(holiday(fromyear:toyear, i)))
    
    #dat$i = i  # maybe you want to keep track of which iteration produced it?
    datalist[[i]] = hol # add it to your list
    
  }
  
  
  all_holidays = do.call(rbind, datalist)
  all_holidays = as.Date(c(all_holidays))
  
  holidays = rep(0, nrow(datah))
  
  holidays = as.factor(ifelse(datah[[datecol]] %in% all_holidays, 1, holidays))
  
  return(holidays)
  
}

################################################################################################
#' 'gettime'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' gettime()
gettime = function(datetimecol, doing, TZ=NULL){
  if (is.null(TZ)==TRUE){
    #TZ = "America/New_York"
    cat("Enter a timezone for TZ=", "\n")
  } else {
    TZ = TZ
    
    # doing = c("year", "month", "day", "hour", "date", "weekday")
    # datetimecol = data$datetime
    # TZ = "America/New_York"
    
    if (length(doing)==1 & doing[1]=="all"){
      doing = c("year", "month", "day", "hour", "minute" ,"second", "weekday_int", "weekday", "doy", "date_int", "weeknum", "date")
    } else {
      doing = doing 
    }
    
    result = list()
    
    for (i in 1:length(doing)){
      
      if (doing[i] == "year"){
        result1 = as.integer(format(as.Date(datetimecol),"%Y", tz=TZ))
      } else if (doing[i] == "month"){
        result1 = as.integer(format(as.Date(datetimecol),"%m", tz=TZ))
      } else if (doing[i] == "day"){
        result1 = as.integer(format(as.Date(datetimecol),"%d", tz=TZ))
      } else if (doing[i] == "hour"){
        result1 = as.integer(strftime(datetimecol, format="%H", tz=TZ))
      } else if (doing[i] == "minute"){
        result1 = as.integer(strftime(datetimecol, format="%M", tz=TZ))
      } else if (doing[i] == "second"){
        result1 = as.integer(strftime(datetimecol, format="%S", tz=TZ))
      } else if (doing[i] == "weekday_int"){
        result1 = as.integer(factor((weekdays(as.Date(datetimecol, tz=TZ))),
                                    levels =c("Monday", "Tuesday", "Wednesday", "Thursday","Friday",
                                              "Saturday", "Sunday"), ordered = TRUE))
      } else if (doing[i] == "doy"){
        result1 = as.integer(yday(datetimecol))
      } else if (doing[i] == "date_int"){
        result1 = as.numeric(datetimecol)
      } else if (doing[i] == "weeknum"){
        result1 = as.integer(strftime(datetimecol, format = "%V", tz=TZ))
      } else if (doing[i] == "date"){
        result1 = as.Date(datetimecol, tz=TZ)
      } else if (doing[i] == "minute_chr"){
        result1 = ifelse(as.integer(strftime(datetimecol, format="%M", tz=TZ))<10,
                         paste0("0", as.integer(strftime(datetimecol, format="%M", tz=TZ))),
                         as.integer(strftime(datetimecol, format="%M", tz=TZ)))
      } else if (doing[i] == "hour_chr"){
        result1 = ifelse(as.integer(strftime(datetimecol, format="%H", tz=TZ))<10,
                         paste0("0", as.integer(strftime(datetimecol, format="%H", tz=TZ))),
                         as.integer(strftime(datetimecol, format="%H", tz=TZ)))
      } else if (doing[i] == "weekday"){
        result1 <- weekdays(as.Date(datetimecol, tz="America/New_York"))
      }
      
      result[[i]] = result1
      
    }
    
    adt = as.data.table(result[[1]])
    for (i in 1:length(doing)){
      if (i == 1){
        adt = as.data.table(result[[i]])
      } else {
        adt[[doing[i]]] = result[[i]]
      }
    }
    colnames(adt) = doing
    # names(result) <- doing
    # result = do.call(cbind.data.table, result)
    # result = as.data.table(result)
    
    # cbind.data.table = function (..., deparse.level = 1) 
    # {
    #   if (!identical(class(..1), "data.frame")) 
    #     for (x in list(...)) {
    #       if (inherits(x, "data.table")) 
    #         return(data.table::data.table(...))
    #     }
    #   data.table(..., check.names = FALSE)
    # }
    
    
    return(adt)
  }
}

################################################################################################
#' 'normpop'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' normpop()
normpop = function(data, normcol, popcol, normby){
  normby = normby/data[[popcol]]
  normed = normby*data[[normcol]]
  return(normed)
}

################################################################################################
#' 'quantilecut'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' quantilecut()
quantilecut <- function(x, n, ...) {
  
  if( !is.vector(x) | !is(x,"numeric")) stop( '"x" must be a numeric vector')
  if( length(n) !=1 | !is(n,"numeric")) stop( 'number of bins "n" must be a single number')
  
  p <- seq(0,1,1/n)
  breaks <- quantile( x, p, na.rm=TRUE )
  eps <- (max(x, na.rm=TRUE)-min(x, na.rm=TRUE)) / 1000
  breaks[1] <- breaks[1] - eps
  breaks[n+1] <- breaks[n+1] + eps  
  out <- cut(x, breaks, ...)
  return( out )
}

################################################################################################
#' 'strsplitj'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' strsplitj()
strsplitj = function(what, delim, whichelem){
  
 # ar = strsplit(what, deparse(delim))
  ar = strsplit(what, delim)
    nar = list()
  for (i in 1:length(ar)){
    nar[[i]] = ar[[i]][whichelem]
    
    # ar[[i]] = as.data.table(ar[[i]])
    # ar[[i]] = t(ar[[30]])
    # setDT(ar[[i]])
    #ar[[i]] = as.data.table(ar[[i]])
  }
  nar = unlist(nar)
  
  return(nar)
}

################################################################################################
#' ''%likein%''
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' '%likein%'()
'%likein%' = function(x, y){
  alist = list()
  for (i in 1:length(y)){
    alist[[i]] = x[x %like% y[i]]
  }
  alist = unlist(alist)
  # alist = unique(alist)
  return(unique(alist))
}


################################################################################################
#' 'charnum'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' charnum()
charnum = function(indat){
  indat[indat < 10 & nchar(indat) ==1] <- paste0("0", indat[indat < 10])
  # indat[indat<10] <- paste0("0", indat[indat<10])
  return(indat)
}

################################################################################################
#' 'listsizes'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' listsizes()
listsizes = sort( sapply(ls(),function(x){format(object.size(get(x)), units="Mb")})) 

################################################################################################
#' 'jpdf'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' jpdf()
jpdf = function(savefile, font="Times", height=8, width=12, ...){
  pdf(savefile, family=font, height=height, width=width, ...)
}


################################################################################################
#' 'summarize'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' summarize()
summarizej = function(datatable, doingwhat, bycol, sdcols){
  summarized = datatable[, lapply(.SD, doingwhat, na.rm=TRUE), by=bycol, .SDcols=sdcols ] 
  return(summarized)
}



################################################################################################
#' 'samplej'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' samplej()
samplej = function(data, samplenum){
  sampled = data[sample(.N, samplenum)]
  return(sampled)
}

################################################################################################
#' 'rowMedian'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' rowMedian()
rowMedian = function(datam, na.rm=FALSE){
  datam2 = apply(datam, 1, median, na.rm=na.rm) 
  return(datam2)
}

################################################################################################
#' 'liststat'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' liststat()
liststat = function(alist, colum, maxormin){
  
  if (maxormin == "min"){
    gfsmins = list()
    for (idf in 1:length(alist)){
      if (is.null(alist[[idf]]) == TRUE){
        next
      } else {
        gfsmins[[idf]] = min(alist[[idf]][[colum]], na.rm=TRUE)
      }
    }
    gfsmin = min(unlist(gfsmins))
    
    return(gfsmin)
  }
  
  if (maxormin == "max"){
    gfsmaxs = list()
    for (idf in 1:length(alist)){
      if (is.null(alist[[idf]]) == TRUE){
        next
      } else {
        gfsmaxs[[idf]] = max(alist[[idf]][[colum]], na.rm=TRUE)
      }
    }
    gfsmaxs = max(unlist(gfsmaxs))
    
    return(gfsmaxs)
  }
  
}

################################################################################################
#' 'logwind'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' logwind()
logwind = function (solar, wind, vert_temp_gradient, urbanORrural, s1height_low, 
                    s2height_high) 
{
  
  # solar = c(800, 900, 1000)
  # wind = c(2, 4, 6)
  # vert_temp_gradient = rep(-0.1, 3)
  # urbanORrural = "rural"
  # s1height = 2
  # s2height = 10
  
  solarcat = as.integer(as.character.factor(cut(solar, breaks = c(Inf, 
                                                                  925, 675, 175, 15, 0), labels = c("5", "4", "3", "2", 
                                                                                                    "1"), right = FALSE)))
  windcat = as.integer(as.character.factor(cut(wind, breaks = c(0, 
                                                                2, 3, 5, 6, Inf), labels = c("1", "2", "3", "4", "5"), 
                                               right = FALSE)))
  pclass = ifelse(solarcat == 1 & windcat == 1, 1, ifelse(solarcat == 
                                                            2 & windcat == 1, 1, ifelse(solarcat == 3 & windcat == 
                                                                                          1, 2, ifelse(solarcat == 4 & windcat == 1, 4, 0))))
  pclass = ifelse(solarcat == 1 & windcat == 2, 1, ifelse(solarcat == 
                                                            2 & windcat == 2, 2, ifelse(solarcat == 3 & windcat == 
                                                                                          2, 3, ifelse(solarcat == 4 & windcat == 2, 4, pclass))))
  pclass = ifelse(solarcat == 1 & windcat == 3, 2, ifelse(solarcat == 
                                                            2 & windcat == 3, 2, ifelse(solarcat == 3 & windcat == 
                                                                                          3, 3, ifelse(solarcat == 4 & windcat == 3, 4, pclass))))
  pclass = ifelse(solarcat == 1 & windcat == 4, 3, ifelse(solarcat == 
                                                            2 & windcat == 4, 3, ifelse(solarcat == 3 & windcat == 
                                                                                          4, 4, ifelse(solarcat == 4 & windcat == 4, 4, pclass))))
  pclass = ifelse(solarcat == 1 & windcat == 5, 3, ifelse(solarcat == 
                                                            2 & windcat == 5, 4, ifelse(solarcat == 3 & windcat == 
                                                                                          5, 4, ifelse(solarcat == 4 & windcat == 5, 4, pclass))))
  pclass = ifelse(pclass == 0 & wind < 2 & vert_temp_gradient < 
                    0, 5, ifelse(pclass == 0 & wind < 2 & vert_temp_gradient >= 
                                   0, 6, pclass))
  pclass = ifelse(pclass == 0 & wind >= 2 & wind < 2.5 & vert_temp_gradient < 
                    0, 4, ifelse(pclass == 0 & wind >= 2 & wind < 2.5 & vert_temp_gradient >= 
                                   0, 5, pclass))
  pclass = ifelse(pclass == 0 & wind >= 2.5 & vert_temp_gradient < 
                    0, 4, ifelse(pclass == 0 & wind >= 2.5 & vert_temp_gradient >= 
                                   0, 4, pclass))
  if (urbanORrural == "urban") {
    # pclass = as.factor(pclass)
    # levels(pclass) = c(0.15, 0.15, 0.2, 0.25, 0.3, 0.3)
    p_exp = c(0.15, 0.15, 0.2, 0.25, 0.3, 0.3)[pclass]
    
  }
  if (urbanORrural == "rural") {
    # pclass = as.factor(pclass)
    # levels(pclass) = c(0.07, 0.07, 0.1, 0.15, 0.35, 0.55)
    p_exp = c(0.07, 0.07, 0.1, 0.15, 0.35, 0.55)[pclass]
    
  }
  # p = as.numeric(as.character.factor(pclass))
  # log_wind = wind * (s1height/s2height)^p
  log_wind = wind * (s1height_low/s2height_high)^p_exp
  
  return(log_wind)
}


################################################################################################
#' 'nadel'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' nadel()
nadel = function (dataNA, oncols) 
{
  # dataNA2 = as.data.frame(dataNA)
  # setDT(dataNA2)
  # 
  # noNA = na.omit(dataNA2, ...)
  # cat("Number of rows omitted: ", nrow(dataNA) - nrow(noNA), 
  #     "\n", "% of obs omitted: ", round((1 - nrow(noNA)/nrow(dataNA)) * 
  #                                         100, 2), "\n")
  
  noNA = asos[complete.cases(asos[, ..desiredCols])]
  
  
  return(noNA)
}



################################################################################################
#' 'allrowna'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' allrowna()
allrowna = function(adatatable){
  adatatable2 = adatatable[rowSums(is.na(adatatable)) != ncol(adatatable), ]
  return(adatatable2)
}

################################################################################################
#' 'resetPar'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' resetPar()
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

################################################################################################
#' 'workspace.size'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' workspace.size()
workspace.size <- function() {
  ws <- sum(sapply(ls(envir=globalenv()), function(x)object.size(get(x))))
  #class(ws) <- "object_size"
  
  if (round(ws*0.000000001,1) < 1) {
    cat(round(ws*0.000001,1), "MB", "\n")
  } else {
    cat(round(ws*0.000000001,1), "GB", "\n")
  }
}

################################################################################################
#' 'getcloseststations'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' getcloseststations()
getcloseststations = function (countypoints, stationpoints) 
{
  
  # countypoints = county_pop_centroids[,6:7]
  # stationpoints = met_stations[,2:3]
  
  if ((class(countypoints) %like% "data.table")[1]==TRUE){
    countypoints = as.data.frame(countypoints)
  } else {
    countypoints = countypoints
  }
  
  if ((class(stationpoints) %like% "data.table")[1]==TRUE){
    stationpoints = as.data.frame(stationpoints)
  } else {
    stationpoints = stationpoints
  }
  
  allclosest = list()
  for (ia in 1:nrow(countypoints)) {
    p1 = countypoints[ia,]
    p2 = stationpoints
    closestlist = list()
    for (ib in 1:nrow(p2)) {
      closestlist[[ib]] = sqrt((p1[, 1] - p2[, 1][ib])^2 + 
                                 (p1[, 2] - p2[, 2][ib])^2)
    }
    #closestlist = rbindlist(closestlist)
    closestlist = unlist(closestlist)
    closestlist = as.data.table(closestlist)
    colnames(closestlist) = changecolnames("closestlist", "distance", 
                                           colnames(closestlist))
    closestlist$stationnum = 1:nrow(p2)
    closestlist = closestlist[order(distance)]
    fiveclosest = head(closestlist, 6)
    distances = t(fiveclosest)[1, ]
    stationnums = t(fiveclosest)[2, ]
    closeststations = data.table(station1 = stationnums[1])
    closeststations$station2 = stationnums[2]
    closeststations$station3 = stationnums[3]
    closeststations$station4 = stationnums[4]
    closeststations$station5 = stationnums[5]
    closeststations$station6 = stationnums[6]
    closeststations$station1dist = distances[1]
    closeststations$station2dist = distances[2]
    closeststations$station3dist = distances[3]
    closeststations$station4dist = distances[4]
    closeststations$station5dist = distances[5]
    closeststations$station6dist = distances[6]
    allclosest[[ia]] = closeststations
    cat(ia, "\n")
  }
  allclosest = rbindlist(allclosest)
  return(allclosest)
}



################################################################################################
#' 'df_t'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' df_t()
df_t = function (x, Y, changeunits=NULL) 
{
  if (is.null(changeunits)==TRUE){
    round(((range(x)[2] - range(x)[1])/Y), digits = 0)
  } else if (changeunits == "FtoC"){
    varc = (x - 32)/1.8
    round(((range(varc)[2] - range(varc)[1])/Y), digits = 0)
  } else if (changeunits == "CtoF"){
    varc = (x * 1.8 + 32)
    round(((range(varc)[2] - range(varc)[1])/Y), digits = 0)
  }
  
}


################################################################################################
#' 'stationpressure'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
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
#' 'tconv'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' tconv()
tconv = function(data, deg){
  
  if (deg=="F"){
    degc = (data * 1.8 + 32)
    return(degc)
  } else if (deg=="C"){
    degf = (data - 32)/1.8
    return(degf)
  } else {
    "enter deg which is 'converting to' units"
  }
}

################################################################################################
#' 'ftoc'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' ftoc()
ftoc = function(data){
  degc = (data - 32)/1.8
  return(degc)
}

################################################################################################
#' 'ctof'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' ctof()
ctof = function(data){
  degf = (data * 1.8 + 32)
  return(degf)
}


################################################################################################
#' 'mphtoms'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' mphtoms()
mphtoms = function(thedata){
  return(thedata*0.44704)
}

################################################################################################
#' 'mstomph'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' mstomph()
mstomph = function(thedata){
  return(thedata/0.44704)
}

################################################################################################
#' 'mstomh'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' mstomh()
mstomh = function(windspeed){
  return(windspeed*3600)
}

################################################################################################
#' 'striptime'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' striptime()
striptime = function(datetime, outputtime="%H:%M:%S", datetimeformat=NULL){
  if (is.null(datetimeformat)==TRUE) {
    return(format(strptime(datetime, format="%Y-%m-%d %H:%M:%S"), outputtime))
  } else {
    return(format(strptime(datetime, format=datetimeformat), outputtime))
  }
}

################################################################################################
#' 'uv2wdws'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' uv2wdws()
uv2wdws <- function(u,v) {
  
  degrees <- function(radians) 180 * radians / pi
  
  mathdegs <- degrees(atan2(v, u))
  wdcalc <- ifelse(mathdegs > 0, mathdegs, mathdegs + 360)
  wd <- ifelse(wdcalc < 270, 270 - wdcalc, 270 - wdcalc + 360)
  ws <- sqrt(u^2 + v^2)
  
  return(cbind(wd, ws))
  
}

################################################################################################
#' 'freqdt'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' freqdt()
freqdt = function(){
  'dt[, .N ,by = Species]'
}

################################################################################################
#' 'wbgt_lilj_dim'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' wbgt_lilj_dim()
wbgt_lilj_dim = function(dt, datetime, year=NULL, month=NULL, day=NULL, hour=NULL, minute=NULL, second=NULL,
                         gmt, avg, lat, lon, 
                         solar, pres, Tair, relhum, dewpoint, speed, zspeed=NULL, dT=NULL, urban=NULL,
                         downscalewind=NULL, urbanorrural=NULL, s1height_low=NULL, s2height_high=NULL, datetimecol=NULL, 
                         justwbgt=NULL, newdata=NULL, station=NULL){
  # dt=ndfd2
  # datetime=ndfd2$datetime
  # year=ndfd2$year
  # month=ndfd2$month
  # day=ndfd2$day
  # hour=ndfd2$hour
  # minute=ndfd2$minute
  # second = NULL
  # gmt=0
  # avg=1
  # lat=ndfd2$lat
  # lon=ndfd2$lon
  # solar=ndfd2$solar_new
  # pres=ndfd2$pres
  # Tair=ftoc(ndfd2$Tair)
  # relhum = ndfd2$rh
  # dewpoint=ftoc(ndfd2$dewpoint)
  # speed=(ndfd2$speed_urban)
  # zspeed = NULL
  # dT = NULL
  # urban = NULL
  # downscalewind = NULL
  # urbanorrural = NULL
  # s1height_low = NULL
  # s2height_high = NULL
  # datetimecol="datetime"
  # newdata=FALSE
  # station="station"
  # justwbgt=TRUE
  
  # library(fishmethods)
  # library(wbgt)
  # year=rep(2018, times=length(wind) )
  # month=rep(6, times=length(wind) )
  # day = rep(1, times=length(wind))
  # hour=rep(15, times=length(wind))
  # minute=rep(0, times=length(wind))
  # second=NULL
  # gmt=rep(-4, times=length(wind) )
  # avg=rep(1, times=length(wind) )
  # lat = rep(35.2550, times=length(wind))
  # lon = rep(-81.5190, times=length(wind))
  # 
  # solar = sol
  # pres = rep(1015, times=length(wind) )
  # Tair = rep(tair, times=length(wind))
  # relhum = rep(relhum, times=length(wind))
  # dewpoint = rep(dewpoint, times=length(wind))
  # speed = wind
  # zspeed=NULL
  # dT=NULL
  # urban=NULL
  # downscalewind=NULL
  # urbanorrural=NULL
  # s1height_low=NULL
  # s2height_high=NULL
  if (is.null(year) == TRUE) {
    year = year(dt[[datetimecol]])
  }
  if (is.null(month) == TRUE) {
    month = month(dt[[datetimecol]])
  }
  if (is.null(day) == TRUE) {
    day = day(dt[[datetimecol]])
  }
  if (is.null(hour) == TRUE) {
    hour = hour(dt[[datetimecol]])
  }
  if (is.null(minute) == TRUE) {
    minute = minute(dt[[datetimecol]])
  }
  if (is.null(second) == TRUE) {
    second = second(dt[[datetimecol]])
  }
  ###################!
  if (is.null(zspeed) == TRUE) {
    zspeed = rep(2, length(year))
    cat("Assuming wind speeds are at 2 meters")
  }
  if (is.null(dT) == TRUE) {
    dT = rep(-0.1, length(year))
  }
  if (is.null(urban) == TRUE) {
    urban = rep(1, length(year))
  }
  if (length(lat) == 1) {
    lat = rep(lat, length(year))
  }
  if (length(lon) == 1) {
    lon = rep(lon, length(year))
  }
  if (length(avg) == 1) {
    avg = rep(avg, length(year))
  }
  if (length(gmt) == 1) {
    gmt =
      rep(gmt, length(year))
  }
  if (is.null(downscalewind) == FALSE) {
    if (is.null(urbanorrural) == TRUE) {
      if (is.null(s1height_low) == TRUE & is.null(s2height_high) == 
          TRUE) {
        speed = logwind(solar, wind = speed, vert_temp_gradient = dT, 
                        urbanORrural = "rural", s1height_low = 2, s2height_high = 10)
        cat("Downscaling wind speeds and assuming rural, and assuming 10m to 2m")
      }
      else {
        speed = logwind(solar = solar, wind = speed, 
                        vert_temp_gradient = dT, urbanORrural = urbanorrural, 
                        s1height_low = s1height_low, s2height_high = s2height_high)
      }
    }
  }
  lilj_wbgt <- wbgt(year, month, day, hour, minute, gmt, avg, 
                    lat, lon, solar, pres, Tair, relhum, speed, zspeed, dT, 
                    urban)
  if (is.null(second) == TRUE){
    decimal_hour = (hour + (minute/60))
  } else {
    decimal_hour = (hour + (minute/60) + (second/3600))
  }
  zenith <- astrocalc4r(day = day, month = month, year = year, 
                        hour = (decimal_hour), timezone = (gmt), lat = lat, lon = lon, 
                        withinput = T)$zenith
  zenith_radians <- NISTunits::NISTdegTOradian(zenith)
  solar_elev <- (90 - zenith)
  solar_elev_radians <- ((solar_elev * pi)/180)
  Ea <- (0.575 * ((exp(((17.67 * (dewpoint - Tair))/(dewpoint + 
                                                       243.5))) * (1.0007 + (3.46e-06 * pres)) * (6.112 * (exp((17.502 * 
                                                                                                                  Tair)/(240.97 + Tair)))))^(1/7)))
  S1 <- 1367
  o <- 5.67 * (10^(-8))
  Smax <- ifelse(zenith <= 89.5, ((S1 * cos(zenith_radians))/(1^2)), 
                 0)
  S <- (solar/Smax)
  a <- (3 - 1.34 * S) - (1.65/S)
  fdb <- ifelse(zenith <= 89.5, exp(a), 0)
  fdif <- ifelse(zenith <= 89.5, 1 - fdb, 0)
  B <- (solar * ((fdb/(4 * o * cos(zenith_radians)) + ((1.2/o) * 
                                                         fdif))) + (Ea * (Tair^4)))
  mstomh = function(windspeed) {
    return(windspeed * 3600)
  }
  u = mstomh(speed)
  C <- ((0.315 * ((u)^(0.58)))/(5.3865 * (10^(-8))))
  
  if (!is.null(justwbgt)){
    return(ctof(lilj_wbgt$Twbg))
  }
  
  tg_d <- (B + C * Tair + 7680000)/(C + 256000)
  wb <- Tair * atan(0.151977 * ((relhum + 8.313659)^0.5)) + 
    atan(Tair + relhum) - atan(relhum - 1.676331) + (0.00391838 * 
                                                       (relhum^1.5) * atan(0.023101 * relhum)) - 4.686035
  wbgt_d <- ctof(((0.7 * wb) + (0.2 * tg_d) + 
                          (0.1 * Tair)))
  wbgt_d_nwb <- ctof(((0.7 * lilj_wbgt$Tnwb) + (0.2 * 
                                                        tg_d) + (0.1 * Tair)))
  nwb <- ctof(lilj_wbgt$Tnwb)
  tg_l <- ctof(lilj_wbgt$Tg)
  tpsy <- ctof(lilj_wbgt$Tpsy)
  wbgt_l <- ctof(lilj_wbgt$Twbg)
  wb <- ctof(wb)
  tg_d <- ctof(tg_d)
  
  if (is.null(newdata)){
    if (is.null(station)){
      data2 = data.table(datetime, year, month, day, hour, minute, gmt, avg, 
                         lat, lon, solar, pres, Tair, relhum, dewpoint, speed, tg_d, tg_l, 
                         wb, wbgt_d_nwb, nwb, tpsy, wbgt_l, wbgt_d)
    } else {
      data2 = data.table(datetime, year, month, day, hour, minute, gmt, avg, 
                         lat, lon, solar, pres, Tair, relhum, dewpoint, speed, tg_d, tg_l, 
                         wb, wbgt_d_nwb, nwb, tpsy, wbgt_l, wbgt_d, station=dt[[station]])
    }
  } else {
    if (is.null(station)){
      data2 = data.table(datetime, tg_d, tg_l, wb, wbgt_d_nwb, nwb, tpsy, wbgt_l, wbgt_d)
      data2 = merge(dt, data2, by=c("datetime"))
    } else {
      data2 = data.table(tg_d, tg_l, wb, wbgt_d_nwb, nwb, tpsy, wbgt_l, wbgt_d)
      # data2 = merge(dt, data2, by=c("datetime", "station"))
      data2 = cbind(dt, data2)
    }
  }
  return(data2)

  
}

################################################################################################
#' 'email'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' email()
email = function(user, password, from, to, body, subject=NULL, smptserver=NULL, log=TRUE, logfile=NULL){
  # user="randompinocchio@gmail.com"
  # from = "randompinocchio@gmail.com"
  # to = "7043005702@vtext.com"
  # body = "testtest"
  # password = "atestingpassword"
  # subject=NULL
  # smptserver=NULL
  # log=TRUE
  # logfile=NULL
  
  com = paste('sendemail ', ' -f', from,' -t ', 
              to, ' -s "smtp.gmail.com:587" -o tls=yes -xu', user,
              ' -xp', password,' -m ', body, sep=" ")
  
  if (is.null(subject)==FALSE){
    com = paste(com, paste(" -u ", subject, sep=" "), sep="")
  } 
  if (is.null(smptserver)==FALSE){
    com = gsub("smtp.gmail.com:587", smptserver, com)
  }
  if (isTRUE(log)){
    if (is.null(logfile)==TRUE){
      com = paste(com, " -l emailr.log ", sep="")
    } else {
      com = paste(com, " -l ", logfile, sep="")
    }
  }
  system(com)
}

################################################################################################
#' 'mae'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' mae()
mae = function(error){
  return(mean(abs(error)))
}
################################################################################################
#' 'rmse'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' rmse()
rmse <- function(error)
{
  return(sqrt(mean(error^2)))
}

################################################################################################
#' 'downloadECO'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' downloadECO()
downloadECO = function(stationname, date1, date2, obtype, savefilename){
  # stationname = "CHAP"
  # date1 = "2019-05-01"
  # date2 = "2019-06-17"
  # obtype = "O"
  # savefilename = "C:/Users/jclar/Documents/R_Projects/weather_site_models/econet_05012019_06172019.txt"
  # 
  # stationname = stnsdf$Station[i]
  # station = paste("station=", stnsdf$Station[i],sep="")
  # obtype = "obtype=O"
  params = "parameter=all"
  # params = "parameter=temp,temp10,tempavg,rh,rhavg,sr,sravg,ws,wsavg,ws02,wd,wdavg,wd02,presavg"
  #qc="qc=good"
  hash="hash=2a62cdd7eaa37ccb5ac8d23afbd45ab28872c0c7"
  delimer = "delimiter=comma"
  
  # #######################
  # firstdate = data.table(date = as.Date(stnsdf$first_ob[i], format="%m/%d/%Y"))
  # firstdate$year <- as.integer(format(as.Date(firstdate$date, tz="America/New_York"), "%Y"))
  # firstdate$month <- as.integer(format(as.Date(firstdate$date, tz="America/New_York"), "%m"))
  # 
  # firstdateind = which(firstdate$year == allposdates$year & firstdate$month == allposdates$month)
  # firstdate = data.table(firstdate = allposdates[firstdateind:nrow(allposdates)]$date)
  # 
  # firstdate$seconddate = firstdate$firstdate
  # firstdate$seconddate = c((tail(firstdate$firstdate, -1) - as.difftime(1, unit="days")), as.Date("2018-12-31"))
  # 
  # firstdate = firstdate[firstdate >= as.Date("2000-01-01")]
  # 
  # for (di in 1:nrow(firstdate)){
  #   ######################
  #   #### DOWNLOAD MONTHLY DATA
  #   url = paste( "http://climate.ncsu.edu/dynamic_scripts/cronos/getCRONOSdata.php?",
  #                paste(station, paste("start=", firstdate$firstdate[di], sep=""), 
  #                      paste("end=", firstdate$seconddate[di], sep=""),
  #                      obtype, params, "show_missing=true", "qc_flags=true", "qc=good", delimer, hash, sep="&"), sep="")
  url = paste("http://climate.ncsu.edu/dynamic_scripts/cronos/getCRONOSdata.php?station=",
              stationname, "&start=", date1,"&end=",date2,"&obtype=",obtype,
              "&parameter=all&show_missing=true&qc_flags=true&qc=good&delimiter=comma&hash=2a62cdd7eaa37ccb5ac8d23afbd45ab28872c0c7", sep="")
  # monthch = ifelse(month(firstdate$firstdate[di]) <10,
  #                  paste0("0", month(firstdate$firstdate[di])), month(firstdate$firstdate[di]))
  # yearmo = paste(monthch, year(firstdate$firstdate[di]), ".txt", sep="")
  
  # mainDir="C:/Users/jclar/OneDrive/R_Projects/CRONOS_API/"
  # mainDir="C:/Users/jclar/Documents/R_Projects/CRONOS_API/station_data3/"
  #mainDir="/home/jordan/cronos/"
  # subDir = stationname
  # dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  
  # newdir= paste(mainDir, subDir, sep="")
  
  # savefilename = paste(newdir, paste(stationname, yearmo,sep="_"),sep="/")
  #    savefilename = "econetchap.txt"
  # cat(url, "\n")
  #savefilename = "keho2.txt"
  #url ="http://climate.ncsu.edu/dynamic_scripts/cronos/getCRONOSdata.php?station=KEHO&start=2005-06-01&end=2005-06-30&obtype=H&parameter=all&show_missing=true&qc_flags=true&delimiter=comma&hash=2a62cdd7eaa37ccb5ac8d23afbd45ab28872c0c7"
  #url ="http://climate.ncsu.edu/dynamic_scripts/cronos/getCRONOSdata.php?station=KEHO&start=2005-06-01&end=2005-06-30&obtype=H&parameter=all&show_missing=true&qc=good&qc_flags=true&delimiter=comma&hash=2a62cdd7eaa37ccb5ac8d23afbd45ab28872c0c7"    
  req <- download.file(url, destfile=savefilename)
  
  # cat("Station", i, "-", di, "out of", nrow(firstdate),"\n")
  
  # }
  
}

################################################################################################
#' 'paste'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' @keywords cats
#' @export
#' @examples
#' paste()
paste = function (..., sep = "", collapse = NULL){
  .Internal(paste(list(...), sep, collapse))
}

################################################################################################
#' 'boxtext'
#'
#' Calculate wet-bulb temperature for cold air temperatures, based on air temp (deg. C) and relative humidity
#' https://stackoverflow.com/questions/45366243/text-labels-with-background-colour-in-r
#' @keywords cats
#' @export
#' @examples
#' boxtext()
boxtext <- function(x, y, labels = NA, col.text = NULL, col.bg = "white", boxheight=1.5, 
                    border.bg = NA, adj = NULL, pos = NULL, offset = 0.5, 
                    padding = c(0.05, 0.05), cex = 1, font = graphics::par('font')){
  # x=crhs_tennis_pf_skin$time
  # y=rep(79,length(crhs_tennis_pf_skin$time))
  # labels=crhs_tennis_pf_skin$sun_shade
  # padding = c(0.000001, 0.000001)
  # col.bg="white"
  # cex= 1.2
  
  # y = yMAX-varlabel_dist_from_top
  # x = xMIN
  # labels = "Air Temperature"
  # adj=0
  # cex = varlabel_cex
  # col.text = "white"
  # col.bg = "black"
  # col="white"
  # padding = c(0, 0)
  # offset = 0.1
  
  
  ## The Character expansion factro to be used:
  theCex <- graphics::par('cex')*cex
  
  ## Is y provided:
  if (missing(y)) y <- x
  
  ## Recycle coords if necessary:    
  if (length(x) != length(y)){
    lx <- length(x)
    ly <- length(y)
    if (lx > ly){
      y <- rep(y, ceiling(lx/ly))[1:lx]           
    } else {
      x <- rep(x, ceiling(ly/lx))[1:ly]
    }       
  }
  
  ## Width and height of text
  textHeight <- graphics::strheight(labels, cex = theCex, font = font)
  textWidth <- graphics::strwidth(labels, cex = theCex, font = font)
  
  ## Width of one character:
  charWidth <- graphics::strwidth("e", cex = theCex, font = font)
  
  ## Is 'adj' of length 1 or 2?
  if (!is.null(adj)){
    if (length(adj == 1)){
      adj <- c(adj[1], 0.5)            
    }        
  } else {
    adj <- c(0.5, 0.5)
  }
  ## Is 'pos' specified?
  if (!is.null(pos)){
    if (pos == 1){
      adj <- c(0.5, 1)
      offsetVec <- c(0, -offset*charWidth)
    } else if (pos == 2){
      adj <- c(1, 0.5)
      offsetVec <- c(-offset*charWidth, 0)
    } else if (pos == 3){
      adj <- c(0.5, 0)
      offsetVec <- c(0, offset*charWidth)
    } else if (pos == 4){
      adj <- c(0, 0.5)
      offsetVec <- c(offset*charWidth, 0)
    } else {
      stop('Invalid argument pos')
    }       
  } else {
    offsetVec <- c(0, 0)
  }
  
  ## Padding for boxes:
  if (length(padding) == 1){
    padding <- c(padding[1], padding[1])
  }
  
  ## Midpoints for text:
  xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]
  
  ## Draw rectangles:
  rectWidth <- textWidth + 0.5*padding[1]*charWidth
  # rectWidth = rectWidth-10
  rectHeight <- textHeight/boxheight + 0.5*padding[2]*charWidth    
  graphics::rect(xleft = xMid - rectWidth/3, 
                 ybottom = yMid - rectHeight/2, 
                 xright = xMid + rectWidth/3, 
                 ytop = yMid + rectHeight/2,
                 col = col.bg, border = border.bg)
  
  ## Place the text:
  graphics::text(xMid, yMid, labels, col = col.text, cex = theCex, font = font, 
                 adj = c(0.5, 0.5))    
  
  ## Return value:
  if (length(xMid) == 1){
    invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                yMid + rectHeight/2))
  } else {
    invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                    yMid + rectHeight/2))
  }    
}

################################################################################################
#' 'darken'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' darken()
darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

################################################################################################
#' 'degribmodel'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' degribmodel()
degribmodel = function(themodel, lat, lon, elev, daterange=NULL){
  # themodel = "nbm"
  # lat = 35.87300564659412
  # lon = -78.78278973488705
  # elev=394
  
  # if (!is.null(daterange)){
  #   
  # }
  
  allgrbspath = paste(allgrbsfolder, themodel, sep="")
  allgrbs = list.files(allgrbspath, full.names=TRUE)
  
  ##########################################################################################################################################
  ##########################################################################################################################################
  ##########################################################################################################################################
  ##########################################################################################################################################
  # theecos = nc_sco_stations[Type == "ECONET"]
  # 
  # ecolat = theecos$Latitude
  # ecolon = theecos$Longitude
  # model = "nbm"
  # latlonname = theecos$Station
  # finaloutputdir = "/home/jordan/models/output/"
  # 
  # ecolat = c(ecolat, 35.9495, 35.2550)
  # ecolon = c(ecolon, -79.0473, -81.5190)
  # latlonname = c(latlonname, "ch", "sh")
  # 
  # latlonstr=list()
  # for (il in 1:length(ecolat)){
  #   latlonstr[[il]] = paste("-P -pnt ", ecolat[il], ",", ecolon[il], sep="")
  # }
  # latlonstr = paste(unlist(latlonstr), collapse = ' ')
  
  latlonstr = paste("-P -pnt ", lat, ",", lon, sep="")
  ##############################################################
  ##############################################################
  allgrbsdat=list()
  for (i in 1:length(allgrbs)){
    
    if (themodel=="href"){
      system(paste(degribpath, " ", paste(allgrbsfolder, grbfilename_nc, sep=""),
                   paste(" ", latlonstr, sep=""), " > /home/jordan/models/href/hreftemp.txt", sep=""))
      
      con <- file("/home/jordan/models/href/hreftemp.txt") 
      open(con);
      results.list <- list();
      current.line <- 1
      while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        theline <- unlist(strsplit(line, split=" "))
        if (!theline[1] %like% "Warning" & !theline[2] %like% "don't" & !theline[1] %like% "Use"){
          if (current.line==1){
            results.list[[current.line]]  <- read.table(text=line, sep=" ")
            results.list[[current.line]] = as.data.table(results.list[[current.line]])
          } else {
            results.list[[current.line]]  <- read.table(text=line, sep=",")
            results.list[[current.line]] = as.data.table(results.list[[current.line]])
          }
        }
        current.line <- current.line + 1
      } 
      close(con)
      results.list[sapply(results.list, is.null)] <- NULL
      results.list = rbindlist(results.list, fill=FALSE)
      
      colnames(results.list) = as.character(unlist(results.list[1,]))
      colnames(results.list) = gsub("),", " ", colnames(results.list))
      colnames(results.list)[1:4] = gsub(",", " ", colnames(results.list)[1:4])
      
      system(paste("rm -rf /home/jordan/models/href/hreftemp.txt"))
      dat = results.list
      dat=dat[-1,]
      colnames(dat) = c("element", "unit", "refTime", "validTime", paste("value", 1:length(lat)))
      
    } else {
      dat = fread(text=system_read(paste(degribpath, " ", allgrbs[[i]],
                                         paste(" ", latlonstr, sep=""), sep="")), sep=",", header=FALSE)
      colnames(dat) = c("element", "unit", "refTime", "validTime", paste("value", 1:length(lat)))
    }
    
    cols = read.table(text=system_read(paste(degribpath, " ", allgrbs[[i]],
                                             paste(" -I", sep=""), sep="")), sep=",", header=TRUE)
    setDT(cols)
    
    allvars_name = paste(trim(strsplitj(as.character.factor(cols$elem), "=", 1)), sep="_")
    allvars = paste(trim(strsplitj(as.character.factor(cols$elem), "=", 1)), 
                    trim(strsplitj(as.character.factor(cols$level), "-", 1)), sep="_")
    
    if (themodel=="gfs"){
      dat$var = strsplitj(trim(as.character.factor(cols$elem)), "=", 1)
      dat$varlevel=allvars
      dat$forecasthr = cols$Proj.hr.
      thevars = c("TMP_2", "TMP_850", "TMP_50000", "DPT_2", "DPT_85000", "TCDC_0", "WIND_10", "RH_2", "GUST_10", "UGRD_10", "VGRD_10")
      dat = dat[varlevel %in% thevars]
      
      dat$splitvar = dat$varlevel
      
    } else if (themodel=="href"){
      dat$var = strsplitj(trim(as.character.factor(cols$elem)), "=", 1)
      dat$varlevel=allvars
      dat$forecasthr = cols$Proj.hr.
      thevars = c("TMP_2", "TMP_850", "TMP_50000", "DPT_2", "DPT_85000", "TCDC_0", "WIND_10", "RH_2", "GUST_10")
      dat = dat[allvars %in% thevars]
      
      dat$splitvar = dat$varlevel
      
    } else if (themodel=="hrrr"){
      dat$var = strsplitj(trim(as.character.factor(cols$elem)), "=", 1)
      dat$varlevel=allvars
      dat$forecasthr = cols$Proj.hr.
      thevars = c("TMP_2", "DPT_2", "TCDC_0", "WIND_10", "RH_2", "GUST_10", "UGRD_10", "VGRD_10")
      dat = dat[allvars %in% thevars]
      
      dat$splitvar = dat$element
      
    } else if (themodel=="nam"){
      dat$var = strsplitj(trim(as.character.factor(cols$elem)), "=", 1)
      dat$varlevel=allvars
      dat$forecasthr = cols$Proj.hr.
      thevars = c("TMP_2", "DPT_2", "TCDC_0", "WIND_10", "RH_2", "GUST_10", "UGRD_10", "VGRD_10")
      dat = dat[varlevel %in% thevars]
      dat$varlevel = trim(dat$varlevel)
      
      dat$splitvar = dat$varlevel
      
      
    } else if (themodel=="nbm"){
      msgnums = which(allvars %in% c("T_2", "WindSpd_10", "RH_2", "Sky_0", "Td_2"))
      dat$var = strsplitj(trim(as.character.factor(cols$elem)), "=", 1)
      dat$forecasthr = cols$Proj.hr.
      dat = dat[msgnums,]
      
      dat$splitvar = dat$element
      
    } else if (themodel=="rtma"){
      # msgnums = which(allvars %in% c("T_2", "WindSpd_10", "RH_2", "Sky_0", "Td_2"))
      # dat$var = strsplitj(trim(as.character.factor(cols$elem)), "=", 1)
      dat$forecasthr = cols$Proj.hr.
      # dat = dat[msgnums,]
      
      dat$splitvar = dat$element
    }
    
    ################################
    ################################
    
    dat2 = list()
    for (id in seq(5, length(lat)+4, 1)){
      dat2[[id]] = data.table(dat[,1:4], var=dat[,..id], forecasthr=dat$forecasthr, splitvar=dat$splitvar)
      dat2[[id]] = split(dat2[[id]], f=dat2[[id]]$splitvar)
      for (id2 in 1:length(dat2[[id]])){
        colnames(dat2[[id]][[id2]]) = c(colnames(dat2[[id]][[id2]])[1:4], dat2[[id]][[id2]]$splitvar[1], colnames(dat2[[id]][[id2]])[6:7])
        dat2[[id]][[id2]] = data.table(dat2[[id]][[id2]][,3:5], forecasthr=dat2[[id]][[id2]]$forecasthr)
        # dat2[[id]][[id2]]$point = latlonname[[id-4]]
        dat2[[id]][[id2]]$lat=lat
        dat2[[id]][[id2]]$lon=lon
      }
      dat2[[id]] = mergeDTlist(dat2[[id]], by=c("refTime", "validTime", "forecasthr", "lat", "lon"))
    }
    dat2 = rbindlist(dat2)
    
    # if (themodel!="nbm"){
    dat2$refTime = paste(paste(substr(dat2$refTime, 1, 4),substr(dat2$refTime, 5, 6),substr(dat2$refTime, 7, 8), sep="-"),
                         paste(substr(dat2$refTime, 9, 10),substr(dat2$refTime, 11, 12),"00", sep=":"), sep=" ")
    dat2$validTime = paste(paste(substr(dat2$validTime, 1, 4),substr(dat2$validTime, 5, 6),substr(dat2$validTime, 7, 8), sep="-"),
                           paste(substr(dat2$validTime, 9, 10),substr(dat2$validTime, 11, 12),"00", sep=":"), sep=" ")
    #}
    
    # theeecos2 = data.table(point=theecos$Station, lat=theecos$Latitude, lon=theecos$Longitude, elev=theecos$Elevation)
    # theeecos2 = rbindlist(list(theeecos2, data.table(point=c("ch", "sh"), lat=c(35.9495, 35.2550), lon=c(-79.0473, -81.5190), elev=c(486, 863))))
    
    dat2$elevation = elev
    
    allgrbsdat[[i]] = dat2
    cat(i, "\n")
  }
  allgrbsdat = rbindlist(allgrbsdat)
  # dat3 = merge(dat2, theeecos2, by="point", all.x=TRUE)
  # all_model_output_v2 = paste("/home/jordan/models/output_v2/", themodel, "/")
  # write.csv(dat3, paste(all_model_output_v2, themodel, "_", current_date, "_", current_run_hr, "z.csv", sep=""))
  
  ################################
  ################################
  if (themodel=="hrrr"){
    hrrr_files_delete = list.files(modelfolder, pattern=".grib2")    
    if (length(hrrr_files_delete)!=0){
      for (ihd in 1:length(hrrr_files_delete)){
        paste("rm -rf ", modelfolder, hrrr_files_delete[[ihd]])
      }
    }
  }
  
  # system(paste("Rscript /home/jordan/R/models/modelwbgt_v2.R",
  #              paste("/home/jordan/models/output_v2/", themodel, "/", themodel, "_", current_date, "_", current_run_hr, "z.csv", sep=""),
  #              paste("/home/jordan/models/output_v2_wbgt/", themodel, "/", themodel, "_", current_date, "_", current_run_hr, "z_wbgt", sep=""), sep=" "))
  
  write.csv(allgrbsdat, "rdu_rtma_052019_082019.csv", row.names=FALSE)
  
  system(paste("Rscript", modelwbgtscript,
               "C:/Users/jclar/Documents/R_Projects/weather_site_models/rdu_rtma_052019_082019.csv",
               "C:/Users/jclar/Documents/R_Projects/weather_site_models/rdu_rtma_052019_082019_wbgt",
               "dewpoint", "0",
               sep=" "))
  
}

################################################################################################
#' 'getsize'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' getsize()
getsize=function(theobj, ...){
  print(format(object.size(theobj), ...))
}

################################################################################################
#' 'yearmo'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' yearmo()
yearmo = function(datetimeobj){
  return(paste(lubridate::year(datetimeobj), charnum(lubridate::month(datetimeobj)), sep=""))
}

################################################################################################
#' 'sqlconn'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' sqlconn()
sqlconn = function(){
  
        cat('
        drv = dbDriver("MySQL")
        con = dbConnect(drv, user="clarkw7_admin", password = "weather50!!", dbname="clarkw7_WXDTU",
                        host="clarkweather.com")
        sqlquery <- paste("SELECT * FROM envoy_2019")
        data <- dbGetQuery(con, sqlquery)
        # data_loc <- dbGetQuery(con, sqlquery)
        dbDisconnect(con)
        setDT(data)
        
        
        drv = dbDriver("MySQL")
        con = dbConnect(drv, user="clarkw7_admin", password = "weather50!!", dbname="clarkw7_WXDTU",
                        host="clarkweather.com")
        dbWriteTable(con, name = "envoy_2019_wbgt", value = data, row.names = FALSE, append=TRUE, overwrite=FALSE)
        # dbWriteTable(con, name = "envoy_2019_wbgt", value = data, row.names = FALSE, append=FALSE, overwrite=TRUE)
        dbDisconnect(con)
        )
        ')
}

################################################################################################
#' 'keepnumbers'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' keepnumbers()
keepnumbers=function(input){
  return(gsub("[^0-9]", "", input))
}

################################################################################################
#' 'winddeg'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' winddeg()
winddeg = function(wind){
  ogwind = data.table(windcol = wind)
  winddirs <- floor(seq(0,360, by=22.5))
  windirs_eng = data.frame(eng=as.character(c("N", "NNE", "NE", "ENE", "E", "ESE", "SE",
                                              "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")),
                           dirs = head(winddirs,-1))
  
  newwind = merge(ogwind, data.table(dirs=windirs_eng$dirs, windcol=windirs_eng$eng), by="windcol", all.x=TRUE)
  return(as.vector(newwind$dirs))
}

################################################################################################
#' 'as.Datej'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' as.Datej()
# as.Date = function(todate, tz="America/New_York", ...){
#   
#   todate_tz = attr(todate, "tzone")
# 
#   as.Date_og = function (x, ...) {
#     UseMethod("as.Date")
#   }
#   if (tz == "America/New_York"){
#     cat("Date derived from object with timezone of: ", todate_tz, "\n")
#     cat("Asumming tz = America/New_York", "\n")
#     return(as.Date_og(todate, tz="America/New_York", ...))
#   } else {
#     return(as.Date_og(todate, ...))
#   }
# }
as.Datej = function(dates, tz=NULL, ...){
  todate_tz = attr(dates, "tzone")
  # nchar(todate_tz)
  
  if(is.null(tz)){
    #if (nchar(todate_tz)!=0){
    if (!is.null(todate_tz)){
      cat("Date derived from object with timezone of: ", todate_tz, "\n")
      newdates = as.Date(dates, tz= todate_tz, ...)
      #return(newdates)
    } else {
      #if (tz == "America/New_York"){
        cat("Asumming tz = America/New_York", "\n")
        newdates = as.Date(dates, tz= "America/New_York", ...)
        # return(as.Date_og(todate, tz="America/New_York", ...))
      #} else {
        # cat("Asumming tz = ", tz, "\n")
        # newdates = as.Date(dates, tz= tz, ...)
      #}
    }
  ##########################  
  } else {
    #if (nchar(todate_tz)!=0){
    if (!is.null(todate_tz)){
      if (todate_tz != tz){
        stop("Date/time object not in same TZ as requested Date TZ: ", "\n")
        #newdates = as.Date(dates, tz = tz, ...)
      } else {
        newdates = as.Date(dates, tz = tz, ...)
      }
    } else {
      #if (tz == "America/New_York"){
        #cat("Asumming tz = America/New_York", "\n")
       # newdates = as.Date(dates, tz= "America/New_York", ...)
        # return(as.Date_og(todate, tz="America/New_York", ...))
     # } else {
       # cat("Asumming tz = ", tz, "\n")
        newdates = as.Date(dates, tz= tz, ...)
      #}
    }
    
  }
  
  return(newdates)
  # as.Datej(as.POSIXct("2019-01-01 05:00:00"))
  # as.Datej(as.POSIXct("2019-01-01 05:00:00", tz="America/New_York"))
  # 
  # as.Datej(as.POSIXct("2019-01-01 05:00:00", tz="America/New_York"), tz="America/New_York")
  # as.Datej(as.POSIXct("2019-01-01 05:00:00", tz="America/New_York"), tz="GMT")
  # 
  # as.Datej(as.POSIXct("2019-01-01 05:00:00"), tz="GMT")
  # 
  # as.Datej("2019-01-01", tz="GMT")
  # 
  # dates2 = as.POSIXct("2019-01-01 05:00:00")
  # attr(dates2, "tzone") <- "GMT"
  # 
  # dates = as.POSIXct("2019-01-01 05:00:00")
  # attr(dates, "tzone")
}


################################################################################################
#' 'deletenulllist'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' deletenulllist()
deletenulllist=function(alist, nullmarker="NULL"){
  for (itt in names(alist)){
    if (length(alist[[itt]])==1){
      if (alist[[itt]][1]==nullmarker){
        alist[[itt]] <- NULL
      }
    }
  }
  return(alist)
}

################################################################################################
#' 'renamecols'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' renamecols()
renamecols = function(thecolnames, pastewhat, mergecol="datetime"){
  # thecolnames=colnames(ndfd)
  # pastewhat="_ndfd"
  # mergecol= c("validTime", "point")
  
  if (length(thecolnames[thecolnames %like% pastewhat]) != (length(thecolnames)-1)){
    mergecolwhich=which(thecolnames %in% mergecol)
    alreadpasted=thecolnames[thecolnames %like% pastewhat]
    
    thecolnames[!thecolnames %in% alreadpasted] = paste(thecolnames[!thecolnames %in% alreadpasted], pastewhat)
    thecolnames[mergecolwhich] <- gsub(pastewhat, "", thecolnames[mergecolwhich])
    return(thecolnames)
  } else {
    cat("Looks like colnames already changed", "\n")
  }
  return(thecolnames)
}

################################################################################################
#' 'mergecols'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' mergecols()
mergecols = function(dt, colnameresult, colnamestojoin){
  # colnameresult = "interval"
  # colnamestojoin = c("interval_eco", "info_k", "info_iss", "info_iso")
  # dt = eco_all
  
  
  for (inc in 1:length(colnamestojoin)){
    
    if (class(dt[[colnamestojoin[[inc]]]])=="character"){
      dt[[colnameresult]] = NA
      dt[[colnameresult]] = as.character(dt[[colnameresult]])
    } else if (class(dt[[colnamestojoin[[inc]]]])=="numeric"){
      dt[[colnameresult]] = NA
      dt[[colnameresult]] = as.numeric(dt[[colnameresult]])
    }
    
    dt[is.na(dt[[colnameresult]]) & !is.na(colnamestojoin[[inc]])][[colnameresult]]  <- dt[is.na(dt[[colnameresult]] ) & !is.na(colnamestojoin[[inc]])][[colnamestojoin[[inc]]]] 
  }
  
  # infocols = colnames(eco_all)[colnames(eco_all) %like% "info"]
  # infocolsdel = infocols[!nchar(infocols) == nchar("info")]
  
  return(dt[, !colnamestojoin, with=FALSE])
  
}

################################################################################################
#' 'maxj'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' maxj()
maxj = function(vector){
  if (length(vector[is.na(vector)]!=0)){
    cat("NAs removed", "\n")
    return(max(vector, na.rm=TRUE))
  } else { return(max(vector, na.rm=FALSE)) }
}

################################################################################################
#' 'minj'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' minj()
minj = function(vector){
  if (length(vector[is.na(vector)]!=0)){
    cat("NAs removed", "\n")
    return(min(vector, na.rm=TRUE))
  } else { return(min(vector, na.rm=FALSE)) }
}

################################################################################################
#' 'meanj'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' meanj()
meanj = function(vector){
  if (length(vector[is.na(vector)]!=0)){
    cat("NAs removed", "\n")
    return(mean(vector, na.rm=TRUE))
  } else { return(mean(vector, na.rm=FALSE)) }
}

################################################################################################
#' 'medianj'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' medianj()
medianj = function(vector){
  if (length(vector[is.na(vector)]!=0)){
    cat("NAs removed", "\n")
    return(median(vector, na.rm=TRUE))
  } else { return(median(vector, na.rm=FALSE)) }
}

################################################################################################
#' 'intomb'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' intomb()
intomb = function(presin){
  return(presin/0.029530)
}

################################################################################################
#' 'makecolorramp'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' makecolorramp()
makecolorramp = function(){
  cat('colorRampPalette(brewer.pal(5,"Set2"))(5)', "\n")
}

################################################################################################
#' 'currenttime'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' currenttime()
currenttime = function(){
  return(Sys.time())
}

################################################################################################
#' 'timestamp'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' timestamp()
timestamp = function(){
  return(Sys.time())
}

################################################################################################
#' 'processISS'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' processISS()
processISS = function(isszippath, isszipfilename, adate=NULL, kdt=NULL){
  library(data.table)
  library(jj)
  
  dir.create(paste(gsub(".zip", "", paste(isszippath, isszipfilename, sep="")), "/", sep=""))
  cat("Unzipped files stored in ", paste(gsub(".zip", "", paste(isszippath, isszipfilename, sep="")), "/", sep=""), "\n")
  unzip(paste(isszippath, isszipfilename, sep=""),
        exdir=gsub(".zip", "", paste(isszippath, isszipfilename, sep="")))  # unzip your file 
  
  iss_filepath = paste(gsub(".zip", "", paste(isszippath, isszipfilename, sep="")), "/", sep="")
  #iss_filepath="E:/OneDrive/Research/Field Work Summer 2019/crhs/20190530/station/"
  iss = fread(paste(iss_filepath, "ISSData.txt", sep=""))
  iss_inside = fread(paste(iss_filepath, "InsideData.txt", sep=""))
  
  iss$RecDateTime = as.POSIXct(iss$RecDateTime, tz="America/New_York")
  iss_inside$RecDateTime = as.POSIXct(iss_inside$RecDateTime, tz="America/New_York")
  
  iss = iss[order(RecDateTime)]
  iss_inside = iss_inside[order(RecDateTime)]
  
  iss = merge(iss, iss_inside[,3:8], by="RecDateTime", all.x=TRUE)
  
  if (length(list.files(iss_filepath, pattern=".txt")[list.files(iss_filepath, pattern=".txt") %like% "LeafSoilData.txt"])>0){
    iss_soil = fread(paste(iss_filepath, "LeafSoilData.txt",sep=""))
    iss_soil$RecDateTime = as.POSIXct(iss_soil$RecDateTime, tz="America/New_York")
    iss_soil = iss_soil[order(RecDateTime)]
    
    soil = data.table(RecDateTime=iss_soil$RecDateTime,
                      soiltemp1=iss_soil$LeafSoilTemp1,
                      soiltemp2=iss_soil$LeafSoilTemp2,
                      soilmoist=iss_soil$SoilMoist1)
    
    iss = merge(iss, soil, by="RecDateTime", all.x=TRUE)
    
  }
  
  if (!is.null(adate)){
    iss = iss[as.Date(RecDateTime) == adate]
  }
  
  iss[TempOut==32767]$TempOut <- NA
  iss = na.omit(iss)
  
  deccols = c(colnames(iss)[4:6], colnames(iss)[8:10], colnames(iss)[13:16], colnames(iss)[28:ncol(iss)])
  
  for (i in deccols){
    iss[[i]] = paste(substr(iss[[i]], 1, nchar(iss[[i]])-1),  substr(iss[[i]], nchar(iss[[i]]), nchar(iss[[i]])), sep=".")
  }
  iss$Barometer = paste(substr(iss$Barometer, 1, 2),  substr(iss$Barometer, 3, 5), sep=".")
  
  numcols = c(colnames(iss)[4:ncol(iss)])
  
  for (i in numcols){
    iss[[i]] = as.numeric(iss[[i]])
  }
  
  iss = iss[SolarRad!=32767]
  
  iss$RecDateTime = as.POSIXct(iss$RecDateTime, tz="America/New_York")
  iss$date = as.Datej(iss$RecDateTime, tz="America/New_York")
  
  return(iss)
}

################################################################################################
#' 'group_boxplots_ex'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' group_boxplots_ex()
group_boxplots_ex = function(){
  
  boxtransp = 0.5
  
  pdf(file="D:/OneDrive/Research/Field Work Summer 2019/plots/ndfd_hourly_error_groupboxplot2.pdf", family = "Times", height = 8, width = 12)
  par(mfrow=c(2,2))
  par(mar=c(0.5,0.5,0.5,2), oma=c(2, 3, 1, 0.2))
  #"CRHS Tennis Court vs. Practice Field (8/15)"
  group_boxplots(ecowbgt_hr_avg_ndfd, ecowbgt_hr_avg_ndfd$wbgt_l_eco - ecowbgt_hr_avg_ndfd$wbgt_ndfd, "hour", ylim=c(-12,12), outline=FALSE, yaxt="n",main="", xaxt="n")
  axis(side=2, at=seq(-20,20,4), cex.axis=1.25)
  abline(h=seq(-20,20,2), lty=2, col=add.alpha("black", 0.4))
  abline(v=seq(-20,26,2), lty=2, col=add.alpha("black", 0.3))
  abline(v=seq(-21,25,2), lty=2, col=add.alpha("black", 0.1))
  abline(h=0, col="red")
  axis(side=1, at =seq(0,24,1), labels=rep("", length(0:24)), tck=-0.02)
  text(x = c(par('usr')[2] - par('usr')[1])*0.3, y = c(par('usr')[4])*0.9, labels="1. ECONET MINUS NDFD WBGT", cex=1.5)
  group_boxplots(ecowbgt_hr_avg_ndfd, ecowbgt_hr_avg_ndfd$wbgt_l_eco - ecowbgt_hr_avg_ndfd$wbgt_ndfd, "hour", ylim=c(-12,12), outline=FALSE, yaxt="n", xaxt="n", main="", add=TRUE, col=add.alpha("white", boxtransp))
  
  group_boxplots(ecowbgt_hr_avg_nbm, ecowbgt_hr_avg_nbm$wbgt_l_eco - ecowbgt_hr_avg_nbm$wbgt_nbm, "hour", ylim=c(-12,12), outline=FALSE, yaxt="n", xaxt="n")
  axis(side=2, at=seq(-20,20,4), cex.axis=1.25)
  abline(h=seq(-20,20,2), lty=2, col=add.alpha("black", 0.4))
  abline(v=seq(-20,26,2), lty=2, col=add.alpha("black", 0.3))
  abline(v=seq(-21,25,2), lty=2, col=add.alpha("black", 0.1))
  abline(h=0, col="red")
  axis(side=1, at =seq(0,24,1), labels=rep("", length(0:24)), tck=-0.02)
  text(x = c(par('usr')[2] - par('usr')[1])*0.3, y = c(par('usr')[4])*0.9, labels="2. ECONET MINUS NBM WBGT", cex=1.5)
  group_boxplots(ecowbgt_hr_avg_nbm, ecowbgt_hr_avg_nbm$wbgt_l_eco - ecowbgt_hr_avg_nbm$wbgt_nbm, "hour", ylim=c(-12,12), outline=FALSE, yaxt="n", xaxt="n", add=TRUE, col=add.alpha("white", boxtransp))
  # par(mar=c(0,0.5,2,2), oma=c(0, 3, 1, 0.2))
  
  group_boxplots(ecowbgt_hr_avg_ndfd_nbm, ecowbgt_hr_avg_ndfd_nbm$wbgt_ndfd - ecowbgt_hr_avg_ndfd_nbm$wbgt_nbm, "hour", ylim=c(-12,12), outline=FALSE, yaxt="n", xaxt="n")
  axis(side=2, at=seq(-20,20,4), cex.axis=1.25)
  axis(side=1, at=seq(1,24,1), labels=rep("", length(1:24)), cex.axis=1.25)
  axis(side=1, at=seq(1,23,2), labels=seq(1,23,2)-1, cex.axis=1.25)
  abline(h=seq(-20,20,2), lty=2, col=add.alpha("black", 0.4))
  abline(v=seq(-20,26,2), lty=2, col=add.alpha("black", 0.3))
  abline(v=seq(-21,25,2), lty=2, col=add.alpha("black", 0.1))
  abline(h=0, col="red")
  text(x = c(par('usr')[2] - par('usr')[1])*0.3, y = c(par('usr')[4])*0.9, labels="3. NDFD MINUS NBM WBGT", cex=1.5)
  group_boxplots(ecowbgt_hr_avg_ndfd_nbm, ecowbgt_hr_avg_ndfd_nbm$wbgt_ndfd - ecowbgt_hr_avg_ndfd_nbm$wbgt_nbm, "hour", ylim=c(-12,12), outline=FALSE, yaxt="n", xaxt="n", add=TRUE, col=add.alpha("white", boxtransp))
  
  group_boxplots(ecowbgt_hr_avg_ndfd_nbm, ecowbgt_hr_avg_ndfd_nbm$speed_ndfd - ecowbgt_hr_avg_ndfd_nbm$speed_nbm, "hour", ylim=c(-3,3), outline=FALSE, yaxt="n", xaxt="n")
  axis(side=2, at=seq(-20,20,1), cex.axis=1.25)
  axis(side=1, at=seq(1,24,1), labels=rep("", length(1:24)), cex.axis=1.25)
  axis(side=1, at=seq(1,23,2), labels=seq(1,23,2)-1, cex.axis=1.25)
  abline(h=seq(-20,20,1), lty=2, col=add.alpha("black", 0.5))
  abline(v=seq(-20,26,2), lty=2, col=add.alpha("black", 0.4))
  abline(v=seq(-21,25,2), lty=2, col=add.alpha("black", 0.2))
  abline(h=0, col="red")
  text(x = c(par('usr')[2] - par('usr')[1])*0.3, y = c(par('usr')[4])*0.9, labels="4. NDFD MINUS NBM WIND 2M", cex=1.5)
  group_boxplots(ecowbgt_hr_avg_ndfd_nbm, ecowbgt_hr_avg_ndfd_nbm$speed_ndfd - ecowbgt_hr_avg_ndfd_nbm$speed_nbm, "hour", ylim=c(-3,3), outline=FALSE, yaxt="n", xaxt="n", add=TRUE, col=add.alpha("white", boxtransp))
  
  dev.off()
}
 
################################################################################################
#' 'jftp'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' jftp()
jftp = function(uploadfile, user, userpwd, servername, uploadlocation){
  # user = "clarkpublic@clarkwx.com"
  # userpwd = "weather50"
  # servername = "199.250.194.196"
  # uploadfile = "/home/jordan/modelsrunning.sh"
  # to upload a directory uploadfile = "/home/jordan/atest*"
  # uploadlocation = "/plots"
  
  system(paste("ncftpput -R -v -u ", '"' ,user, '" -p ', userpwd, " ", servername, " ", uploadlocation, " ", uploadfile))
}

################################################################################################
#' 'trimallcols'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' trimallcols()
trimallcols = function (data) {
  for (i in 1:length(colnames(data))){
    data[[colnames(data)[i]]] = trim(data[[colnames(data)[i]]])
  }
  return(data)
}

################################################################################################
#' 'convertunits'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' convertunits()
convertunits = function(what, from, to){
  if (from == "F" & to == "C"){
    return(what - 32)/1.8
  } else if (from == "C" & to == "F"){
    return(what * 1.8 + 32)
  } else if (from == "ms" & to == "mph"){
    return(what/0.44704)
  } else if (from == "mph" & to == "ms"){
    return(what*0.44704)
  } else if (from == "ms" & to == "mh"){
    return(what*3600)
  } else if (from == "in" & to == "mb"){
    return(what*33.8637526)
  } else if (from == "mb" & to =="in"){
    return(what*0.0295301)
  } else if (from == "mm" & to =="in"){
    return(what*0.0393701)
  } else if (from == "in" & to =="mm"){
    return(what*25.4)
  }else {
    cat("Unit conversion not available in function", "\n")
  }
  
}

################################################################################################
#' 'pardefault'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' pardefault()
pardefault = function(){
  cat("par(mar=c(5.1,4.1,4.1,2.1))", "\n")
}

################################################################################################
#' 'boxtext2'
#'
#' A function to darken provided color
#' @keywords cats
#' @export
#' @examples
#' boxtext2()
boxtext2 <- function(x, y, labels = NA, col.text = NULL, col.bg = NA, 
                     border.bg = NA, adj = NULL, pos = NULL, offset = 0.5, 
                     padding = c(0.5, 0.5), cex = 1, font = graphics::par('font')){
  
  ## The Character expansion factro to be used:
  theCex <- graphics::par('cex')*cex
  
  ## Is y provided:
  if (missing(y)) y <- x
  
  ## Recycle coords if necessary:    
  if (length(x) != length(y)){
    lx <- length(x)
    ly <- length(y)
    if (lx > ly){
      y <- rep(y, ceiling(lx/ly))[1:lx]           
    } else {
      x <- rep(x, ceiling(ly/lx))[1:ly]
    }       
  }
  
  ## Width and height of text
  textHeight <- graphics::strheight(labels, cex = theCex, font = font)
  textWidth <- graphics::strwidth(labels, cex = theCex, font = font)
  
  ## Width of one character:
  charWidth <- graphics::strwidth("e", cex = theCex, font = font)
  
  ## Is 'adj' of length 1 or 2?
  if (!is.null(adj)){
    if (length(adj == 1)){
      adj <- c(adj[1], 0.5)            
    }        
  } else {
    adj <- c(0.5, 0.5)
  }
  
  ## Is 'pos' specified?
  if (!is.null(pos)){
    if (pos == 1){
      adj <- c(0.5, 1)
      offsetVec <- c(0, -offset*charWidth)
    } else if (pos == 2){
      adj <- c(1, 0.5)
      offsetVec <- c(-offset*charWidth, 0)
    } else if (pos == 3){
      adj <- c(0.5, 0)
      offsetVec <- c(0, offset*charWidth)
    } else if (pos == 4){
      adj <- c(0, 0.5)
      offsetVec <- c(offset*charWidth, 0)
    } else {
      stop('Invalid argument pos')
    }       
  } else {
    offsetVec <- c(0, 0)
  }
  
  ## Padding for boxes:
  if (length(padding) == 1){
    padding <- c(padding[1], padding[1])
  }
  
  ## Midpoints for text:
  xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]
  
  ## Draw rectangles:
  rectWidth <- textWidth + 2*padding[1]*charWidth
  rectHeight <- textHeight + 2*padding[2]*charWidth    
  graphics::rect(xleft = xMid - rectWidth/2, 
                 ybottom = yMid - rectHeight/2, 
                 xright = xMid + rectWidth/2, 
                 ytop = yMid + rectHeight/2,
                 col = col.bg, border = border.bg)
  
  ## Place the text:
  graphics::text(xMid, yMid, labels, col = col.text, cex = theCex, font = font, 
                 adj = c(0.5, 0.5))    
  
  ## Return value:
  if (length(xMid) == 1){
    invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                yMid + rectHeight/2))
  } else {
    invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                    yMid + rectHeight/2))
  }    
}
