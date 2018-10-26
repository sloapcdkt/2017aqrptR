#################
## Appendix  A ##
#################

#######################################
## define consistent color palatte   ##
#######################################
colors <- RColorBrewer::brewer.pal(8, "Set1")[-6]
colors <- c(colors, colors[c(2,6,7)])
sites <- c( "San Luis Obispo", "Morro Bay", "NRP",
            "Paso Robles", "Atascadero",  "Red Hills", "Carrizo Plains",
            "CDF", "Mesa2", "Oso Flaco")
colors <- data.frame(sites=sites, colors=colors, stringsAsFactors = FALSE)
rownames(colors) <- c("slo", "morro", "nrp", "paso", "atas", "red", "carrizo", "cdf", "mesa", "oso")
rm(sites)




################################
## Figures A1-A3              ##
################################

# load data. Historic hourly data from CDF was previous extracted from AQS and formatted
# for a different analysis, so just use that cleaned up file:
old<-read.csv("arch-cdf.csv", stringsAsFactors = FALSE) 

# format date, month, year variables
old$date<-as.POSIXct(old$date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
old$month<-format(old$date, "%m")
old$year<-format(old$date, "%Y")

# load libaries 
library(openair) # for wind rose function
library(dplyr)   # for data sorting

# plot function
plot.rose<-function(monthid){
  
  old %>%
    filter(month==monthid) %>%
    select(ws=wsv, wd=wdv, date) -> sub
  
  windRose(sub, main="", type="year", 
           paddle=FALSE, key.position="right", annotate = FALSE,
           key = list(footer="mph"), breaks=c(0, 3,6, 9, 12), grid.line=10)
}

###
# Figures A1-A3
###

# April
svg("figa1.svg", width = 8, height = 4, 
    pointsize = 10)
plot.rose("04")   
dev.off()

# May
svg("figa2.svg", width = 8, height = 4, 
    pointsize = 10)
plot.rose("05")   
dev.off()

# June
svg("figa3.svg", width = 8, height = 4, 
    pointsize = 10)
plot.rose("06")   
dev.off()

# Clean up
rm(old, plot.rose)


##########################################
## Difference in Differences Analysis   ##
## not in 2017 report                   ##
##########################################

##################
## load needed libraries and functions:
library(openair)
library(dplyr)
source('00_AQSloader.R')

##################
## Load CDF/Oso Data from AQS export 
##################
cdf<-load.aqs("AMP501_1683881-0.txt")
names(cdf)<-c("date", "ws.cdf", "wd.cdf", "pm10.cdf", "pm10.oso")


###################
## bring in S1 wind data:
###################

# get file names
files <- list.files(pattern = "^vdv")

# loop thru files, load them, then merge together
for (i in 1:length(files)){  
  assign(paste0("s1.", i), read.csv(files[i], comment.char="#", skip=6, as.is = TRUE))
}
s1 <- do.call(rbind, mget(paste0("s1.", 1:length(files))))
rm(list=paste0("s1.", 1:length(files)), files, i)

# format S1
s1 <- s1[, c(1, 31, 24)]
names(s1)<-c("date", "wd.s1", "ws.s1")
s1$date<-as.POSIXct(as.character(s1$date), tz="UTC", format="%Y-%m-%d %H:%M:%S")

# check 2017 vs earlier
s1$year <- as.numeric(format(s1$date, "%Y"))
par(mfrow = c(2,3))
for (i in 2012:2017){
  hist(s1$ws[s1$year==i], main = i)
}
for (i in 2012:2017){
  hist(s1$wd[s1$year==i], main = i)
}
# looks good, so clean up
par(mfrow = c(1,1))
s1$year <- NULL
rm(i)

##################
## merge hourly cdf and s1 data
##################

data<-merge(cdf, s1, by="date", all = TRUE)
rm(cdf, s1, load.aqs)
 
# write data file for AppendixA.Rmd (used later)
year <- as.numeric(format(data$date, "%Y"))
write.table(data[year >= 2016,], "DiD.csv", row.names = FALSE, sep = ",")

##########################################
## Continued in AppendixA.Rmd document! ##
##########################################

##--------------------------------------##

##########################################
## The following additional analyses    ##
## are not included in the 2017 AQR but ##
## are provided here for continuity with##
## the previous years' reports          ##
##########################################

##########################################
## Filter Days Analysis                 ##
##########################################

## need these variables later
data$day<-format(data$date, "%Y-%m-%d")
data$hour<-format(data$date, "%H")

###################
## Apply filter criteria
###################

## filter to only the hours 1000-1500; require complete data
data %>%
  filter (hour >= 10) %>%
  filter (hour <= 15) -> noprecip   

noprecip$pm10.oso <- NULL # not needed for this

noprecip <- noprecip[complete.cases(noprecip), ]
days <- tapply(noprecip$date, noprecip$day, length)
noprecip <- noprecip[noprecip$day %in% names(which(days == 6)), ]

## apply criterion: Site S1 must have all hourly wind speeds > or = 5 m/s
as.data.frame(noprecip) %>% 
  group_by(day) %>%
  summarize(min.ws = min(ws.s1, na.rm=T)) -> min.ws

id<-min.ws$day[min.ws$min.ws >= 5]
a<-noprecip$day %in% id
noprecip<-noprecip[a,]

## apply criterion:  Site S1 must NOT have any hourly wind direction >310 degrees

noprecip %>% 
  group_by(day) %>%
  summarize(max.wd = max(wd.s1, na.rm=T)) -> max.wd

id<-max.wd$day[max.wd$max.wd <= 310]
a<-noprecip$day %in% id
noprecip<-noprecip[a,]

## apply criterion:  The CDF site must NOT have any hourly wind direction <285 degrees

noprecip %>% 
  group_by(day) %>%
  summarize(min.wd = min(wd.cdf, na.rm=T)) -> min.wd
id<-min.wd$day[min.wd$min.wd >= 285]  
a<-noprecip$day %in% id
noprecip<-noprecip[a,]

## apply criterion:  Site S1 must have at least 3 of the 6 hours > 10 m/s
noprecip %>% 
  group_by(day) %>%
  summarize(hours = sum(ws.s1>10, na.rm=T)) -> hours

id<-hours$day[hours$hours >= 3]
a<-noprecip$day %in% id
noprecip<-noprecip[a,]

## apply criterion:  Site S1 vector average wind direction must be between 
## 285 and 300 degrees for the 6-hr period.

noprecip$cos<-cos(2*pi*noprecip$wd.s1/360)*noprecip$ws.s1
noprecip$sin<-sin(2*pi*noprecip$wd.s1/360)*noprecip$ws.s1

noprecip %>%
  group_by(day) %>%
  summarize(cos = sum(cos, na.rm=T)/6) ->cos
noprecip %>%
  group_by(day) %>%
  summarize(sin = sum(sin, na.rm=T)/6) ->sin
vec<-merge(cos, sin, by="day")
vec$ws<-sqrt(vec$sin^2+vec$cos^2)
vec$wd<-atan2(vec$sin, vec$cos)*180/pi+360

id<-vec$day[vec$wd >= 285 & vec$wd <= 300]
a<-noprecip$day %in% id
noprecip<-noprecip[a,]


## cleanup
rm(cos, hours, max.wd, min.wd, min.ws, sin, vec, a, id, days)


##############################
## summarize data
# how many days do we have?
length(unique(noprecip$day)) 

noprecip$ratio <- noprecip$pm10.cdf/noprecip$ws.s1

# calc summary
noprecip %>% 
  group_by(year=format(noprecip$date, "%Y")) %>%
  summarize(days=length(pm10.cdf)/6,
            mean.pm=round(mean(pm10.cdf),0),
            sd.pm=round(sd(pm10.cdf),0),
            max.pm=round(max(pm10.cdf),0),
            mean.wscdf=round(mean(ws.cdf),1),
            sd.wscdf=round(sd(ws.cdf),1),
            max.wscdf=round(max(ws.cdf),1),
            mean.wsS1=round(mean(ws.s1),1),
            sd.wsS1=round(sd(ws.s1),1),
            max.wsS1=round(max(ws.s1),1),
            mean.ratio = round(mean(ratio), 1))-> overview.noprecip

overview.noprecip # print data for table in Appendix



################
# some exploratory graphs:
################
boxplot(noprecip$pm10.cdf~format(noprecip$date, "%Y"),
        main="CDF hourly PM10 during Filter Days",
        notch = TRUE)

par(mfrow=c(3,3))
for (i in 2011:2017){
  hist(noprecip$pm10.cdf[format(noprecip$date, "%Y")==i],
       main="", xlab=i)
}
par(mfrow=c(1,1))

plot(noprecip$ws.s1, noprecip$pm10.cdf,
     col = format(noprecip$date, "%Y"), pch = 16)

legend("topleft", unique(format(noprecip$date, "%Y")),
       col = unique(format(noprecip$date, "%Y")),
       pch = 16)

############################
## Plot normalized values vs WS
############################

svg("figa5.svg", width = 6, height = 4, 
    pointsize = 10)

plot(overview.noprecip$mean.wsS1, overview.noprecip$mean.pm,
     ylab="Mean CDF PM10, ug/m3",
     xlab="Mean S1 wind speed, m/s",
     xaxt = "n", yaxt = "n", bty = "n",
     xlim=c(10,12),
     ylim=c(250, 370))
axis(1, at = 10:12)
axis(2, at = seq(260, 360, by = 20), las = 2)
text(overview.noprecip$mean.wsS1, overview.noprecip$mean.pm, labels=overview.noprecip$year, pos=1)

dev.off()

#############################
## ANOVA

## prep data
noprecip$year<-format(noprecip$date, "%Y")
noprecip$ratio<-with(noprecip, pm10.cdf/ws.s1)

## doublecheck mean of ratios
mean(noprecip$ratio[noprecip$year < 2015])

## are the ratios gaussian?
hist(noprecip$ratio, freq=F)   #close enough; actually tighter than gaussian
lines(density(noprecip$ratio))
lines(seq(0, 60, by = 0.1), dnorm((seq(0, 60, by = 0.1)),mean(noprecip$ratio), sd(noprecip$ratio) ), col="blue")

## are the baseline years significantly different from one another?
m1 <-lm(ratio~year, data=noprecip, subset=year <= 2014)
summary(m1)## 
anova(m1)  ## NO, p-value=0.09458
par(mfrow=c(2,2))
plot(m1)   ## looks OK

## is 2015 different from the pooled baseline years?
m2<-lm(ratio~(year==2015), noprecip, subset = year < 2016)
summary(m2)
anova(m2) ## YES, p-value=0.04945
plot(m2)  ## looks OK

# equal variance?
var.test(noprecip$ratio[noprecip$year < 2015], noprecip$ratio[noprecip$year == 2015])

## is 2016 different from the pooled baseline years?
m3<-lm(ratio~(year==2016), noprecip, subset = year != 2015 & year != 2017)
summary(m3)
anova(m3) ## YES, p-value=0.0005
plot(m3)  ## looks OK

var.test(noprecip$ratio[noprecip$year < 2015], noprecip$ratio[noprecip$year == 2016])

## is 2017 different from the pooled baseline years?
m4<-lm(ratio~(year==2017), noprecip, subset = year < 2015 | year == 2017)
summary(m4)
anova(m4) ## YES, p-value=0.005
plot(m4)  ## looks OK

var.test(noprecip$ratio[noprecip$year < 2015], noprecip$ratio[noprecip$year == 2017])

## 2015 vs baseline
## t-test, assuming equal variances gives identical results as m2
t.test(noprecip$ratio[noprecip$year < 2015],noprecip$ratio[noprecip$year==2015], var.equal =T)
## assumption of equal variances is valid
var.test(noprecip$ratio[noprecip$year < 2015],noprecip$ratio[noprecip$year==2015])
## try non-parametric, even though ratio is ~Normal.
kruskal.test(ratio~(year==2015), data=noprecip[noprecip$year < 2016, ])      ## 2015 vs not 2015: p-value = 0.03037


## 2017 vs baseline
## t-test, assuming equal variances gives identical results as m4
t.test(noprecip$ratio[noprecip$year < 2015],noprecip$ratio[noprecip$year==2017], var.equal =T)
## assumption of equal variances is valid 
var.test(noprecip$ratio[noprecip$year < 2015],noprecip$ratio[noprecip$year==2017])
## try non-parametric, even though ratio is ~Normal.
kruskal.test(ratio~(year==2017), data=noprecip[noprecip$year != 2015 & noprecip$year != 2016, ])    


######################
## clean up
rm(i, m1, m2, m3, m4, noprecip, overview.noprecip)
par(mfrow=c(1,1))

####################################################
## Decision Tree                                  ##
####################################################

################
## Figure A6

## just apply model from previous report
lst <- split(data, data$day)

mean2<-function(x) {   ## create AQS-like averages: Need at least 16 valid hours & truncate result
  ifelse(sum(is.na(x))<7, trunc(mean(x, na.rm=TRUE)), NA)
}

pm10 <- sapply(lst, function(a) mean2(a$pm10.cdf) )
preds <- sapply(lst, function(a) ifelse(a$ws.s1[a$hour == 15] > 9.445 
                                     & a$wd.cdf[a$hour == 13] > 289.5, TRUE, FALSE))
exceeds <- pm10 > 50
years <- sapply(lst, function(a) format(a$date[1], "%Y"))

exceeds <- tapply(exceeds, years, sum, na.rm = TRUE)
preds <- tapply(preds, years, sum, na.rm = TRUE)


#### Plot of Predicted vs observed exceedences
svg("figa6.svg", width = 6, height = 4, 
    pointsize = 10)

## plot
plot(names(preds), preds, type="n",
     ylim=c(40, 100), xlim = c(2011, 2018),
     las=1, bty = "n", 
     ylab="Days",
     xlab="", xaxt = "n")

axis(1, at = 2011:2017)

points(names(preds), exceeds,
       type="l",
       pch=16,
       lwd=2,
       col=colors["cdf", "colors"])
points(names(preds), preds,
       type="l",
       pch=16,
       lwd=2,
       col=colors["carrizo", "colors"])

text(x = 2017, y = c(exceeds[7], preds[7]),
     pos = 4,
     labels = c("Observed", "Predicted"),
     font = 2,
     col = colors[c("cdf", "carrizo"), "colors"])

dev.off()

###################
## clean up      ##
###################

rm(colors, data, lst, exceeds, pm10, preds, year, years, mean2)
detach("package:openair", unload=TRUE)
