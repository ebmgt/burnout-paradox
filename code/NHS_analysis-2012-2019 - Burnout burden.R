#This file is best used within R Studio
# rbadgett@kumc.edu
### Start----------------------------------------
version
citation(package = "base", lib.loc = NULL, auto = NULL)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#setwd("../plots")
##* Graphics --------------------------
#windows(600, 600, pointsize = 12) # Size 600x600
getwd()
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
old.par <- par(no.readonly=T)
plot.new()
xmin <- par("usr")[1] + strwidth("A")
xmax <- par("usr")[2] - strwidth("A")
ymin <- par("usr")[3] + strheight("A")
ymax <- par("usr")[4] - strheight("A")
##* Libraries------------------------------------
library(openxlsx) # read.xlsx
library(plyr)
library(splines)
#library(dominanceanalysis) # Helps interpret LM by estimating contributions to R2 from each predictor
#library(stringr) #str_locate, str_to_title
##* Constants declaration -------------------
`%notin%` <- Negate(`%in%`)
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
date.current <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE))
(date.current.pretty <- as.character(strftime (Sys.time(), format="%m/%d/%Y", tz="", usetz=FALSE)))
p.value <- sprintf(p.value, fmt='%#.3f')
I2.label <- expression(paste( plain("I") ^ plain("2"), plain("(%)")))
summary.label <- bquote(paste("RE Model (I^2 = ",(formatC(res$I2, digits=1, format="f")), "%)"))
##* Encoding characters---------------
# https://www.unicodepedia.com/groups/
# http://www.unicode.org/charts/
##* Footnotes
# https://www.unicodepedia.com/groups/general-punctuation/
# Dagger  \u2020
# Double dagger  \u2021
# Section symbol \A7
# Double Vertical Line \u2016
# Para    \B6 or \u0086
##*Greek
# https://unicode.org/charts/nameslist/n_0370.html
##* Troubleshooting grab -----------------------
options(error=NULL)
library(tcltk) # For troubleshooting
# msgbox for troubleshooting: 
# tk_messageBox(type = "ok", paste(current.state,': ', nrow(data.state.current),sepo=''), caption = "Troubleshooting")
# browser()
# Finish, quit is c or Q
# enter f press enter and then enter Q

### Data grab -----------------------------------
# co <- read.table("https://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
file.filter   <- matrix(c("Spreadsheets","*.csv;*.xls;*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
filename      <- choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
data.NHS.2012_2019 <- read.table(filename, header=TRUE, sep=",", na.strings="NA", stringsAsFactors = FALSE, dec=".", strip.white=TRUE)
head(data.NHS.2012_2019)
nrow(data.NHS.2012_2019)
summary(data.NHS.2012_2019)

attach(data.NHS.2012_2019)

### Data cleaning--------------------------------
##* Rename columns?--------------------

##* Remove anyone? --------------------
#data.harm   <- data.harm[complete.cases(data.harm), ]
# All added 2021-02-01
data.NHS.2012_2019 <- data.NHS.2012_2019[!is.na(data.NHS.2012_2019$SHMI.Index),]
nrow(data.NHS.2012_2019)
data.NHS.2012_2019 <- data.NHS.2012_2019[!is.na(data.NHS.2012_2019$Survey.clinical.proportion),]
nrow(data.NHS.2012_2019)
data.NHS.2012_2019 <- data.NHS.2012_2019[!is.na(data.NHS.2012_2019$Responses),]
nrow(data.NHS.2012_2019)

# Added for burnout burden study
data.NHS.2012_2019 <- data.NHS.2012_2019[!is.na(data.NHS.2012_2019$WorkStress.Index),]


## Descriptive stats-----------------------------
unique(data.NHS.2012_2019$Code)
length(unique(data.NHS.2012_2019$Code))

attach (data.NHS.2012_2019)
summary (Responses)
summary (Survey.clinical.proportion)
summary(Engagement.Index.dichot)
summary(Engagement.Index.dichot)
summary(Engagement.Index.dichot)
summary(Engagement.Index.dichot)
summary(RecommendWork.Index.dichot)
summary(WorkStress.Index)
dettach (data.NHS.2012_2019)

# Summary of number of Trusts and respondents by year
dt <- ddply(data.NHS.2012_2019, .(Year.Index), summarize, Trust.count = length(Code), Responses.median= median (Responses, na.rm = TRUE))

# Burnout burden
data.NHS.2012_2019$Year.Trust <- 1
data.NHS.2012_2019$WorkStress.Burden <- 0
WorkStress.Median <- summary(data.NHS.2012_2019$WorkStress.Index)[3]

data.NHS.2012_2019 <- data.NHS.2012_2019[order(data.NHS.2012_2019$Code, data.NHS.2012_2019$Year.Index),]

for (n in 2:nrow(data.NHS.2012_2019)){
  if (data.NHS.2012_2019$Code[n] != data.NHS.2012_2019$Code[n-1]){data.NHS.2012_2019$Year.Trust[n] <- 1}else{data.NHS.2012_2019$Year.Trust[n] = data.NHS.2012_2019$Year.Trust[n-1] + 1}
  if (data.NHS.2012_2019$WorkStress.Index[n] < WorkStress.Median){
    if (data.NHS.2012_2019$Year.Trust[n] == 1){data.NHS.2012_2019$WorkStress.Burden[n] <- 1}else{data.NHS.2012_2019$WorkStress.Burden[n] <- data.NHS.2012_2019$WorkStress.Burden[n-1] + 1}
    }
}

table(data.NHS.2012_2019$WorkStress.Burden)

aggregate(data.NHS.2012_2019$SHMI.Index, list(data.NHS.2012_2019$WorkStress.Burden), FUN=mean)

dt <- ddply(data.NHS.2012_2019, .(WorkStress.Burden), summarize, Count = length(Code), SHMI = mean(SHMI.Index), SHMI.sd = sd(SHMI.Index), SHMI.var = sd(SHMI.Index)^2) #Trust.count = length(Code), Responses.median= median (Responses, na.rm = TRUE))
dt

## Spline analysis -------------------
# What is optimal number of knots
for(df in 1:3) {
  # Use natural rather than B-splines
  sf <- lm(SHMI ~ ns(WorkStress.Burden, df=df), weights = SHMI.var, data=dt)
  sf.out <- summary(sf)
  (pvalue <- sprintf(pf(sf.out$fstatistic[1], sf.out$fstatistic[2], sf.out$fstatistic[3], lower.tail=F), fmt='%#.4f'))
  r.squared.current <- sf.out$adj.r.squared
  print (paste('Knots (interior): ',df-1, '; df: ',df, '; R2 (adjusted) = ', sprintf(sf.out$adj.r.squared, fmt='%#.4f'), '; p-value: ',pvalue,sep = ''))
  df = df+1
}
sf <- lm(SHMI ~ ns(dt$WorkStress.Burden, df=2), weight=SHMI.var, data = dt)

# Linear regression --------------------
# lm.out <- lm(SHMI ~ WorkStress.Burden, weight=Count, data = dt) # More sig, but not seem methodologically best
lm.out <- lm(SHMI ~ WorkStress.Burden, weight=SHMI.var, data = dt)

(sf.out <- summary(sf))
summary(lm.out)
# Test of nonlinear
(anova.out <- anova(lm.out, sf))

# Plot ---------------
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
par(mar=c(8,4.1,4.1,2.1), mfrow=c(1,1))
data.temp <- mutate(dt, smooth=fitted(sf))

Main <- paste("Regression analysis of Summary Hospital-level Mortality Indicator",sep='')
base::plot(dt$WorkStress.Burden, dt$SHMI, pch = 19, cex=0.5, col="black", xlab = 'Years of Workstess > median across NHS all 2012 - 2019', ylab = "SHMI", main=Main)
abline(1, 0, lty = 2, col= 'gray')
#data.temp <- data.temp[order(data.temp$factor),]
lines(data.temp$WorkStress.Burden, data.temp$smooth, col = 'red')
df.optimal <- 2
i <- 1
for(i in 1:df.optimal-1) {
  knot.index <- round(length(data.temp$WorkStress.Burden)/df.optimal,0)*i
  points(data.temp$WorkStress.Burden[knot.index],data.temp$smooth[knot.index], pch=16,col="red")
}

abline(lm.out$coefficients[1], lm.out$coefficients[2], lty = 1, col= 'blue')

R2 <- sprintf("%1.1f%%", 100*sf.out$adj.r.squared)
R2.value <- formatC(100*sf.out$adj.r.squared, digits=1, format="f")
R2.label <- bquote(R^2  ~ "%")
R2.label <- bquote(R^2 ~ "=" ~ .(R2.value) ~ "%")

pvalue <- round(anova.out$`Pr(>F)`[2],3)
text(par("usr")[1]+strwidth("A")+6,par("usr")[3] + strheight("A") + 0.0025,adj=c(0,0),paste("Knots (interior) = ",df.optimal-1,sep=))
text(par("usr")[1]+strwidth("A")+6,par("usr")[3] + strheight("A") + 0.0010,adj=c(0,0),paste("P (non-linear) = ",pvalue,sep=))
text(par("usr")[1]+strwidth("A")+6,par("usr")[3] + strheight("A") - 0.0005,adj=c(0,0),R2.label)

mtext(expression(paste(bold("Notes: "),)), side = 1, line = 4,adj=0,cex=1)
#mtext(expression(paste("Red",)), side = 1, line = 5,adj=0,cex=1, col='red')
mtext(expression(paste("Blue"," is linear regression",)), side = 1, line = 5,adj=0,cex=1)
mtext(expression(paste("Red"," is spline regression with optimal number of interior knots",)), side = 1, line = 6,adj=0,cex=1)
mtext(expression(paste("Splines are natural cubic splines",)), side = 1, line = 7,adj=0,cex=1)

FileName <- 'plot of regression of years.burden vs SHMI'
rstudioapi::savePlotAsImage(
  paste(FileName, ' - ', date.current, '.png', sep=''),
  format = "png", width = 800, height = 600)

