#inputFile<-"..."
#outputFile<-"..."

# Find quantile regression components of from an input file,
# Write to an output file

library(seas)
source ("read.SILO.R")

# Determine trends, add to projection data
trendData<-as.data.frame(read.met(file(inputFile)))
tdate <- as.Date(paste(trendData$year,trendData$day),"%Y %j")
trendData$month <- as.integer(format.Date(tdate, "%m"))
months<-1:12
monthNames<-c("J","F","M","A","M","J","J","A","S","O","N","D")

q10<-function(x) { return(quantile(x, probs=0.10)) }
q50<-function(x) { return(quantile(x, probs=0.50)) }
q90<-function(x) { return(quantile(x, probs=0.90)) }
n<-function(x, y) { return(sum(x > y)) }

change<-data.frame(month=months)

for (month in months) {
   df <- trendData[trendData$month == month,]
   
   for (v in c("maxt", "mint")) {
      # Work out trend of each (10,50,90)th quantile in each month
      s<-data.frame(year=unique(df$year), 
                    q10=tapply(df[[v]], list(year=df$year), q10),
                    q50=tapply(df[[v]], list(year=df$year), q50),
                    q90=tapply(df[[v]], list(year=df$year), q90),
                    mean=tapply(df[[v]], list(year=df$year), mean))

      s1<-subset(s, s$year > 1957)

      m1 <- lm(q10 ~ year, s1)
      if (anova(m1)[["Pr(>F)"]][1] < 0.1) {
        m10<-as.double(m1$coefficients[2])
      } else {
        m10 <- 0
      }
      change[[paste(v,".m10",sep="")]][month] <- m10

      m2 <- lm(q50 ~ year, s1)
      if (anova(m2)[["Pr(>F)"]][1] < 0.1) {
        m50<-as.double(m2$coefficients[2])
      } else {
        m50 <- 0
      }
      change[[paste(v,".m50",sep="")]][month] <- m50

      m3 <- lm(q90 ~ year, s1)
      if (anova(m3)[["Pr(>F)"]][1] < 0.1) {
        m90<-as.double(m3$coefficients[2])
      } else {
        m90 <- 0
      }
      change[[paste(v,".m90",sep="")]][month] <- m90

      # Now trend in monthly mean. Don't bother with p-value.
      m <- as.double((lm(mean ~ year, s1))$coefficients[2])
      change[[paste(v,"Trend",sep="")]][as.integer(month)]<-m
   }
}

# Apportion temperature based on historical changes in tmax/tmin
# If either trend, in Tmax or Tmin, is negative, then just assume 
# the split is 50/50, otherwise things get too complex. 
change$maxtProp <- change$maxtTrend / (change$maxtTrend + change$mintTrend)
change$maxtProp[change$maxtTrend<0 | change$mintTrend<0] <- 0.5

write.csv(change, outputFile, row.names=F)
