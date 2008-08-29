#quantileChanges<-"Birchip.quantileTrendChange.csv"
#co2<-350
#nYears<-40
#meanChanges<-"Woomelang.change.csv"
#inMetFile<-"Woomelang.ppd.met"
#outBaselineFile<-"Woomelang.baseline.met"
#outScenarioFile<-"Woomelang.scenario.met"

library(seas)
source ("read.SILO.R")

c1 <- read.csv(quantileChanges)
c2 <- read.csv(meanChanges)
change <- merge(c1, c2, by="month")                                               

# Develop the change scenario from the historical series.
historicalData<-read.met(file(inMetFile))
hdate <- as.Date(paste(historicalData$year,historicalData$day),"%Y %j")
historicalData$month <- as.integer(format.Date(hdate, "%m"))

scenario <- list(year=historicalData$year,
                       month=historicalData$month,
                       day=historicalData$day,
                       rain=historicalData$rain,
                       maxt=historicalData$maxt,
                       mint=historicalData$mint)

# Ancillary variables via linear model
radnLm <- lm(radn ~ maxt, historicalData)
panLm <- lm(pan ~ maxt, historicalData)
vpLm <- lm(vp ~ maxt, historicalData)
scenario$radn<-predict(radnLm,newdata=data.frame(maxt=scenario$maxt))
scenario$pan<-predict(panLm,newdata=data.frame(maxt=scenario$maxt))
scenario$vp<-predict(vpLm,newdata=data.frame(maxt=scenario$maxt))

for (what in c("latitude", "tav", "amp")) {
  scenario[[what]] <- historicalData[[what]]
}

write.apsim(file=outBaselineFile, scenario) 

scenario<-as.data.frame(scenario)

for (month in 1:12) {
   df <- scenario[scenario$month == month,]
   # Temperature scenario
   for (v in c("maxt", "mint")) {
      # Build interpolation function for each trend. 
      ## x is cdf position, y is oC/year trend
      fun<-approxfun(x=c(4,16,28), 
                     y=c(change[[paste(v,".m90",sep="")]][month],
                         change[[paste(v,".m50",sep="")]][month],
                         change[[paste(v,".m10",sep="")]][month]), rule=2)

      for (year in unique(scenario$year)) {
         # 1. Change dispersion by bending cdf
         old <- df[[v]][df$year == year]
         iold <- order(old, decreasing=T)
         cdf <- old[iold]
         newcdf <- nYears * fun(1:length(cdf)) + cdf
         new <- vector()
         new[iold]<-newcdf

         # 2. Shift mean, while accounting for change introduced in step 1.
         shift1 <- mean(new) - mean(old)
         if (v == "maxt") {
            shift2 <- change$maxtProp[month] * change$tmean[month]
         } else {
            shift2 <- (1.0 - change$maxtProp[month]) * change$tmean[month]
         }
         new <- new + (shift2 - shift1)

         if (length(new) > 0) {
            scenario[[v]][scenario$month == month & scenario$year == year] <- new
         }
      }
   }

   # Rainfall scenario
   for (year in unique(scenario$year)) {
      old <- sum(scenario$rain[scenario$year==year & scenario$month == month])
      new <- max(old + change$rain[month], 0.0)
      scale <- new / old
      ix <- scenario$month == month & scenario$year == year & scenario$rain > 0
      if (any(ix)) {
         scenario$rain[ix] <- scenario$rain[ix] * scale
      }
   }
}
scenario<-as.list(scenario)

scenario$co2 <- co2
for (what in c("latitude", "tav", "amp")) {
  scenario[[what]] <- historicalData[[what]]
}
scenario$radn<-predict(radnLm,newdata=data.frame(maxt=scenario$maxt))
scenario$pan<-predict(panLm,newdata=data.frame(maxt=scenario$maxt))
scenario$vp<-predict(vpLm,newdata=data.frame(maxt=scenario$maxt))

write.apsim(file=outScenarioFile, scenario) 

