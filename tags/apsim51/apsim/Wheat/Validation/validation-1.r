# Validation runs #1. 
# Produce 1 page for each treatment. Just predicted & observed data.

library(RODBC)

# The "latest" runs (ie your development tree)
#indir<-"//build_machine/build_machine/development/apsim/Wheat/Validation"
indir<-"c:/development/apsim/Wheat/Validation"

# Where to write output
#outDir<-"."
outDir<-indir

Crop<-"Wheat"

# "Aggregate" lists
groups<-list(c("biomass_wt", "grain_wt", "head_wt"),
             c("deadleaf_wt", "leaf_wt", "stem_wt"),
             c("lai", "tiller_no"),
             c("biomass_n", "grain_n"),
             c("leaf_n", "stem_n"),
             c("stage"),
             c("swdef_photo", "swdef_expan"),
             c("nfact_pheno", "nfact_expan"))

            
report <- function(outDir, obs,pred,Groups,Crop,expName,tName) {
   fnames <- vector()
   colours <- c("red", "blue", "green")
   obs$date <- as.Date(obs$date,"%d/%m/%Y")
   pred$date <- as.Date(as.character(pred$date),"%d/%m/%Y")
   for (group in Groups) {
      xlims <- range(obs$date,pred$date,finite=T)
      y <- vector()
      for (series in unlist(group)) {
         y <- c(y,pred[[series]],obs[[series]])
      }   
      ylims <- range(y,finite=T)
      if (sum(is.finite(ylims)) == 2) {
        fname <- paste(expName,tName,length(fnames),"png",sep=".")
        fnames <- c(fnames, fname)
        png(file=paste(outDir, fname, sep="/"), bg=rgb(243,243,243,max=255), width=350, height=350)
        plot(NA, xlim=xlims, ylim=ylims, cex.axis=.8, ylab="", xaxt="n", xlab="")
        axis.Date(1,c(pred$date,obs$date))
        for (series in unlist(group)) {
           colour <- colours[match(series,unlist(group))]
  
           if (sum(!is.na(pred[[series]])) > 0) {
              matplot(pred$date,pred[[series]],type="l",col=colour,add=T)
           }
           if (sum(!is.na(obs[[series]])) > 0) {
              points(obs$date,obs[[series]],pch=15,col=colour)
           }          
        }
        legend(xlims[1],ylims[2],unlist(group),col=colours,pch=15)
        dev.off()
     }
   }  
   return(fnames)
}

read.apsim<-function(filename) {
  s<-file.info(filename)
  data <- NULL
  if (!is.na(s$size) && s$size > 0) {
    conn <- file(filename)
    hdr <- readLines(conn, 4)[3]
    hdr<-sub(" +", "", hdr)
    data <- read.table(conn,header=F,skip=4,col.names=unlist(strsplit(hdr, " +")))
    names(data)<-tolower(names(data))
  }
  return(data)
}

squash<-function(pred, obs) {
  ids<- match(as.character(obs$x), as.character(pred$x))
  return (data.frame(x=obs$y, y=pred$y[ids]))
}

mdbFilename<-paste(indir, "/", Crop, ".mdb",sep="")
db <- odbcConnectAccess(mdbFilename)

crops <- sqlQuery(db, "SELECT Crops.CropID, Crops.Crop FROM Crops;")

experiments<-sqlQuery(db, "SELECT Experiments.ExpID, Experiments.Experiment FROM Experiments;")
traits<-tolower(as.character(sqlQuery(db, "SELECT Traits.Trait FROM Traits;")$Trait))

allTreatments<-vector()
allData<-list()

mhtml<-file(paste(outDir, "validation.html", sep="/"),open="w")
cat (file=mhtml,"<html><head>")
cat (file=mhtml,"<link href=\"../../../docs/shared/docstyle.css\" rel=\"stylesheet\" type=\"text/css\">", append=T)
cat (file=mhtml,"<title>", Crop, "Validation Runs</title>", append=T)
cat (file=mhtml,"</head><body><p class=\"Title1\">\n", append=T)

for (e in 1:dim(experiments)[1]) {
  expID<-experiments$ExpID[e]
  expName<-as.character(experiments$Experiment[e])
  treatments<-sqlQuery(db,paste(
    "TRANSFORM First(Levels.Level) AS FirstOfLevel \
     SELECT Experiments.ExpID, Designs.TreatmentID \
     FROM (Experiments INNER JOIN Treatments ON Experiments.ExpID = \
     Treatments.ExpID) INNER JOIN ((Factors INNER JOIN Levels ON \
     Factors.FactorID = Levels.FactorID) INNER JOIN Designs ON Levels.LevelID = \
     Designs.LevelID) ON Treatments.TreatmentID = Designs.TreatmentID \
     WHERE (((Experiments.ExpID)= ", expID, ")) \
     GROUP BY Experiments.ExpID, Designs.TreatmentID \
     ORDER BY Designs.TreatmentID \
     PIVOT Factors.Factor;", sep=""))

  for (t in 1:dim(treatments)[1]) {
    tID<-treatments$TreatmentID[t]
    tName<- as.character(sqlQuery(db, paste("SELECT Treatments.TreatmentName \
         FROM Treatments WHERE (Treatments.TreatmentID = ", tID, ");"))$TreatmentName)

    allTreatments<-append(allTreatments, paste(expName, "_", tName, sep=""))

    pred<-read.apsim(paste(indir, "/", Crop, "_", expName, "_", tName, ".out", sep=""))
    
    obs<-sqlQuery(db,paste(
         "TRANSFORM Avg(PlotData.Value) AS AvgOfValue \
          SELECT Plots.TreatmentID, PlotData.Date \
          FROM Plots INNER JOIN (Traits INNER JOIN PlotData ON Traits.TraitID = \
          PlotData.TraitID) ON Plots.PlotID = PlotData.PlotID \
          WHERE (((Plots.TreatmentID)=", tID, ")) \
          GROUP BY Plots.TreatmentID, PlotData.Date \
          ORDER BY PlotData.Date PIVOT Traits.Trait;"));
    names(obs) <- tolower(names(obs))
    images<-report(outDir, obs, pred, groups, Crop, expName, tName)

    fname<-paste(Crop, expName, tName, "html", sep=".")
    cat(file=mhtml, "<a href=\"", fname, "\">", append=T, sep="")
    cat(file=mhtml, Crop, expName, tName,  "</a><br>\n", append=T)
    fhtml<-file(paste(outDir, fname,sep="/"),open="w")
    cat (file=fhtml,"<html><head>")
    cat (file=fhtml,"<link href=\"../../../docs/shared/docstyle.css\" rel=\"stylesheet\" type=\"text/css\">", append=T)
    cat (file=fhtml,"<title>", Crop, expName, tName, "</title>\n", append=T)
    cat (file=fhtml,"</head><body><p class=\"Title1\">", Crop, expName, tName, "</p>\n",  append=T)
    cat (file=fhtml,"<table><tr>\n", append=T)
    r<-1
    for (image in images) {
      if (r%%4 == 0) {cat(file=fhtml,"</tr>\n<tr>", append=T); r<-1}
      cat(file=fhtml,"<td><img src=\"", image, "\"></td>\n", sep="", append=T) 
      r<-r+1
    }
    cat (file=fhtml,"</tr></table><hr>", append=T)
    cat (file=fhtml,"<p class=\"Title2\"><a href=\"validation.html\">Up</a></p>", append=T)
    sysinfo<-Sys.info()
    cat (file=fhtml,"<p class=\"Code\">", Sys.info()[7], "<br>",  Sys.info()[4], "<br>", date(), "</p>", append=T)
    cat (file=fhtml,"</body></html>\n", append=T)
    close(fhtml)
  } 
} 
cat(file=mhtml, "</p></body></html>\n", append=T)
close(mhtml)
