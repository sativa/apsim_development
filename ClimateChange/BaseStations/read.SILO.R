# read an apsim format met file
read.met <- function(conn) {
  result<- list()
  inHeader <- T
  lines<- readLines(conn, 50, warn=F)
  lc = 2
  while (inHeader) {
     line <- sub('^!.*', '', lines[lc])         ;# zap comments
     line <- sub('[[:space:]]+$', '', line)     ;# and trailing spaces again
     if (line != "") {
        nv <- unlist(strsplit(line, "=", fixed=T))
        if (length(nv) == 2) {
           name<- sub('[[:space:]]+$', '', nv[1])
           value<- nv[2]  ;# should zap units sometime
           result[[name]] <- value
        } else {
           # must be the name line
           mynames<-unlist(strsplit(lines[lc], " +"))
           myunits<-unlist(strsplit(lines[lc+1], " +"))
           inHeader<-F
        }
     }
     lc<-lc+1
  }
  result <- c(result,read.table(conn,header=F,skip=lc,col.names=mynames))
}

# read an SILO native format met file
read.silo <- function(conn) {
  header<- readLines(conn, 2, warn=F)
  result <- read.table(conn,header=F,skip=2,
                       col.names=c("station","year","month","day", 
                                   "maxt", "maxt_s", "mint", "mint_s", "rain", "rain_s",
                                   "evap", "evap_s", "radn", "radn_s", "VP", "VP_s"))
}

# Convert one of our met structures to a msc style dataframe
met2msc <- function(met) {
  result<-data.frame(date = as.Date(paste(met$year,met$day),"%Y %j"),
                t_max = met$maxt,
                t_min = met$mint,
                precip = met$rain,
                rain = met$rain,
                solar = met$radn,
                evap = met$pan,
                vp = met$vp)
  attr(result$t_max, "units") <- "°C"
  attr(result$t_max, "long.name") <- "daily maximum temperature"
  attr(result$t_min, "units") <- "°C"
  attr(result$t_min, "long.name") <- "daily minimum temperature"
  attr(result$precip, "units") <- "mm"
  attr(result$precip, "long.name") <- "rainfall"
  attr(result$solar, "units") <- "mJ/m2"
  attr(result$solar, "long.name") <- "Solar radiation"
  attr(result$rain, "units") <- "mm"
  attr(result$rain, "long.name") <- "rainfall"

  return(result)
}

write.apsim <- function (file, df) {
  scalars <- vector(); vectors <- vector(); 
  for (name in names(df)) {
     if (length(df[[name]]) == 1) {
        scalars <-c(scalars, name)
     } else {
        vectors <-c(vectors, name)
     }
  }

  cat("[weather.met.weather]\n", file=file,append=F)
  for (name in scalars) {
    cat(name, " = ", df[[name]], "\n", file=file, append=T)
  }
  
  cat(paste(vectors,collapse=" "), "\n", file=file, append=T)
  cat(paste(rep("()", length(vectors)),collapse=" "), "\n", file=file, append=T)

  z<-data.frame(year=df$year)
  for (name in vectors) {
     z[[name]]<-df[[name]]
  }
  write.table(round(z,1), file=file, append = T, quote = F, sep = " ",row.names = F,col.names = F)
###  for (i in 1:length(df[[vectors[1]]])) {
###    for (name in vectors) {
###       cat(round(df[[name]][i],1), " ", file=file, append=T) 
###    }
###    cat("\n",file=file, append=T) 
###  }
}

# Strip the infilled data from a met file
# Implicit assumption  about order of variables here. Be careful.
strip.infilling <- function (data) {
   vars <-  c("radn","maxt","mint","rain","pan","vp")
   codes <- strsplit(as.character(data$code), NULL)
   for (row in 1:length(codes)) {
      code<- unlist(codes[row])
      for (ivar in 1:length(vars)) {
         if (code[ivar] != "0") {
            data[[vars[ivar]]][row] <- NA
         }
      }
   }
   return(data)
}
