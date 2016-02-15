###############################################################################
#
# generate.R
# Take in NAMCS data, and output a sample EHR dataset based on NAMCS data
#

###############################################################################
# Libraries
library(foreign)

###############################################################################
# Config

# How many patients do you want data for?
config.n <- 300000

# Should data be randomized?
# This slightly modifies continuous variables so that they're not 
# strictly identical. Selecting "FALSE" will lead to identical patients
config.randomize <- TRUE

# If you're randomizing data, how many SDs do you want to move the data?
config.random.sd <- 0.25

# Sample at state or national level?
# NAMCS provides weights to produce a dataset that represents state or
# national data, but not both.  If state, put "PATWTST"; if national, put "PATWT"
config.sample <- "PATWTST"

# Documentation Sensitivity?
# EHR data is often incomplete.  How much data do you want to keep?
# Select a percent from 0 (keep nothing) - 1 (keep everything)
# Note: this only removes documented diagnosis information
config.sens <- 0.8

# What year should the data be generated for?
config.year <- 2012

###############################################################################
# Load data
namcs.source <- read.dta("raw/namcs2012-stata.dta")

###############################################################################
# Generate weighted sample

wt.idx <- sample(1:nrow(namcs.source),
                 config.n,
                 replace=T,
                 prob=namcs.source[,config.sample])

namcs <- namcs.source[wt.idx,]

###############################################################################
# Create patient df
patient <- data.frame("patientid" = 1:nrow(namcs),
                      "age" = namcs$AGE,
                      "agecat" = as.character(namcs$AGER),
                      "sex" = as.character(namcs$SEX),
                      "race" = as.character(namcs$RACER),
                      "ethnicity" = as.character(namcs$ETHIM),
                      "state" = as.character(namcs$FIPSSTOFF)
                      )

###############################################################################
# Create diagnosis df

parse.dx <- function(ptidx, dx) {
  tmp <- gsub('-', '', dx)
  tmp2 <- ifelse(nchar(tmp) > 3,
                 paste(substr(tmp, 1, 3),
                       '.',
                       substr(tmp, 4, nchar(tmp)), sep=''),
                 tmp)
  return(data.frame("patientid" = ptidx,
                    "icd9" = tmp2))
}

diagnosis <- data.frame("patientid" = integer(),
                        "icd9" = character())
for(dv in c("DIAG1", "DIAG2", "DIAG3")) {
  idx <- which(namcs[,dv] != '-9')
  diagnosis <- rbind(diagnosis,
                     parse.dx(idx, as.character(namcs[idx,dv])))
}

# @TODO: Add in chronic conditions

###############################################################################
# Create prescription df

prescription <- data.frame("patientid" = integer(),
                        "drugid" = character())

for(dvn in 1:10) {
  dv <- paste("DRUGID", dvn, sep='')
  idx <- which(namcs[,dv] != '')
  prescription <- rbind(prescription,
                        data.frame("patientid" = idx,
                                   "drugid" = as.character(namcs[idx,dv])) )
}

###############################################################################
# Create encounter df
# This is kinda weird, since each observation _is_ an encounter
# so while, in theory, this is a 1:many relation w/ patient, in reality
# it's just a 1:1. However, the type of data is at an encounter level, not pt
# @TODO: Add a method for imputing multiple encounters over time

# Convert time with md from factor to number of minutes
tmp.timemd <- as.character(namcs$TIMEMD)
tmp.time <- rep(0, config.n) # this is our running total
tmp.timemin <- regexpr('minute', tmp.timemd) # location of minutes
tmp.timehr <- regexpr('hour', tmp.timemd) # location of hours
tmp.timeand <- regexpr('and', tmp.timemd) # location of and
tmp.time <- tmp.time + ifelse(tmp.timehr > 0,
    60*(as.numeric(substr(tmp.timemd, 0, tmp.timehr-2))),
    0) # Adding in hours, where applicable
tmp.timeminstart <- ifelse(tmp.timeand > 0, tmp.timeand+4, 0)
tmp.time <- ifelse(tmp.timemin > 0,
                   tmp.time+as.numeric(substr(tmp.timemd,
                                   tmp.timeminstart,
                                   tmp.timemin-1)),
                   tmp.time) # add in minutes, where applicable

rm(list = c("tmp.timemd", "tmp.timemin", "tmp.timehr",
     "tmp.timeand", "tmp.timeminstart"))

# Pick out a visit date based on month and day of week

# Need to handle leap years
if(((config.year %% 4 == 0) & (config.year %% 100 != 0)) | 
   (config.year %% 400 == 0)) {
  tmp.numdaysmnth <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  tmp.cumdays <- c(0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  tmp.ttldays <- 366
} else {
  tmp.numdaysmnth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  tmp.cumdays <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)  
  tmp.ttldays <- 365
}
# What DOW did the year start on?
tmp.yrstart <- as.POSIXlt(as.Date(
  paste(config.year, '01', '01', sep='-')))$wday+1 # 1 = Sunday
# Vector of months for each encounter
tmp.mnth <- as.numeric(namcs$VMONTH)
tmp.dow <- as.numeric(namcs$VDAYR)
tmp.days <- 1:tmp.ttldays
tmp.days <- tmp.yrstart + ((tmp.days-1) %% 7)
tmp.days <- ifelse(tmp.days > 7, tmp.days-7, tmp.days)

# Run through all data, and pick out a DOY for each encounter
tmp.doy <- apply(data.frame('dow' = tmp.dow, 'mnth' = tmp.mnth), 1, function(d){
  tmp.dows <- which(tmp.days == d[1])
  tmp.dows <- tmp.dows[which(tmp.dows >= tmp.cumdays[d[2]] &
                               tmp.dows <= (tmp.cumdays[d[2]] +
                                              tmp.numdaysmnth[d[2]]) )]
  return(sample(tmp.dows,1))
})
tmp.doy <- as.Date(tmp.doy-1, origin=paste(config.year, "01", "01", sep="-"))

# build encounter df
encounter <- data.frame('encounterid' = 1:nrow(namcs),
                        'patientid' = 1:nrow(namcs),
                        'payertype' = as.character(namcs$PAYTYPER),
                        'reasonforvisit' = as.character(namcs$RFV1),
                        'timewithprovider' = tmp.time,
                        'providerspecialty' =  as.character(namcs$SPECR_17),
                        'dos' = tmp.doy)

###############################################################################
# Create encounter.measurement df

tmp.vars <- c("HTIN", "WTLB", "BMI", "TEMPF", "BPSYS", "BPDIAS", "USETOBAC")
tmp.loincs <- c("8302-2", "29463-7", "39156-5", "8310-5",
                "8480-6", "8462-4", "11367-0")

# Change TEMPF to numeric
namcs$TEMPF <- as.character(namcs$TEMPF)
namcs$TEMPF[which(namcs$TEMPF == 'Blank')] <- '-9'
namcs$TEMPF <- as.numeric(namcs$TEMPF)

# Change USETOBAC to numeric/SNOMED
namcs$USETOBAC <- as.character(namcs$USETOBAC)
namcs$USETOBAC[which(namcs$USETOBAC == 'Blank')] <- '-9'
namcs$USETOBAC[which(namcs$USETOBAC == 'Unknown')] <- '160614008'
namcs$USETOBAC[which(namcs$USETOBAC == 'Current')] <- '110483000'
namcs$USETOBAC[which(namcs$USETOBAC == 'Not current')] <- '711563001'
namcs$USETOBAC <- as.numeric(namcs$USETOBAC)
# @TODO: 711563001 is not ideal, as it's no "known" use, when all we know
# is that they're not a current user.

encountermeasure <- data.frame('encounterid' = integer(),
                               'measurement' = character(),
                               'measurementvalue' = integer())

for(i in 1:length(tmp.vars)) {
  idx <- which(namcs[,tmp.vars[i]] != '-9')
  encountermeasure <- rbind(encountermeasure,
      data.frame( "encounterid" = idx,
                  "measurement" = tmp.loincs[i],
                  "measurementvalue" = namcs[idx,tmp.vars[i]]))
}

###############################################################################
# Create labresults df
labresults <- data.frame("patientid" = integer(),
                         "loinc_num" = character(),
                         "date_result" = as.Date(character()),
                         "obs_quan" = integer())

tmp.vars <- c("CHOLRES", "HDLRES", "LDLRES", "TGSRES", "A1CRES", "FBGRES")
tmp.day <- c("DAYDCHOL", "DAYDHDL", "DAYDLDL", "DAYDTGS", "DAYDA1C", "DAYDFBG")
tmp.loinc <- c("2093-3", "2085-9", "2089-1", "3043-7", "41995-2", "1557-8")

for(i in 1:length(tmp.vars)) {
  idx <- which(!(namcs[,tmp.vars[i]] %in% c("-9", "-7")) )
  labresults <- rbind(labresults,
      data.frame( "patientid" = idx,
                  "loinc_num" = tmp.loinc[i],
                  "date_result" = ifelse(namcs[idx,tmp.day[i]] < -365,
                                    as.Date(encounter$dos[idx]),
                                    as.Date(encounter$dos[idx]+
                                      namcs[idx,tmp.day[i]])),
                  "obs_quan" = as.numeric(namcs[idx,tmp.vars[i]])))
}
labresults$date_result <- as.Date(labresults$date_result, origin="1970-01-01")

###############################################################################
# Randomize data

if(config.randomize) {
  # Encounter measurements!
  measure.sd <- aggregate(encountermeasure$measurementvalue,
                    by=list("measurement" = encountermeasure$measurement),
                    sd)
  for(i in 1:nrow(measure.sd)) {
    if(measure.sd$measurement[i] != "11367-0") {
      idx <- which(encountermeasure$measurement == measure.sd$measurement[i])
      encountermeasure$measurementvalue[idx] <- 
        encountermeasure$measurementvalue[idx] +
        rnorm(length(idx),
              mean = 0,
              sd=(config.random.sd*measure.sd$x[i]))
    }
  }
  
  # Lab results!
  measure.sd <- aggregate(labresults$obs_quan,
                          by=list("loinc_num" = labresults$loinc_num),
                          sd)
  for(i in 1:nrow(measure.sd)) {
      idx <- which(labresults$loinc_num == measure.sd$loinc_num[i])
      labresults$obs_quan[idx] <- 
        labresults$obs_quan[idx] +
        rnorm(length(idx),
              mean = 0,
              sd=(config.random.sd*measure.sd$x[i]))
  }
}

###############################################################################
# Remove data
if(config.sens < 1) {
  idx.keep <- rbinom(nrow(diagnosis), 1, prob=config.sens)
  diagnosis <- diagnosis[which(idx.keep == 1),]
}

###############################################################################
# Add keys, as needed
diagnosis$diagnosisid <- 1:nrow(diagnosis)
diagnosis <- diagnosis[,c("diagnosisid", "patientid", "icd9")]

prescription$prescriptionid <- 1:nrow(prescription)
prescription <- prescription[,c("prescriptionid", "patientid", "drugid")]

encountermeasure$measureid <- 1:nrow(encountermeasure)
encountermeasure <- encountermeasure[,c("measureid", "encounterid",
                                        "measurement", "measurementvalue")]

labresults$labresultid <- 1:nrow(labresults)
labresults <- labresults[,c("labresultid", "patientid", "loinc_num",
                            "date_result","obs_quan")]

###############################################################################
# Output result files

write.csv(patient, file="patient.csv", row.names = FALSE)
write.csv(diagnosis, file="diagnosis.csv", row.names = FALSE)
write.csv(prescription, file="prescription.csv", row.names = FALSE)
write.csv(encounter, file="encounter.csv", row.names = FALSE)
write.csv(encountermeasure, file="encountermeasure.csv", row.names = FALSE)
write.csv(labresults, file="labresult.csv", row.names = FALSE)
