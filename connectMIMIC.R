library(DBI)
library(RODBC)
library(odbc)
library(dplyr)
library(dbplyr)
library(stringi)
library(finalfit)
library(mice)

get.outlier.locs <- function(data.series){
  data.center <- mean(data.series, na.rm = TRUE)
  data.spread <- sd(data.series, na.rm = TRUE)
  return(which(abs(data.series-data.center) > 3*data.spread))
}

signin <- read.table("signin.txt")[[1]]

conTF <- dbCanConnect(RPostgres::Postgres(),
             dbname = "mimiciv",
             port = 5432,
             user = signin[1],
             password = signin[2])

con <- dbConnect(RPostgres::Postgres(),
                    dbname = "mimiciv",
                    port = 5432,
                    user = signin[1],
                    password = signin[2])

dbListObjects(con)

triage.table <- dbGetQuery(con, "SELECT * FROM mimiciv_ed.triage;")
triage.table$chiefcomplaint <- tolower(triage.table$chiefcomplaint)

head(triage.table$chiefcomplaint)
sum(is.na(triage.table$chiefcomplaint))
pain.list <- triage.table[stri_detect_regex(triage.table$chiefcomplaint, "pain"),'chiefcomplaint']

pain.list <- stri_replace_all_regex(pain.list, "_", "")

sort(unique(pain.list))


chest.pain.table <- triage.table[stri_detect_regex(triage.table$chiefcomplaint, "chest pain"),]
rm(triage.table)

patient.table <- dbGetQuery(con, "SELECT * FROM mimiciv_hosp.patients;")
combo <- left_join(chest.pain.table, patient.table)
rm(patient.table)
rm(chest.pain.table)

ed.stay.table <- dbGetQuery(con, "SELECT * FROM mimiciv_ed.edstays;")
combo2 <- left_join(combo, ed.stay.table, by = c("subject_id", "stay_id", "gender"))
rm(combo)
rm(ed.stay.table)

older.adults <- combo2[combo2$anchor_age >= 65, ]
rm(combo2)

older.adults <- older.adults[older.adults$acuity %in% c(2,3),]
older.adults <- older.adults[older.adults$disposition %in% c("HOME", "ADMITTED"),]

dem.table <- dbGetQuery(con, "SELECT * FROM mimiciv_hosp.admissions;")
combo3 <- left_join(older.adults, dem.table, by = c("subject_id", "hadm_id", "race"))
rm(older.adults)
rm(dem.table)

reduced.df <- combo3[, c("subject_id", "stay_id", "temperature", "heartrate",
                         "resprate", "o2sat", "sbp", "dbp", "acuity", 
                         "chiefcomplaint", "gender", "anchor_age",
                         "dod", "hadm_id", "intime", "outtime", "dischtime", "race", 
                         "disposition", "insurance", "marital_status"
                         )]
rm(combo3)

reduced.df$subject_id <- factor(reduced.df$subject_id)
reduced.df$stay_id <- factor(reduced.df$stay_id)
reduced.df$acuity <- factor(reduced.df$acuity)
reduced.df$hadm_id <- factor(reduced.df$hadm_id)
reduced.df$race <- factor(reduced.df$race)
reduced.df$disposition <- factor(reduced.df$disposition)
reduced.df$insurance <- factor(reduced.df$insurance)
reduced.df$marital_status <- factor(reduced.df$marital_status)
reduced.df$gender <- factor(reduced.df$gender)

# fix temps
celsius.temp.locs <- which(reduced.df$temperature < 50)
celsius.temps <- reduced.df$temperature[celsius.temp.locs]
fahrenheit.temps <- celsius.temps * 1.8 + 32
reduced.df[celsius.temp.locs,"temperature"] <- fahrenheit.temps

summary(reduced.df)

odd.temp.locs <- which(reduced.df$temperature < 90)
odd.temps <- reduced.df$temperature[odd.temp.locs] # hypothermia is <95

hist(reduced.df$o2sat)
o2.outlier.locs <- get.outlier.locs(reduced.df$o2sat)
reduced.df$o2sat[o2.outlier.locs] # normal is 95-100 and its a percentage
reduced.df$o2sat[reduced.df$o2sat > 100] <- NaN

hist(reduced.df$sbp)
reduced.df$sbp[reduced.df$sbp < 50 & !is.na(reduced.df$sbp)] <- NaN # normal is 90-120

hist(reduced.df$dbp)
reduced.df$dbp[get.outlier.locs(reduced.df$dbp)] # high is >110 and should be <sbp
outers <- get.outlier.locs(reduced.df$dbp)
reduced.df$dbp[outers] <- NaN

reduced.df$discharge_date <- reduced.df$dischtime

# transform dod to mortality 25, 30, 35 days
time.to.death <- difftime(reduced.df$dod, reduced.df$discharge_date, units = "days")
mortality25 <- ifelse(is.na(time.to.death), 0,
                      ifelse(time.to.death <= 25, 1, 0))
mortality30 <- ifelse(is.na(time.to.death), 0,
                    ifelse(time.to.death <= 30, 1, 0))
mortality35 <- ifelse(is.na(time.to.death), 0,
                      ifelse(time.to.death <= 35, 1, 0))

reduced.df$mortality25 <- mortality25
reduced.df$mortality30 <- mortality30
reduced.df$mortality35 <- mortality35

# intime outtime to get treatment time
treatment.time <- difftime(reduced.df$outtime, reduced.df$intime, units = "hours")
hist(as.numeric(treatment.time))
reduced.df$treatment_time <- as.numeric(treatment.time)

# clean race
table(reduced.df$race)
condensed.race <- case_match(reduced.df$race,
  "AMERICAN INDIAN/ALASKA NATIVE" ~ "american indian or alaska native",
  c("ASIAN", "ASIAN - ASIAN INDIAN", "ASIAN - CHINESE",
    "ASIAN - KOREAN", "ASIAN - SOUTH EAST ASIAN") ~ "asian",
  c("BLACK/AFRICAN", "BLACK/AFRICAN AMERICAN",
    "BLACK/CAPE VERDEAN", "BLACK/CARIBBEAN ISLAND") ~ "black",
  c("HISPANIC OR LATINO", "HISPANIC/LATINO - CENTRAL AMERICAN",
    "HISPANIC/LATINO - COLUMBIAN", "HISPANIC/LATINO - CUBAN",
    "HISPANIC/LATINO - DOMINICAN", "HISPANIC/LATINO - GUATEMALAN",
    "HISPANIC/LATINO - HONDURAN", "HISPANIC/LATINO - MEXICAN",
    "HISPANIC/LATINO - PUERTO RICAN", "HISPANIC/LATINO - SALVADORAN") ~ "hispanic or latino",
  c("WHITE", "WHITE - BRAZILIAN", "WHITE - EASTERN EUROPEAN",
    "WHITE - OTHER EUROPEAN", "WHITE - RUSSIAN") ~ "white",
  c("MULTIPLE RACE/ETHNICITY", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
    "OTHER", "PORTUGUESE", "SOUTH AMERICAN") ~ "other",
  .default = "unknown"
)
reduced.df$race <- factor(condensed.race)

levels(reduced.df$insurance) <- c(levels(reduced.df$insurance), 'Missing')
reduced.df$insurance[is.na(reduced.df$insurance)] <- "Missing"

levels(reduced.df$marital_status) <- c(levels(reduced.df$marital_status), 'Missing')
reduced.df$marital_status[is.na(reduced.df$marital_status)] <- "Missing"
reduced.df$mortality30 <- factor(reduced.df$mortality30)
# clean chief complaints
unique(reduced.df$chiefcomplaint)

tab.ff <- summary_factorlist(reduced.df,
    dependent = "disposition", # name of grouping / treatment variable
    explanatory = c("temperature", "heartrate", "resprate", "o2sat", "sbp",
                    "dbp", "acuity", "gender", "anchor_age", "race",
                    "insurance", "marital_status", "mortality30", "treatment_time"),
    total_col = TRUE, # add column with statistics for the whole sample
    add_row_total = TRUE, # add column with number of valid cases
    include_row_missing_col = FALSE,
    na_include = TRUE # make variables' missing data explicit
  )

smaller.df <- reduced.df[, c("disposition", "temperature", "heartrate", "resprate", "o2sat", "sbp",
                             "dbp", "acuity", "gender", "anchor_age", "race",
                             "insurance", "marital_status", "mortality30", "treatment_time")]

# drop extraneous variables and repeat missingness table
# missingness as indicator or impute it
# 3 or less impute, ow drop, find citation
# group table 1 by proxy, dem, outcome N= mean(sd) etc
md.pattern(smaller.df, rotate.names = TRUE)

missings <- apply(smaller.df, 1, function(row){sum(is.na(row))})
hist(missings)
smaller.df <- smaller.df[missings <= 3, ]

str(smaller.df)
init = mice(smaller.df, maxit=0) 
meth = init$method
predM = init$predictorMatrix
set.seed(127)
imputed = mice(smaller.df, method=meth, predictorMatrix=predM, m=5)
imp.df <- complete(imputed)
md.pattern(imp.df, rotate.names = TRUE)
tab.ff2 <- summary_factorlist(imp.df,
                             dependent = "disposition", # name of grouping / treatment variable
                             explanatory = c("temperature", "heartrate", "resprate", "o2sat", "sbp",
                                             "dbp", "acuity", "gender", "anchor_age", "race",
                                             "insurance", "marital_status", "mortality30", "treatment_time"),
                             total_col = TRUE, # add column with statistics for the whole sample
                             add_row_total = TRUE, # add column with number of valid cases
                             include_row_missing_col = FALSE,
                             na_include = TRUE # make variables' missing data explicit
)
