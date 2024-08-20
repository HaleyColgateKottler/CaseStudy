library(DBI)
library(RODBC)
library(odbc)
library(dplyr)
library(dbplyr)
library(stringi)

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
con

dbListObjects(con)

triage.table <- dbGetQuery(con, "SELECT * FROM mimiciv_ed.triage;")
triage.table$chiefcomplaint <- tolower(triage.table$chiefcomplaint)
chest.pain.table <- triage.table[stri_detect_regex(triage.table$chiefcomplain, "chest pain"),]
rm(triage.table)

length(unique(chest.pain.table$subject_id))

patient.table <- dbGetQuery(con, "SELECT * FROM mimiciv_hosp.patients;")
ed.stay.table <- dbGetQuery(con, "SELECT * FROM mimiciv_ed.edstays;")

combo <- left_join(chest.pain.table, patient.table)
rm(patient.table)
combo2 <- left_join(combo, ed.stay.table, by = c("subject_id", "stay_id", "gender"))

rm(combo)
rm(chest.pain.table)
rm(ed.stay.table)

older.adults <- combo2[combo2$anchor_age >= 65, ]
rm(combo2)
summary(older.adults)

older.adults <- older.adults[older.adults$acuity %in% c(2,3),]
older.adults <- older.adults[older.adults$disposition %in% c("HOME", "ADMITTED"),]

dem.table <- dbGetQuery(con, "SELECT * FROM mimiciv_hosp.admissions;")
combo3 <- left_join(older.adults, dem.table, by = c("subject_id", "hadm_id", "race"))
rm(older.adults)
rm(dem.table)

reduced.df <- combo3[, c("subject_id", "stay_id", "temperature", "heartrate",
                         "resprate", "o2sat", "sbp", "dbp", "acuity", 
                         "chiefcomplaint", "gender", "anchor_age",
                         "dod", "hadm_id", "intime", "outtime", "race", 
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

summary(reduced.df)


# fix temps
odd.temp.locs <- which(reduced.df$temperature < 80)
odd.temps <- reduced.df$temperature[odd.temp.locs]
reversed.temp <- reduced.df[odd.temp.locs[1],]
reduced.df[odd.temp.locs[1],"temperature"] <- 96.7

celsius.temp.locs <- odd.temp.locs[2:13]
celsius.temps <- odd.temps[2:13]
fahrenheit.temps <- celsius.temps * 1.8 + 32
reduced.df[celsius.temp.locs,"temperature"] <- fahrenheit.temps
summary(reduced.df$temperature)

hist(reduced.df$o2sat)
odd.o2.locs <- which(reduced.df$o2sat > 100)
odd.o2 <- reduced.df$o2sat[odd.o2.locs]
reduced.df$o2sat[odd.o2.locs] <- odd.o2*.1

hist(reduced.df$sbp)
