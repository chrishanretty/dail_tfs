library(tidyverse)
library(legislatoR)
library(mgcv)
n_threads <- 20

here::i_am("R/006_model.R")

### Read in the speech information
dat <- readRDS(here::here("outputs", "daily_speaker.rds"))

aux <- get_core(legislature = "irl")
ids <- get_ids(legislature = "irl")
pol <- get_political(legislature = "irl")
dat <- left_join(dat,
                 ids |> dplyr::select(wikidataid, dpsi),
                 by = join_by(memberID == dpsi))
                 
dat <- left_join(dat,
                 aux,
                 by = join_by(wikidataid))

### 
earliest_start <- pol |>
    group_by(pageid) |>
    summarize(earliest_start = min(session_start, na.rm = TRUE))

dat <- left_join(dat,
                 earliest_start,
                 by = join_by(pageid))

dat <- dat |>
    mutate(experience = as.numeric(date - earliest_start) / 365.25)

### There's one entry with negative experience, which doesn't make sense
dat <- dat |>
    mutate(experience = ifelse(experience < 0, NA, experience))

### 
dat <- dat |>
    mutate(age = as.numeric(date - as.Date(birth)) / 365.25)

### Check on TDs who seem to have implausible ages
dat |>
    filter(age < 21) |>
    dplyr::select(name, wikititle, wikidataid, birth) |>
    distinct()

### Eugene Timmons' DOB is given as 1st Jan. 2000, when it should be NA
### Patrick O'Reilly (member ID of 20) is linked to 
### There's one entry which is negative
dat$birth[which(dat$wikidataid == "Q1373190")] <- NA

### Errors with Patrick O'Reilly, who is given a birth date of 1st
### April 1927, when it should be 6th April 1906. Possibly an old
### import?
dat$birth[which(dat$wikidataid == "Q7147417")] <- as.Date("1906-04-06")

### Recalculate age
dat <- dat |>
    mutate(age = as.numeric(date - as.Date(birth)) / 365.25)

### Are there any people speaking after their death?
dat |>
    filter(date > as.Date(death)) |>
    group_by(name, memberID, wikidataid, death) |>
    summarize(latest = max(date))

### There is a 1999 speech by John Timoney which is an error in the original data
errpos <- which(dat$memberID == 1073 & dat$date == as.Date("1999-12-16"))
dat <- dat[-errpos,]

dat <- dat |>
    mutate(pageid = factor(pageid))

dat <- dat |>
    mutate(wday = wday(date, label = TRUE),
           month = month(date, label = TRUE))

dat <- dat |>
    mutate(date.num = as.numeric(date - min(date, na.rm = TRUE)) / 365.25)

dat <- dat |>
    mutate(cohort = factor(as.character(earliest_start)))

govt_spells <- read.csv(here::here("data", "spells_in_govt.csv")) |>
    mutate(start_date = as.Date(start_date),
           end_date = as.Date(end_date)) 

dat <- left_join(dat,
                 govt_spells,
                 by = join_by(party_name == Party,
                              date >= start_date,
                              date < end_date))

dat <- dat |>
    mutate(in_govt = coalesce(in_govt, 0L))

dat <- dat |>
    mutate(party_name = factor(party_name))

dat <- dat |>
    mutate(wday = factor(wday, ordered = FALSE),
           month = factor(month, ordered = FALSE))

m <- bam(Future ~ s(age, bs = "cr", k = 30) + s(cohort, bs = "re") +
             s(date.num, bs = "cr", k = 20) + s(pageid, bs = "re") + 
             s(party_name, bs = "re") +
             in_govt + 
             wday + month,
         family = betar(),
         discrete = TRUE,
         nthreads = n_threads,
         data = dat)

saveRDS(m,
        file = here::here("working", "dail_model_all.rds"))

## pdf(file = "~/Desktop/dail.pdf")
## plot(m, select = 1)
## dev.off()
