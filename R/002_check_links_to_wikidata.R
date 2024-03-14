library(data.table)
library(tidyverse)
library(reclin2)
library(WikidataQueryServiceR)
library(stringdist)

here::i_am("R/002_check_links_to_wikidata.R")

### Read in the debates
debates <- fread(here::here("data", "Dail_debates_1919-2013.tab"),
                 sep = "\t", quote = "", header = TRUE,
                 showProgress = TRUE,
                 data.table = FALSE,
                 verbose = FALSE)

### Drop the content
debates <- debates |>
    dplyr::select(-speech)

debates <- debates |>
    mutate(date = as.Date(date))

### Join with the lookup
lu <- read.csv(here::here("working",
                          "id_2_wikidata_checked.csv")) |>
    mutate(person = ifelse(person_corrected != "",
                           person_corrected,
                           person)) |>
    dplyr::select(-person_corrected, -check) |>
    dplyr::select(memberID, person)

debates <- left_join(debates,
                     lu,
                     by = join_by(memberID))

### Just post-1937 stuff
debates <- debates |>
    filter(date >= as.Date("1937-07-01"))

### Now let's do a query for dobs and vitals
qry <- '
SELECT DISTINCT ?person ?personLabel ?dob ?sexLabel WHERE {
  ?person wdt:P31 wd:Q5 . ## not fictional person
  ?person p:P39 ?ps . ## person with a position statement
  ?ps ps:P39 ?position .
 { ?position wdt:P279* wd:Q654291 } . ## has property of being a Teachta Dála
 OPTIONAL { ?person wdt:P569 ?dob } .
 OPTIONAL { ?person wdt:P21 ?sex . } .
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
}
'


res <- query_wikidata(qry)


### There's one date which is messed up
res$dob[which(res$personLabel == "Eugene Timmons")] <- NA_character_

res <- res |>
    mutate(dob = as.Date(substr(dob, 0, 10)))



res <- res |>
    group_by(person, personLabel, sexLabel) |>
    summarize(dob = min(as.Date(dob), na.rmn = TRUE),
              .groups = "drop"
              )
    
###
debates2 <- left_join(debates,
                      res,
                      by = join_by(person))

### What are the ages like?
debates2 <- debates2 |>
    mutate(age = as.numeric(date - dob) / 365.25)

### There's one error, where John Joseph Timoney (Clann na Poblachta)
### is recorded as speaking in response to an address of Mary
### McAleese, despite having died thirty years earlier


