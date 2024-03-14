library(data.table)
library(tidyverse)
library(reclin2)
library(WikidataQueryServiceR)

here::i_am("R/001_link_tds_to_wikidata.R")

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

### Join with Dáil dates
ddates <- read.csv(here::here("data", "dail_sessions.csv")) |>
    mutate(start_date = as.Date(start_date),
           end_date = as.Date(end_date)) |>
    dplyr::select(start_date, end_date,
                  dail_label = label)

debates <- left_join(debates,
                     ddates,
                     by = join_by(date >= start_date,
                                  date <= end_date))

### Get the unique list of TDs for Dáil session
tds <- debates |>
    group_by(memberID, partyID, member_name, party_name, dail_label,
             start_date, end_date) |>
    summarize(earliest = min(date, na.rm = TRUE),
              latest = max(date, na.rm = TRUE),
              constys = paste(unique(const_name), collapse = "; "),
              .groups = "drop")


### Restrict it to 9th Dáil or later
rm(debates)
tds <- tds |>
    filter(!is.na(dail_label))


### For each unique Dáil session, get the members who had a start date greater than or equal to the session start date, but where the start date was before the end date

qry_base <- '
SELECT DISTINCT ?person ?personLabel ?position ?positionLabel ?dob ?sexLabel ?start WHERE {
  bind(("{{start_date}}T00:00:00Z"^^xsd:dateTime) as ?termstart) .
  bind(("{{end_date}}T00:00:00Z"^^xsd:dateTime) as ?termstop) . 
  ?person wdt:P31 wd:Q5 . ## not fictional person
  ?person p:P39 ?ps . ## person with a position statement
  ?ps ps:P39 ?position .
 OPTIONAL { ?person wdt:P569 ?dob } .
 OPTIONAL { ?person wdt:P21 ?sex . } .
 { ?position wdt:P279* wd:Q654291 } . ## has property of being a Teachta Dála
 { ?ps pq:P580 ?start . }
  filter(?start >= ?termstart) .
  filter(?start <= ?termstop)
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
}
'

holder <- list()

for (i in unique(tds$dail_label)) {

    start_date <- tds |>
        filter(dail_label == i) |>
        pull(start_date) |>
        unique()
    
    end_date <- tds |>
        filter(dail_label == i) |>
        pull(end_date) |>
        unique()

### Get the list of MPs
    qry <- glue::glue(qry_base,
                      .open = "{{",
                      .close = "}}")

    res <- query_wikidata(qry)
    local_tds <- tds |>
        filter(dail_label == i)

### Amend the member_name to remove titles and parentheticals
    tidy_name <- function(x) {
        x <- sub("\\(.*?\\)", "", x)
        x <- sub("(Mr. |Ms. |Mrs. |Dr. |Capt. |Prof. )",
                 "",
                 x)
        x
    }
    local_tds <- local_tds |>
        mutate(personLabel = tidy_name(member_name))
    
### Now try record linkage
    pp <- reclin2::pair(res,
                        local_tds)

    ### Using a single variable, name, comparing them using a function
    ### which is good for strings
    pp <- compare_pairs(pp,
                        on = c("personLabel"),
                        default_comparator = cmp_jarowinkler(0.9))

    ### These two steps create a scoring/`weights` variable
    m <- problink_em(~personLabel, pp)
    pp <- predict(m, pairs = pp, add = TRUE)

    ### Exact one-to-one matching, as specified by the n and m arguments
    pp <- select_n_to_m(pp, variable = "ntom", score = "weights",
                        threshold = 0,
                        n = 1, m = 1)

    ### Get the links
    linked_data_set <- link(pp, selection = "ntom")
    ### Coerce one element to character cos otherwise it'll cause problems
    holder[[i]] <- linked_data_set |>
        mutate(dob = as.character(dob))
    Sys.sleep(1)
}

### We've now matched within session
holder <- bind_rows(holder)
### Let's pull out the unique matches across people
holder <- holder |>
    distinct(person, personLabel.x, memberID, member_name)

### Write this out for checking
write.csv(holder,
          file = here::here("working", "id_2_wikidata_for_checking.csv"),
          row.names = FALSE)

