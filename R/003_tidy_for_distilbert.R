library(data.table)
library(tidyverse)
library(tidytext)

here::i_am("R/003_tidy_for_distilbert.R")

### Read in the debates
debates <- fread(here::here("data", "Dail_debates_1919-2013.tab"),
                 sep = "\t", quote = "", header = TRUE,
                 showProgress = TRUE,
                 data.table = FALSE,
                 verbose = FALSE)

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

### Restrict it to 9th Dáil or later
debates <- debates |>
    filter(!is.na(dail_label))

### Exclude written answers
nrow(debates)
debates <- debates |>
    filter(!grepl("Written Answers", title))
nrow(debates)

### Remove column numbers in square brackets
debates$speech <- gsub("\\[0-9+\\]", "", debates$speech)

### Split over each day
### and tokenize (sentence)
dates <- unique(debates$date)
dates <- format(dates, "%Y-%m-%d")
for (d in dates) {
    tmp <- debates |>
        filter(date == as.Date(d))
    tmp <- tmp |>
        unnest_tokens(output = sents,
                      input = speech,
                      "sentences",
                      to_lower = FALSE,
                      drop = TRUE)

### Save this
    outfile <- paste0(d, ".rds")
    outfile <- here::here("working/sents",
                          outfile)
    saveRDS(tmp, file = outfile)

}
