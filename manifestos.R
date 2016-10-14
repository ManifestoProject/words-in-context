library(magrittr)
library(dplyr)
library(tidytext)
library(tokenizers)
library(purrr)
library(stringr)
library(parallel)
library(tidyr)
library(tm)
library(manifestoR)
mp_setapikey("manifesto_apikey.txt")

NCORES = 32

source("associations.R")

## get cleaned texts in data table

mp_corpus(countryname == "Germany" & date > 200000 & party != 41952) %>%
  tm_map(function(doc) { meta(doc, "party") <- substr(meta(doc, "party"), 1, 4); doc }) -> corp

corp %>%
  content() %>%
  lapply(meta, "party") %>%
  unlist() %>%
  unique -> parties

parties %>%
  lapply(function(p) {
    corp %>%
      tm_filter(. %>% meta("party") %>% equals(p)) %>%
      content() %>%
      lapply(. %>%
               stemDocument(language = "german") %>%
               removePunctuation() %>%
               (content_transformer(tolower))() %>%
               removeWords(c(stopwords("de"), readLines("additional_stopwords.txt"))) %>%
               content() %>%
               { gsub("\\s+", " ", .) } %>%
               paste(collapse = " ")) %>%
      unlist()
  }) %>%
  set_names(parties)-> raw_texts

raw_texts %>%
  mclapply(text_fingerprint, mc.cores = 8) %>%
  bind_rows(.id = "party") -> party_fingerprints

data_frame(party = parties,
           partyname = c("Greens", "Left", "SocDem", "Liberals", "ChrDem", "AfD")) %>%
  right_join(party_fingerprints) %>%
  select(words, context_words, p_context, partyname) %>%
  spread(partyname, p_context) %>%
  arrange(desc(pmax(AfD, ChrDem, Greens, Left, Liberals, SocDem, na.rm = TRUE) -
                 pmin(AfD, ChrDem, Greens, Left, Liberals, SocDem, na.rm = TRUE))) -> fingerprints_wide

write.csv(fingerpints_wide,
          "party_fingerprints.csv", row.names = FALSE)

source("js.R")
fingerprints_wide %>%
  group_by(words) %>%
  summarise(js_all = multi_js(list(AfD, ChrDem, Greens, Left, Liberals, SocDem))) %>%
  ungroup() %>%
  arrange(desc(js_all)) -> party_differences
