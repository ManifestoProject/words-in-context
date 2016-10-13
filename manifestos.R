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
mp_setapikey("../manifesto_apikey.txt")

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
               removeWords(stopwords("de")) %>%
               content() %>%
               tolower() %>%
               { gsub("\\s+", " ", .) } %>%
               paste(collapse = " ")) %>%
      unlist()
  }) %>%
  set_names(parties)-> raw_texts

raw_texts %>%
  mclapply(text_fingerprint, mc.cores = 8) -> party_fingerprints

bind_rows(party_fingerprints, .id = "party") -> party_fingerprints