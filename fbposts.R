library(magrittr)
library(dplyr)
library(tidytext)
library(tokenizers)
library(purrr)
library(stringr)
library(parallel)
library(tidyr)
library(tm)

NCORES = 32

remove_words <- function(text, words =  readLines("../data/exclude_stems.txt")) {
    words %>%
      paste(collapse = ")|(") %>%
      paste("(", ., ")") %>%
      gsub("", text) %>%
      { gsub("\\s+", " ", .) }
}

## Reading in data

files <- list.files("../data/fb", full.names = TRUE) %>%
  { subset(., grepl(".csv", fixed = TRUE, .)) } 

files %>%
  lapply(read.csv, header = FALSE, stringsAsFactors = FALSE) %>%
  lapply(. %$% V4) %>%
  lapply(function(text) { gsub( "(http)|(www)|(facebook)|(dass)|(html)", "", text)}) %>%   ## TODO more cleaning of stopwords, short words, ...?
  mclapply(remove_words, mc.cores = 8) %>%
  set_names(gsub("-stemmed.csv", "", basename(files))) -> raw_texts

raw_texts$AfD %>% { grepl("partei", .)} %>% table()





## General analysis functions

associations <- function(text,
                         words = tokenize_ngrams(text, n = 1)[[1]],
                         n = 3) {
  
  if (length(words) <= 2*n) {
      data_frame(words = character(0),
                 context_words = character(0),
                 n_context = integer(0))
  } else {
      context <- tokenize_ngrams(text, n = 2*n + 1)[[1]]

      data_frame(words = words[(n+1):(length(words)-n)],
                 context = context) %>%
        by_row(..f = . %$%
                 tokenize_ngrams(context, n = 1) %>%
                 unlist() %>%
                 data_frame(context_words = .),
               .collate = "rows") %>%
        filter(words != context_words) %>%
      #  mutate(words = substr(words, 1, 5),
      #         context_words = substr(context_words, 1, 5)) %>%
        group_by(words, context_words) %>%
        summarise(n_context = n()) %>%
        ungroup()      
  }
  
}

multi_associations <- function(texts, ...) {
    
    texts %>%
      mclapply(associations, mc.cores = NCORES, ...) %>%
      bind_rows() %>%
      group_by(words, context_words) %>%
      summarise(n_context = sum(n_context)) %>%
      ungroup()
}

context_distribution <- function(target_word,
                                 text = "",
                                 context_size = 3,
                                 associations = associations(text, context_size)) {
  associations %>%
    filter(words == target_word) %>%
    mutate(p_context = n_context/sum(n_context))
}

context_fingerprint <- function(text = "",
                                context_size = 3,
                                associations = associations(text, context_size)) {
      
  associations %>%
    group_by(words) %>%
    mutate(p_context = n_context/sum(as.numeric(n_context)),
           n_total = n()) %>%
    ungroup() %>%
    mutate(p_total = n_total/sum(as.numeric(n_total)),
           context_characteristic = p_context/p_total)
    
}

text_fingerprint <- function(text) {
  multi_associations(text) %>%
    filter(str_length(words) > 2 & str_length(context_words) > 2 & !grepl("_", words) & !grepl("_", context_words)) %>%
    { context_fingerprint(associations = .) } %>%
    filter(!is.na(context_characteristic) & p_total > 0.000001) %>%
    arrange(desc(context_characteristic))
}

## Test stuff





# AfD_fingerprint %>%
#   select(words, context_words, context_characteristic) %>%
#   full_join(select(Greens_fingerprint, words, context_words, context_characteristic), by = c("words", "context_words")) %>%
#   mutate(diff = context_characteristic.x - context_characteristic.y) %>%
#   arrange(abs(diff)) %>%
#   head(n = 30)









## Analysis

raw_texts %>%
  mclapply(text_fingerprint, mc.cores = 8) -> party_fingerprints

bind_rows(party_fingerprints, .id = "party") -> party_fingerprints



party_fingerprints %>%
  mutate(context_characteristic = log(context_characteristic)) %>%
  select(words, context_words, context_characteristic, party) %>%
  spread(party, context_characteristic) %>%
  { filter(., complete.cases(.)) } %>%
  arrange(desc(pmax(AfD, CDU, DieGruenen, DieLinke, NPD, SPD) - pmin(AfD, CDU, DieGruenen, DieLinke, NPD, SPD))) %>%
  head(n = 40)

party_fingerprints %>%
  mutate(context_characteristic = log(context_characteristic)) %>%
  select(words, context_words, context_characteristic, party) %>%
  spread(party, context_characteristic) %>%
  mutate_each(funs(round(., digits = 4)*100), AfD, CDU, DieGruenen, SPD, DieLinke, NPD) %>%
  { filter(., complete.cases(.)) } %>%
  # arrange(desc(pmax(AfD, CDU, DieGruenen, SPD, DieLinke, NPD) - pmin(AfD, CDU, DieGruenen, SPD, DieLinke, NPD))) %>%
  arrange(desc(pmax(AfD, CDU, DieGruenen, SPD, DieLinke, NPD))) %>%
  head(n = 40)

party_fingerprints %>%
  select(words, context_words, p_context, party) %>%
  spread(party, p_context) %>%
  filter(words == "tat") %>%
  head(n = 40)

party_fingerprints %>%
  select(words, context_words, p_context, party) %>%
  filter(words == "tat") %>%
  group_by(party) %>%
  summarise(n = n()) %>%
  ungroup()

## Words with (dis-)similar context distribution across parties

party_fingerprints  %>%
  filter(!party %in% c("AfD")) %>%
  group_by(words) %>%
  summarise(n_parties = length(unique(party))) %>%
  ungroup() %$%
  table(n_parties)

# Kullback-Leibler Divergence

kld <- function(x,y) {
  if (all(is.na(x)) && all(is.na(y))) {
    return(NA)
  } else {
    sum(x*log2(x/y), na.rm = TRUE)      
  }
}

sqd <- function(x, y) {
    sqrt(sum((ifelse(is.na(x), 0.0, x) - ifelse(is.na(y), 0.0, y))^2))
}

smooth_kld <- function(x, y, lambda = 1) {
  kld( (x + lambda/length(x))/(1+lambda), (y + lambda/length(y))/(1+lambda) )
}


# Jensen-Shannon Divergence

js <- function(x,y) {
  m <- (x+y)/2
  0.5*(kld(x,m)+kld(y,m))
}

multi_js <- function(l, fun = kld, ...) {

  l <- subset(l, !unlist(lapply(l, . %>% is.na() %>% all())))
    
  m <- l %>%
    as.data.frame() %>%
    rowSums(na.rm = TRUE) %>%
    divide_by(length(l))
  l %>%
    lapply(fun, m, ...) %>%
    unlist() %>%
    sum() %>%
    divide_by(length(l))
}




party_fingerprints %>%
  mutate(context_characteristic = p_context) %>%
  select(words, context_words, p_context, party) %>%
  spread(party, p_context, fill = NA) %>%
  group_by(words) %>%
  summarise(js_AfD_CDU = js(AfD, CDU),
            js_AfD_DieGruenen = js(AfD, DieGruenen),
            js_AfD_DieLinke = js(AfD, DieLinke),
            js_AfD_NPD = js(AfD, NPD),
            js_AfD_SPD = js(AfD, SPD),
            js_CDU_DieGruenen = js(CDU, DieGruenen),
            js_CDU_DieLinke = js(CDU, DieLinke),
            js_CDU_NPD = js(CDU, NPD),
            js_CDU_SPD = js(CDU, SPD),
            js_DieGruenen_DieLinke = js(DieGruenen, DieLinke),
            js_DieGruenen_NPD = js(DieGruenen, NPD),
            js_DieGruenen_SPD = js(DieGruenen, SPD),
            js_DieLinke_NPD = js(DieLinke, NPD),
            js_DieLinke_SPD = js(DieLinke, SPD),
            js_NPD_SPD = js(NPD, SPD),
            js_all = multi_js(list(AfD, CDU, DieGruenen, DieLinke, NPD, SPD))) %>%
  ungroup() %>%
  { mutate(., js_sum = rowSums(select(., matches("js_.*_")), na.rm = TRUE)) } %>%
  arrange(desc(js_all)) -> party_differences



#party_differences %>%
#  filter(grepl("fluch", words)) %>%
#  arrange(desc(js_all)) %>%
#  mutate_each(funs(round(., digits = 3)), -words) %>%
#  select(words, js_all, everything())

party_fingerprints %>% filter(p_context > 0.01) %>% write.csv("party_fingerprints.csv")

party_differences %>%
  arrange(desc(js_sum)) %>%
  mutate_each(funs(round(., digits = 3)), -words) %>%
  select(words, js_sum, js_all, everything()) %>%
  write.csv("party_differences.csv")

party_differences %>%
  arrange(desc(js_sum)) %>%
  mutate_each(funs(round(., digits = 3)), -words) %>%
  select(words, js_sum, js_all, everything())

cor(party_differences$js_all, party_differences$js_sum)


