
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

text_fingerprint <- function(text, ...) {
  multi_associations(text, ...) %>%
    filter(str_length(words) > 2 & str_length(context_words) > 2 & !grepl("_", words) & !grepl("_", context_words)) %>%
    { context_fingerprint(associations = .) } %>%
    filter(!is.na(context_characteristic) & p_total > 0.000001) %>%
    arrange(desc(context_characteristic))
}