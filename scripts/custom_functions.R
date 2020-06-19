# Functions for COPE project - cleaning and analysis 

numeric_descriptives <- function(variable_list, dataframe){
  
  dataselect <- dataframe %>%
    select(variable_list) 
  
  means <- dataselect %>%
    map_df(mean, na.rm = T)
  sds <- dataselect %>%
    map_df(sd, na.rm = T)
  
  table <- data.frame(rbind(means,sds)) %>%
    mutate(value = c("Mean", "SD")) %>%
    select(value, everything ())
  return(table)
}


# perfecting age calculation from stack overflow
#https://stackoverflow.com/questions/31126726/efficient-and-accurate-age-calculation-in-years-months-or-weeks-in-r-given-b
library(data.table)
get_age <- function(birthdays, ref_dates){
  x <- data.table(bday <- unclass(birthdays),
                  #rem: how many days has it been since the lapse of the
                  #  most recent quadrennium since your birth?
                  rem = ((ref <- unclass(ref_dates)) - bday) %% 1461)
  #cycle_type: which of the four years following your birthday
  #  was the one that had 366 days? 
  x[ , cycle_type := 
       foverlaps(data.table(start = bdr <- bday %% 1461L, end = bdr),
                 #these intervals were calculated by hand;
                 #  e.g., 59 is Feb. 28, 1970. I made the judgment
                 #  call to say that those born on Feb. 29 don't
                 #  have their "birthday" until the following March 1st.
                 data.table(start = c(0L, 59L, 424L, 790L, 1155L), 
                            end = c(58L, 423L, 789L, 1154L, 1460L), 
                            val = c(3L, 2L, 1L, 4L, 3L),
                            key = "start,end"))$val]
  I4 <- diag(4L)[ , -4L] #for conciseness below
  #The `by` approach might seem a little abstruse for those
  #  not familiar with `data.table`; see the edit history
  #  for a more palatable version (which is also slightly slower)
  x[ , extra := 
       foverlaps(data.table(start = rem, end = rem),
                 data.table(start = st <- cumsum(c(0L, rep(365L, 3L) +
                                                     I4[.BY[[1L]],])),
                            end = c(st[-1L] - 1L, 1461L),
                            int_yrs = 0:3, key = "start,end")
       )[ , int_yrs + (i.start - start) / (end + 1L - start)], by = cycle_type]
  #grand finale -- 4 years for every quadrennium, plus the fraction:
  4L * ((ref - bday) %/% 1461L) + x$extra
}
  # for word clouds, from Anna Niedbala
wordMatrix = function(text,stopwords,n) {
  
  text=lemmatize_strings(textProcess(text,stopwords))
  text=gsub("\\bnumb\\b"," number ",text)
  text=gsub("\\bnumberer"," number ",text)
  text=gsub("\\be mail\\b"," email ",text)
  text=gsub("\\bre sell\\b"," resell ",text)
  text=gsub("\\bo k\\b"," okay ",text)
  text=gsub("\\badvert[[:alpha:]]+\\b"," ads ",text)
  text=gsub("\\blist\\b"," ad ",text)
  text=gsub("\\bdatum\\b"," date ",text)
  text=lemmatize_strings(textProcess(text,""))
  corpus=VCorpus(VectorSource(text))    
  BigramTokenizer <-
    function(x) unlist(lapply(lapply(ngrams(words(x$content), n),sort), paste, collapse = " "), use.names = FALSE)
  
  dtm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer,removePunctuation = TRUE))
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  print(head(d, 10))
  
  set.seed(1234)
  
  return(d)
}

# https://gist.github.com/noamross/a549ee50e8a4fd68b8b1
source_rmd = function(file, skip_plots = TRUE) {
  temp = tempfile(fileext=".R")
  knitr::purl(file, output=temp)
  
  if(skip_plots) {
    old_dev = getOption('device')
    options(device = function(...) {
      .Call("R_GD_nullDevice", PACKAGE = "grDevices")
    })
  }
  source(temp)
  if(skip_plots) {
    options(device = old_dev)
  }
}
