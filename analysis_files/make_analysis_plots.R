library(bibtex)
library(ggplot2)

entry.list <- vector(mode = "list", length = 0)
for (i in seq(LETTERS)){
  my.file <- paste0("../Cages/", LETTERS[i], ".bib")
  if(file.exists(my.file)){
    # read .bib file and add entries to list
    entry.list <- c(entry.list, read.bib(my.file))
  }
}

year    <- as.numeric(lapply(entry.list, function(x){x$year}))
journal <- as.character(lapply(entry.list, function(x){x$journal}))
journal <- gsub(pattern = "NULL", replacement = "N/A", x = journal)
journal <- gsub(pattern = "[{}]", replacement = "", x = journal)
journal <- gsub(pattern = "\\&", replacement = "and", x = journal, 
                fixed = TRUE)
journal <- gsub(pattern = "{IEEE}", replacement = "IEEE", x = journal, 
                fixed = TRUE)

df <- data.frame(year    = year,
                 journal = journal)
