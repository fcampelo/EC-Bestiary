# Consolidate bestiary entries as a single data frame

# Load required packages
library(rcrossref)
library(bibtex)
library(dplyr)
library(data.table)

# ==========
# Process bestiary files and extract entries
entry.list <- vector(mode = "list", length = 0)
for (i in seq(LETTERS)){
  my.file <- paste0("../Cages/", LETTERS[i], ".bib")
  if(file.exists(my.file)){
    # read .bib file and add entries to list
    entry.list <- c(entry.list, bibtex::read.bib(my.file))
  }
}

for (i in seq_along(entry.list)){
  x        <- entry.list[[i]]
  names(x) <- tolower(names(x))
  tmp <- strsplit(names(entry.list)[i], ":")[[1]]
  metaphor <- gsub("\\.", " ", tmp[1])
  submetaphor <- ifelse(length(tmp) == 2,
                        gsub("^ ", "", gsub("\\.", " ", tmp[2])),
                        NA)

  if("journal" %in% names(x)) {
    where.pub <- x$journal
  } else if("booktitle" %in% names(x)) {
    where.pub <- x$booktitle
  } else where.pub <- NA
  
  tmp <- data.frame(Metaphor    = metaphor,
                    SubMetaphor = submetaphor,
                    Title       = as.character(x$title),
                    Year        = as.integer(x$year),
                    Author      = paste(as.character(x$author),
                                        collapse = "; "),
                    Where.pub   = as.character(where.pub),
                    Is.journal  = "journal" %in% names(x),
                    DOI         = ifelse("doi" %in% names(x),
                                         as.character(x$doi),
                                         NA))
  if (i == 1) df <- tmp else df <- rbind(df, tmp)
}

# Manual fixes of some names 
# (performed after a preliminary run and manual verification)
where.pub <- df$Where.pub
where.pub <- gsub("[{}]", "", where.pub)
where.pub <- gsub("\\&", "&", where.pub, fixed = TRUE)
where.pub <- gsub("^ | $", "", where.pub)
where.pub <- gsub("\\textquotesingle10", "", where.pub, fixed = TRUE)
where.pub <- gsub("  ", " ", where.pub)
where.pub <- gsub("J Circuits Systems", "Journal of Circuits, Systems and Computers", where.pub)
where.pub <- gsub("ORSA Journal on Computing", "INFORMS Journal on Computing", where.pub)

df$Where.pub <- where.pub
df$Citations <- cr_citation_count(df$DOI)$count
saveRDS(df, "data/00_bestiaryDF.rds")

dlist <- rcrossref::cr_works(dois = df$DOI)
saveRDS(dlist, "./data/00_bestiary_data_list.rds")


alias <- read.csv("./metaphor_aliases.csv", header = TRUE, sep = ";")
alias <- df %>%
  left_join(alias, by = c("Title" = "paper")) %>%
  select(Metaphor, SubMetaphor, Title, aliases, acronyms, NOTES)

write.csv(df, "./data/00_bestiaryDF.csv", row.names = FALSE)







