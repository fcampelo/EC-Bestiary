# Load consolidated data and alias list
all.papers    <- readRDS("./2018-05-02_journal_paper_titles/01_consolidated_data.rds")
alias.list    <- read.csv("metaphor_aliases.csv", header = TRUE, sep = ";", 
                          stringsAsFactors = FALSE)

# Get matches for each entry (based on the alias list)
title.matches <- character(nrow(alias.list))
for (i in seq(title.matches)){
  # Target patterns for the i-th metaphor
  targets <- unlist(strsplit(gsub(", ", ",", alias.list$aliases[i]), 
                             split = ","))
  matches <- numeric()
  if(length(targets)){
    # for each alias
    for (j in seq(targets)){
      # get paper titles that match the particular
      matches <- c(matches, grep(pattern = targets[j], 
                                 x = all.papers$title, 
                                 ignore.case = TRUE))
    }
  }
  # Remove possible double matches and collapse everything into a single string
  title.matches[i] <- paste0(unique(matches), 
                             collapse = ",") 
}