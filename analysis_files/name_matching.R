library(dplyr)

# Load consolidated data and alias list
df         <- readRDS("./data/00_bestiaryDF.rds")
all.papers <- readRDS("./data/00_consolidated_data.rds")
all.papers <- all.papers %>%
  mutate(title = tolower(title))

queries <- df %>%
  select(Title, Metaphor, SubMetaphor) %>%
  mutate(across(everything(), tolower))


# ==========
# Get matches for each entry (based on the alias list)
title.matches <- vector(mode = "list", length = nrow(alias.list))
for (i in seq(title.matches)){
  cat("\nProcessing title matches: ", 
      sprintf("%03d of %d", i, nrow(alias.list)))
  matches <- numeric()
  
  # Target aliases for the i-th metaphor
  targets.alias <- unlist(strsplit(gsub(", ", ",", alias.list$aliases[i]), 
                                   split = ","))
  # Include the original paper title as its own alias
  targets.alias <- tolower(c(alias.list$paper[i], targets.alias))
  
  # for each alias
  for (j in seq(targets.alias)){
    # get paper titles that match the alias
    matches <- c(matches, grep(pattern = targets.alias[j], 
                               x       = all.papers$title,
                               fixed   = TRUE))}

  # Target acronyms for the i-th metaphor
  # Using the Regexp: [^a-zA-Z0-9]acronym[^a-zA-Z0-9]
  # For some reason grep does not like "lowecase = TRUE" when using regexp,
  # so I manually tolower() the acronym.

  targets.acronym <- unlist(strsplit(gsub(", ", ",", 
                                          alias.list$acronyms[i]), 
                            split = ","))
  if(length(targets.acronym)){
    # for each alias
    for (j in seq(targets.acronym)){
      # get paper titles that match the acronym
      my.pattern <- paste0("[^a-zA-Z0-9]",
                           tolower(targets.acronym[j]),
                           "[^a-zA-Z0-9]")
      matches <- c(matches, grep(pattern = my.pattern, 
                                 x       = all.papers$title))
      }
  }
  
  # Remove possible double matches and collapse everything into a single string
  title.matches[[i]] <- unique(matches)
}


# For each entry, get distribution of citations per year
title.matches <- lapply(title.matches,
                        function(x, all.papers){
                          pub.years <- sort(all.papers$year[x])
                          ord <- order(pub.years)
                          pubs.by.year  <- table(pub.years)
                          return(list(match.indx = x[ord],
                                      pub.years = pub.years[ord],
                                      pubs.by.year  = pubs.by.year[ord]))
                        }, 
                        all.papers = all.papers)
names(title.matches) <- alias.list$metaphor

saveRDS(title.matches, 
     "./data/03_title_matches.rds")
