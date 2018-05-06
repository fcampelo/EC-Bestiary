# Load consolidated data and alias list
all.papers    <- readRDS("./2018-05-02_journal_paper_titles/01_consolidated_data.rds")
alias.list    <- read.csv("metaphor_aliases.csv", header = TRUE, sep = ";", 
                          stringsAsFactors = FALSE)

# Get matches for each entry (based on the alias list)
title.matches <- vector(mode = "list", length = nrow(alias.list))
for (i in seq(title.matches)){
  cat("\nProcessing title matches: ", 
      sprintf("%03d of %d", i, nrow(alias.list)))
  matches <- numeric()
  
  # Target aliases for the i-th metaphor
  targets.alias <- unlist(strsplit(gsub(", ", ",", alias.list$aliases[i]), 
                                   split = ","))
  if(length(targets.alias)){
    # for each alias
    for (j in seq(targets.alias)){
      # get paper titles that match the alias
      matches <- c(matches, grep(pattern = targets.alias[j], 
                                 x = all.papers$title, 
                                 ignore.case = TRUE))
    }
  }
  
  # # Target acronyms for the i-th metaphor
  # targets.acronym <- unlist(strsplit(gsub(", ", ",", alias.list$acronyms[i]), 
  #                                  split = ","))
  # if(length(targets.acronym)){
  #   # for each alias
  #   for (j in seq(targets.acronym)){
  #     # get paper titles that match the acronym
  #     #
  #     # Claus, what is the regexp to match the following query?
  #     # "targets.acronym[j], surrounded by non-alphanumeric characters"
  #     # 
  #     # my.pattern <- 
  #     # matches <- c(matches, grep(pattern = my.pattern, 
  #     #                            x = all.papers$title, 
  #     #                            ignore.case = TRUE))
  #   }
  # }
  
  
  # Remove possible double matches and collapse everything into a single string
  title.matches[[i]] <- unique(matches)
}


# For each entry, get distribution of citations per year
title.matches <- lapply(title.matches,
                        function(x, all.papers){
                          pub.years <- all.papers$year[x]
                          pubs.by.year  <- table(pub.years)
                          return(list(match.indx = x,
                                      pub.years = pub.years,
                                      pubs.by.year  = pubs.by.year))
                        }, 
                        all.papers = all.papers)
names(title.matches) <- alias.list$metaphor
