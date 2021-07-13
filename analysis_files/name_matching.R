library(dplyr)

# Load consolidated data and alias list
df         <- readRDS("./data/00_bestiaryDF.rds")
all.papers <- readRDS("./data/00_consolidated_data.rds")
alias      <- read.csv("./metaphor_aliases.csv", header = TRUE, sep = ",")


targets <- tolower(gsub("\\ |[\\-]+|[0-9]+", "", all.papers$title))

queries.pos <- lapply(tolower(gsub("\\ |[\\-]+|[0-9]+", "", alias$search.keys)),
                      function(y)unique(strsplit(y, split = "\\|")[[1]]))
queries.neg <- lapply(tolower(gsub("\\ |[\\-]+|[0-9]+", "", alias$search.remove)),
                      function(y)unique(strsplit(y, split = "\\|")[[1]]))

keywords <- c("algorithm", "optim", "search", "heuristic", "metaheuristic")

res <- vector("list", length(queries.pos))
names(res) <- gsub("\\:NA$", "", paste(df$Metaphor, df$SubMetaphor, sep = ":"))

for (i in seq_along(res)){
  message("Processing metaphor ", i , "/", length(res), ": ", names(res)[i])
  search.terms <- apply(as.matrix(expand.grid(queries.pos[[i]], keywords)), 
                        MARGIN = 1, 
                        FUN = paste, collapse = "")
  idx1 <- grepl(pattern = gsub("\\|$", "", 
                               paste(search.terms, "|", 
                                     collapse = "", sep = "")), targets)
  
  if(length(queries.neg[[i]]) > 0){
    search.terms <- apply(as.matrix(expand.grid(queries.neg[[i]], keywords)), 
                          MARGIN = 1, 
                          FUN = paste, collapse = "")
    idx2 <- grepl(pattern = gsub("\\|$", "", 
                                 paste(search.terms, "|", 
                                       collapse = "", sep = "")), targets)
  } else {
    idx2 <- logical(length(idx1))
  }
  
  idx <- which(idx1 & !idx2)
  if (length(idx) > 0) {
    res[[i]]$papers <- all.papers[idx, ]
  } else {
    res[[i]]$papers<- all.papers[-seq_along(all.papers$doi), ]
  }
}




# 
# 
# 
# # For each entry, get distribution of citations per year
# title.matches <- lapply(title.matches,
#                         function(x, all.papers){
#                           pub.years <- sort(all.papers$year[x])
#                           ord <- order(pub.years)
#                           pubs.by.year  <- table(pub.years)
#                           return(list(match.indx = x[ord],
#                                       pub.years = pub.years[ord],
#                                       pubs.by.year  = pubs.by.year[ord]))
#                         }, 
#                         all.papers = all.papers)
# names(title.matches) <- alias.list$metaphor
# 
# saveRDS(title.matches, 
#         "./data/03_title_matches.rds")
