library(rjson)
library(dplyr)
library(rcrossref)

# OpenCitations' base url to retrieve citing papers

# Load bestiary df
df <- readRDS("data/00_bestiaryDF.rds")

mapply(
  FUN = function(toIN, toRM, df){
    if(!is.na(toIN)){
      idx <- grep(tolower(toIN), tolower(df$Title))
      if(!(is.na(toRM)) & toRM != ""){
        idxRM <- grep(tolower(toRM), tolower(df$Title[idx]))
        if(length(idxRM) > 0) idx <- idx[-idxRM]
      }
      paste(ifelse(is.na(df$SubMetaphor[idx]), df$Metaphor[idx], df$SubMetaphor[idx]),
            collapse = "|")
    } else {
      NA
    }
  }, 
  df$search.keys, 
  df$search.remove, MoreArgs = list(df=df))



# Example: Whale Algo
target <- df[326, ]

baseurl <- "https://opencitations.net/index/coci/api/v1/citations/"

myurl <- paste0(baseurl, target$DOI)

result <- rjson::fromJSON(file = myurl) %>%
  lapply(as.data.frame) %>%
  bind_rows() %>%
  rowwise() %>%
  select(DOI = citing, Cites = cited)

continue <- TRUE
depth    <- 1
while(continue & depth <= 2){
  cat("\nExploring level", depth)
  res <- rcrossref::cr_works(dois = result$DOI)$data %>%
    select(DOI = doi, Title = title, Where = container.title, 
           author = author, references = reference)
  res$IsMetaphorical <- do.call(rbind, 
                                sapply(df$search.keys, 
                                       function(x, titles) grepl(x, titles), 
                                       titles = res$Title, simplify = FALSE)) %>% 
    colSums(na.rm = TRUE) > 0
  results <- result %>%
    left_join(res, by = "DOI")
  depth <- depth + 1
}



sapply(result, function(x)x$citing)
