library(rjson)

df <- readRDS("./data/00_bestiaryDF.rds")
baseurl <- "https://opencitations.net/index/coci/api/v1/citations/"

citing <- vector("list", nrow(df))
for (i in seq_along(df$DOI)){
  name <- df$Metaphor[i]
  if(!is.na(df$SubMetaphor[i])) name <- paste0(name, "-", df$SubMetaphor[i])
  citing[[i]]$Metaphor      <- df$Metaphor[i]
  citing[[i]]$SubMetaphor   <- df$SubMetaphor[i]
  citing[[i]]$DOI           <- df$DOI[i]
  citing[[i]]$Title         <- df$Title[i]
  citing[[i]]$Year          <- df$Year[i]
  citing[[i]]$Citing.Papers <- NA
  
  if(!is.na(df$DOI[i])){
    query.url <- paste0(baseurl, df$DOI[i])
    result    <- rjson::fromJSON(file = query.url)
    citing[[i]]$Citing.Papers <- unlist(lapply(result, function(x) x[["citing"]]))
    message(sprintf("Checking metaphor %03d/%03d: %s -- %05d citations retrieved.", 
                    i, nrow(df), name, length(citing[[i]]$Citing.Papers)))

  } else {
    message(sprintf("No DOI for metaphor %03d/%03d: %s -- citations not retrieved.", 
                    i, nrow(df), name))
  }
}

saveRDS(citing, "./data/00_citing_papers.rds")