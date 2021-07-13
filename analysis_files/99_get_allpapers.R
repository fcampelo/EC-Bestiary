# This takes a while to run.
# ==============================================================================

# Load required packages
library(rcrossref)
library(dplyr)
# ==========

journals <- (readRDS("./data/00_bestiaryDF.rds") %>%
               filter(Is.journal) %>%
               select(Where.pub))[, 1, drop = TRUE]


# Set years for scraping
years    <- 1990:2021
starts   <- "-01-01" #c("-01-01", "-05-01", "-09-01")
ends     <- "-12-31" ##c("-04-30", "-08-31", "-12-31")

# ==========
# Perform scraping

## Create directory
if(!dir.exists("./data")) dir.create("./data")

# Initialize data frame for storing empty queries
empty_queries <- data.frame(journal = character(),
                            year    = numeric())

# Iterate over journals vector
errors <- data.frame(i = numeric(), j = numeric(), k = numeric())
for (i in seq_along(journals)){
  saveRDS(errors, "./data/00_ERRlist.rds")
  
  # Iterate over years of interest
  for (j in seq_along(years)){
    
    cat("\n", i, ") ", journals[i], ":", years[j])
    if(file.exists(paste0("./data/journal", 
                          sprintf("%02d-%d.rds", 
                                  i, years[j])))) {
      cat(" --> file detected: skipping.")
      next
    }
    
    
    # Initialize data frame for journal i - year j
    journal_papers <- data.frame(journal   = character(),
                                 year      = numeric(),
                                 title     = character(),
                                 authors   = character(),
                                 doi       = character(),
                                 ref.count = numeric(),
                                 stringsAsFactors = FALSE)
    
    for (k in seq_along(starts)){
      Sys.sleep(.5 + runif(1))
      start.date <- paste0(years[j], starts[k])
      end.date   <- paste0(years[j], ends[k])
      
      
      # Retrieve the data
      cat(".")
      paper.data <- data.frame(data = numeric())
      paper.data <- try(cr_works(filter = c(from_pub_date   = start.date,
                                            until_pub_date  = end.date,
                                            container_title = journals[i]),
                                 limit = 1000),
                        silent = TRUE)
      
      # Isolate the relevant information
      if (class(paper.data) == "try-error") {
        npap <- FALSE
      } else if (length(paper.data) == 1 && 
                 names(paper.data) == "message" && 
                 is.null (paper.data$message)){
        npap <- FALSE
        errors <- rbind(errors,
                        data.frame(i = i, j = j, k = k))
      }
      else {
        npap <- nrow(paper.data$data)
      }
      
      cat(":")
      if(npap){
        # Set fields that are guaranteed to exist
        my.journal <- rep(journals[i], npap)
        my.year    <- rep(years[j], npap)
        
        # Get title, if available
        if("title" %in% names(paper.data$data)){
          my.title   <- paper.data$data$title
        } else {
          my.title <- rep(NA, npap)
        }
        
        # Get DOI, if available
        if("doi" %in% names(paper.data$data)){
          my.doi <- paper.data$data$doi
        } else {
          my.doi <- rep(NA, npap)
        }
        
        # Get reference count, if available
        if("reference.count" %in% names(paper.data$data)){
          my.nref <- as.numeric(paper.data$data$reference.count)
        } else {
          my.nref <- rep(NA, npap)
        }
        
        # Get authors, if available
        if("author" %in% names(paper.data$data)){
          my.authors <- paper.data$data$author
          my.authors <- unlist(lapply(my.authors, 
                                      function(x){
                                        if (!is.null(x)){
                                          if (!("given" %in% names(x))){
                                            x$given <- rep("_NA_", nrow(x))
                                          }
                                          if (!("family" %in% names(x))){
                                            x$family <- rep("_NA_", nrow(x))
                                          }
                                          paste(x$family, 
                                                x$given, 
                                                sep = ", ", 
                                                collapse = "; ")
                                        } else {
                                          return("_NA_, _NA_")
                                        }}))
          
        } else {
          my.authors <- rep(NA, npap)
        }
        
        # Update data frame with results
        journal_papers <- rbind(journal_papers,
                                data.frame(journal   = my.journal,
                                           year      = my.year,
                                           title     = my.title,
                                           authors   = my.authors,
                                           doi       = my.doi,
                                           ref.count = my.nref,
                                           stringsAsFactors = FALSE))
        
        cat(".")
      } else {
        # Update empty queries
        empty_queries <- rbind(empty_queries,
                               data.frame(journal = journals[i],
                                          year    = years[j]))
      }
    }
    
    # Save results
    if (nrow(journal_papers)){
      saveRDS(journal_papers, 
              file = paste0("./data/journal", 
                            sprintf("%02d-%d.rds", i, years[j])))
    }
  }
  
  saveRDS(empty_queries, file = "./data/01_empty_queries.rds")
}


# ==========
# Consolidate all entries in a single (somewhat large) dataframe
file.list  <- dir("./data/", pattern = "journal[0-9]*-[0-9]*.rds")
all.papers <- data.frame(journal   = character(),
                         year      = numeric(),
                         title     = character(),
                         authors   = character(),
                         doi       = character(),
                         ref.count = numeric(),
                         stringsAsFactors = FALSE)
for (i in seq(file.list)){
  cat("\nAppending file ", sprintf("%02d", i))
  my.df <- readRDS(paste0("./data/", file.list[i]))
  all.papers <- rbind(all.papers, my.df)
}

all.papers <- all.papers %>%
  mutate(across(where(is.factor), as.character))

saveRDS(object = all.papers, 
        file   ="./data/00_consolidated_data.rds")


# Retrieve citation counts for papers with DOI
citations <- data.frame(doi = character(nrow(all.papers)), 
                        citations = integer(nrow(all.papers))+NA)
t0 <- Sys.time()
for (i in 1:ceiling(nrow(all.papers)/100)){
  idx <- unique(pmin(nrow(all.papers), (1 + (i-1)*100):(i*100)))
  cat(sprintf("\nRetrieving citations: %06d-%06d of %d. Elapsed time: %2.1f %s", 
              min(idx), max(idx), nrow(all.papers), Sys.time() - t0, units(Sys.time() - t0)))
  
  my.count <- try(cr_citation_count(all.papers$doi[idx]), silent = TRUE)
  if (is.data.frame(my.count)){
    citations[idx, ] <- my.count
  }
  saveRDS(citations, "./data/citations.rds")
}

all.papers$citations <- citations

saveRDS(object = all.papers, 
        file   ="./data/00_consolidated_data.rds")
