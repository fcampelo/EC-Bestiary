# This script scrapes the titles of papers published in the 
# journals which published metaphors, as of May 1st 2018. 
# This takes a while to run. It saves its results in a folder named
# YYYY-MM-DD_journal_paper_titles
# ==============================================================================

# Load required packages

#install.packages("rcrossref")
#install.packages("bibtex")
library(rcrossref)
library(bibtex)

options(warn = 1) # For immediately echoing warnings as they occur

# ==========
# Process bestiary files and extract entries
entry.list <- vector(mode = "list", length = 0)
for (i in seq(LETTERS)){
  my.file <- paste0("../Cages/", LETTERS[i], ".bib")
  if(file.exists(my.file)){
    # read .bib file and add entries to list
    entry.list <- c(entry.list, read.bib(my.file))
  }
}

# ==========
# Extract journals with at least one entry in the bestiary
journal <- as.character(lapply(entry.list, function(x){x$journal}))
journal <- gsub(pattern = "NULL", replacement = NA, x = journal)
journal <- gsub(pattern = "[{}]", replacement = "", x = journal)
journal <- gsub(pattern = "\\&", replacement = "&", x = journal, 
                fixed = TRUE)
journal <- gsub(pattern = "{IEEE}", replacement = "IEEE", x = journal, 
                fixed = TRUE)
journals <- table(journal[!is.na(journal)])
journals <- names(sort(journals, decreasing = TRUE))


# ==========
# Set years for scraping
years    <- 2000:2018


# ==========
# Perform scraping

## Create directory
my.dir <- paste0(as.character(Sys.Date()), "_journal_paper_titles")
if(!dir.exists(my.dir)) dir.create(my.dir)

# Initialize data frame for storing empty queries
empty_queries <- data.frame(journal = character(),
                            year    = numeric())

# Iterate over journals vector
for (i in seq_along(journals)){
  # Initialize data frame for iteration i
  journal_papers <- data.frame(journal   = character(),
                               year      = numeric(),
                               title     = character(),
                               authors   = character(),
                               doi       = character(),
                               ref.count = numeric())
  
  # Iterate over years of interest
  for (j in seq_along(years)){
    cat("\n", journals[i], ":", years[j])
    start.date <- paste0(years[j], "-01-01")
    end.date   <- paste0(years[j], "-12-31")
    
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
    } else npap <- nrow(paper.data$data)
    
    cat(".")
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
      if("DOI" %in% names(paper.data$data)){
        my.doi <- paper.data$data$DOI
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
                                         ref.count = my.nref))
      
      cat(". jackpot!")
      
    } else {
      # Update empty queries
      empty_queries <- rbind(empty_queries,
                             data.frame(journal = journals[i],
                                        year    = years[j]))
      cat(". empty!")
    }
  }
  
  # Save results
  if (nrow(journal_papers)){
    saveRDS(journal_papers, 
            file = paste0(my.dir, "/journal", sprintf("%02d", i), "_papers.rds"))
  }
  saveRDS(empty_queries, file = paste0(my.dir, "/00_empty_queries.rds"))
}
