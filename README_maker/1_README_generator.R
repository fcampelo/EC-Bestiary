require(bibtex)

# Open md file for creating page
md.file  <- file("../README.md", open = "wt", encoding = "UTF-8")

# Write to md file: title, DOI badge, and update date
writeLines("# Evolutionary Computation Bestiary  ", con = md.file)
writeLines("[![DOI](https://zenodo.org/badge/54759561.svg)](https://zenodo.org/badge/latestdoi/54759561)\n", con = md.file)
writeLines(paste("Updated", as.character(Sys.Date()), "  \n***  "), con = md.file)

# Write to md file: openings
md.open  <- file("README_openings.md", open = "r", encoding = "UTF-8")
a <- readLines(con = md.open)
close(md.open)
for (i in seq(a)){
  writeLines(a[i], con = md.file)
}


# Add algorithms for each letter
for (i in seq(LETTERS)){
  # Write to md file: Letter title
  writeLines(paste("\n###", LETTERS[i]), con = md.file)
  my.file <- paste0("../Cages/", LETTERS[i], ".bib")
  if(file.exists(my.file)){
    # read .bib file
    beasts <- read.bib(my.file)
    
    if(length(beasts) != 0){
      # sort by metaphor name within letter
      beasts <- beasts[order(names(beasts))]
      
      for (j in 1:length(beasts)){
        # Extract metaphor name
        x.label <- gsub(pattern = "[.]", replacement = " ", 
                        x = names(beasts[[j]]))
        
        # Process entry information
        x.text  <- paste(capture.output(print(beasts[[j]])), collapse = " ")
        x.text  <- strsplit(x.text, split = "[(]URL")[[1]][1]
        if(grepl(pattern = "doi: ", x = x.text)){
          x.doi  <- strsplit(x.text, split = "doi: ")[[1]][2]
          x.link <- paste0("https://doi.org/", x.doi)
          x.text <- gsub(pattern = x.doi, 
                         replacement = paste0("[", x.doi, "](", x.link, ")"), 
                         x = x.text)
        }
        
        # Write to md file: entry
        writeLines(paste0("- **<u>", x.label, "</u>**: ", x.text), 
                   con = md.file)
      }
    }
  }
}



# Write to md file: Maintainers and Contributors
md.close <- file("README_Maintainers_and_Contributors.md", open = "r", encoding = "UTF-8")
a <- readLines(con = md.close)
close(md.close)
writeLines("\n***\n", con = md.file)
for (i in seq(a)){
  writeLines(a[i], con = md.file)
}


# Write to md file: closings (How to Contribute, More Info, and License)
md.close <- file("README_closings.md", open = "r", encoding = "UTF-8")
a <- readLines(con = md.close)
close(md.close)
writeLines("\n***\n", con = md.file)
for (i in seq(a)){
  writeLines(a[i], con = md.file)
}




close(md.file)
