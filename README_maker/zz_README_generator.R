require(bibtex)

# Open md file for creating page
md.file  <- file("../README.md", open = "wt", encoding = "UTF-8")

# Write to md file: title and date
writeLines("# Evolutionary Computation Bestiary  ", con = md.file)
writeLines(paste("Updated", as.character(Sys.Date()), "\n***  "), con = md.file)

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
  writeLines(paste("###", LETTERS[i]), con = md.file)
  my.file <- paste0("../cages/", LETTERS[i], ".bib")
  if(file.exists(my.file)){
    beasts <- read.bib(my.file)
    for (j in 1:length(beasts)){
      entry <- beasts[[j]]
    }
  }
}



# Write to md file: closings
md.close <- file("README_closings.md", open = "r", encoding = "UTF-8")
a <- readLines(con = md.close)
close(md.close)
for (i in seq(a)){
  writeLines(a[i], con = md.file)
}

close(md.file)
