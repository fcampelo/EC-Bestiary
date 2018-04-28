library(bibtex)
library(ggplot2)

entry.list <- vector(mode = "list", length = 0)
for (i in seq(LETTERS)){
  my.file <- paste0("../Cages/", LETTERS[i], ".bib")
  if(file.exists(my.file)){
    # read .bib file and add entries to list
    entry.list <- c(entry.list, read.bib(my.file))
  }
}

year    <- as.numeric(lapply(entry.list, function(x){x$year}))
journal <- as.character(lapply(entry.list, function(x){x$journal}))
journal <- gsub(pattern = "NULL", replacement = "N/A", x = journal)
journal <- gsub(pattern = "[{}]", replacement = "", x = journal)
journal <- gsub(pattern = "\\&", replacement = "and", x = journal, 
                fixed = TRUE)
journal <- gsub(pattern = "{IEEE}", replacement = "IEEE", x = journal, 
                fixed = TRUE)
journal <- toupper(journal)

df <- data.frame(year    = year,
                 journal = journal)

p1 <- ggplot(data = subset(df, df$year > 1999), aes(x = year))
p1 + 
  geom_density(mapping = aes(y = ..count..), 
               fill = "gray", size = 0) + 
  geom_bar(fill = "#333333") + 
  geom_density(mapping = aes(y = ..count..), color = "gray") +
  ggtitle(label = "New metaphors per year (since 2000)",
          subtitle = "Based on the first-hit rule of the EC Bestiary")

ggsave("../img/new_metaphors_per_year.png", device = "png", 
       width = 18, height = 9, units = "in")

df2 <- within(df, 
              journal <- factor(journal, 
                                levels = names(sort(table(journal), 
                                                    decreasing = TRUE))))
p2 <- ggplot(data = subset(df2, 
                           df2$year > 1999 & df2$journal != "N/A"), 
             aes(x = journal))
p2 + 
  geom_bar() + 
  ggtitle(label = "Metaphor-friendly journals (since 2000)",
          subtitle = "Journals where 'novel' metaphors were published") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("../img/new_metaphors_by_journal.png", device = "png", 
       width = 18, height = 10, units = "in")
