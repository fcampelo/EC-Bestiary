# load packages
# install.packages("ggplot2")
# install.packages("ggExtra")
library(ggplot2)
library(ggExtra)


# Load data
pubs.data   <- readRDS("./data/03_title_matches.rds")
bestiary.df <- readRDS("./data/00_bestiaryDF.rds")

if(!identical(names(pubs.data), as.character(bestiary.df$metaphor))){
  stop("Files 03_title_matches.rds and 00_bestiaryDF.rds must have matching entries (in order")
}

# Preprocess data

# Isolate a few classic metaphors
metaphors.to.remove <- c("Genes",      # Genetic Algorithm
                         "Ant Colony") # Ant colony optimization
rm.indx     <- which(names(pubs.data) %in% metaphors.to.remove)
pubs.data   <- pubs.data[-rm.indx]
bestiary.df <- bestiary.df[-rm.indx, ] 

newdf <- data.frame(firstPub    = numeric(),
                    thisPub     = numeric(),
                    metaphor.ID = numeric())
for (i in 1:nrow(bestiary.df)){
  pub.yrs  <- pubs.data[[i]]$pub.years
  orig.pub <- min(min(pub.yrs, bestiary.df$year[i]))
  if (length(pub.yrs) == 0) pub.yrs <- orig.pub
  newdf    <- rbind(newdf,
                    data.frame(firstPub    = rep(orig.pub, length(pub.yrs)),
                               thisPub     = pub.yrs,
                               metaphor.ID = rep(i, length(pub.yrs))))
}


# Plot joint distributions
ggplot(data = bestiary.df, aes(x = year)) + 
  geom_density(mapping = aes(y = ..count..), 
               fill = "gray", size = 0) + 
  geom_bar(fill = "#333333") + 
  geom_density(mapping = aes(y = ..count..), color = "gray")

ggplot(data = newdf, aes(x = thisPub)) + 
  geom_density(mapping = aes(y = ..count..), 
               fill = "gray", size = 0) + 
  geom_bar(fill = "#333333") + 
  geom_density(mapping = aes(y = ..count..), color = "gray")

