# This routine mines the titles of papers published in the 
# journals which published 2+ metaphors, as of April 2018. 
# According to the Bestiary, these journals are 
# (see `make_analysis_plots.R` for details)
# This script takes a while to run. It saves its results in a file named:
# YYYY-MM-DD_journal_paper_titles.rds
# ==============================================================================
#                                                JOURNAL               METAPHORS
#                                                APPLIED SOFT COMPUTING   11
#                                      ADVANCES IN ENGINEERING SOFTWARE    9
#                                              COMPUTERS AND STRUCTURES    6
#                                                  INFORMATION SCIENCES    5
#                                      EXPERT SYSTEMS WITH APPLICATIONS    4
#                         IEEE TRANSACTIONS ON EVOLUTIONARY COMPUTATION    4
#                                     NEURAL COMPUTING AND APPLICATIONS    4
#                                    SWARM AND EVOLUTIONARY COMPUTATION    4
#                                          THE SCIENTIFIC WORLD JOURNAL    4
#         COMMUNICATIONS IN NONLINEAR SCIENCE AND NUMERICAL SIMULATION     3
#                                    COMPUTERS AND OPERATIONS RESEARCH     3
#                                              KNOWLEDGE-BASED SYSTEMS     3
#                                                       ACTA MECHANICA     2
#                    INTERNATIONAL JOURNAL OF BIO-INSPIRED COMPUTATION     2
# INTERNATIONAL JOURNAL OF COMPUTATIONAL INTELLIGENCE AND APPLICATIONS     2
#                                            PROCEDIA COMPUTER SCIENCE     2
#                                                   SWARM INTELLIGENCE     2
# ==============================================================================

#install.packages("rcrossref")
library(rcrossref)

years    <- 2010:2018
journals <- c("Applied Soft Computing",
              "Advances in Engineering Software",
              "Computers & Structures",
              "Information Sciences",
              "Expert Systems with Applications",
              "IEEE Transactions on Evolutionary Computation",
              "Neural Computing and Applications",
              "Swarm and Evolutionary Computation",
              "The Scientific World Journal",
              "Communications in Nonlinear Science and Numerical Simulation",
              "Computers & Operations Research",
              "Knowledge-Based Systems",
              "Acta Mechanica",
              "International Journal of Bio-Inspired Computation",
              "International Journal of Computational Intelligence and Applications",
              "Procedia Computer Science",
              "Swarm Intelligence")

journal.papers <- vector(mode = "list", length = length(journals))
names(journal.papers) <- journals

for (i in seq_along(journal.papers)){
  title <- character()
  year  <- numeric()
  for (j in seq_along(years)){
    cat("\n", journals[i], ":", years[j])
    start.date <- paste0(years[j], "-01-01")
    end.date   <- paste0(years[j], "-12-31")
    paper.data <- cr_works(filter = c(from_pub_date   = start.date,
                                      until_pub_date  = end.date,
                                      container_title = journals[i]),
                           limit = 1000)
    title <- c(title, paper.data$data$title)
    year  <- c(year, rep(years[j], times = nrow(paper.data$data)))
  }
  journal.papers[[i]] <- data.frame(year  = year,
                                    title = title)
}

for (i in seq_along(journal.papers)){
  journal.papers[[i]] <- cbind(journal.papers[[i]], 
                               Journal = journals[i])
}

all.papers <- do.call(rbind, args = journal.papers)

saveRDS(object = all.papers,
        file   = paste0(as.character(Sys.Date()), 
                        "_journal_paper_titles.rds"))
