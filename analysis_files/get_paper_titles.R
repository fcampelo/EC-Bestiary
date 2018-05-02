# This routine mines the titles of papers published in the 
# journals which published metaphors, as of May 1st 2018. 
# This script takes a while to run. It saves its results in a file named:
# YYYY-MM-DD_journal_paper_titles.rds
# ==============================================================================
# [1] "APPLIED SOFT COMPUTING"                                                       
# [2] "ADVANCES IN ENGINEERING SOFTWARE"                                             
# [3] "COMPUTERS AND STRUCTURES"                                                     
# [4] "INFORMATION SCIENCES"                                                         
# [5] "EXPERT SYSTEMS WITH APPLICATIONS"                                             
# [6] "IEEE TRANSACTIONS ON EVOLUTIONARY COMPUTATION"                                
# [7] "NEURAL COMPUTING AND APPLICATIONS"                                            
# [8] "SWARM AND EVOLUTIONARY COMPUTATION"                                           
# [9] "THE SCIENTIFIC WORLD JOURNAL"                                                 
# [10] "COMMUNICATIONS IN NONLINEAR SCIENCE AND NUMERICAL SIMULATION"                 
# [11] "COMPUTERS AND OPERATIONS RESEARCH"                                            
# [12] "KNOWLEDGE-BASED SYSTEMS"                                                      
# [13] "ACTA MECHANICA"                                                               
# [14] "INTERNATIONAL JOURNAL OF BIO-INSPIRED COMPUTATION"                            
# [15] "INTERNATIONAL JOURNAL OF COMPUTATIONAL INTELLIGENCE AND APPLICATIONS"         
# [16] "PROCEDIA COMPUTER SCIENCE"                                                    
# [17] "SWARM INTELLIGENCE"                                                           
# [18] "AI COMMUNICATIONS"                                                            
# [19] "APPLIED INTELLIGENCE"                                                         
# [20] "ARTIFICIAL INTELLIGENCE REVIEW"                                               
# [21] "COMPLEXITY"                                                                   
# [22] "COMPUTATIONAL INTELLIGENCE"                                                   
# [23] "COMPUTATIONAL INTELLIGENCE AND NEUROSCIENCE"                                  
# [24] "COMPUTER-AIDED DESIGN"                                                        
# [25] "COMPUTERS AND GEOSCIENCES"                                                    
# [26] "ECOLOGICAL INFORMATICS"                                                       
# [27] "ELECTRICAL AND ELECTRONIC ENGINEERING"                                        
# [28] "ELECTRONICS LETTERS"                                                          
# [29] "ENGINEERING APPLICATIONS OF ARTIFICIAL INTELLIGENCE"                          
# [30] "IEEE CONTROL SYSTEMS MAGAZINE"                                                
# [31] "INTERNATIONAL JOURNAL OF COMPUTATIONAL SCIENCE AND ENGINEERING"               
# [32] "INTERNATIONAL JOURNAL OF INFORMATION TECHNOLOGY AND DECISION MAKING"          
# [33] "INTERNATIONAL JOURNAL OF OPERATIONAL RESEARCH"                                
# [34] "INTERNATIONAL JOURNAL OF PRODUCTION RESEARCH"                                 
# [35] "INTERNATIONAL JOURNAL OF SCIENCE AND ADVANCED TECHNOLOGY"                     
# [36] "INTERNATIONAL JOURNAL OF SCIENTIFIC AND ENGINEERING RESEARCH"                 
# [37] "INTERNATIONAL JOURNAL OF SOFT COMPUTING"                                      
# [38] "INTERNATIONAL JOURNAL OF SWARM INTELLIGENCE RESEARCH"                         
# [39] "IRAN UNIVERSITY OF SCIENCE AND TECHNOLOGY"                                    
# [40] "IRANIAN JOURNAL OF SCIENCE AND TECHNOLOGY,  TRANSACTIONS OF CIVIL ENGINEERING"
# [41] "IRAQ J. ELECTRICAL AND ELECTRONIC ENGINEERING"                                
# [42] "ISA TRANSACTIONS"                                                             
# [43] "J CIRCUITS SYSTEMS"                                                           
# [44] "JOURNAL OF ADVANCED COMPUTATIONAL INTELLIGENCE AND INTELLIGENT INFORMATICS"   
# [45] "JOURNAL OF APPLIED AND COMPUTATIONAL MATHEMATICS"                             
# [46] "JOURNAL OF ARTIFICIAL INTELLIGENCE RESEARCH"                                  
# [47] "JOURNAL OF COMPUTATIONAL PHYSICS"                                             
# [48] "JOURNAL OF CONTROL,  AUTOMATION AND ELECTRICAL SYSTEMS"                       
# [49] "JOURNAL OF HEURISTICS"                                                        
# [50] "JOURNAL OF NATURAL GAS SCIENCE AND ENGINEERING"                               
# [51] "JOURNAL OF SCHEDULING"                                                        
# [52] "JOURNAL OF WATER RESOURCES PLANNING AND MANAGEMENT"                           
# [53] "MATHEMATICAL PROBLEMS IN ENGINEERING"                                         
# [54] "MEMETIC COMPUTING"                                                            
# [55] "NATURAL COMPUTING"                                                            
# [56] "NEUROCOMPUTING"                                                               
# [57] "ORSA JOURNAL ON COMPUTING"                                                    
# [58] "PROGRESS IN ARTIFICIAL INTELLIGENCE"                                          
# [59] "PROGRESS IN ELECTROMAGNETICS RESEARCH"                                        
# [60] "SCIENCE CHINA INFORMATION SCIENCES"                                           
# [61] "SENSOR LETTERS"                                                               
# [62] "SIMULATION"                                                                   
# [63] "SOFT COMPUTING"   
# ==============================================================================

#install.packages("rcrossref")
library(rcrossref)

years    <- 2000:2018
journals <- c("Applied Soft Computing",
              "Advances in Engineering Software",
              "Computers & Structures",
              "Information Sciences",
              "Expert Systems With Applications",
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
              "Swarm Intelligence",
              "AI Communications",
              "Applied Intelligence",
              "Artificial Intelligence Review",
              "Complexity",
              "Computational Intelligence",
              "Computational Intelligence and Neuroscience",
              "Computer-Aided Design",
              "Computers & Geosciences",
              "Ecological Informatics",
              "Electrical and Electronic Engineering",
              "Electronics Letters",
              "Engineering Applications of Artificial Intelligence",
              "IEEE Control Systems Magazine",
              "International Journal of Computational Science and Engineering",
              "International Journal of Information Technology & Decision Making",
              "International Journal of Operational Research",
              "International Journal of Production Research",
              "International Journal of Science and Advanced Technology",
              "International Journal of Scientific and Engineering Research",
              "International Journal of Soft Computing",
              "International Journal of Swarm Intelligence Research",
              "Iran University of Science and Technology",
              "Iranian Journal of Science and Technology",
              "Iraq J. Electrical and Electronic Engineering",
              "ISA Transactions",
              "J Circuits Systems",
              "Journal of Advanced Computational Intelligence and Intelligent Informatics",
              "Journal of Applied and Computational Mathematics",
              "Journal of Artificial Intelligence Research",
              "Journal of Computational Physics",
              "Journal of Control, Automation and Electrical Systems",
              "Journal of Heuristics",
              "Journal of Natural Gas Science and Engineering",
              "Journal of Scheduling",
              "Journal of Water Resources Planning and Management",
              "Mathematical Problems in Engineering",
              "Memetic Computing",
              "Natural Computing",
              "Neurocomputing",
              "ORSA Journal on Computing",
              "Progress in Artificial Intelligence",
              "Progress in Electromagnetics Research",
              "Science China Information Sciences",
              "Sensor Letters",
              "Simulation",
              "Soft Computing")

empty_queries <- data.frame(journal = character(),
                            year    = numeric())

for (i in seq_along(journals)){
  title   <- character()
  year    <- numeric()
  authors <- character()
  for (j in seq_along(years)){
    cat("\n", journals[i], ":", years[j])
    start.date <- paste0(years[j], "-01-01")
    end.date   <- paste0(years[j], "-12-31")
    paper.data <- cr_works(filter = c(from_pub_date   = start.date,
                                      until_pub_date  = end.date,
                                      container_title = journals[i]),
                           limit = 1000)
    if(nrow(paper.data$data)){
      title   <- c(title, paper.data$data$title)
      year    <- c(year, rep(years[j], times = nrow(paper.data$data)))
      if("author" %in% names(paper.data$data)){
        authors <- c(authors, paper.data$data$author)
      } else authors <- c(authors, NA)
    } else {
      empty_queries <- rbind(empty_queries,
                             data.frame(journal = journals[i],
                                        year    = years[j]))
    }
  }
  
  if (length(year) > 0){
    authors <-lapply(X = authors, function(x){paste(x$given, x$family)})
    authors <- unlist(lapply(authors, paste0, collapse = ", "))
    journal_papers <- data.frame(year    = year,
                                 title   = title,
                                 authors = authors,
                                 journal = rep(journals[i], 
                                               length(year)))
    saveRDS(journal_papers, 
            file = paste0("tmpdata/", i, ".rds"))
  } else saveRDS(empty_queries, file = "tmpdata/empty_queries.rds")
}



# 
# 
# 
# 
# 
# all.papers <- do.call(rbind, args = journal.papers)
# rownames(all.papers) <- seq(nrow(all.papers))
# 
# saveRDS(object = all.papers,
#         file   = paste0(as.character(Sys.Date()), 
#                         "_journal_paper_titles.rds"))
