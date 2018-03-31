doi2bib <- function(x){
  if(!is.na(x$DOI)){
    mydoi   <- gsub(pattern = "/", replacement = "%2F", x = x$DOI)
    myadd <- paste0("https://www.doi2bib.org/bib/", mydoi)
    xdata   <- read_html(myadd)
    
    
  } else return(NA)
}