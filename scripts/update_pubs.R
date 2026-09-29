library(orcidtr)
library(httr2)
library(dplyr)
library(tibble)
library(magrittr)
library(purrr)

rm(list = ls())
test <- orcid_works("0000-0002-7830-2776")

get_authors <- function(orcid, put_code) {
  
  url <- paste0(
    "https://pub.orcid.org/v3.0/",
    orcid,
    "/work/",
    put_code
  )
  
  response <- httr2::request(url) |>
    httr2::req_headers(
      Accept = "application/vnd.orcid+json"
    ) |>
    httr2::req_perform()
  
  work <- httr2::resp_body_json(response)
  
  contributors <- work$contributors$contributor
  
  if (is.null(contributors)) {
    return(NA_character_)
  }
  
  authors <- vapply(
    contributors,
    function(x) {
      paste0(
        x$`credit-name`$value %||% "",
        collapse = ""
      )
    },
    character(1)
  )
  
  paste(authors, collapse = "; ")
}



add_openalex_citations <- function(pubs) {
  input <- subset(pubs, type != "preprint")
  dois <- input$doi
  res <-  data.frame(matrix(ncol = 3, nrow = 0)) %>%
    set_colnames (c("doi", "cited_by", "FWCI"))
  
  for(x in 1:length(dois)){
    response <- request(paste0("https://api.openalex.org/works/https:://doi.org/", dois[x])) %>% 
      req_perform() %>% 
      resp_body_json() 
    
    resx <- data.frame(
      doi = dois[x], 
      cited_by = response$cited_by_count, 
      FWCI = if_else(is.numeric(response$fwci), paste(response$fwci), "NA")
    )
    #print(resx)
    res <- res %>% 
      rbind(resx)
    #print(res)
  }
  
  return(pubs %>% 
          left_join(x = ., y = res, by = "doi"))
}



pubs <- orcid_works("0000-0002-7830-2776") %>% 
  mutate(authors = mapply(FUN = get_authors, put_code = .$put_code, MoreArgs = list(orcid = "0000-0002-7830-2776")),
         ) %>%
  add_openalex_citations(.) %>%
  mutate(journal = case_when(
    doi == "10.21203/rs.3.rs-7015602/v1" ~ "Research Square", 
    doi == "10.1101/2022.02.15.480490" ~ "bioRxiv", 
    .default = journal
  )) %>% 
  left_join(y = read.csv(file = "data/Shiny CV_journal logos.csv")) %>%
  mutate(authors = gsub(pattern = "Harshini Weerasinghe; Helen Stölting; Adam J. Rose; Ana Traven; Joseph Heitman", 
                        replacement = "Harshini Weerasinghe*; Helen Stölting*; Adam J. Rose; Ana Traven; *shared first author", 
                        x = authors))


write.csv(x = pubs, file = "data/CV_pubs.csv", fileEncoding = "UTF-8", row.names = FALSE)
writeLines(format(Sys.Date(), "%d %B %Y"), "data/pubs_last-updated.txt")
