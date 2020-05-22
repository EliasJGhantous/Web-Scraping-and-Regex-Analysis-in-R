library(xml2)
library(rvest)
library(dplyr)
library(stringr)

# Read the HTML files
paul_romer_data <- read_html('../data/rawdata/paul_romer_GoogleScholarCitations.html')
william_nordhaus_data <- read_html('../data/rawdata/william_nordhaus_GoogleScholarCitations.html')

# Extracting scholar names
paul_romer_data %>% html_nodes(css = "#gsc_prf_in") %>% html_text()
william_nordhaus_data %>% html_nodes(css = "#gsc_prf_in") %>% html_text()

# Extracting scholars' affiliated institutions
(paul_romer_data %>% html_nodes(css = ".gsc_prf_il") %>% html_text())[1]
(william_nordhaus_data %>% html_nodes(css = ".gsc_prf_il") %>% html_text())[1]

# making Paul Romer csv
paperName <- paul_romer_data %>% html_nodes(css = ".gsc_a_at") %>% html_text()
author_journal <- (paul_romer_data %>% html_nodes(css = ".gs_gray") %>% html_text())
author_journal <- author_journal[4:length(author_journal)]
researcher <- c()
journal <- c()
for (i in 1:length(author_journal)){
  if ((i %% 2) != 0){
    researcher <- append(researcher, author_journal[i])
  }
  else {
    journal <- append(journal, author_journal[i])
  }
}

citations <- paul_romer_data %>% html_nodes(css = ".gsc_a_c") %>% html_text()
citations <- str_replace_all(citations, "[^[:alnum:]]", "")
citations <- as.integer(citations[3:length(citations)])

year <- paul_romer_data %>% html_nodes(css = ".gsc_a_y") %>% html_text()
year <- str_replace_all(year, "[^[:alnum:]]", " ")
year <- as.integer(year[3:length(year)])

paul_romer_df <- data.frame(paperName, researcher, journal, citations, year, stringsAsFactors = FALSE)

# making William Nordhaus csv
paperName <- william_nordhaus_data %>% html_nodes(css = ".gsc_a_at") %>% html_text()
author_journal <- (william_nordhaus_data %>% html_nodes(css = ".gs_gray") %>% html_text())
author_journal <- author_journal[4:length(author_journal)]
researcher <- c()
journal <- c()
for (i in 1:length(author_journal)){
  if ((i %% 2) != 0){
    researcher <- append(researcher, author_journal[i])
  }
  else {
    journal <- append(journal, author_journal[i])
  }
}

citations <- william_nordhaus_data %>% html_nodes(css = ".gsc_a_c") %>% html_text()
citations <- str_replace_all(citations, "[^[:alnum:]]", "")
citations <- as.integer(citations[3:length(citations)])

year <- william_nordhaus_data %>% html_nodes(css = ".gsc_a_y") %>% html_text()
year <- str_replace_all(year, "[^[:alnum:]]", " ")
year <- as.integer(year[3:length(year)])

william_nordhaus_df <- data.frame(paperName, researcher, journal, citations, year, stringsAsFactors = FALSE)

# writing dataframes to csv
write.csv(paul_romer_df, file = "../data/cleandata/paul_romer_GoogleScholarCitations.csv", row.names = FALSE)
write.csv(william_nordhaus_df, file = "../data/cleandata/william_nordhaus_GoogleScholarCitations.csv", row.names = FALSE)