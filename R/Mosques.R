
library(tidyverse)
library(rvest)
library(httr)

url <- "https://web.archive.org/web/20200510143039/http://www.giza.gov.eg:80/areas/Shamal/Mosques.aspx"


html <- read_html(url)
nodes <- html_elements(html,css = ".MsoNormalTable")
table <- as.data.frame(html_table(nodes))


