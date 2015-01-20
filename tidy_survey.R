library(tidyr)
library(dplyr)
library(stringr)
library(zoo)
f <- function(){
    con <- file('F://R//stoplight.csv')
    update_date <- readLines(con, 6)[6] 
    update_date <- str_extract(update_date, '[0-9]+/[0-9]+/[0-9]+')
    update_date <- as.Date(update_date, "%m/%d/%Y") 
    data <- tbl_df(read.csv(con, skip = 9))
    names(data) <- tolower(names(data))
    names(data) <- gsub('f.r.howard', 'hmh', names(data))
    names(data) <- gsub('sh.cl', 'shcl', names(data))
    names(data) <- gsub('sh.nv', 'shnv', names(data))  
    names(data) <- gsub('northern.california.network', 'ncn', names(data))
    benchmarks <- select(data, report.text:calendar..prior.year.pr)
    survey <- data %>%
        select(contains('report'), contains('score')) %>%
        gather(class, score, contains('score')) %>%
        select(report.text, class, score) %>%
        mutate(class = gsub(pattern = 'qtr\\.', replacement ='', class)) %>%
        mutate(facility_quarter_year = str_extract(class, '(shnv|shcl|uvmc|hmh|ncn)\\.[0-9]\\.201[0-9]')) %>%
        filter(!is.na(facility_quarter_year)) %>%
        separate(col = facility_quarter_year, into = c('facility', 'quarter', 'year'), remove = TRUE, convert = TRUE) %>%
        mutate(facility = toupper(facility)) %>%
        mutate(date = as.Date(as.yearqtr(year + (quarter-1)/4))) %>%
        inner_join(benchmarks) %>%
        mutate(question = report.text, last_update = update_date) %>%
        select(last_update, question, date, facility:year, contains('score')) %>%
        write.csv('F://R//clean_survey.csv', row.names = FALSE) 
        return
    
}
    