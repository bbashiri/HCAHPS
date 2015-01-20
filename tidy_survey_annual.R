library(tidyr)
library(dplyr)
library(stringr)
library(zoo)
f <- function(){
    con <- file('F://R//stoplight_annual.csv')
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
        select(contains('report'), contains('score'), contains('size'), contains('pr')) %>%
        gather(class, score, contains('score')) %>%
        select(report.text, class, score) %>%
        mutate(facility_year = str_extract(class, '(shnv|shcl|uvmc|hmh|ncn)\\.201[0-9]')) %>%
        filter(!is.na(facility_year)) %>%
        separate(col = facility_year, into = c('facility', 'year'), remove = TRUE, convert = TRUE) %>%
        mutate(facility = toupper(facility)) %>%
        mutate(date = as.Date(ISOdate(year, 1, 1))) %>%
        inner_join(benchmarks) %>%
        mutate(question = report.text, last_update = update_date) %>%
        select(last_update, question, date, facility:year, contains('score')) %>%
        write.csv('F://R//clean_survey_annual.csv', row.names = FALSE) 
        return 
    
}
    