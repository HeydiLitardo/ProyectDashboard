daily_stats <- read.csv("./data/daily_stats.csv", header = TRUE, stringsAsFactors = TRUE) %>%
    mutate(date = ymd(date))

monthly_stats <- read.csv("./data/monthly_stats.csv", header = TRUE) %>%
    mutate(date = ymd(date))

yearly_stats <- read.csv("./data/yearly_stats.csv", header = TRUE) %>%
    mutate(date = ymd(date))

countries_stats <- read.csv("./data/countries_stats.csv", header = TRUE) %>%
    mutate(date = ymd(date))
