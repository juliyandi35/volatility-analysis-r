library(quantmod)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)

tesla_data <- getSymbols("TSLA", auto.assign = FALSE)
head(tesla_data)
tail(tesla_data)

tesla_2019_data <- tesla_data["2019"]
combined_2019_data <- tesla_2019_data[, "TSLA.Close"]

symbols <- c("F", "GM", "HMC", "TM", "NSANY")
for (symbol in symbols) {
  symbol_data <- getSymbols(symbol, auto.assign = FALSE)
  close_column <- paste(symbol, ".Close", sep = "")
  symbol_2019_data <- symbol_data["2019", close_column]
  combined_2019_data <- merge(combined_2019_data, symbol_2019_data, join = "left")
}
head(combined_2019_data)

combined_2019_df <- data.frame(combined_2019_data)
combined_2019_df <- tibble::rownames_to_column(combined_2019_df, "Date")
combined_2019_df$Date <- as.Date(combined_2019_df$Date, format = "%Y-%m-%d")

for (col in names(combined_2019_df)[-1]) {
  symbol <- str_sub(col, 1, -7)
  new_col_name <- paste(symbol, "% Change")
  col_values <- combined_2019_df[[col]]
  combined_2019_df[[new_col_name]] <- 100*(col_values - lag(col_values))/lag(col_values)
}
head(combined_2019_df)
combined_2019_df <- combined_2019_df[-1, ]

combined_2019_df <- select(combined_2019_df, Date, `TSLA % Change`:`NSANY % Change`)
names(combined_2019_df)[-1] <- str_sub(names(combined_2019_df)[-1], 1, -10)
combined_2019_df <- gather(combined_2019_df, key = "Symbol", value = "% Change", TSLA:NSANY)

filter(combined_2019_df, Date == "2019-01-03")

ggplot(data = combined_2019_df) +
  geom_line(aes(x = Date, y = `% Change`)) +
  facet_wrap("Symbol", nrow  = 2) +
  scale_x_date(date_labels = "%b")

combined_2019_df %>%
  group_by(Symbol) %>%
  summarize(`Standard Deviation of % Change` = sd(`% Change`)) %>%
  arrange(desc(`Standard Deviation of % Change`))
