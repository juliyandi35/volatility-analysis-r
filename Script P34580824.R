library(quantmod)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(xts)

data <- read_excel("DATA SAHAM MY SIAP RUN.xlsx")
head(data)
tail(data)

values <- data[, c("SIN_MY", "SRI_MY", "SYR_MY", "KLCI_MY",
                   "SIN_ID", "SRI_ID", "SYR_ID", "ICI_ID")]
data <- xts(values, order.by = data$BULAN)

data_df <- data.frame(data)
data_df <- tibble::rownames_to_column(data_df, "Date")
data_df$Date <- as.Date(data_df$Date, format = "%Y-%m-%d")

for (col in names(data_df)[-1]) {
  symbol <- str_sub(col, 1, -1)
  new_col_name <- paste(symbol, "% Change")
  col_values <- data_df[[col]]
  data_df[[new_col_name]] <- 100*(col_values - lag(col_values))/lag(col_values)
}
head(data_df)
data_df <- data_df[-1, ]

writexl::write_xlsx(data_df,"Change Percentage.xlsx")

data_df <- select(data_df, Date, `SIN_MY % Change`:`ICI_ID % Change`)
names(data_df)[-1] <- str_sub(names(data_df)[-1], 1, -10)
data_df <- gather(data_df, key = "Symbol", value = "% Change", SIN_MY:ICI_ID)

ggplot(data = data_df) +
  geom_line(aes(x = Date, y = `% Change`)) +
  facet_wrap("Symbol", nrow  = 2) +
  scale_x_date(date_labels = "%b")

data_df %>%
  group_by(Symbol) %>%
  summarize(`Standard Deviation of % Change` = sd(`% Change`)) %>%
  arrange(desc(`Standard Deviation of % Change`))
