# Question 2.1

if (!require(tidyverse)) {
    install.packages("tidyverse")
}

library(tidyverse)
library(readxl)


df1 <- read_excel("Production.xlsx", sheet = "Data")
glimpse(df1)

# Exploring the data content of line 2 - True header
str(df1[2, ])

# Removing Year from row 2 and promoting it to new header
names(df1) <- str_replace_all(df1[2, ], "Year ", "")
df1 <- df1[-c(1, 2), ]
print(df1)
glimpse(df1)

# Question 2.2
# part a
df1_long <- df1 %>%
    pivot_longer(
        cols = -c("Production Country / Maturity Type"),
        names_to = "Year",
        values_to = "Value"
    ) %>%
    drop_na(Value)

print(df1_long)
# The resulting dataframe has 507 rows and 3 columns

# part b
df1_long <- df1_long %>%
    separate("Production Country / Maturity Type",
        into = c("ProductionCountry", "MaturityType"), sep = " / "
    )

print(df1_long)

# part c
df1_long <- df1_long %>%
    separate(Value, into = c("NumProducts", "Score"), sep = " - ") %>%
    mutate(
        Score = as.numeric(gsub("/10", "", Score)),
        NumProducts = as.numeric(NumProducts)
    )

print(df1_long)

# part d
cat("Number of Rows: ", nrow(df1_long), "\n")
cat("Number of Columns: ", ncol(df1_long), "\n")

# part e
cat(
    "Number of distinct Countries: ",
    df1_long %>% distinct(ProductionCountry) %>% nrow(), "\n"
)

cat(
    "Number of distinct Years: ",
    df1_long %>% distinct(Year) %>% nrow(), "\n"
)
