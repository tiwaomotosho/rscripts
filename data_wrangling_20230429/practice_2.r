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

# Question 2.3
df1_grouped <- subset(
    df1_long,
    Year == 2021
) %>%
    group_by(ProductionCountry) %>%
    summarize(avgScore = mean(Score)) %>%
    filter(avgScore >= 6.8 & avgScore <= 7.0)

print(df1_grouped)

# Question 2.4
df2 <- read_excel("Production.xlsx", sheet = "Continent")
names(df2) <- c("Country", "Continent")
df2 <- df2[-c(1, 2), ]
print(df2)
cat(
    "Number of distinct Countries: ",
    length(unique(df2$Country)), "\n"
)
cat(
    "Number of Countries not in Data Worksheet: ",
    length(unique(setdiff(df2$Country, df1_long$ProductionCountry))), "\n"
)

# Question 2.5
# Justification

# Part a
# Finding the yearly weighted average rating
#

# Part b
df1_asian <- df1_long %>%
    left_join(df2, by = c("ProductionCountry" = "Country")) %>%
    filter(Continent == "Asia") %>%
    group_by(ProductionCountry, Year) %>%
    summarize(
        sumAnnualRating = sum(NumProducts * Score)
    ) %>%
    group_by(ProductionCountry) %>%
    summarize(
        avgAnnualRating = mean(sumAnnualRating)
    ) %>%
    arrange(desc(avgAnnualRating)) %>%
    slice(1:5)

print(df1_asian)

# part c
# Create the bar chart with data labels
ggplot(df1_asian, aes(
    x = reorder(ProductionCountry, -avgAnnualRating), y = avgAnnualRating
)) +
    geom_col(color = "skyblue", fill = "steelblue") +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal() +
    labs(
        title = "Comparison of Countries by Yearly Weighted Average Score",
        x = "Production Country",
        y = "Yearly Weighted Average Score"
    ) +
    geom_text(aes(label = avgAnnualRating), vjust = -0.1)

# part d
