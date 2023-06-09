# Question 1.1

if (!require(tidyverse)) {
    install.packages("tidyverse")
}

library(tidyverse)

df1 <- read.csv("show.csv", header = FALSE, sep = ",")
names(df1) <- str_replace_all(df1[1, ], " ", "")
df1 <- df1[-1, ] %>%
    mutate(DateAdded = as.Date(DateAdded, format = "%d/%m/%Y"))


df2 <- read.csv("MaturityType.csv", header = FALSE, sep = ",")
names(df2) <- str_replace_all(df2[1, ], " ", "")
df2 <- df2[-1, ]

str(df1)
summary(df1)
print(dplyr::as_tibble(df1))

# Check the unique dates to confirm the Date format
print(head(df1$DateAdded, 20))

str(df2)
summary(df2)
print(dplyr::as_tibble(df2))


# Question 1.2

# display number of duplicated rows
cat("Number of duplicated rows:", sum(duplicated(df1$ShowId)), "\n")

# display duplicated rows
duplicated_rows <- df1[duplicated(df1) | duplicated(df1, fromLast = TRUE), ]
print(dplyr::as_tibble(duplicated_rows))

# display number of rows before and after removing duplicates
cat("Number of rows before removing duplicates:", nrow(df1), "\n")

# remove duplicated rows and assign back to df
df1_unique <- unique(df1)

# display number of rows before and after removing duplicates
cat("Number of rows after removing duplicates:", nrow(df1_unique), "\n")

# Replace text in a specific column
df1_unique$ImdbScore <- gsub("/10", "", df1_unique$ImdbScore)

# Convert to numeric values
df1_unique$ImdbScore <- as.numeric(df1_unique$ImdbScore)

str(df1_unique) # display new analysis

# Question 1.3
# For a non-empty result, we perform exploratory analysis on Dataframe
# First, we filter "TV Show for Mature Audiences"
# Then Rating (Maturity Type) is "TV-MA"
# Then, Group by counting ProductionCountry and ReleaseDate
# Then arrange count in descending order

df1_grouped <- subset(
    df1_unique,
    Rating == "TV-MA"
) %>%
    group_by(ProductionCountry, ReleaseDate) %>%
    summarize(count = n()) %>%
    arrange(desc(count))

print(dplyr::as_tibble(df1_grouped))

# Finding the TV Shows from United States in 2019
df1_filtered <- subset(
    df1_unique,
    Rating == "TV-MA" &
        ProductionCountry == "United States" &
        ReleaseDate == "2019"
)
print(dplyr::as_tibble(df1_filtered))

# Question 1.4
# Pattern matching based on conditions
# Then, sorting based on conditions
# df_14 means dataframe for 1.4

df_14 <- df1_unique %>%
    filter(grepl("teen", Description) & grepl("high school", Description)) %>%
    select(Title, ReleaseDate, ImdbScore) %>%
    arrange(ReleaseDate, desc(ImdbScore))

# Print the first 5 rows
print(dplyr::as_tibble(head(df_14, 5)))


# Question 1.5
# Filter for TV-G -"TV Show Suitable for General Audiences"
# Group by counting number of shows and average IMDB Score

df1_15 <- subset(
    df1_unique,
    Rating == "TV-G"
) %>%
    group_by(ProductionCountry) %>%
    summarize(count = n(), avgImdb = mean(ImdbScore)) %>%
    arrange(desc(count))

print(dplyr::as_tibble(head(df1_15, 3)))

# Question 1.6
# Drawing a Boxplot for ImdbScore
# ReleasedDate > 2010 and
# Production Country - "United States" and "Canada"

df_16 <- subset(
    df1_unique,
    ReleaseDate > "2010" &
        (ProductionCountry == "United States" | ProductionCountry == "Canada")
) %>%
    select(ProductionCountry, ReleaseDate, ImdbScore)

# print(dplyr::as_tibble(df_16))

# Create the boxplot using ggplot2
ggplot(df_16, aes(
    x = ProductionCountry,
    y = ImdbScore, fill = ProductionCountry
)) +
    stat_summary(fun.data = function(x) {
        r <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1))
        r <- c(r, mean(x))
        names(r) <- c("ymin", "lower", "middle", "upper", "ymax", "mean")
        r
    }, geom = "errorbar", width = 0.4) +
    geom_boxplot() +
    stat_summary(
        fun.y = "mean", geom = "text",
        aes(label = paste0("Mean: ", round(..y.., 2))),
        position = position_dodge(width = 0.75), vjust = 0, size = 3
    ) +
    theme_minimal() +
    ggtitle("Boxplot of Production Country vs IMDB Score") +
    xlab("Production Country") +
    ylab("IMDB Score")


# Calculate summary statistics
stats <- df_16 %>%
    group_by(ProductionCountry) %>%
    summarise(
        pmin = quantile(ImdbScore, 0),
        p25 = quantile(ImdbScore, 0.25),
        mean = mean(ImdbScore),
        median = median(ImdbScore),
        p75 = quantile(ImdbScore, 0.75),
        pmax = quantile(ImdbScore, 1)
    )

print(stats)

# Question 1.7
# Justifications
# 1 Children moving should use rating "TV-Y" and contain "child" in description
# It is safe in case the parents are not available for guidance
# 2 Movies should be based on IMDB Score in descending order

df_17 <- subset(
    df1_unique,
    Rating == "TV-Y"
) %>%
    filter(grepl("child", Description)) %>%
    select(Title, ReleaseDate, ImdbScore) %>%
    arrange(desc(ImdbScore))

print(dplyr::as_tibble(head(df_17, 5)))
