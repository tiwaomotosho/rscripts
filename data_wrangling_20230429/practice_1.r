# Question 1.1

if (!require(tidyverse)) {
    install.packages("tidyverse")
}

library(tidyverse)

df1 <- read.csv("show.csv", header = FALSE, sep = ",")
names(df1) <- str_replace_all(df1[1, ], " ", "")
df1 <- df1[-1, ] %>%
    mutate(DateAdded = as.Date(DateAdded, format = "%d/%m/%Y"))


df2 <- read.csv("MaturityType.csv", header = FALSE,  sep = ",")
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
dup_rows <- duplicated(df1)
print(dplyr::as_tibble(dup_rows))