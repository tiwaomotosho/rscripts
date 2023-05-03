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