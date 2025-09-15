# 1 IMPORTING CLASS 1 SURVEY FILE
class1.csv <-
  read.csv(
    "https://github.com/kijohnson/Advanced-Data-Analysis/raw/refs/heads/main/Class%201%20Survey%20Fall%202025%202.csv"
  )

#2a The number of observations is 58
#2b  The number of variables is 27

#3a RENAMING COLUMNS
class1.csv <- class1.csv %>%
  rename_with(~ c("id", "like_cats", "like_dogs", "have_desert", "slogan", "fav_day",
                  "larkORowl", "fav_food", "fav_drink", "fav_season", "fav_month",
                  "hobby", "program", "specialization", "stat_software", "R_exp",
                  "coding_comfort", "coding_length", "top_three", "public_health_interest",
                  "fav_num", "bday", "bmonth", "country", "state", "city",
                  "highest_educ_level"),
              .cols = 1:27)



#3b DISPLAYING THAT THEY ARE RENAMED
colnames(class1.csv)


#4 GETTING TYPE OF EACH VARIABLE
var_classes <- sapply(class1.csv, class)

num_factors   <- sum(var_classes == "factor")
num_integers  <- sum(var_classes == "integer")
num_numerics  <- sum(var_classes == "numeric")
num_characters <- sum(var_classes == "character")

#DISPLAYING THE NUMBER OF EACH TYPE OF VARIABLE
cat("Number of factor variables:   ", num_factors, "\n")
cat("Number of integer variables:  ", num_integers, "\n")
cat("Number of numeric variables:  ", num_numerics, "\n")
cat("Number of character variables:", num_characters, "\n")

#5a CHECKING BIRTHDAY VARIABLE FOR ANY UNUSUAL OR MISSING VALUES
table(class1.csv$bday, useNA = "always")

#   - NA values will be ignored when counting variable classes
#   - Variables with unexpected classes (e.g., "list") will be flagged for manual review

#5a CHECKING BIRTHMONTH VARIABLE FOR ANY UNUSUAL OR MISSING VALUES
table(class1.csv$bmonth, useNA = "always")

#   - NA values will be ignored when counting variable classes
#   - Variables with unexpected classes (e.g., "list") will be flagged for manual review


#5b WHAT TO DO WITH ALL UNUSUAL VALUES

# Unusual values (e.g., NA, -99, 999, or wrong data types) are not removed here,
# but if encountered, they will be handled separately depending on the analysis:
#   - NA values will be ignored when counting variable classes
#   - Miscoded numeric placeholders (like -99, 999) can be recoded to NA
#   - Variables with unexpected classes (e.g., "list") will be flagged for manual review

#5c CLEANING BIRTH MONTH INTO NUMERIC

table(class1.csv$bmonth, useNA = "always")
list_flags <- var_classes == "list"
if (any(list_flags)) {
  cat("List-type variables found:\n")
  print(names(class1.csv)[list_flags])
}
class1.csv$bmonth <- as.numeric(as.character(class1.csv$bmonth))

#5c FINDING MEDIAN BIRTHMONTH
median_month <- median(class1.csv$bmonth, na.rm = TRUE)

cat("Median birth month:", median_month, "\n")

#5c CLEANING BIRTH DAY INTO NUMERIC

table(class1.csv$bday, useNA = "always")
list_flags <- var_classes == "list"
if (any(list_flags)) {
  cat("List-type variables found:\n")
  print(names(class1.csv)[list_flags])
}
class1.csv$bday <- as.numeric(as.character(class1.csv$bday))


#5c FINDING MEDIAN BIRTH DAY
median_bday <- median(class1.csv$bday, na.rm = TRUE)

cat("Median birthday (day of month):", median_bday, "\n")

#6a CREATING NEW VARIABLE BSEASON
class1.csv$bseason <- with(class1.csv, ifelse(class1.csv$bmonth %in% c(12, 1, 2), "Winter",
                                              ifelse(class1.csv$bmonth %in% c(3, 4, 5), "Spring",
                                                     ifelse(class1.csv$bmonth %in% c(6, 7, 8), "Summer",
                                                            ifelse(class1.csv$bmonth %in% c(9, 10, 11), "Fall", NA)))))

#6a CONVERTING TO FACTOR FOR NICER TABLES
class1.csv$bseason <- factor(class1.csv$bseason, 
                             levels = c("Winter", "Spring", "Summer", "Fall"))


#6b TABLE OF SEASONS IN COLUMNS BY BIRTH MONTH IN ROWS
season_month_table <- table(class1.csv$bmonth, class1.csv$bseason)
print(season_month_table)

#6c SUMS PER COLUM
season_counts <- addmargins(season_month_table, margin = 1)  # sum across rows = totals by season
print(season_counts)

#6c CLASSMATES BORN IN EACH SEASON
cat("Number of classmates born in each season:\n")
print(colSums(season_month_table, na.rm = TRUE))

#7 FREQUENCY TABLE PER COUNTRY
library(dplyr)

country_freq <- class1.csv %>%
  count(country, name = "Frequency") %>%
  arrange(desc(Frequency))

print(country_freq)

#7 FREQUENCY OF EACH COUNTRY COMPARED TO GHANA
library(dplyr)
library(ggplot2)

# Summarize frequencies
country_freq <- class1.csv %>%
  count(country, name = "Frequency")

# Add a flag for Ghana
country_freq <- country_freq %>%
  mutate(is_ghana = ifelse(country == "Ghana", "Ghana", "Other"))

# Plot with Ghana highlighted
ggplot(country_freq, aes(x = reorder(country, -Frequency), y = Frequency, fill = is_ghana)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Ghana" = "darkorange", "Other" = "steelblue")) +
  labs(title = "Number of Respondents by Country",
       x = "Country", y = "Frequency", fill = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#WRITING THE CLEANED DATAFRAME TO A CSV FILE

write.csv(class1.csv, file = "class1_clean.csv", row.names = FALSE)



