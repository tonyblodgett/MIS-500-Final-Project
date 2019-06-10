# load library
library(dplyr)
library("ggpubr")

# Importing data
# Read CSV file from current directory
# view data
azmerit_data <- read.csv("AzMerit By District v2.csv ", header = TRUE)
#View(azmerit_data)

district_city_data <- read.csv("Book1.csv ", header = TRUE)
#View(district_city_data)

households_by_city <- read.csv("Households By City.csv ", header = TRUE)
#View(households_by_city)

#join data together
city_household <- merge (district_city_data, households_by_city, by.x ="City", by.y = "ï..City" )
View(city_household)

city_household_test <- merge (city_household, azmerit_data, by.x ="ï..District.name", by.y = "District.Charter.Holder.Name" )
View(city_household_test)

View(summary(city_household_test))

#Math test only
city_household_test_math <- (city_household_test %>%
                              filter(Content.Area == "Math"))

View(summary(households_by_city))

#histogram of all score percentages
windows()
gghistogram(city_household_test_math, 
            x="Performance.Percentage", 
            add = "mean", 
            rug = TRUE)

#individual performance level plots to look for normalization
windows()
city_household_test_level1 <- (city_household_test %>%
                               filter(Performance.Level == "Percent Performance Level 1"))
gghistogram(city_household_test_level1, 
            x="Performance.Percentage", 
            add = "mean", 
            color ="Performance.Level",
            fill ="Performance.Level",
            palette = c("#E7B800"),
            rug = TRUE)

windows()
city_household_test_level2 <- (city_household_test %>%
                                 filter(Performance.Level == "Percent Performance Level 2"))
gghistogram(city_household_test_level2, 
            x="Performance.Percentage", 
            add = "mean", 
            color ="Performance.Level",
            fill ="Performance.Level",
            palette = c("#000000"),
            rug = TRUE)

windows()
city_household_test_level3 <- (city_household_test %>%
                                 filter(Performance.Level == "Percent Performance Level 3"))
gghistogram(city_household_test_level3, 
            x="Performance.Percentage", 
            add = "mean", 
            color ="Performance.Level",
            fill ="Performance.Level",
            palette = c("#1c62c4"),
            rug = TRUE)

windows()
city_household_test_level4 <- (city_household_test %>%
                                 filter(Performance.Level == "Percent Performance Level 4"))
gghistogram(city_household_test_level4, 
            x="Performance.Percentage", 
            add = "mean", 
            color ="Performance.Level",
            fill ="Performance.Level",
            palette = c("#00AFBB"),
            rug = TRUE)


# Box plots

windows()
p <- ggboxplot(households_by_city, x = "ï..City", y = "Joint.Household", 
               ylab = "Percent Passing", xlab = "School")
p + rotate_x_text(45)



