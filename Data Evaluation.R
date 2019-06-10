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

# Get the mean by City for tested, passing, levels 1 to 4 for Math and English
View(summary(city_household_test))

#Math test only
city_household_test_math <- (city_household_test %>%
                              filter(Content.Area == "Math"))

View(summary(households_by_city))

#using pipes

View(gilbert %>%
       group_by(Content.Area) %>%
       summarize(mean_area = mean(Percent.Passing, na.rm = TRUE),
                 count_area = n(),
                 min_rnd = min(Percent.Passing),
                 max_rnd = max(Percent.Passing),
                 sd = sd(Percent.Passing, na.rm = TRUE)))

View(city_household_test_math %>%
       group_by(ï..District.name) %>%
       #filter(ï..District.name == "Gilbert Unified District") %>%
       summarize(mean_level1 = mean(Percent.Performance.Level.1, na.rm = TRUE),
                 mean_level2 = mean(Percent.Performance.Level.2, na.rm = TRUE),
                 mean_level3 = mean(Percent.Performance.Level.3, na.rm = TRUE),
                 mean_level4 = mean(Percent.Performance.Level.4, na.rm = TRUE),
                 count_district = n(),
                 sd_level1 = sd(Percent.Performance.Level.1, na.rm = TRUE),
                 sd_level2 = sd(Percent.Performance.Level.2, na.rm = TRUE),
                 sd_level3 = sd(Percent.Performance.Level.3, na.rm = TRUE),
                 sd_level4 = sd(Percent.Performance.Level.4, na.rm = TRUE)))

#histogram of all score percentages
windows()
gghistogram(city_household_test_math, 
            x="Performance.Percentage", 
            add = "mean", 
            rug = TRUE)

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
# ++++++++++++++++++++
# Plot round by Conference

windows()
p <- ggboxplot(households_by_city, x = "ï..City", y = "Joint.Household", 
               ylab = "Percent Passing", xlab = "School")
p + rotate_x_text(45)



