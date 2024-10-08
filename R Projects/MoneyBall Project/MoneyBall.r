# import batting and salaries CSV
batting <- read.csv("MoneyBall Project/Batting.csv")
salaries <- read.csv("MoneyBall Project/Salaries.csv")

# create batting average column (hits/at bats)
batting$BA <- batting$H / batting$AB


# create on-base percentage, singles and slugging percentage column
batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)
batting$X1B <- (batting$H - batting$X2B - batting$X3B - batting$HR)
batting$SLG <- ((batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR)) / batting$AB

# merge batting and salary data, salaries df only goes up to 1985 while batting goes up to 1871
batting <- subset(batting, yearID >= 1985)
data <- merge(batting, salaries, by = c("playerID", "yearID"))

# analyze the 3 key players lost during the oakland A's off-season. Players: Jason Giambi (giambja01), Johnny Damon (damonjo01), Rainer Gustavo Olmedo ('saenzol01').
tres <- subset(data, playerID %in% c("giambja01", "damonjo01", "saenzol01"))

# filter to 2001 offseason and select relevant columns
tres1 <- subset(tres, yearID == 2001)
tres2 <- tres1[, c("playerID", "H", "X2B", "X3B", "HR", "OBP", "SLG", "BA", "AB")]

# Find replacement players. The three constraints:
# The total combined salary of the three players can not exceed 15 million dollars.
# Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
# sum(tres2$AB) = 1469/3 = 489.6, round up to 500
# Their mean OBP had to equal to or greater than the mean OBP of the lost players
# mean(tres2$OBP)=0.3638687

data1 <- filter(data, yearID == 2001, salary <= 15000000, AB >= 500, OBP > 0.3638687)

# remove giambja01, damonjo01, and saenzol01, sanity check
data2 <- subset(data1, !playerID %in% c("giambja01", "damonjo01", "saenzol01"))

# sort by OBP highest to lowest
data3 <- arrange(data2, desc(OBP))

# export to excel
write.xlsx(data3, "MoneyBall Project/MoneyBall.xlsx")

# create scatterplot of salary vs. OBP where the top 10 players in OBP are highlighted, playerID is on the top 10


top10 <- ggplot(data3, aes(x = OBP, y = salary)) +
    geom_point() +
    geom_point(data = head(data3, 10), color = "#ff5100", size = 3) +
    geom_text(aes(label = playerID), color = "gray20", data = head(data3, 10), check_overlap = FALSE) +
    scale_y_continuous(labels = comma) +
    theme_minimal() +
    labs(title = "Top 10 OBP vs Salary", x = "On-Base Percentage", y = "Salary")
png(filename = "MoneyBall Project/MoneyBall Top 10 Available.png", width = 1000, height = 1000)
print(top10)
dev.off()
