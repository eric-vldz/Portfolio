# imported packages in terminal, data.table, ggplot2, ggthemes, dplyr

# Import the ggplot2 data.table libraries and use fread to load the csv file 'Economist_Assignment_Data.csv' into a dataframe called df (Hint: use drop=1 to skip the first column)
df <- fread("Economist_Assignment_Data.csv", drop = 1)

# Use ggplot() + geom_point() to create a scatter plot object called pl. You will need to specify x=CPI and y=HDI and color=Region as aesthetics
pl <- ggplot(df, aes(x = CPI, y = HDI, color = Region)) +
    geom_point()
png(filename = "CPI_HDI_phase1.png", width = 800, height = 800)
print(pl)
dev.off()

# Change the shape and size of the points to circles
pl1 <- pl + geom_point(shape = 1, size = 5)
png(filename = "CPI_HDI_phase2.png", width = 800, height = 800)
print(pl1)
dev.off()

# Add a trend line to the scatter plot
pl2 <- pl1 + geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "red", formula = y ~ log(x))
png(filename = "CPI_HDI_phase3.png", width = 800, height = 800)
print(pl2)
dev.off()

# set variable pointsToLabel to a vector of country names to label
pointsToLabel <- c(
    "Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
    "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
    "India", "Italy", "China", "South Africa", "Spane",
    "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
    "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
    "New Zealand", "Singapore"
)

# Add text labels to the plot for specified countries
pl3 <- pl3 <- pl2 + geom_text(aes(label = Country), color = "gray20", data = subset(df, Country %in% pointsToLabel), check_overlap = TRUE)
png(filename = "CPI_HDI_phase4.png", width = 800, height = 800)
print(pl3)
dev.off()

# Change the theme
pl4 <- pl3 + theme_bw()
png(filename = "CPI_HDI_phase5.png", width = 800, height = 800)
print(pl4)
dev.off()

# Add X axis labels
pl5 <- pl4 + scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)", limits = c(.9, 10.5), breaks = 1:10)
png(filename = "CPI_HDI_phase6.png", width = 800, height = 800)
print(pl5)
dev.off()

# Add Y axis labels and ttle
pl6 <- pl5 + scale_y_continuous(name = "Human Development Index, 2011 (1=Best)", limits = c(0.2, 1.0)) + ggtitle("Corruption and Human development")
png(filename = "CPI_HDI_phase7.png", width = 800, height = 800)
print(pl6)
dev.off()

# Change the theme to economist white
pl7 <- pl6 + theme_economist_white()
png(filename = "CPI_HDI_phase8.png", width = 800, height = 800)
print(pl7)
dev.off()
