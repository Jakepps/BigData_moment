library(gplots)
#вычислить max, min, mean по каждому столбцу, 
max_values <- apply(df, 2, max)
min_values <- apply(df, 2, min)
mean_values <- apply(df, 2, mean)

#подсчитать количество людей, отдавших предпочтение >0.7 и <0.3 (составить вектор), 
data <- df[,3:12]
 
votes_7_3 <- apply(data, 2, function(x) sum(x > 7 & !is.na(x)) ||  function(x) sum(x < 3 & !is.na(x)))

#вывести рейтинг фильмов (книг...)  в списке по убыванию, 
votes <- sort(apply(data, 2, mean), decreasing = TRUE)

#построить столбчатую диаграмму оценок (можно сделать разными способами),
table <- read.csv("Лучшая любовница Герльта из Ривии.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

sd_ratings <- apply(ratings, 2, sd)

my_palette <- colorRampPalette(c("green", "yellow", "red"))

barplot2(data, main = "Лучшая любовница Герльта из Ривии", 
         xlab = "Любовница", ylab = "Рейтинг",
         col=my_palette(length(data)))
