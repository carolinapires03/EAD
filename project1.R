library(dplyr)

bgg = read.csv("/home/barbara/MDS/EAD/EAD/bgg_db_1806.csv", sep=",")

df <- subset(bgg, select = -c(rank, bgg_url, game_id, names, image_url, designer, year))
df$num_categories <- sapply(strsplit(df$category, ","), function(x) if (is.na(x[1])) 0 else length(x))
df$num_mechanics <- sapply(strsplit(df$mechanic, ","), function(x) if (is.na(x[1])) 0 else length(x))
df <- subset(df, select = -c(mechanic, category))

str(df)
sum(is.na(df)) # no missing values
summary(df)

# remove wrong values
cols_to_check <- c("max_time", "min_time", "avg_time")
df <- df[apply(df[, cols_to_check] > 5, 1, all), ]
cols_to_check <- c("min_players", "max_players")
df <- df[apply(df[, cols_to_check] != 0, 1, all), ]
summary(df)

set.seed(42)
s_df <- df %>% sample_n(200)

summary(s_df)


###########################################################
            ## UNIVARIATE ANALYSIS ##
###########################################################


library(e1071)  # for skewness and kurtosis

summary_stats <- data.frame(
  Min = sapply(s_df, min),
  Max = sapply(s_df, max),
  Mean = sapply(s_df, mean),
  Trimmed_Mean = sapply(s_df, mean, trim = 0.1),
  Median = sapply(s_df, median),
  Q1 = sapply(s_df, quantile, probs = 0.25),
  Q3 = sapply(s_df, quantile, probs = 0.75),
  Range = sapply(s_df, function(x) diff(range(x))),
  IQR = sapply(s_df, IQR),
  Variance = sapply(s_df, var),
  SD = sapply(s_df, sd),
  Coef_Variation = sapply(s_df, function(x) sd(x) / mean(x)),
  Skewness = sapply(s_df, skewness),
  Kurtosis = sapply(s_df, kurtosis)
)
rounded_stats <- as.data.frame(lapply(summary_stats, function(x) round(x, 3)))
print(format(rounded_stats, scientific = FALSE))

# boxplots
for (col in names(s_df)) {
  par(mfrow = c(1, 1))
  boxplot(s_df[[col]], main = paste("Boxplot of", col))
}

# histograms
for (col in names(s_df)) {
  hist(s_df[[col]], main = paste("Histogram of", col), xlab = col)
  par(mfrow = c(1, 1))
}


# TODO: remove some outilers


