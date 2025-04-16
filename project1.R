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


library(e1071)

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
rounded_stats$Variable <- rownames(summary_stats)
rounded_stats <- rounded_stats[, c(ncol(rounded_stats), 1:(ncol(rounded_stats)-1))]
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

scaled_temp <- scale(s_df[, c("num_votes", "owned", "max_players", "max_time", "min_time", "avg_time")])
valid_rows <- apply(scaled_temp, 1, function(row) all(row < 3 & row > -3))
df_clean <- s_df[valid_rows, ]

# boxplots
for (col in names(df_clean)) {
  par(mfrow = c(1, 1))
  boxplot(df_clean[[col]], main = paste("Boxplot of", col))
}

# histograms
for (col in names(df_clean)) {
  hist(df_clean[[col]], main = paste("Histogram of", col), xlab = col)
  par(mfrow = c(1, 1))
}

scaled_df <- as.data.frame(scale(df_clean))
for (col in names(scaled_df)) {
  hist(scaled_df[[col]], main = paste("Histogram of", col), xlab = col)
  par(mfrow = c(1, 1))
}


###########################################################
              ## BIVARIATE ANALYSIS ##
###########################################################
library(corrplot)
library(ggplot2)


numeric_vars <- names(df_clean) # non-standardized data

for (i in 1:(length(numeric_vars) - 1)) {
  for (j in (i + 1):length(numeric_vars)) {
    x_var <- sym(numeric_vars[i])
    y_var <- sym(numeric_vars[j])
    
    p <- ggplot(df_clean, aes(x = !!x_var, y = !!y_var)) +
      geom_point(alpha = 0.5) +
      labs(title = paste("Scatterplot:", numeric_vars[i], "vs", numeric_vars[j]),
           x = numeric_vars[i],
           y = numeric_vars[j]) +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      theme_minimal(base_size = 12)
    
    print(p)
  }
}

# For most of the variables scatterplots dont show a linear relationship, harder to see when one of the variables is discrete
# NUMBER OF MECHANICS
# Number of mechanics and weight are greater with the age restriction (positive relationship)
# OWNED
# Most games owned are for +7 yr old
# num votes and owned have a very strong linear relationship (seems almost perfect)
# AGE
# because of the previous, naturally the num_votes is higher for age +7
# GEEK RATINGS
# while theres low geek ratings for all of the weights, from around rating 7 there seems to be a monotonic positive relationship
# geek rating vs owned (and num votes) seems to be linear related, apart from some extremes
# geek rating also gets higher and more variance for age +7
# AVG RATING
# avg rating is very strongly related to weight, more than geek rating. That means avg rating is more influenced by weight than geek rating
# different from geek rating, avg rating does not seem to have a linear relationship with owned/num_votes
# even with the cited differences, avg rating and geek rating have a strong correlation. However its interisting to note how the dots have a triangular shape, which shows low geek ratings can have either low or high avg ratings
# MAX TIME
# strong correlation to weight
# greater max times are achieved for +10 age games
# MIN TIME
# strong correlation to weight again
# min time and max time highly correlated, which is expected
# AVG TIME
# perfect correlation to max time (they are equivalent)
# therefore, all of the same conclusions as max time
# MAX and MIN PLAYERS
# number of players (min or max) doesnt seem to have relevant relationships with other variables
# for the rest, there is no linear/no strong linear relationship or further insight that could be obtained


# !! REMOVING PERFECT CORRELATION VARIABLE (avg_time = max_time) !!
scaled_df <- subset(scaled_df, select = -c(avg_time))
dim(scaled_df)



# Pearson
cor_matrix <- cor(scaled_df)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black", 
         cl.cex = 1,    
         number.cex = 0.8)

# Spearman
cor_matrix <- cor(scaled_df, method="s")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black", 
         cl.cex = 1,    
         number.cex = 0.8)


# Tests for Correlation

vars <- names(scaled_df)

for (i in 1:(length(vars) - 1)) {
  for (j in (i + 1):length(vars)) {
    result <- cor.test(scaled_df[[vars[i]]], scaled_df[[vars[j]]], method = "spearman", exact = FALSE)
    assoc <- if (round(result$p.value, 4) < 0.05) " -- ASSOCIATED" else ""
    cat(vars[i], "vs", vars[j], 
        "- p-value:", round(result$p.value, 4), 
        "rho:", round(result$estimate, 3),
        assoc, "\n")
  }
}


