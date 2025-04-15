library(dplyr)

bgg = read.csv("/home/barbara/MDS/EAD/EAD/bgg_db_1806.csv", sep=",")

df <- subset(bgg, select = -c(rank, bgg_url, game_id, names, image_url, designer, year))
df$num_categories <- sapply(strsplit(df$category, ","), function(x) if (is.na(x[1])) 0 else length(x))
df$num_mechanics <- sapply(strsplit(df$mechanic, ","), function(x) if (is.na(x[1])) 0 else length(x))
df <- subset(df, select = -c(mechanic, category))

str(df)
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
