# READ IN DATA AND REMOVE EXTRA DATA FRAME COLUMN, THEN SUMMARIZE
dat = read.csv("~/Documents/NLP/twitter.csv", header = TRUE)
twitter_dat = dat[2:4]
summary(twitter_dat)

# VISUALIZATIONS and ASSUMPTIONS TESTING
retweetsbox <- boxplot(twitter_dat$retweets, main = "Boxplot of Retweets", 
        xlab = "Retweets",ylab="Number of Retweets per Tweet")
favoritesbox <- boxplot(twitter_dat$favorites, main = "Boxplot of Favorites",xlab = "Favorites",
                        ylab = "Number of Favorites per Tweet")
cor <- cor(twitter_dat[1:2])

# LOGISTIC REGRESSION COMBINED VARIABLES
log_all <- glm(twitter_dat$media~twitter_dat$retweets+twitter_dat$favorites, family = "binomial")
summary(log_all)
plot(log_all)

# LOGISTIC REGRESSION RETWEETS
log_retweet <- glm(twitter_dat$media ~ twitter_dat$retweets, family = "binomial")
summary(log_all)
plot(log_retweet)

# LOGISTIC REGRESSION FAVORITES
log_favorites <- glm(twitter_dat$media ~ twitter_dat$favorites, family = "binomial")
summary(log_all)
plot(log_favorites)

