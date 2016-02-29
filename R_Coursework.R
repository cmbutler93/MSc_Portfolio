football <- read.csv("~/Documents/DATA_PGMG/foot20teams6props.csv",header = T)
names(football) <- c( "Team", "Points" , "Goals2Shots" , 
  "LongGoals" , "Short2LongPasses" , "BallCrosses" ,
  "ConcededGoals","NumYellowCards")
tiers <- c(rep("top_tier",4),rep("mid_tier",12),rep("bottom_tier",4))
fb_ordered <- football[with(football, order( Points, decreasing = T)),]
lm1_results <- lm(Points ~ Goals2Shots + LongGoals
                  + Short2LongPasses + BallCrosses +
                  ConcededGoals + NumYellowCards,
                  data = fb_ordered)
fb_pca <- prcomp(fb_ordered[,2:7],scale = T)
variance <- 100*fb_pca$sdev^2/sum(fb_pca$sdev^2)
points_aov <- oneway.test(fb_ordered$Points ~ tiers)
goals2shots_aov <- oneway.test(fb_ordered$Goals2Shots ~ tiers)
longgoals_aov <-  oneway.test(fb_ordered$LongGoals ~ tiers)
short2longpasses_aov <-  oneway.test(fb_ordered$Short2LongPasses ~ tiers)
ballcrosses_aov <-  oneway.test(fb_ordered$BallCrosses ~ tiers)
concededgoals_aov <- oneway.test(fb_ordered$ConcededGoals ~ tiers)
numyellowcards_aov <- oneway.test(fb_ordered$NumYellowCards ~ tiers)

oneway_aov <- function(variable)
{
  tier_list <- tiers 
  results <- oneway.test(variable ~ tier_list)
  pvalue <- results$p.value
  if (pvalue < .05)
  {
    print(pvalue)
    print("Reject null hypothesis.")
  }
  else
  {
    print(pvalue)
    print("Fail to reject null hypothesis.")
  }
}
apply_test <- apply(fb_ordered[,2:7],2,oneway_aov)

pvalues <- c(6.248472e-05, 0.267708, 0.4914416, 0.03039631, 0.2959648, 0.0001972267)
hypothesis <- c("Reject null hypothesis.", 
                "Fail to reject null hypothesis.",
                "Fail to reject null hypothesis.",
                "Reject null hypothesis.",
                "Fail to reject null hypothesis.",
                "Reject null hypothesis.")
aov_results <- cbind(pvalues, hypothesis)