fish <- read.csv("~/Downloads/fish.csv")
fish$MethodFactor <- factor(fish$Method)

fish.lda <- lda(MethodFactor ~ Aroma + Flavor + Texture + Moisture, data = fish)
fish.lm <- lm(cbind(Aroma, Flavor, Texture, Moisture) ~ MethodFactor, data = fish)

fish.manova <- Manova(fish.lm)

E <- fish.manova$SSPE

# calculate pooled Standard variance. This formula works for J groups
n <- fish %>% count %>% pull
j <- fish %>% dplyr::select(Method) %>%  unique %>% count %>% pull
S.pl <- E / (n - j)

ybar <- mean(fish.lda$means)

L <- function(i) {
  ybar_i <- fish.lda$means[i,]
  (t(ybar_i) %*% solve(S.pl) * ybar) - 0.5(t(ybar_i) %*% solve(S.pl) * ybar_i)
}