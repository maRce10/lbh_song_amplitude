# simulate data in which resposne to playback is mediated by body size

library(lmer)

indivs <- 30
size <- rnorm(indivs, mean = 0, sd = 1)
reps <- 20

# sim data with response affected by body size (PC1)
df_l <- replicate(reps, {

  before <- rnorm(indivs, mean = 100, sd = 4)
  after <- ifelse(size < mean(size), before - rnorm(indivs, mean = 5, sd = 0.5), before + rnorm(indivs, mean = 5, sd = 0.5))
  
  df <- data.frame(ID = rep(1:indivs, 2), cal.spl = c(before, after), PC1 = rep(size, 2), Treatment = rep(c("before", "after"), each = indivs))
  
}, simplify = FALSE)

sim_dat <- do.call(rbind, df_l)

table(sim_dat$ID, sim_dat$Treatment)

interaction_mod <- lmer(cal.spl ~ PC1 + Treatment + Treatment:PC1 + (1|ID), data = sim_dat, REML = FALSE)
summary(interaction_mod)


# sim data with response unrelated to body size (PC1)
increases <- sample(c(TRUE, FALSE), indivs, replace = TRUE)

df_l2 <- replicate(reps, {
  
  before <- rnorm(indivs, mean = 100, sd = 4)
  after <- ifelse(increases, before - rnorm(indivs, mean = 5, sd = 0.5), before + rnorm(indivs, mean = 5, sd = 0.5))
  
  df <- data.frame(ID = rep(1:indivs, 2), cal.spl = c(before, after), PC1 = rep(size, 2), Treatment = rep(c("before", "after"), each = indivs))
  
}, simplify = FALSE)

sim_dat2 <- do.call(rbind, df_l2)

table(sim_dat2$ID, sim_dat2$Treatment)

interaction_mod <- lmer(cal.spl ~ PC1 + Treatment + Treatment:PC1 + (1|ID), data = sim_dat2, REML = FALSE)
summary(interaction_mod)

