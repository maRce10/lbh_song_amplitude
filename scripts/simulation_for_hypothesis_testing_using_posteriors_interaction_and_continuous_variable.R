# simulation to test use of posterior distribution for hypothesis testing

library(MCMCglmm)
library(cowplot)
library(ggplot2)
library(viridis)

# two factors:  1) when and 2) how much

indivs <- 30
light <- sample(c("1_shade", "2_light"), indivs, replace = TRUE)
size <- rnorm(indivs, mean = 0, sd = 1)
reps <- 20
interaction <- TRUE
sig_effect <- TRUE
# negative <- "1_shade"
negative <- "2_light"

# sim data with differences among all interaction levels

df_l <- lapply(1:indivs, function(x) {
  
  water <- rnorm(n = reps, mean = rnorm(1, mean = 10, sd = 1), sd = 2)
  
  water <- sort(water)
  
  height <- water + rnorm(reps, mean = 2, sd = 2) 
  
  
  if(light[x] == negative & interaction) height <- sort(height, decreasing = TRUE)
  
  df <- data.frame(ID = x, water = water, height = height,  light = light[x])
  
})

sim_dat <- do.call(rbind, df_l)

# remove effect
if (!sig_effect)
  sim_dat$height <- sample(sim_dat$height)

# remove water-light association when light
# sim_dat$height[sim_dat$light == "2_light"] <- sample(sim_dat$height[sim_dat$light == "2_light"])

table(sim_dat$ID, sim_dat$light)

gg_raw <- ggplot(sim_dat, aes(x = water, y = height, color = light)) + 
  geom_point() + 
  geom_smooth(method = "lm")
# 
# gg_raw


md_sig <- MCMCglmm(height ~ water * light, random = ~ ID, data = sim_dat, verbose = FALSE)
summary(md_sig)

md_sig2 <- MCMCglmm(height ~ light * water - 1, random = ~ ID, data = sim_dat, verbose = FALSE)
summary(md_sig2)

# extract mcmcs
mcmcs2 <- md_sig2$Sol

mcmcs2 <- as.data.frame(mcmcs2)

mcmcs2$`light1_shade:water` <- mcmcs2$`light2_light:water` + mcmcs2$water

mcmcs2 <- mcmcs2[, c("light1_shade:water", "water")]


# make empty list
mcmc_l <- list()

for (i in 1:ncol(mcmcs2))
  mcmc_l[[length(mcmc_l) + 1]] <- data.frame(val = mcmcs2[, i], treatment = colnames(mcmcs2)[i])

mcmc_df <- do.call(rbind, mcmc_l)
colnames(mcmc_df)[1] <- "water-height_slope" 


mcmc_df$treatment <- ifelse(grepl("shade", mcmc_df$treatment),"2_light", "1_shade")


gg_mcmc <- ggplot(mcmc_df, aes(x = treatment, y = `water-height_slope`, col = treatment)) +
  # scale_fill_viridis_d() +
  geom_violin()


combs <- combn(x = unique(mcmc_df$treatment), m = 2)

pvals_l <- lapply(1:ncol(combs), function(x){
  
  mcmc1 <- mcmc_df$`water-height_slope`[mcmc_df$treatment == combs[1, x]]
  mcmc2 <- mcmc_df$`water-height_slope`[mcmc_df$treatment == combs[2, x]]
  p <- if (mean(mcmc2) > mean(mcmc1))
    sum(mcmc1  > mcmc2) / length(mcmc1) else
      sum(mcmc2  > mcmc1) / length(mcmc1)
  
  out <- data.frame(mcmc1 =  combs[1, x], mcmc2 = combs[2, x], p = p)
  
  return(out)
})


cowplot::plot_grid(gg_raw, gg_mcmc)
summary(md_sig2)

pvals_l[[1]]
