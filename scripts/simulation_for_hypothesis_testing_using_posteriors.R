# simulation to test use of posterior distribution for hypothesis testing

library(MCMCglmm)
library(cowplot)

# two factors:  1) when and 2) how much

how_much <- sample(c("1_low", "2_high"), indivs, replace = TRUE)
indivs <- 30
size <- rnorm(indivs, mean = 0, sd = 1)
reps <- 20
interaction <- TRUE
sig_effect <- TRUE

# sim data with differences among all interaction levels

df_l <- replicate(reps, {
  
  before <- ifelse(how_much == "1_low", rnorm(indivs, mean = 10, sd = 1), rnorm(indivs, mean = 13, sd = 1))
  
  if (interaction)
  after <- ifelse(how_much == "1_low", before + rnorm(indivs, mean = 2.5, sd = 0.5),
                  before + rnorm(indivs, mean = 5, sd = 0.5))
  else
    after <- ifelse(how_much == "2_low", before + rnorm(indivs, mean = 2.5, sd = 0.5),
                    before + rnorm(indivs, mean = 5, sd = 0.5))
  
  # after <- before + rnorm(indivs, mean = 3, sd = 0.5)
  
  df <- data.frame(ID = rep(1:indivs, 2), val = c(before, after), how.much = rep(how_much, 2), when = rep(c("1_before", "2_after"), each = indivs))
  
}, simplify = FALSE)

sim_dat <- do.call(rbind, df_l)
# sim_dat$how.much <- factor(sim_dat$how.much, levels = c("1_low", "1_high"))
# sim_dat$when <- factor(sim_dat$when, levels = c("before", "after"))

# remove effect
if (!sig_effect)
sim_dat$val <- sample(sim_dat$val)

table(sim_dat$ID, sim_dat$when)
table(sim_dat$how.much, sim_dat$when)

gg_raw <- ggplot(sim_dat, aes(x = when, y = val, color = how.much)) + 
  geom_violin()

md_sig <- MCMCglmm(val ~ how.much + when + how.much:when, random = ~ ID, data = sim_dat, verbose = FALSE)
summary(md_sig)

# extract mcmcs
mcmcs <- md_sig$Sol

# make empty list
mcmc_l <- list()

##  baseline: before low
mcmc_l[[length(mcmc_l) + 1]] <- data.frame(spl = mcmcs[,"(Intercept)"], when = "1_before", how.much = "1_low")

##  before high
mcmc_l[[length(mcmc_l) + 1]] <- data.frame(spl = mcmcs[,"(Intercept)"] + mcmcs[,"how.much2_high"], when = "1_before", how.much = "2_high")

# after low
mcmc_l[[length(mcmc_l) + 1]] <- data.frame(spl = mcmcs[,"(Intercept)"] + mcmcs[,"when2_after"], when = "2_after", how.much = "1_low")

# chase after playback
mcmc_l[[length(mcmc_l) + 1]] <- data.frame(spl = mcmcs[,"(Intercept)"] + 
                                             mcmcs[,"when2_after"] + 
                                             mcmcs[,"how.much2_high"] +
                                             mcmcs[, "how.much2_high:when2_after"], when = "2_after", how.much = "2_high")

mcmc_df <- do.call(rbind, mcmc_l)
colnames(mcmc_df)[1] <- "val" 

# gg_mcmc <- ggplot(mcmc_df, aes(x = when, y = val, color = how.much)) + 
#   scale_fill_viridis() +
#   geom_violin()
# 
# cowplot::plot_grid(gg_raw,gg_mcmc)
sim_dat$data <- "raw"
mcmc_df$data <- "mcmc"

clumped <- rbind(sim_dat[, -1], mcmc_df)
ggplot(clumped, aes(x = when, y = val, color = how.much, fill = data)) + 
  scale_fill_grey() +
  scale_color_viridis_d() +
  geom_violin(lwd = 1.1)

mcmc_df$when.how.much <- paste(mcmc_df$when, mcmc_df$how.much, sep = "-")

combs <- combn(x = unique(mcmc_df$when.how.much), m = 2)


pvals_l <- lapply(1:ncol(combs), function(x){
  
  mcmc1 <- mcmc_df$val[mcmc_df$when.how.much == combs[1, x]]
  mcmc2 <- mcmc_df$val[mcmc_df$when.how.much == combs[2, x]]
  p <- if (mean(mcmc2) > mean(mcmc1))
    sum(mcmc1  > mcmc2) / length(mcmc1) else
      sum(mcmc2  > mcmc1) / length(mcmc1)
  
  out <- data.frame(mcmc1 =  combs[1, x], mcmc2 = combs[2, x], p = p)
  
  return(out)
})

do.call(rbind, pvals_l)
summary(md_sig)$solutions