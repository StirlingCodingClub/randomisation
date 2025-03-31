birds <- read.csv("data/Bumpus_data.csv");
bird_t_test <- t.test(wgt ~ surv, data = birds, alternative = "less");


iter    <- 9999;          # Total iterations (+1 for observed data = 10k)
diff    <- NULL;          # To add difference between groups
N_birds <- dim(birds)[1]; # Total number of birds
for(i in 1:iter){   
  bird_samp   <- sample(x = birds[,2], size = N_birds, replace = FALSE);
  samp_alive  <- which(bird_samp == "alive");
  samp_dead   <- which(bird_samp == "dead");
  mn_samp_a   <- mean(birds[samp_alive, 5]);
  mn_samp_d   <- mean(birds[samp_dead, 5]);
  diff[i]     <- mn_samp_a - mn_samp_d;
}

obs_alive <- which(birds[,2] == "alive");
obs_dead  <- which(birds[,2] == "dead");
obs_diff  <- mean(birds[obs_alive,5]) - mean(birds[obs_dead,5]); 

less_or_equal_obs <- sum(diff <= obs_diff) + 1;
total_generated   <- length(diff) + 1;
new_p_value       <- less_or_equal_obs / total_generated;












# Read in the data
full_data <- read.csv("data/wing_loadings.csv");
# Just get the SO1 and SO2 species of interest
dat <- full_data[full_data$Species == "SO1" | full_data$Species == "SO2",];

diff <- rep(x = NA, times = 9999);
for(i in 1:length(diff)){
  or  <- sample(x = 1:dim(dat)[1], size = dim(dat)[1], replace = FALSE);
  hl  <- dat$Head_Length_mm[or];
  crs <- cor(dat$Ovipositor_Length_mm, hl);
  diff[i] <-crs;
}

actual_corr <- cor(dat$Ovipositor_Length_mm, dat$Head_Length_mm);

extreme <- sum(abs(diff) >= actual_corr);
p_val   <- (extreme + 1) / (length(diff) + 1);





full_data <- read.csv("data/wing_loadings.csv");
# Just get the SO1 and SO2 species of interest
dat <- full_data[full_data$Species == "SO1",];

hl  <- dat$Head_Length_mm;




iter            <- 1000;
resampled_means <- NULL;
N_wasps         <- length(hl);

resampled_mn <- rep(x = NA, length = iter);
for(i in 1:iter){
  resampled_hl    <- sample(x = hl, size = length(hl), replace = TRUE);
  resampled_mn[i] <- mean(resampled_hl);
}
sd_resampled <- sd(resampled_mn);
LCI <- mean(hl) - 1.96 * sd_resampled;
UCI <- mean(hl) + 1.96 * sd_resampled;




iter            <- 1000;
resampled_means <- NULL;
N_wasps         <- length(hl);

resampled_mn <- rep(x = NA, length = iter);
for(i in 1:iter){
  resampled_hl    <- sample(x = hl, size = length(hl), replace = TRUE);
  resampled_mn[i] <- mean(resampled_hl);
}
srt  <- sort(resampled_mn);
LCI <- srt[50];
UCI <- srt[950];


  
  
  