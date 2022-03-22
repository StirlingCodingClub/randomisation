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


  
  
  