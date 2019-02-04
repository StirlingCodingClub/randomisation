trees <- read.csv("FpetTrees.csv");



lat1 <-  28.289;
lat2 <-  28.2918;
lon1 <- -113.115;
lon2 <- -113.109;

plot(x = trees[,4], y = trees[,3], xlab = "Longitude", ylab = "Latitude",
     pch = 20, cex.lab = 1.25, cex.axis = 1.25, 
     xlim = c(lon1 - 0.001,  lon2 + 0.001), 
     ylim = c(lat1 - 0.0005, lat2 + 0.0005));

polygon(x = c(lon1, lon1, lon2, lon2),  y = c(lat1, lat2, lat2, lat1),
        col = "tan");
points(x = trees[,4], y = trees[,3], pch = 20);


iteration    <- 100;
N_trees      <- dim(trees)[1];
simulated_NN <- NULL;
while(iteration > 0){
    rand_lat  <- runif(n = N_trees, min = lat1, max = lat2);
    rand_lon  <- runif(n = N_trees, min = lon1, max = lon2);
    trees[,3] <- rand_lat;
    trees[,4] <- rand_lon;
    mean_NN   <- get_mean_nearest_neighbour(trees);
    simulated_NN[iteration] <- mean_NN;
    iteration <- iteration - 1;
}


iteration    <- 9;
N_trees      <- dim(trees)[1];
simulated_NN <- NULL;
par(mfrow = c(3, 3), mar = c(0, 0, 0, 0));
while(iteration > 0){
    rand_lat  <- runif(n = N_trees, min = lat1, max = lat2);
    rand_lon  <- runif(n = N_trees, min = lon1, max = lon2);
    trees[,3] <- rand_lat;
    trees[,4] <- rand_lon;
    mean_NN   <- get_mean_nearest_neighbour(trees);
    simulated_NN[iteration] <- mean_NN;
    plot(x = trees[,4], y = trees[,3], xlab = "", ylab = "",
         pch = 20, cex.lab = 1.25, cex.axis = 1.25, xaxt = "n", yaxt = "n",
         xlim = c(lon1 - 0.001,  lon2 + 0.001), 
         ylim = c(lat1 - 0.0005, lat2 + 0.0005));
    polygon(x = c(lon1, lon1, lon2, lon2),  y = c(lat1, lat2, lat2, lat1),
            col = "tan");
    points(x = trees[,4], y = trees[,3], pch = 20);
    iteration <- iteration - 1;
}





get_mean_nearest_neighbour <- function(trees, lat = 3, lon = 4){
    N_trees  <- dim(trees)[1];
    tree_lat <- trees[,lat];
    tree_lon <- trees[,lon];
    nearests <- rep(x = NA, times = N_trees);
    for(i in 1:N_trees){
        nearest_neigh <- Inf;
        for(j in 1:N_trees){
            if(i != j){
                diff_lat <- tree_lat[i] - tree_lat[j];
                diff_lon <- tree_lat[i] - tree_lat[j];
                sq_lat   <- diff_lat * diff_lat;
                sq_lon   <- diff_lon * diff_lon;
                neigh_d  <- sqrt(sq_lat + sq_lon);
                if(neigh_d < nearest_neigh){
                    nearest_neigh <- neigh_d;
                }
            }
        }
    nearests[i] <- nearest_neigh;
    }
    return(mean(nearests));
}