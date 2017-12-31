library(tidyverse)
library(dplyr)
library(dbscan)
DIR <- "C:/Users/mjohn/Desktop/gen3/trax/"
NUM_WASPS <- 3 #input number of wasps in trax file
CONE <- 0.5 #angle of chase threshold
BOX <- 70 #pixel width of box of chase
SPD <- 2 #speed in pixel distance per frame
#open and read text file and reformats it to a tibble
setwd(DIR)
dat <- list.files(pattern="*.txt") %>% 
  map(read.table, header = TRUE) %>% 
  map(as_tibble) %>% map(na.omit)
#computes dx and dy and rad/speed calculations
dat_rad <- list()
dat_spd <- list()
cat <- list()
cat <- lapply(seq_along(dat), function(k) {
  n <- 1:NUM_WASPS
  x<-n+(2*(n-1))
  y<-x+1
  dy <- as.matrix(dat[[k]][-1,y]-dat[[k]][-nrow(dat[[k]]),y])
  dx <- as.matrix(dat[[k]][-1,x]-dat[[k]][-nrow(dat[[k]]),x])
  dat_rad[[k]] <- as_tibble(atan2(dy, dx))
  dat_spd[[k]] <- as_tibble(sqrt(abs(dx^2)+abs(dy^2)))
  return(cbind(dat_rad[[k]],dat_spd[[k]]))
})
dat_rad <- lapply(cat, function(x) {x[,1:NUM_WASPS]})
dat_spd <- lapply(cat, function(x) {x[,(NUM_WASPS+1):ncol(x)]})
#Ensures parameters are met
chase <- vector('list', length(dat))
for (i in 1:length(dat)){
  combn_index <- combn(1:NUM_WASPS,2)
  chase[[i]] <- apply(combn_index, 2, function(x) {
    x1<-x[1]+(2*(x[1]-1))
    y1<-x1+1  
    x2<-x[2]+(2*(x[2]-1))
    y2<-x2+1
    p1<-x[1]*3
    p2<-x[2]*3
    f1 <- mutate(dat[[i]][-1,], idx = 2:nrow(dat[[i]])) %>%
      filter(
        abs(dat_rad[[i]][x[1]] - dat_rad[[i]][x[2]]) < CONE &
          dat_spd[[i]][x[1]] > SPD &
          dat_spd[[i]][x[2]] > SPD &
          abs(as.matrix(dat[[i]])[-1,x1] - as.matrix(dat[[i]])[-1,x2]) < BOX &
          abs(as.matrix(dat[[i]])[-1,y1] - as.matrix(dat[[i]])[-1,y2]) < BOX) %>%
      select(idx,x1,y1,p1,x2,y2,p2)
  })
}
#clusters
cluster <- list()
for (k in seq_along(chase)){ 
  for (kk in seq_along(chase[[k]])){
    idx <- chase[[k]][[kk]][1]
    chase.y <- dat_spd[[k]][as.matrix(idx), kk]
    if (nrow(idx)!=0){
      t <- as.data.frame(idx, chase.y)
      name <- list.files(pattern="*.txt")[k]
      cluster[[name]][[kk]] <-dbscan(as.matrix(t), eps = 25, minPts = 15, borderPoints = T)["cluster"]
    } else {
      name <- list.files(pattern="*.txt")[k]
      cluster[[name]][[kk]] <- NA
    }
    #name <- list.files(pattern="*.txt")[k]
    #scan[[name]][[kk]] <- as.matrix(cbind(idx, chase.y))
  }
}
#master data frame with location, cluster, and prob
master <- list()
for (k in seq_along(chase)){
  for (kk in seq_along(chase[[k]])){
    if (nrow(chase[[k]][[kk]])!=0){
      name <- list.files(pattern="*.txt")[k]
      master[[name]][[kk]] <- cbind(cluster[[k]][[kk]], chase[[k]][[kk]])
    }
  }
}
#graph for verification
library(ggplot2)
ggplot(data = master[[3]][[3]]) +
  geom_point(mapping = aes(x=X2, y=Y2, color = "red")) +
  geom_point(mapping = aes(x=X3, y=Y3, color = "blue")) +
  facet_wrap(~cluster)
#computer distances
grp <- vector('list', length(master))
for (k in seq_along(master)){
  for (kk in seq_along(master[[k]])){
    if(is.null(master[[k]][[kk]])){
      NULL
    } else {
      grp[[k]][[kk]] <- split(master[[k]][[kk]], master[[k]][[kk]][1])
    }
  }
}

distance <- vector('list', length(grp))
for (i in seq_along(grp)){
  for (ii in seq_along(grp[[i]])){
    if (!is.null(grp[[i]][[ii]])){
      distance[[i]][[ii]] <- vector('list', length(grp[[i]]))
      for (iii in seq_along(grp[[i]][[ii]])){
        f1 <- grp[[i]][[ii]][[iii]]
        w1 <- f1[,3:4]
        w2 <- f1[,6:7]
        c1 <- abs(w1-w2)^2
        c2 <- sum(sqrt(c1[,1]+c1[,2]))
        name <- list.files(pattern="*.txt")[i]
        distance[[i]][[ii]][[iii]] <- c2
      }
    } else {NULL}
  }
}
library(reshape2)
melt(unlist(distance))


#density plot of distances
den <- as.data.frame(melt(unlist(distance)))
ggplot(den, aes(x=value)) + geom_density()
