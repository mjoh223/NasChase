library(tidyverse)
DIR <- "C:/Users/mjohn/Desktop/gen3/trax/" #directory of trax file
NUM_WASPS <- 3 #input number of wasps in trax file
CONE <- 0.9 #angle of chase threshold
BOX <- 30 #pixel width of box of chase
SPD <- 2 #speed in pixel distance per frame
#open and read text file and reformats it to a tibble
setwd(DIR)
dat <- list.files(pattern="*.txt") %>% map(read.table, header = TRUE) %>% map(as_tibble) %>% map(na.omit)
nan_dat <- list.files(pattern="*.txt") %>% map(read.table, header = TRUE) %>% map(as_tibble)
dat_rad <- list()
dat_spd <- list()
for (i in 1:length(dat)){
  name <- list.files(pattern="*.txt")[i]
  dat_rad[[name]] <- transmute(dat[[i]], 
                rad1 = atan2(Y1[-1]-Y1, X1[-1]-X1),
                rad2 = atan2(Y2[-1]-Y2, X2[-1]-X2),
                rad3 = atan2(Y3[-1]-Y3, X3[-1]-X3))
  dat_spd[[name]] <- transmute(dat[[i]],       
                spd1 = sqrt(abs(X1[-1]-X1)^2+abs(Y1[-1]-Y1)^2), #For some reason this gives a really high value for the last row, try removing that row
                spd2 = sqrt(abs(X2[-1]-X2)^2+abs(Y2[-1]-Y2)^2),
                spd3 = sqrt(abs(X3[-1]-X3)^2+abs(Y3[-1]-Y3)^2))
}
chase <- list()
for (i in 1:length(dat)){
  combn_index <- combn(1:NUM_WASPS,2)
  name <- list.files(pattern="*.txt")[i]
  chase[[name]] <- apply(combn_index, 2, function(x) {
    x1<-x[1]+(2*(x[1]-1))
    y1<-x1+1
    x2<-x[2]+(2*(x[2]-1))
    y2<-x2+1
    ifelse(abs(dat_rad[[i]][x[1]] - dat_rad[[i]][x[2]]) < CONE & 
             abs(dat[[i]][x1]-dat[[i]][x2]) < BOX &
             abs(dat[[i]][y1]-dat[[i]][y2]) < BOX &
             dat_spd[[i]][x[1]] > SPD &
             dat_spd[[i]][x[2]] > SPD, TRUE, FALSE)
  })
  colnames(chase[[name]]) <- c(paste0(combn_index[1,1], "and", combn_index[2,1], sep=""), paste0(combn_index[1,2], "and", combn_index[2,2], sep=""), paste0(combn_index[1,3], "and", combn_index[2,3], sep=""))
}

lapply(chase, summary)
lapply(chase, function(x) length(which(x[,1]==TRUE))) #1and2
lapply(chase, function(x) length(which(x[,2]==TRUE))) #1and3
lapply(chase, function(x) length(which(x[,3]==TRUE))) #2and3
which(chase$ma_choice1.txt[,3]==TRUE)+300


library(ggplot2)
plot1 <- filter(dat[[1]], chase$ma_choice1.txt[,3]==1)
plot2 <- filter(dat[[1]], chase$ma_choice1.txt[,3]==1)
ggplot(plot1, aes(X2,Y2)) +
  geom_point(size=1, color='red')+
  geom_point(data = plot2, aes(X3,Y3), size=1,color='blue')


#plot(dat_rad$experiment_1$X1,dat_rad$experiment_1$Y1 , type="l", asp = 1)
#length <- 0.2
#arrows(dat_rad$experiment_1$X1, dat_rad$experiment_1$Y1, x1=dat_rad$experiment_1$X1+length*cos(dat_rad$experiment_1$rad1), y1=dat_rad$experiment_1$Y1+length*sin((dat_rad$experiment_1$rad1)), 
#       length=0.05, col="Gray")
