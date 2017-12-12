library(tidyverse)
DIR <- "C:/Users/mjohn/Desktop/gen3/trax/" #directory of trax file
NUM_WASPS <- 3 #input number of wasps in trax file
CONE <- 0.9 #angle of chase threshold
BOX <- 100 #pixel width of box of chase
#open and read text file and reformats it to a tibble
setwd(DIR)
dat <- read.table("ma_choice1.txt", header = TRUE)
dat <- na.omit(dat)
dat <- as_tibble(dat)
#calculates angle of movement for each wasp
wasp_index <- 1:NUM_WASPS
dxy <- list()
rad <- list()
for (i in wasp_index){
    x<-i+(2*(i-1))
    y<-x+1
    a<-dat[x:y]
    name <- paste0("WASPID", i, sep = "")
    dxy[[name]] <- with(a, c(a[-1,]-a[-nrow(a),]))
    a<-as.numeric(unlist(dxy[[i]][1]))
    b<-as.numeric(unlist(dxy[[i]][2]))
    rad[[name]] <- atan2(b,a)
}
dxy <- as_tibble(dxy)
rad <- as_tibble(rad)
#Theta of all pairwise wasp combinations are compared to each other
chase <- list()
combn(1:length(rad), 2)
chase <- apply(combn(1:length(rad), 2), 2, function(x){
  x1<-x[1]+(2*(x[1]-1))
  x2<-x[2]+(2*(x[2]-1))
  y1<-x1+1
  y2<-x2+1
  name <- paste0("WASP", x[1], "to", x[2], sep = '')
  chase[[name]] <- which(abs(rad[x[1]] - rad[x[2]]) < CONE & 
        abs(dat[-1,x1] - dat[-1,x2]) < BOX & #x - x
        abs(dat[-1,y1] - dat[-1,y2]) < BOX) #y - y
  return(chase)
})
#fixes frame index if trajectory file began with NaN
if (as.numeric(row.names(dat)[1] != 1)) pad <- as.numeric(row.names(dat)[1])-1
for (i in 1:length(chase)){
  chase[[i]] <- as.numeric(unlist(chase[i]))+pad
}

as_tibble(bind_cols(dat[-1,],dat[-1,chase]))
x<-dat[unlist(chase[3]),1]
y<-dat[unlist(chase[3]),2]
p <- cbind(x,y)
library(ggplot2)
ggplot(p, aes(x,y)) +
  geom_point()


plot(x,y , type="l", asp = 1)
length <- 0.2
arrows(dat$X1, dat$Y1, x1=dat$X1+length*cos(rad$WASPID1), y1=dat$Y1+length*sin(rad$WASPID1), 
       length=0.05, col="Gray")
