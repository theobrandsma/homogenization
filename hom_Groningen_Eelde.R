#-------------------------------------------------------------------------------
# Homogenization of station Groningen/Eelde using quantile matching
#-------------------------------------------------------------------------------

rm(list=ls(all = TRUE))


# Import the data --------------------------------------------------------------

Eelde_prehom <- read.table(file="Eelde_prehom.txt",header=TRUE,skip=0,sep=',')
GroningenEelde_overlap <- read.table(file="GroningenEelde_overlap.txt",header=TRUE,skip=0,sep=',')


# Calculate the smoothed quantiles ---------------------------------------------

# TN
# put the overlap data in one file (first Tn, later Tx and Tg), with year month day T_pre, T_post 
x <- GroningenEelde_overlap
x <- x[,c(1,3,4,5,7)]
x <- x[x[,2] >= 1946 & x[,2] <= 1951,]
x[,5] <- x[,5]/10
x <- cbind(x[x[,1]==6,2:5],x[x[,1]==4,5])
names(x)[4:5] <- c('Tpre','Tpost')


# calculate for each month the quantiles and their difference (old-new) and the smooth
yout <- data.frame(matrix(rep(0,12*19*6),nc=6,nr=19*12))
names(yout) <- c('mon','quan','T.pre','T.after','jump','jump.sm')
quan <- seq(0.05,0.95,0.05)
yout$quan <- as.integer(rep(quan,12)*100)
yout$mon <- rep(1:12,rep(19,12))

for(i in 1:12){
  ind <- yout[,1] ==i
  yout[ind,3] <- quantile(x[x[,2]==i,4],probs=quan)
  yout[ind,4] <- quantile(x[x[,2]==i,5],probs=quan)
  yout[ind,5] <- yout[ind,3]-yout[ind,4]
  y <- loess(yout[ind,5]~yout[ind,2],span=0.6,degree=2,family="gaussian")
  yout[ind,6] <- y$fitted
 }


# smooth each quantile between months
x <- yout
# add months ND as -1 and 0 and JF as 13 and 14
y1 <- x[x[,1]==11,]; y1[,1] <- -1
y2 <- x[x[,1]==12,]; y2[,1] <- 0
y3 <- x[x[,1]==1,]; y3[,1] <- 13
y4 <- x[x[,1]==2,]; y4[,1] <- 14
x <-rbind(y1,y2,x,y3,y4)

# add the smoothed jump to yout as a new column
x <- cbind(x,x[,6])
names(x)[7] <- 'jump.sm2'
for(i in 1:19){
  qa <- 5*i
  yi <- x[,2]==qa
  out <- loess(x[yi,6]~x[yi,1],span=0.6,degree=2,family="gaussian")
  x[yi,7] <- out$fitted
}

x <- x[x[,1]>= 1 & x[,1] <= 12,]
yout.EeldeTn.v0 <- x

# TX
# put the overlap data in one file (first Tn, later Tx and Tg), with year month day T_pre, T_post 
x <- GroningenEelde_overlap
x <- x[,c(1,3,4,5,8)]
x <- x[x[,2] >= 1946 & x[,2] <= 1951,]
x[,5] <- x[,5]/10
x <- cbind(x[x[,1]==6,2:5],x[x[,1]==4,5])
names(x)[4:5] <- c('Tpre','Tpost')

# calculate for each month the quantiles and their difference (old-new) and the smooth 
yout <- data.frame(matrix(rep(0,12*19*6),nc=6,nr=19*12))
names(yout) <- c('mon','quan','T.pre','T.after','jump','jump.sm')
quan <- seq(0.05,0.95,0.05)
yout$quan <- as.integer(rep(quan,12)*100)
yout$mon <- rep(1:12,rep(19,12))

for(i in 1:12){
  ind <- yout[,1] ==i
  yout[ind,3] <- quantile(x[x[,2]==i,4],probs=quan)
  yout[ind,4] <- quantile(x[x[,2]==i,5],probs=quan)
  yout[ind,5] <- yout[ind,3]-yout[ind,4]
  y <- loess(yout[ind,5]~yout[ind,2],span=0.6,degree=2,family="gaussian")
  yout[ind,6] <- y$fitted
}

# smooth each quantile between months
x <- yout
# add months ND as -1 and 0 and JF as 13 and 14
y1 <- x[x[,1]==11,]; y1[,1] <- -1
y2 <- x[x[,1]==12,]; y2[,1] <- 0
y3 <- x[x[,1]==1,]; y3[,1] <- 13
y4 <- x[x[,1]==2,]; y4[,1] <- 14
x <-rbind(y1,y2,x,y3,y4)

# add the smoothed jump to yout as a new column
x <- cbind(x,x[,6])
names(x)[7] <- 'jump.sm2'
for(i in 1:19){
  qa <- 5*i
  yi <- x[,2]==qa
  out <- loess(x[yi,6]~x[yi,1],span=0.6,degree=2,family="gaussian")
  x[yi,7] <- out$fitted
}

x <- x[x[,1]>= 1 & x[,1] <= 12,]
yout.EeldeTx.v0 <- x


# TG
# put the overlap data in one file (first Tn, later Tx and Tg), with year month day T_pre, T_post 
x <- GroningenEelde_overlap
x <- x[,c(1,3,4,5,6)]
x <- x[x[,2] >= 1946 & x[,2] <= 1951,]
x[,5] <- x[,5]/10
x <- cbind(x[x[,1]==6,2:5],x[x[,1]==4,5])
names(x)[4:5] <- c('Tpre','Tpost')


# calculate for each month the quantiles and their difference (old-new) and the smooth and plot
yout <- data.frame(matrix(rep(0,12*19*6),nc=6,nr=19*12))
names(yout) <- c('mon','quan','T.pre','T.after','jump','jump.sm')
quan <- seq(0.05,0.95,0.05)
yout$quan <- as.integer(rep(quan,12)*100)
yout$mon <- rep(1:12,rep(19,12))

for(i in 1:12){
  ind <- yout[,1] ==i
  yout[ind,3] <- quantile(x[x[,2]==i,4],probs=quan)
  yout[ind,4] <- quantile(x[x[,2]==i,5],probs=quan)
  yout[ind,5] <- yout[ind,3]-yout[ind,4]
  y <- loess(yout[ind,5]~yout[ind,2],span=0.6,degree=2,family="gaussian")
  yout[ind,6] <- y$fitted
}

# smooth each quantile between months
x <- yout
# add months ND as -1 and 0 and JF as 13 and 14
y1 <- x[x[,1]==11,]; y1[,1] <- -1
y2 <- x[x[,1]==12,]; y2[,1] <- 0
y3 <- x[x[,1]==1,]; y3[,1] <- 13
y4 <- x[x[,1]==2,]; y4[,1] <- 14
x <-rbind(y1,y2,x,y3,y4)

# add the smoothed jump to yout as a new column
x <- cbind(x,x[,6])
names(x)[7] <- 'jump.sm2'
for(i in 1:19){
  qa <- 5*i
  yi <- x[,2]==qa
  out <- loess(x[yi,6]~x[yi,1],span=0.6,degree=2,family="gaussian")
  x[yi,7] <- out$fitted
}

x <- x[x[,1]>= 1 & x[,1] <= 12,]

yout.EeldeTg.v0 <- x


# Calculate the homogenized series --------------------------------------------- 

# correct the Groningen part 
x <- GroningenEelde_overlap
x <- x[x[,1]==6,]
x <- x[,c(3,4,5,6,7,8)]
x[,4:6] <- x[,4:6]/10
x <- cbind(x,x[,4:6])
names(x)[7:9] <- c('Tg.hom','Tn.hom','Tx.hom')

# remove the januari 1952 data from the file
x <- x[x[,1] <= 1951,]

# make the correction using the data in yout.EeldeTg.v0, yout.EeldeTn.v0, yout.EeldeTx.v0
# TG
y <- yout.EeldeTg.v0
for(i in 1:12){
  ind <- x[,2] == i
  yquan <- quantile(x[ind,4],probs=c(1:19/20))
  yquan <- as.numeric(yquan)
  dT <- approx(yquan,y[y[,1]==i,7],x[ind,4],rule=2)$y
  x[ind,7] <- x[ind,7] - dT
}

# TN
y <- yout.EeldeTn.v0
for(i in 1:12){
  ind <- x[,2] == i
  yquan <- quantile(x[ind,5],probs=c(1:19/20))
  yquan <- as.numeric(yquan)
  dT <- approx(yquan,y[y[,1]==i,7],x[ind,5],rule=2)$y
  x[ind,8] <- x[ind,8] - dT
}
# TX
y <- yout.EeldeTx.v0
for(i in 1:12){
  ind <- x[,2] == i
  yquan <- quantile(x[ind,6],probs=c(1:19/20))
  yquan <- as.numeric(yquan)
  dT <- approx(yquan,y[y[,1]==i,7],x[ind,6],rule=2)$y
  x[ind,9] <- x[ind,9] - dT
}

# combine Groningen and Eelde
x1 <- x
x1 <- x1[x1[,1] <= 1950,]

x2 <- Eelde_prehom
x2 <- x2[,c(3:8)]
x2 <- x2[x2[,1] >= 1951,]
x2 <- cbind(x2,x2[,4:6])
names(x2)[7:9] <- c('Tg.hom','Tn.hom','Tx.hom')

x <- rbind(x1,x2)
x$statno <- 280
x <- x[,c("statno","year","month","day","Tg","Tn","Tx","Tg.hom","Tn.hom","Tx.hom")]
Eelde_hom_v0 <- round(x,1)


# Write the homogenized file to disk--------------------------------------------

write.table(Eelde_hom_v0, file = "Eelde_hom_v0.txt", sep = ",", row.names = FALSE)




