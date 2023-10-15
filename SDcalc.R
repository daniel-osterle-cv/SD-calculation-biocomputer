setwd("yourdirectory")
dat.workspace <- "SDcalc.RData"
load(paste(getwd(),dat.workspace,sep="/"))

#function to get alternative, better and exact sd for N numbers
#returns vector of length 3, containing above mention sd values
get.alt.sd <- function(N=100,min=1,max=10){
  #browser()
 x <- runif(N,min,max) #generate random numbers
 j=2
 diff.num <- numeric(length(x)-1) #initialize vector
 
 #calculate diff's b/w 2 sequential values
 for(i in 1:(length(x)-1)){
   diff.num[i] <- abs(x[j]-x[i])
   j <- j+1
 }
 
 better.diff <- matrix(data=0,nrow=N,ncol=N) #initialize matrix
 #calculate more precise sd
 for(i in 1:(length(x)-1)){
   for(j in (i+1):length(x)) better.diff[j-1,i] <- abs(x[i]-x[j])
 }
   
 alt.sd <- sum(diff.num)
 better.sd <- sum(better.diff)
 real.sd <- sd(x)
 c(alt.sd=alt.sd,better.sd=better.sd,real.sd=real.sd)
}

#function to sample the sd for N.num numbers N.sample times
#(e.g. get.alt.sd() calculates the sd's for 100 numbers, this function repeats this 100 times)
#output is a matrix with 3 columns for different sd types and N.sample rows
sample.sd <- function(N.num=100,min=1,max=10,N.sample=100){
  #browser()
  m.sd <- matrix(data=0,nrow=N.sample,ncol=3)
  for(i in 1:N.sample) m.sd[i,] <- get.alt.sd(N=N.num,min=min,max=max)
  m.sd
  
}

#calculate the correlation between two vectors containing sd values, repeat N.cor times
#output: matrix containing the correlation alt/exact better/exact and cor(better/exact)-cor(alt/exact)
sample.cor <- function(N.cor=100,...){
  m.cor <- matrix(data=0,nrow=N.cor,ncol=3)
  for(i in 1:N.cor){
    m.sd <- sample.sd(...)
    m.cor[i,1] <- cor(m.sd[,1],m.sd[,3])
    m.cor[i,2] <- cor(m.sd[,2],m.sd[,3])
    m.cor[i,3] <- 1-m.cor[i,1]
  }
  colnames(m.cor) <- c("Cor Alt/Real","Cor Better/Real","Diff Cor Alt")
  m.cor
}

#probe the correlation for different amounts of numbers
#output: matrix with sample.cor()-matrix for different numbers
test.size <- function(N.cor=100){
  #browser()
  m.all <- matrix(data=0,nrow=N.cor,ncol=57)
  #m.diff <- matrix(data=0,nrow=N.cor,ncol=19)
  
    j <- c(1:9,seq(10,100,by = 10))
    k <- 1
  for(i in 1:length(j)){
    m.all[,c(k:(k+2))] <- sample.cor(N.cor,N.num=j[i])
    #m.diff[,i] <- m.all[,(k+2)]
    k <- k+3
  }
  m.all
}
#transfer diff's to separate matrix
k <- 1
m.diff <- matrix(data=0,nrow=100,ncol=19)
for(i in 1:19){
  m.diff[,i] <- m.cor.and.diff[,(k+2)]
  k <- k+3
}
colnames(m.diff) <- c(1:9,seq(10,100,by = 10))

k <- 1
m.alt.sd.size <- matrix(data=0,nrow=100,ncol=19)
for(i in 1:19){
  m.alt.sd.size[,i] <- m.cor.and.diff[,k]
  k <- k+3
}
colnames(m.alt.sd.size) <- c(1:9,seq(10,100,by = 10))

pdf(paste(getwd(),"sd.correlations.pdf",sep="/"))
par(mfrow=c(2,2))
#!!technically, the correlation in the title is not the exact correlation for the plot
plot(sample.sd()[,c(1,3)],main="Alternative vs Exact sd \n 100 numbers",xlab="Alternative sd",ylab="Exact sd")
legend(x = "bottomright",legend = paste("Correlation:",round(cor(sample.sd()[,c(1,3)])[3],2)))

plot(sample.sd(N.num=10)[,c(1,3)],main="Alternative vs Exact sd \n 10 numbers",xlab="Alternative sd",ylab="Exact sd")
legend(x = "bottomright",legend = paste("Correlation:",round(cor(sample.sd(N.num=10)[,c(1,3)])[3],2)))

plot(sample.sd(N.num=5)[,c(1,3)],main="Alternative vs Exact sd \n 5 numbers",xlab="Alternative sd",ylab="Exact sd")
legend(x = "bottomright",legend = paste("Correlation:",round(cor(sample.sd(N.num=5)[,c(1,3)])[3],2)))

plot(sample.sd(N.num=3)[,c(1,3)],main="Alternative vs Exact sd \n 3 numbers",xlab="Alternative sd",ylab="Exact sd")
legend(x = "bottomright",legend = paste("Correlation:",round(cor(sample.sd(N.num=3)[,c(1,3)])[3],2)))

par(mfrow=c(1,1))
boxplot(m.alt.sd.size,
        xlab="x=Amount of Numbers",
        ylab="Correlation",
        main="Correlation Alternative/Exact sd")
abline(h=0.8,col="red")

boxplot(m.diff,
        xlab="Amount of Numbers",
        ylab="1-cor(Alt/Exact sd)",
        main="Difference to perfect Correlation(1)")

abline(h=0.2,col="red")


dev.off()

save.image(paste(getwd(),dat.workspace,sep="/"))
