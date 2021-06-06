
rm(list=ls())
setwd('E:\\teeth\\data')
library(data.table)
library(dplyr)
load("C:/Users/Admin/Documents/WeChat Files/wxid_npl1fprtj8rc12/FileStorage/File/2021-05/test(2).rda")

eva <- function(y,x){
  k <- sample(1:nrow(x),replace=T)
  sapply(1:100,function(s){
    temp <- sapply(1:ncol(y),function(j){
      ifile <- data.frame(y=y[,j],x)  
      model <- lm(y~.,data=ifile[k,])
      p <- as.matrix(cbind(1,ifile[,-1])) %*% cbind(coef(model))
      c(
        e.train=sum((y[unique(k),j]-p[unique(k)])^2),
        a.train=sum(y[unique(k),j]^2),
        e.test=sum((y[-unique(k),j]-p[-unique(k)])^2),
        a.test=sum(y[-unique(k),j]^2)
      )
    }) %>% rowSums
    c(train=1-as.numeric(temp[1]/temp[2]),test=1-as.numeric(temp[3]/temp[4]))
  })
}

eva2 <- function(y){
  y.cards <- unique(y)
  x.cards <- (1:14)[-y.cards]
  y <- do.call(cbind,cards[y.cards])
  x <- do.call(cbind,cards[x.cards])
  rlt <- list(
    y=y.cards,rlt=rowMeans(eva(y,x))
  )
  print(rlt)
  return(rlt)
}

test2 <- lapply(do.call(c,apply(combn(1:14,2),2,list)),eva2)
test3 <- lapply(do.call(c,apply(combn(1:14,3),2,list)),eva2)
test7 <- lapply(do.call(c,apply(combn(1:14,7),2,list)),eva2)
save(test2,test3,test7,file='test_missing.rda')
test11 <- lapply(do.call(c,apply(combn(1:14,11),2,list)),eva2)
test13 <- lapply(do.call(c,apply(combn(1:14,13),2,list)),eva2)

##########################

rm(list=ls())
setwd('E:\\teeth\\data')
library(data.table)
library(dplyr)
load("C:/Users/Admin/Documents/WeChat Files/wxid_npl1fprtj8rc12/FileStorage/File/2021-05/test(2).rda")

eva <- function(y,init){
  c1 <- do.call(cbind,cards[y])+1
  c2 <- do.call(cbind,cards[-y])+1
  rlti <- do.call(rbind,lapply(1:init,function(k){
    set.seed(k)
    j <- sample(1:nrow(c1),replace=T)
    rlti <- sapply(1:ncol(c1),function(i){
      model <- lm(log(c1[j,i])~log(c2[j,]))
      p <- exp(cbind(1,log(c2)) %*% cbind(coef(model)))-1
      c1 <- c1-1
      c(error=sum((p[-unique(j)]-c1[-unique(j),i])^2),
        actual=sum(c1[-unique(j),i]^2))
    })
    rlti
  }))
  rlti <- apply(rlti,2,function(x){tapply(x,rownames(rlti),sum)})
  rlti <- list(y=y,
               rlti=1-rlti[2,]/rlti[1,],
               rlt=1-sum(rlti[2,])/sum(rlti[1,]))
  print(rlti)
  rlti
}
eva2 <- function(y2go,init,pick=100){
  y2go <- combn(1:14,y2go)
  if(ncol(y2go)>pick){
    y2go <- y2go[,sort(sample(1:ncol(y2go),pick)),drop=F]
  }
  apply(y2go,2,eva,init=init)
}
system.time(test <- lapply(1:13,eva2,init=100,pick=100))


###########################

rm(list=ls())
setwd('E:\\teeth\\data')
library(data.table)
library(dplyr)
library(ggplot2)
load("C:/Users/Admin/Documents/WeChat Files/wxid_npl1fprtj8rc12/FileStorage/File/2021-05/test(2).rda")
load('test_missing.rda')
x <- test[[13]]
for(i in 1:length(x)){x[[i]]$y <- (1:14)[-x[[i]]$y]}
x <- x[14:1]
out1 <- sapply(1:14,function(i){
  out <- rep(NA,14*3)
  out[-((i-1)*3+1:3)] <- x[[i]]$rlti
  out
})
rownames(out1) <- colnames(do.call(cbind,cards))
colnames(out1) <- gsub('_g','',rownames(out1)[1:14*3-2])

ggplot(data = reshape::melt(out1), aes(x=X2, y=X1, fill=value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(0,1), space = "Lab") +
  labs(x='',y='',fill='')
write.csv(out1,'rgb_prediction.csv')

ggplot()+
  geom_line(
    data=data.frame(cbind(1:13,t(sapply(test,function(x){
      c(Average_Score=mean(unlist(lapply(x,function(x){x$rlti}))),
        Total_Score=mean(unlist(lapply(x,function(x){x$rlt}))))
      }))))%>%reshape::melt(id=1),aes(x=V1,y=value,colour=variable),
    size=1) +
  labs(x="#Missing Blocks",y='1-error^2/actual^2',colour='')

############################

rm(list=ls())
setwd('E:\\teeth\\data')
library(data.table)
library(dplyr)
library(randomForest)
load("C:/Users/Admin/Documents/WeChat Files/wxid_npl1fprtj8rc12/FileStorage/File/2021-05/x2model(2).rda")

id.x <- sapply(strsplit(x$file,'/'),function(x){paste(x[-2],collapse='/')})
id.y <- paste0(
  sapply(strsplit(y$id,'@'),function(x){paste(x[-2],collapse='/')}),'.')
id <- id.x[id.x%in%id.y]
x <- x[match(id,id.x),-1]
y <- y[match(id,id.y),-1:-4]

#

eva <- function(y,init){
  c1 <- do.call(cbind,cards[y])+1
  c2 <- do.call(cbind,cards[-y])+1
  rlti <- do.call(rbind,lapply(1:init,function(k){
    set.seed(k)
    j <- sample(1:nrow(c1),replace=T)
    rlti <- sapply(1:ncol(c1),function(i){
      model <- lm(log(c1[j,i])~log(c2[j,]))
      p <- exp(cbind(1,log(c2)) %*% cbind(coef(model)))-1
      c1 <- c1-1
      c(error=sum((p[-unique(j)]-c1[-unique(j),i])^2),
        actual=sum(c1[-unique(j),i]^2))
    })
    rlti
  }))
  rlti <- apply(rlti,2,function(x){tapply(x,rownames(rlti),sum)})
  rlti <- list(y=y,
               rlti=1-rlti[2,]/rlti[1,],
               rlt=1-sum(rlti[2,])/sum(rlti[1,]))
  print(rlti)
  rlti
}
eva2 <- function(y2go,init,pick=100){
  y2go <- combn(1:14,y2go)
  if(ncol(y2go)>pick){
    y2go <- y2go[,sort(sample(1:ncol(y2go),pick)),drop=F]
  }
  apply(y2go,2,eva,init=init)
}
pca <- function(A){
  A <- scale(A)
  A.svd <- svd(A)
  d <- A.svd$d
  r <- length(d)
  prop <- d^2; info <- sum(prop)/sum(A.svd$d^2);prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  rlt <- list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop,info=info)
  return(rlt)
}

cards <- lapply(1:15,function(i){x[,1:3+(i-1)*3]})
cards.pca <- sapply(lapply(cards,pca),function(x){x$X[,1]})

test <- randomForest(y~.,data=data.frame(y=y$wio,do.call(cbind,cards)))
test

############################

rm(list=ls())
setwd('E:\\teeth\\data')
library(data.table)
library(dplyr)
library(randomForest)
library(ggplot2)
load("C:/Users/Admin/Documents/WeChat Files/wxid_npl1fprtj8rc12/FileStorage/File/2021-05/x2model(2).rda")
stdcard <- read.csv('STDCARD.csv')
save(p,stdcard,x,y,file='data4lab.rda')

id.x <- x$file
id.y <- paste0(gsub('@','/',y$id),'.')
id <- id.x[id.x%in%id.y]
x <- x[match(id,id.x),-1]
y <- y[match(id,id.y),-1:-4]

#############
# Alignment
#############

unique(sapply(strsplit(id,'/'),function(x){x[1]}))
unique(p$usr_id)

p <- mutate(p,id=gsub('.JPG','',paste0(usr_id,'/',date,'/',image_name,'.')))
id2 <- sapply(strsplit(id,'/'),function(x){paste0(x[1],'/',x[3],'/',x[length(x)])})
sum(p$id%in%id2)
p <- p[match(id2,p$id),]

raw <- data.frame(p%>%select(id,gt_w,pred_res_float,E_float),x,y) %>% 
  filter(!is.na(id))
# unique(cbind(p$gt_w,y))[!is.na(unique(cbind(p$gt_w,y)))[,1],]

x <- raw[,5:49]
y <- raw[,50:53]
for(i in 1:14){
  x[,i*3+1:3] <- x[,i*3+1:3]/x[,1:3]
}


#############
# Modeling division
#############

x <- x[,-1:-3]
eva <- function(m){
  xi <- rep((m-1)*3,each=3)+1:3
  xi <- x[,-xi,drop=F]
  pre <- predict(lm(as.matrix(y)~as.matrix(xi)))
  pra <- predict(lm(as.matrix(y)~as.matrix(x)))
  pra <- sqrt(rowSums(((pra-y)^2)[,1:3]))
  pre <- sqrt(rowSums(((pre-y)^2)[,1:3]))
  raw$E_float*pre/pra
}
eva2 <- function(n){
  print(n)
  mlist <- combn(14,n)
  test <- apply(mlist,2,eva)
  list(mlist=mlist,rlt=test)
}
test <- lapply(1:13,eva2)

rlt1 <- cbind(summary(raw$E_float),
              sapply(test,function(x){summary(as.vector(x$rlt))}))

rlt2 <- sapply(1:14,function(i){
  sapply(test,function(j){
    mean(j$rlt[,colSums((j$mlist)==i)>0])
  })
})
rlt2 <- rbind(mean(raw$E_float),rlt2)
dimnames(rlt2) <- list(0:13,gsub('_g','',colnames(x)[0:13*3+1]))

rlt3 <- sapply(1:14,function(i){
  sapply(test,function(j){
    median(j$rlt[,colSums((j$mlist)==i)>0])
  })
})
rlt3 <- rbind(median(raw$E_float),rlt3)
dimnames(rlt3) <- list(0:13,gsub('_g','',colnames(x)[0:13*3+1]))

ggplot() +
  geom_line(data=reshape::melt(t(rlt1[2:5,])),
            aes(x=X1-1,y=value,colour=X2)) +
  labs(x='#Missing Blocks',y='Error Distance',colour='')

ggplot() +
  geom_line(data=reshape::melt(rlt3),
            aes(x=X1,y=value,colour=X2)) +
  labs(x='#Missing Blocks',y='Error Distance (Median)',colour='Include Colour')

ggplot() +
  geom_line(data=reshape::melt(rlt2),
            aes(x=X1,y=value,colour=X2)) +
  labs(x='#Missing Blocks',y='Error Distance (Mean)',colour='Include Colour')

#############
# Modeling
#############

eva <- function(m){
  xi <- rep(m*3,each=3)+1:3
  xi <- x[,-xi,drop=F]
  pre <- predict(lm(as.matrix(y)~as.matrix(xi)))
  pra <- predict(lm(as.matrix(y)~as.matrix(x)))
  pra <- sqrt(rowSums(((pra-y)^2)[,1:3]))
  pre <- sqrt(rowSums(((pre-y)^2)[,1:3]))
  raw$E_float*pre/pra
}
eva2 <- function(n){
  print(n)
  mlist <- combn(14,n)
  test <- apply(mlist,2,eva)
  list(mlist=mlist,rlt=test)
}
test <- lapply(1:13,eva2)

rlt1 <- cbind(summary(raw$E_float),
              sapply(test,function(x){summary(as.vector(x$rlt))}))

rlt2 <- sapply(1:14,function(i){
  sapply(test,function(j){
    mean(j$rlt[,colSums((j$mlist)==i)>0])
  })
})
rlt2 <- rbind(mean(raw$E_float),rlt2)
dimnames(rlt2) <- list(0:13,gsub('_g','',colnames(x)[1:14*3+1]))

rlt3 <- sapply(1:14,function(i){
  sapply(test,function(j){
    median(j$rlt[,colSums((j$mlist)==i)>0])
  })
})
rlt3 <- rbind(median(raw$E_float),rlt3)
dimnames(rlt3) <- list(0:13,gsub('_g','',colnames(x)[1:14*3+1]))

ggplot() +
  geom_line(data=reshape::melt(t(rlt1[2:5,])),
            aes(x=X1-1,y=value,colour=X2)) +
  labs(x='#Missing Blocks',y='Error Distance',colour='')

ggplot() +
  geom_line(data=reshape::melt(rlt3),
            aes(x=X1,y=value,colour=X2)) +
  labs(x='#Missing Blocks',y='Error Distance (Median)',colour='Include Colour')

reshape::melt(rlt2) %>% 
  group_by(X2) %>% 
  summarise(value=mean(value)) %>%
  arrange(value)
reshape::melt(rlt3) %>% 
  group_by(X2) %>% 
  summarise(value=mean(value)) %>% 
  arrange(value)

cbind(sort(rowMeans(apply(rlt2,1,rank))))
cbind(sort(rowMeans(apply(rlt3,1,rank))))
cbind(sort(sort(colMeans(rlt3))))
cbind(sort(sort(colMeans(rlt2))))
