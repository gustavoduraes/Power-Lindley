require(TeachingDemos)
require(tidyverse)
require(latex2exp)
require(rjags)
require(plot3D)
require(rgl)
require(colorRamps)


lik <- function(y,alpha,beta,n){
  n*(log(alpha)+2*log(beta)-log(beta+1)) + sum(1+y**alpha)+(alpha-1)*sum(log(y))-beta*sum(y^alpha)
  
}
like = function(y,a,p) {
  like = 1
  for(obs in y){
    like = like*((p*(a*(p/(1-p))*obs^(a-1)*exp(-p/(1-p)*obs^a))+(1-p)*((a*(p/(1-p))**2*obs^(2*a-1)*exp(-p/(1-p)*obs^a)))))
  }
  return(like)
}

likelihood <- Vectorize(like,c("p"))


pl <- function(x,a,p){
  ((p*(a*(p/(1-p))*x^(a-1)*exp(-p/(1-p)*x^a))+(1-p)*((a*(p/(1-p))**2*x^(2*a-1)*exp(-p/(1-p)*x^a)))))
}




#### Gráficos PL
X=seq(0,5,by=0.01)
Exemplos <- data.frame(X,a1 =pl(X,1,0.5),a2 =pl(X,2,0.5),a5 =pl(X,5,0.5))
Ex <- Exemplos %>% gather(alpha,Y,-X)
p2 <- ggplot(Ex,aes(x=X,y=Y,lty=alpha))+geom_line()+theme_classic()+
  scale_linetype_discrete(name= element_blank(),labels = c(expression(alpha==1),expression(alpha==2),expression(alpha==5)))+
  xlab(TeX("$x$"))+ylab(TeX("$f(x)$"))+ggtitle(expression(delta==0.5))+theme(legend.text = element_text(size=15))

Exemplos <- data.frame(X,a1 =pl(X,1,0.25),a2 =pl(X,2,0.25),a5 =pl(X,5,0.25))
Ex <- Exemplos %>% gather(alpha,Y,-X)
p1 <- ggplot(Ex,aes(x=X,y=Y,lty=alpha))+geom_line()+theme_classic()+
  scale_linetype_discrete(name= element_blank(),labels = c(expression(alpha==1),expression(alpha==2),expression(alpha==5)))+
  xlab(TeX("$x$"))+ylab(TeX("$f(x)$"))+ggtitle(expression(delta==0.25))+theme(legend.text = element_text(size=15))

Exemplos <- data.frame(X,a1 =pl(X,1,0.75),a2 =pl(X,2,0.75),a5 =pl(X,5,0.75))
Ex <- Exemplos %>% gather(alpha,Y,-X)
p3 <- ggplot(Ex,aes(x=X,y=Y,lty=alpha))+geom_line()+theme_classic()+
  scale_linetype_discrete(name= element_blank(),labels = c(expression(alpha==1),expression(alpha==2),expression(alpha==5)))+
  xlab(TeX("$x$"))+ylab(TeX("$f(x)$"))+ggtitle(expression(delta==0.75))+theme(legend.text = element_text(size=15))
require(gtable)
require(gridExtra)
require(ggpubr)

ggarrange(p1,p2,p3,ncol=3,common.legend=TRUE,legend="bottom",labels = c("a","b","c"))
ggsave("F1.png",width = 15,height = 6,dpi=1000,units = "cm")

###### priori alpha
require(latex2exp)
X <-  seq(0,10,by=0.01)
Exemplos <- data.frame(X,a1 =dgamma(X,4.8,1),a2 =dgamma(X,39,10),a3 =dgamma(X,381,100))
Ex <- Exemplos %>% gather(alpha,Y,-X)
ggplot(Ex,aes(x=X,y=Y,lty=alpha))+geom_line()+theme_classic()+
  scale_linetype_discrete(name= element_blank(),
                          labels =c(bquote(list(k==4.8,theta==1)),bquote(list(k==39,theta==10)),bquote(list(k==381,theta==100))))+
  xlab(TeX("$\\alpha$"))+ylab(TeX("$f(\\alpha)$"))+theme(legend.text = element_text(size=7))+
  scale_x_continuous(breaks = 3.8,labels=c("3.8"))
ggsave("F2.png",width = 12,height = 8,dpi=1000,units = "cm")

###### priori delta
X <-  seq(0,1,by=0.01)
Exemplos <- data.frame(X,b1 =dbeta(X,6,12),b2 =dbeta(X,12,6),b3 =dbeta(X,120,60))
Ex <- Exemplos %>% gather(alpha,Y,-X)
ggplot(Ex,aes(x=X,y=Y,lty=alpha))+geom_line()+theme_classic()+
  scale_linetype_discrete(name= element_blank(),
                          labels =c(bquote(list(lambda==6,phi==12)),bquote(list(lambda==12,phi==6)),bquote(list(lambda==120,phi==60))))+
  xlab(TeX("$\\delta$"))+ylab(TeX("$f(\\delta)$"))+theme(legend.text = element_text(size=7))
ggsave("F3.png",width = 12,height = 8,dpi=1000,units = "cm")

##### JAGS
y = c(1.312, 1.314, 1.479, 1.552, 1.700, 1.803, 1.861, 1.865, 1.944, 1.958, 1.966, 1.997, 2.006, 2.021, 2.027, 2.055, 2.063, 2.098,
2.14, 2.179, 2.224, 2.240, 2.253, 2.270, 2.272, 2.274, 2.301, 2.301, 2.359, 2.382, 2.382, 2.426, 2.434, 2.435, 2.478, 2.490, 2.511,
2.514, 2.535, 2.554, 2.566, 2.57, 2.586, 2.629, 2.633, 2.642, 2.648, 2.684, 2.697, 2.726, 2.770, 2.773, 2.800, 2.809, 2.818, 2.821,
2.848, 2.88, 2.954, 3.012, 3.067, 3.084, 3.090, 3.096, 3.128, 3.233, 3.433, 3.585, 3.585)

N = length(y) 

dataList = list(

  y=y, Ntotal=Ntotal  )

Modelo=" 
data { C <- 10000
for(i in 1:Ntotal){ 
ones[i] <- 1 
}
}
model{
for (i in 1:Ntotal ) {
spy[i] <- (d*(a*(d/(1-d))*y[i]^(a-1)*exp(-d/(1-d)*y[i]^a))+
(1-d)*((a*(d/(1-d))**2*y[i]^(2*a-1)*exp(-d/(1-d)*y[i]^a))))/C
ones[i]~dbern(spy[i])
}
a~dgamma(380,100)
d~dbeta(1,1)
} "
writeLines(Modelo, con="modelo.txt" )
modelo <- jags.model("modelo.txt",inits = list(list(a=3,d=0.01),list(a=4,d=0.1),list(a=3.2,d=0.025),list(a=3.8,d=0.08),list(a=3.6,d=0.05)),data=dataList,n.chains = 5,n.adapt = 0)
samples=coda.samples(modelo,variable.names=c("a","d"),n.iter=1e2)
png("F4.png",width = 20,height = 20,units = "cm",res=1000)
plot(samples)
dev.off()


#### verossimilhanca
seq.alpha=seq(3,4.6,length.out = 200)
seq.p=seq(0.01,0.1,length.out = 200)
teste <- likelihood(y,seq.alpha,seq.beta) %>% as.matrix()
par(mar=c(5,5,5,5))

png("F5.png",width = 20,height = 20,units = "cm",res=1000)
persp3D(x=seq.alpha,y=seq.beta,z = teste,theta=45,ticktype="detailed",xlab =expression(alpha),ylab=expression(delta),
        zlab="Like",expand=0.75,phi=0,cex.axis=0.75)
dev.off()

png("F6.png",width = 20,height = 20,units = "cm",res=1000)
filled.contour(x=seq.alpha,y=seq.beta ,z = teste,col=blue2red(30),xlab=expression(alpha),
               ylab=expression(delta));segments(3.7,0,3.7,0.048,lty=2,lwd=0.5);segments(y0=0.048,x0=0,y1=0.048,x1=3.7,lty=2,lwd=0.5)


### Estimativas pontuais
samples[[1]][,1] %>% median()
samples[[1]][,2] %>% median()
burn <- 1001

## Estimativas intervalares
emp.hpd(samples[[1]][,1])
emp.hpd(samples[[1]][,2])

## Diagnostico
png("F7.png",width = 20,height = 20,units = "cm",res=1000)
gelman.plot(list(samples[[1]][,1],samples[[2]][,1],samples[[3]][,1],samples[[4]][,1],samples[[5]][,1]))
dev.off()
gelman.diag(list(samples[[1]][,1],samples[[2]][,1],samples[[3]][,1],samples[[4]][,1],samples[[5]][,1]))

png("F8.png",width = 20,height = 20,units = "cm",res=1000)
geweke.plot(mcmc(samples[[3]]))
dev.off()



#### Simulacoes da power lindley utilizando um método peculiar
require(flexsurv)
fun_simulations <- function(n,alpha,delta){
  draws <- sample(c("rweibull","rgamma"),size=n,replace = T,prob = c(delta,(1-delta)))
  random_draw <- numeric()
  for(i in 1:n){
    random_draw[i] <- ifelse(draws[i]=="rweibull",rweibull(n,shape=alpha,scale=(delta/(1-delta))^(-1/alpha)),rgengamma.orig(n,shape=alpha,k=2,scale=(delta/(1-delta))^(-1/alpha)))
  }
  return(random_draw)
}
simulacoes_pl <- fun_simulations(1e3,3.8,0.05)

### Conferindo parâmetros da simulação através da comparação com a esperança e desvio padrâo da PL
media_pl <- function(alpha,delta){gamma(1/alpha)*(alpha*(delta/(1-delta)+1)+1)/(alpha^2*(delta/(1-delta))**(1/alpha)*(delta/(1-delta)+1))}
var_pl <- function(alpha,delta){(2*gamma(2/alpha)*(alpha*(delta/(1-delta)+1)+2)*alpha^2*(delta/(1-delta)+1)-
                                   gamma(1/alpha)**2*(alpha*(delta/(1-delta)+1)+1)**2)/(alpha^4*(delta/(1-delta))**(2/alpha)*(delta/(1-delta)+1)**2)}
mean(simulacoes_pl);media_pl(3.8,0.05)
sd(simulacoes_pl);sqrt(var_pl(3.8,0.05))

#### Simulacoes para avaliar o método
n <- c(25,50,75,100,200)
alpha <- c(0.2,1.5,0.9)
delta <- c(0.8,0.5,0.26)
resultados <- list(array(0,dim=list(25,3)),array(0,dim=list(50,3)),array(0,dim=list(75,3)),array(0,dim=list(100,3)),array(0,dim=list(200,3)))
alpha.est <- array(0,dim=list(5,3))
delta.est <- array(0,dim=list(5,3))
MMSE.alpha <- array(0,dim=list(5,3))
MMSE.delta <- array(0,dim=list(5,3))
prioris_alpha <- list(c(20,100),c(150,100),c(90,100))
prioris_delta <- list(c(40,10),c(25,25),c(20,60))

B <- 1e2
captura_alpha <- array(0,dim=list(B,3,5))
captura_delta <- array(0,dim=list(B,3,5))
capturas <- list()
set.seed(25062019)
for(b in 1:B){
for(i in 1:5){
  for(j in 1:3){
    resultados[[i]][,j] <- fun_simulations(n[i],alpha[j],delta[j])
### Modelo considerando uma priori razoável
y <- resultados[[i]][,j]
Ntotal <- n[i]
dataList <- list(y=y,Ntotal=Ntotal)
Modelo=paste0(" 
data { C <- 10000000000000000000000000000
for(i in 1:Ntotal){ 
ones[i] <- 1 
}
}
model{
for (i in 1:Ntotal ) {
spy[i] <- (d*(a*(d/(1-d))*y[i]^(a-1)*exp(-d/(1-d)*y[i]^a))+
(1-d)*((a*(d/(1-d))**2*y[i]^(2*a-1)*exp(-d/(1-d)*y[i]^a))))/C
ones[i]~dbern(spy[i])
}
a~dgamma(",prioris_alpha[[j]][1],",",prioris_alpha[[j]][2],")
d~dbeta(",prioris_delta[[j]][1],",",prioris_delta[[j]][2],")
}")
writeLines(Modelo, con="modelo.txt" )
modelo <- jags.model("modelo.txt",data=dataList)
samples=coda.samples(modelo,variable.names=c("a","d"),n.iter=1e3)
captura_alpha[b,j,i] <- between(alpha[j],emp.hpd(samples[[1]][,1],conf = .5)[1],emp.hpd(samples[[1]][,1],conf = .5)[2])
captura_delta[b,j,i] <- between(delta[j],emp.hpd(samples[[1]][,2],conf = .5)[1],emp.hpd(samples[[1]][,2],conf = .5)[2])
  }
}
} 


captura_alpha[,1,1] %>% mean
captura_alpha[,1,2] %>% mean
captura_alpha[,1,3] %>% mean
captura_alpha[,1,4] %>% mean
captura_alpha[,1,5] %>% mean

captura_alpha[,2,1] %>% mean
captura_alpha[,2,2] %>% mean
captura_alpha[,2,3] %>% mean
captura_alpha[,2,4] %>% mean
captura_alpha[,2,5] %>% mean

captura_alpha[,3,1] %>% mean
captura_alpha[,3,2] %>% mean
captura_alpha[,3,3] %>% mean
captura_alpha[,3,4] %>% mean
captura_alpha[,3,5] %>% mean

captura_delta[,1,1] %>% mean
captura_delta[,1,2] %>% mean
captura_delta[,1,3] %>% mean
captura_delta[,1,4] %>% mean
captura_delta[,1,5] %>% mean

captura_delta[,2,1] %>% mean
captura_delta[,2,2] %>% mean
captura_delta[,2,3] %>% mean
captura_delta[,2,4] %>% mean
captura_delta[,2,5] %>% mean

captura_delta[,3,1] %>% mean
captura_delta[,3,2] %>% mean
captura_delta[,3,3] %>% mean
captura_delta[,3,4] %>% mean
captura_delta[,3,5] %>% mean


