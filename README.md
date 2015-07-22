# Practice-R
setwd(“C:\\Users\\andy\\Desktop\\iris”)
mydata <- read.csv(file.choose(),header=T)
cor.test(mydata$team_off_eff,mydata$RPI)
head(mydata)
hist(mydata[,2])
A <- 1:10
B <- 11:20
cor.test(A,B)
mydata <- read.csv(file.choose(), head=TRUE)
cor.test(mydata$team_off_eff,mydata$RPI)
cor(mydata, use="complete.obs", method="kendall")
B <- read.csv(file.choose())

cor(B[,5:20], use="complete.obs", method="kendall")

smumary(B$CO2.emissions..kt.)
summary(B$CO2.emissions..kt.)
cor.test(B$Foreign.direct.investment,B$Improved.water.source..urban)
mytable <- table(B$Labor.Force,B$Improved.water.source..urban)
dim(mytable)
print(mytable)
margin(A)
A <- rep(1:20,5)
A
summary(A)
time(A)
frequency(A)
range(A)
C <- table(A)
cbind(table(A))
cell <- read.csv(file.choose(), head=FALSE)
cellname <- c("revenue", "pp", "cc", "dd", "ww")
colnames(cell) <- cellname
hclust(dcell)
hcdcell <- dist(dell)
memory.limit(size = NA)
TT <- read.csv(file.choose(), head=TRUE)
dTT <- dist(TT)
WW <- hclust(dTT)
plot(WW)
ir <- read.csv(file.choose(), head=FALSE)
summary(ir)
plot(ir)
sl <- ir$C
plot(sl)
table(sl)
hist(sl)
lines(sl)
pl <- density(sl)
hist(pl, freq=FALSE)
pl
plot(pl)
hist(sl, freq=FALSE)
sl.d <- density(sl)
hist(sl.d)
density(sl)
plot(sl)
lines(sl.d)
?hist
mod <plot(B$Foreign.direct.investment, B$CO2.emissions..kt.)
abline(mod)
lm(B$CO2.emissions..kt., B$Foreign.direct.investment)
mad <- (B$CO2.emissions..kt. ~ B$Foreign.direct.investment)
summary(mad)
abline(mad)
A <- 1:10
B <- 11:20
mod <- lm(B~A)
plot(A,B)
abline(mod)
summary(mod)
medata <- read.csv(file.choose(), head= T)
mad <- plot(medata$A,medata$B)
abline(mad)
mad <- lm(medata$B ~ medata$A | E)
par(mfrow=c(1,1))
plot(mad)
library(mosaic)
xyplot(medata$B ~ medata$A | medata$E)
libra ry(mosaic)
med <- read.csv(file.choose(), head=T)
lm1 <- lm(A ~ B+E-1, data=med)
summary(lm1)
plot(A~B, data=subset(med,Class="Iris-virginica"))
abline(2.26+1.93, 0.8)
ed <- read.csv(file.choose())
lm3 <- lm(satisfaction~price+value+variety+kinds+reliable+working, data=subset(ed, gender="f"))
summary(lm3)
plot(lmed)
par(mfrow=c(2,2))
med <- ed[ which(ed$gender=="m"),]
mlm <- lm(satisfaction~price+value+variety+kinds+reliable+working, data=med)
summary(mlm)
flm <- lm(satisfaction~price+value+variety+kinds+reliable+working, data=fed)
summary(flm)
plot(flm)
we <- read.csv(file.choose())
cor(newwe, use="complete", method="kendall")
newwe <- we[c(0,-7)]
table(med, 3)
fmed <- med[which( med$gender=="f"), ]
summary(fmed)
mmed.c <- mmed[1:35,-7]
cmmed <- cor(mmed.c, use="complete", method="kendall")
summary(cmmed)
cmmed
fmale <- lm(satisfaction ~ price+value+variety+kinds+reliable+working, data=fmed.c)
summary(fmale)
newdata <- rep(1:10, 5)
table(fmed$satisfaction)
time(fmed$satisfaction)
frequency(fmed$satisfaction)
t.test(fmed.c, mmed.c)
A <- 1:10
B <- 1:10
t.test(A, B)
pwr.anova.test(k=5,f=.8,power=.8)
library(ISLR)
?poly
attach(Smarket)
summary(Smarket)
train = (Year < 2005)
test = !train
train_data = Smarket[train,]
test_data = Smarket[test,]
Direction_testing = Direction[test]
Direction_testing
logistic_model = glm(Direction~Lag1+Lag2, data=train_data, family=binomial)
summary(logistic_model)
logistic_pro <- predict(logistic_model, test_data, type="response")
summary(logistic_pro)
logistic_pre <- rep("Down",252)
logistic_pre[logistic_pro >0.5] <- "up"
view(logistic_pre)
View(logistic_pro)
A <- 1:10
B <- 11:20
fit <- aov(A ~ B)
fit
A[B]
A[B] <- "P"
A
table(logistic_pre, Direction_testing)
table(A,B)
mean(A = B)
mean(logistic_pre != Direction_testing)
newdata <- read.csv(file.choose())
testing <-(newdata$Number<9)
training <- !testing
test.data <- newdata[testing,]
train.data <- newdata[training,]
lg_o <- glm(G.1~ A+B+C+D+E, data=test.data, family=binomial)
lg_o
summary(lg_o)
pre_model <- predict(lg_o, train.data, type="response" )
View(pre_model)
lda_model = lda(Direction~Lag1+Lag2, data=train_data, family=binomial)
library(MASS)
lda_pro <- predict(lda_model, test_data, type="response")
name(lad_pro)
names(lda_pro)
View(lda_pro)
table(lda_pro$class, Direction_testing)
mean(lda_pro$class != Direction_testing)
sqrt(25)
abs(25)



library(ggplot2)
library(GGally)
df <- data.frame(gp = factor(rep(letters[1:3], each = 10)),
                 y = rnorm(30))
ds <- ddply(df, .(gp), summarise, mean = mean(y), sd = sd(y))

naa <- read.csv(file.choose())

Ncaa <- tourney_results_final

diff <- ncaa$wscore - ncaa$lscore
diff[>0] < "W"
result <- rep("L", 844)
result[diff>0] <- "W"
View(result)
ncaa <- Ncaa[1:844,]
ncaa$diff <- diff

names(ncaa[27])
train_ncaa2 <- ncaa[1:400,]
test_ncaa <- ncaa[401:844,]
g <- glm(diff~ team_adj_tempo + team_off_eff + team_off_eff, data=train_ncaa2, family=binomial)


SMI <- read.csv(file.choose())
SMI
class(SMI)
NSMI <- ts(SMI, Start=c(2009), end=c(2015), fre=12)
myts <- ts(SMI, start=c(2009, January), end=c(2015,December), frequency=12)
myts

NSMI <- read.csv(file.choose())
class(NSMI)
USMI <- ts(NSMI$Spend, start=c(2009,1), freq=12)
USMI
plot(tsSMI)
plot(decompose(USMI))
SMI.hw <- HoltWinters(USMI)

plot(SMI.hw)
smi <- predict(SMI.hw, n.ahead=2*12)
smi

auto <- read.csv(file.choose())
is.numeric(auto$Spend)
model <- lm(auto$Spend ~ auto$Sales+auto$Google.trend)
model
par(mfrow=c(1,1))
plot(model)
summary(model)

library("party")
Atreemodel <- ctree(auto$Spend ~ auto$Sales+auto$Google.trend)
plot(Atreemodel,type="simple")
treemodel

auto

j<- 1:10
j
f <- c(1:10)
f
is.numeric(f)
signif(3.74, digits=2)

mytrans <- function(x) { 
  if (!is.matrix(x)) {
    warning("argument is not a matrix: returning NA")
    return(NA_real_)
  }
  y <- matrix(1, nrow=ncol(x), ncol=nrow(x)) 
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) {
      y[j,i] <- x[i,j] 
    }
  }
  return(y)
}
z <- matrix(1:10, nrow=5, ncol=2)
z
tz <- mytrans(f)
tz

CG <- read.csv(file.choose())
aov.CG <- aov(CG$TotalProfit~CG$ProductCategory)
summary(aov.CG)
model.tables(aov.CG,"mean")
is.numeric(CG$TotalProfit)
etaSquared(aov.CG, anova=T)
TukeyHSD(aov.CG)
