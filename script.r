#import library and help function
library(dplyr)
source("helper.R")

#import and clean data
data <- read.csv("project/Spotify-2000.csv")
#drop unneccessary for this project
data <- data[, !(colnames(data) %in% c("Index","Title","Popularity","Length","Popularity"))]

#Project Part 1

keep <- c("Danceability","Tempo","Valence","Artist","Top.Genre")
df <- data[keep]

Sy <- df$Danceability
#test normality of danceability 
dist.plot <- qqnorm(Sy, main = "Danceability qq plot")
qqline(Sy, col ="red",lwd=3)
alpha <- 0.05
n <- length(Sy)
# Part A
# H0: 𝜇 = 0.6
# H1: 𝜇 <> 0.6

#using t test
#Degree of freedom
v <- n-1
#t critical val
tcrit <- qt(alpha/2, df=v, lower.tail = F)
#mean of danceability
ybar <- mean(Sy)
#claim average
mu0 <- 60
#standard deviation, standard error, and margin error of danceability
sy <- sd(Sy)
SE <- sy/sqrt(n)
eps <- tcrit*SE
#t stat
tstat <- (ybar - mu0)/SE
#p-value
pvalA <- 2*pt(-abs(tstat),df=v, lower.tail = T)
#Confidence Interval
CIL_A <- ybar - eps
CIU_A <- ybar + eps
#table summary
metric_name_A <- c("CI Lower","CI Upper", "Claim mu", "T.stat","T.crit", "p-value","alpha value")
metric_val_A <- c(CIL_A, CIU_A, mu0, tstat, tcrit, pvalA, alpha)
metric_summary_A <- data.frame(metric_name_A, metric_val_A)

#Part B
# H0: sd = 0.1
# H1: sd <> 0.1
# claim standard deviation
sd0 <- 10

#chi crit val
chicrit <- qchisq(alpha/2, df = v, lower.tail = F)

#chi stat 
chistat <- (v)*((sy/sd0)^2)
#p value
pvalB <- pchisq(q=chistat, df = v, lower.tail = F)
#Confidence Interval
CIL_B <- sqrt((v*sy^2)/qchisq(alpha/2, df = v, lower.tail = F))
CIU_B <- sqrt((v*sy^2)/qchisq(1-alpha/2, df = v, lower.tail = F))

#Table summary
metric_name_B <- c("CI Lower","CI Upper", "Claim sd", "Chi.stat","Chi.crit", "p-value","alpha value")
metric_val_B <- c(CIL_B, CIU_B, sd0, chistat, chicrit, pvalB, alpha)
metric_summary_B <- data.frame(metric_name_B, metric_val_B)


#Part C
#define random variables
X1 <- df$Tempo
X2 <- df$Valence
#Q-Q plot for tempo
V1 <- qqnorm(X1, main = "tempo normal qq plot")
qqline(X1,col="steel blue",lwd=3)
#Q-Q plot for valence
V2 <- qqnorm(X2, main ="valence normal qq plot")
qqline(X2,col="orange", lwd=3)
#correlation coefficient for QQ plot tempo
cor(V1$x,V1$y)
#correlation coefficient for QQ plot valence
cor(V2$x,V2$y)

#Part D
#use t test because the two rvs are independent and have normal distribution as proven in Part C
# H0: mean(pop) = mean(c.rock)
# H1: mean(pop) =/= mean(c.rock)
pop_gen <- filter(df, Top.Genre =="pop")$Danceability
c.rock_gen <- filter(df, Top.Genre =="classic rock")$Danceability
popbar <- mean(pop_gen)
c.rockbar <- mean(c.rock_gen)
std_pop <- sd(pop_gen)
std_c.rock <- sd(c.rock_gen)
n1 <- length(pop_gen)
n2 <- length(c.rock_gen)

#degree of freedom with equal variance assumption
v_D <- (n1 + n2)-2

#t-crit part D
tcrit_D <- qt(alpha/2, df = v_D,lower.tail = F)

#Standard error
SE_12 <- sqrt(std_pop^2/n1 + std_c.rock^2/n2)
#t stat for part D
tstat_D <- (popbar - c.rockbar)/SE_12

pval_D <- 2*pt(-abs(tstat_D),df=v_D, lower.tail = T)

#margin of error
eps_D <- tstat_D*SE_12

#Confidence Interval 
CIL_D <- (popbar-c.rockbar)-eps
CIU_D <- (popbar-c.rockbar)+eps

#Table summary
metric_name_D <- c("CI Lower","CI Upper", "Claim mu difference", "T.stat","T.crit", "p-value","alpha value")
metric_val_D <- c(CIL_D, CIU_D, 0, tstat_D, tcrit_D, pval_D, alpha)
metric_summary_D <- data.frame(metric_name_D, metric_val_D)

#Part E
# H0: var(pop) = var(c.rock)
# H1: var(pop) =/= var(c.rock)
# correlation coefficient for pop, classic rock
rpop_c.rock <- cor(pop_gen,c.rock_gen)
SSE <- std_c.rock^2*(n1-1)*(1-rpop_c.rock^2)
MSE <- SSE/(n-2)
SST <- std_c.rock^2*(n-1)
MST <- SST/(n-1)
SSM <- SST - SSE
MSM <- SSM/1
# F stat value
Fstat <- MSM/MSE
# F crit value
Fcrit <- qf(alpha/2, 1, n2-2, lower.tail = F)
Fcrit2 <- qf(1-alpha/2, 1, n2-2, lower.tail = F)
# p val 
pval_E <- pf(Fstat, 1, n2-2)
#Confidence Interval 
CIL_E <- Fstat * qf(alpha/2, n2-1, n1-1)
CIU_E <- Fstat * 1/qf(alpha/2, n1-1, n2-1)

#Table summary
metric_name_E <- c("CI Lower","CI Upper", "Claim var difference", "F.stat","F.crit", "p-value","alpha value")
metric_val_E <- c(CIL_E, CIU_E, 0, Fstat, Fcrit, pval_E, alpha)
metric_summary_E <- data.frame(metric_name_E, metric_val_E)




#Phase 2
# clean data a bit more
data <- data[, !(colnames(data) %in% c("Index","Title","Artist","Top.Genre","Popularity","Length","Popularity"))]
#part A: Simple linear Regression
df <- data[c("Danceability","Valence")]
y <- df$Danceability
x1 <- df$Valence

corr_table <- cor(df)

plot(x1,y, xlab = "Valence", ylab = "Danceability", main ="Scatter plot of Valence-Danceability")

#Simple linear with danceability and tempo
mod_A <- nemolm2(y,cbind(x1,x1^2))

#Standardized Resid
pA_sres <- mod_A$sres

plot(x1, pA_sres, xlab = "Valence", ylab ="Standardized Residual", main = "Standardized residual plot Simp. Lin. Reg")
abline(0,0, col = "red", lwd=2)

#betahat0 and betahat1
pA_betahat0 <- mod_A$betahat[1]
pA_betahat1 <- mod_A$betahat[2]

#SEbetahat0 and SEbetahat1
pA_SEbeta0 <- mod_A$SEbetahat[1]
pA_SEbeta1 <- mod_A$SEbetahat[2]

#r2, r2adj, p-val
pA_r2 <- mod_A$r2
pA_r2adj <- mod_A$r2adj
pA_pval <- mod_A$pval
metric.title <- c("r2","Adjusted r2", "P-Value")
metric.val <- c(pA_r2,pA_r2adj,pA_pval)
metric.sum_A <- data.frame(metric.title, metric.val)


#part B: Simple Quadratic Regression
mod_B <- nemolm2(y, cbind(x1,x1^2))
pB_sres <- mod_B$sres

#betahat0 and betahat1 and betahat2
pB_betahat0 <- mod_B$betahat[1]
pB_betahat1 <- mod_B$betahat[2]
pB_betahat2 <- mod_B$betahat[3]

#SEbetahat0 and SEbetahat1
pB_SEbeta0 <- mod_B$SEbetahat[1]
pB_SEbeta1 <- mod_B$SEbetahat[2]
pB_SEbeta2 <- mod_B$SEbetahat[3]

#r2, r2adj, p-val
pB_r2 <- mod_B$r2
pB_r2adj <- mod_B$r2adj
pB_pval <- mod_B$pval
metric.val_B <- c(pB_r2,pB_r2adj,pB_pval)
metric.sum_B <- data.frame(metric.title, metric.val_B)
plot(x1, pB_sres, xlab="Valence", ylab="Standardized Residual",main="Standardized residual plot Simp. Quad. Reg")
abline(0,0, col="blue", lwd=2)



#part C: Multiple Linear Regression
keep2 <- c("Danceability","Valence","Tempo","Energy")
df2 <- data[keep2]
corr_table2 <- cor(df2)
x2 <- df2$Tempo
x3 <- df2$Energy
mod_C <- nemolm2(y, cbind(x1,x2,x3))

n <- length(y)

SSE <- mod_C$sse
MSE <- mod_C$mse
SSM <- mod_C$ssm
MSM <- mod_C$msm
SST <- SSE + SSM
MST <- SST/(n-1)

pC_pval <- mod_C$pval
pC_Fstat <- MSM/MSE

#anova table
anova.title <- c("Source of Error", "DOF", "Sum of Square", "Mean Square", "Fstat", "P-Value")
anova.valueM <- c("Model", 3, SSM, MSM, pC_Fstat, pC_pval)
anova.valueE <- c("Error Term", n-4, SSE, MSE, ":)", ":)")
anova.valueT <- c("Total", n-1, SST,MST, ":)", ":)")
anova.table <- data.frame(anova.title,anova.valueM,anova.valueE,anova.valueT)

#variance inflation factor, barplot
MYv123 <- nemolm2(y, cbind(x1,x2,x3), 0)
MYv12 <- nemolm2(y, cbind(x1,x2), 0)
MYv13 <- nemolm2(y, cbind(x1,x3), 0)
MYv23 <- nemolm2(y, cbind(x2,x3), 0)

M1v23 <- nemolm2(x1, cbind(x2,x3))
M2v13 <- nemolm2(x2, cbind(x1,x3))
M3v12 <- nemolm2(x3, cbind(x1,x2))
vif1 <- 1/(1-MYv23$r2)
vif2 <- 1/(1-MYv13$r2)
vif3 <- 1/(1-MYv12$r2)

vif <- c(vif1,vif2,vif3)

barplot(vif, horiz = T,
        main = "variance inflation factors",
        names.arg = c("Valence","Tempo","Energy"),
        xlim = c(0,10))
abline(v=5,col="blue",lty="longdash")

pC_sres <- mod_C$sres
plot(x1, pC_sres, xlab = "Valence", ylab ="Standardized Residual", main = "Standardized residual plot Mult. Lin. Reg.")
abline(0,0, col = "purple", lwd=2)




# AV plots for X1

plot(MYv23$sres, M1v23$sres,
     main = "Added Variable Plot for Valence",
     xlab ="S.Residual for Danceability~Tempo,Energy",
     ylab = "S.Residual for Valence~Tempo,Valence")
# abline(intercept, slop)
abline(0,0, lwd=2)

m1 <-cor(MYv23$sres,M1v23$sres)*sd(M1v23$sres)/sd(MYv23$sres)
abline(mean(M1v23$sres)*m1*mean(MYv23$sres),m1, col="red",lwd=2)

# AV X2
plot(MYv13$sres, M2v13$sres,
     main = "Added Variable Plot for Tempo",
     xlab ="S.Residual for Danceability~Valence,Energy",
     ylab = "S.Residual for Tempo~Valence,Energy")
# abline(intercept, slop)
abline(0,0, lwd=2)

m1 <-cor(MYv13$sres,M2v13$sres)*sd(M2v13$sres)/sd(MYv13$sres)
abline(mean(M2v13$sres)*m1*mean(MYv13$sres),m1, col="orange",lwd=2)

# AV X3
plot(MYv12$sres, M3v12$sres,
     main = "Added Variable Plot for Energy",
     xlab ="S.Residual for Danceability~Valence,Tempo",
     ylab = "S.Residual for Energy~Valence,Tempo,")
# abline(intercept, slop)
abline(0,0, lwd=2)

m1 <-cor(MYv12$sres,M3v12$sres)*sd(M3v12$sres)/sd(MYv12$sres)
abline(mean(M3v12$sres)*m1*mean(MYv12$sres),m1, col="blue",lwd=2)



SSEred <- mod_C_reduced$sse
SSEfull <- mod_C$sse
partial_f_stat <- ((SSEred - SSEfull)/(n-3-n+4))/(SSEfull/n-4)
pval_partial_f <- pf(partial_f_stat, 1, n-4, lower.tail = F)

mod_C$sres
#part 4: Time Series
keep3 <- c("Year", "Danceability")
df3 <- data[keep3]
df3 %>% 
  plot(ylab= "Danceability", main="Danceability against Year")
# transformed the danceability into mean by each year
df4 <- df3 %>% 
  group_by(Year) %>%
  summarize(mean=mean(Danceability)) 
# delete the outlier
df4 <- filter(df4, Year != "1960")
df4 %>%
  plot(xlab= "Year", ylab="Mean of Danceability",main="Mean of Danceability~Year")

# standardized the year variable
x_year <- df4$Year
y_mean <- df4$mean
x_year <- (x_year - mean(x_year))/sd(x_year)
# ACF, PACF
acf_plot <- acf(y_mean, lag=60, main="ACF for Danceability")
pacf_plot <- pacf(y_mean, lag=60, main="PACF for Danceability")

# Models for Polynomial and Linear Regression
mod_time <- nemolm2(y_mean, cbind(x_year, x_year^2,x_year^3,x_year^4,x_year^5))
mod_time2 <- nemolm2(y_mean, x_year)

df4 %>%
  plot(xlab= "Year", ylab="Mean of Danceability",main="Mean of Danceability~Year")
lines(df4$Year,mod_time$predicted, col="red", main="Polynomial Reg")
lines(df4$Year,mod_time2$predicted, col="blue")
legend("topright", legend=c("Polynomial Reg", "Linear Reg"),
       col=c("red", "blue"), lty = 1)


plot(data$Danceability, ylab = "Danceability", main = "Scatter Plot with Simple Lin. Reg Predicted")
lines(mod_A$predicted, col="red", lwd=2)

plot(data$Danceability, ylab = "Danceability", main = "Scatter Plot with Simple Quad. Reg Predicted")
lines(mod_B$predicted, col="blue", lwd=2)

plot(data$Danceability, ylab = "Danceability", main = "Scatter Plot with Multi. Lin. Reg Predicted")
lines(mod_C$predicted, col="purple", lwd=2)

metric.mse <- c(mod_A$mse, mod_B$mse, mod_C$mse)
metric.reg <- c("Simple Linear Reg","Simple Quadratic Reg", "Multiple Linear Reg")
metric.sum_final <- data.frame(metric.reg, metric.mse)






