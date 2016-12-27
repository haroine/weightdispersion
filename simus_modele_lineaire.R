library(rmutil)
library(ggplot2)
library(sampling)
library(foreach)
library(Hmisc)
source("simus_mixte.R")
library(scales)


set.seed(1005192119)
N <- 1000
n <- 50

## Auxiliary variable (revenue)
X <- rpareto(N,2000,100)

vrai_beta <- -0.1
vrai_alpha <- 10000
## Variable of interest: linear model, 
## (weak) negative correlation with the revenue
Y <- vrai_beta*X + rnorm(N,0,1500) + vrai_alpha

df <- data.frame(X,Y)

## Plot of the model
plot_lm <- ggplot(df, aes(x=X, y=Y)) +
  geom_point() +
  geom_abline(slope = vrai_beta, intercept = vrai_alpha)

print(plot_lm)

## Custom scaling
custom_trans <- trans_new(name="custom_trans", transform = function(x) {
  (x**(7))**(1/24)
},
inverse = function(x) { 
  (x**(24))**(1/7)
})

## Plot the model, scaled
plot_lm_scaled <- ggplot(df, aes(x=X, y=Y)) +
  geom_point() +
  geom_abline(slope = vrai_beta, intercept = vrai_alpha) +
  scale_x_continuous(trans=custom_trans, 
                     breaks = c(500,1000,2000,5000,10000,20000))

print(plot_lm_scaled)

########## Sampling
df$sampled <- 0
df_copy <- df

#### Generation of standard error plot

listPlotsArticle <- list()
points <- seq(0,1,by=0.05)

for(k in 1:length(points)) {
  print(k)
  listPlotsArticle[[k]] <- graph_mixte(points[k])
}

len_list <- length(listPlotsArticle)
sdmean <- rep(0, len_list)
sd95 <- rep(0, len_list)
sdBeta <- rep(0, len_list)
sdAlpha <- rep(0, len_list)

for(j in 1:len_list) {
  sdmean[j] <- listPlotsArticle[[j]]$sd_mean
  sd95[j] <- listPlotsArticle[[j]]$sd_95
  sdBeta[j] <- listPlotsArticle[[j]]$sd_param[2]
  sdAlpha[j] <- listPlotsArticle[[j]]$sd_param[1]
}

df_sd <- data.frame(points,sdmean,sd95,sdAlpha,sdBeta)

df_sd$sdmean_norm <- df_sd$sdmean / max(df_sd$sdmean)
df_sd$sd95_norm <- df_sd$sd95 / max(df_sd$sd95)
df_sd$sdAlpha_norm <- df_sd$sdAlpha / max(df_sd$sdAlpha)
df_sd$sdBeta_norm <- df_sd$sdBeta / max(df_sd$sdBeta)

names(df_sd)[1] <- "alpha"

currentAlpha <- 0.2
point_df <- data.frame(rep(currentAlpha,3),
     c(df_sd[df_sd$alpha==currentAlpha,c("sdmean_norm","sd95_norm","sdBeta_norm")]))
names(point_df)[1] <- "alpha"

plot_sd <- ggplot(df_sd, aes(x=alpha)) +
  geom_smooth(aes(y=sdmean_norm, colour="mean"),se=0) +
  geom_smooth(aes(y=sd95_norm, colour="q95"),se=0) +
  geom_smooth(aes(y=sdBeta_norm, colour="beta"),se=0) +
  ylab("Standard error (as percent of max)") +
  scale_color_manual(values=c("beta"="forestgreen","mean"="red","q95"="blue")) +
  guides(colour=guide_legend(title=NULL))

print(plot_sd)

# save(listPlotsArticle, custom_trans,
#      df_sd, plot_sd, vrai_alpha, vrai_beta, file="listPlots.RData")
