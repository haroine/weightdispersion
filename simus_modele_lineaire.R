library(rmutil)
library(ggplot2)
library(sampling)
library(foreach)
library(Hmisc)
source("simus_mixte.R")
library(scales)


set.seed(1005192119)
N <<- 1000
n <<- 50
N_simus_models <<- 31

real_beta <- -0.1
real_alpha <- 10000
nSim_design <- 31

location_pareto <- 2000
dispersion_pareto <- 100
sigma_norm <- 1500

## compute real values of X under the model
# real_values <- foreach(k_values=1:100000, .combine=rbind) %do% {
#   X <- rpareto(N,location_pareto,dispersion_pareto)
#   
#   c(mean(X), quantile(X, probs = c(0.95)))
# }
# 
# real_values <- colMeans(real_values)
real_values <<- c(1999.584,6003.200)

## Simulate models
df <- data.frame(matrix(0,nrow = N*N_simus_models, ncol = 3))
names(df) <- c("X","Y","k_sim")

for(k in 1:N_simus_models) {
  
  df[(1:N)+(k-1)*N,"k_sim"] <- k
  
  ## Auxiliary variable (revenue)
  X <- rpareto(N,location_pareto,dispersion_pareto)
  df[(1:N)+(k-1)*N,"X"] <- X
  
  ## Variable of interest: linear model, 
  ## (weak) negative correlation with the revenue
  df[(1:N)+(k-1)*N,"Y"] <- real_beta*X + rnorm(N,0,sigma_norm) + real_alpha
  
}



## Plot of the model
# plot_lm <- ggplot(df, aes(x=X, y=Y)) +
#   geom_point() +
#   geom_abline(slope = vrai_beta, intercept = vrai_alpha)
# 
# print(plot_lm)

## Custom scaling
custom_trans <- trans_new(name="custom_trans", transform = function(x) {
  (x**(7))**(1/24)
},
inverse = function(x) { 
  (x**(24))**(1/7)
})

## Plot the model, scaled
# plot_lm_scaled <- ggplot(df, aes(x=X, y=Y)) +
#   geom_point() +
#   geom_abline(slope = vrai_beta, intercept = vrai_alpha) +
#   scale_x_continuous(trans=custom_trans, 
#                      breaks = c(500,1000,2000,5000,10000,20000))
# 
# print(plot_lm_scaled)

#### Generation of standard error plot

listPlotsArticle <- list()
points <- seq(0,1,by=0.05)
# points <- seq(0,1,by=0.5)


for(k in 1:length(points)) {
  print(paste(k,"-",points[k]))
  listPlotsArticle[[k]] <- graph_mixte_model(points[k], df, nSimus=nSim_design)
}

len_list <- length(listPlotsArticle)
msemean <- rep(0, len_list)
mse95 <- rep(0, len_list)
mseBeta <- rep(0, len_list)
mseAlpha <- rep(0, len_list)

for(j in 1:len_list) {
  msemean[j] <- listPlotsArticle[[j]]$mse_mean
  mse95[j] <- listPlotsArticle[[j]]$mse_95
  mseBeta[j] <- listPlotsArticle[[j]]$mse_param[2]
  mseAlpha[j] <- listPlotsArticle[[j]]$mse_param[1]
}

df_mse <- data.frame(points,msemean,mse95,mseAlpha,mseBeta)

df_mse$msemean_norm <- df_mse$msemean / max(df_mse$msemean)
df_mse$mse95_norm <- df_mse$mse95 / max(df_mse$mse95)
df_mse$mseAlpha_norm <- df_mse$mseAlpha / max(df_mse$mseAlpha)
df_mse$mseBeta_norm <- df_mse$mseBeta / max(df_mse$mseBeta)

names(df_mse)[1] <- "alpha"

currentAlpha <- 0.2
point_df <- data.frame(rep(currentAlpha,3),
     c(df_mse[df_mse$alpha==currentAlpha,c("msemean_norm","mse95_norm","mseBeta_norm")]))
names(point_df)[1] <- "alpha"

plot_mse <- ggplot(df_mse, aes(x=alpha)) +
  # geom_point(aes(y=msemean_norm, colour="mean")) +
  # geom_point(aes(y=mse95_norm, colour="q95")) +
  # geom_point(aes(y=mseBeta_norm, colour="beta")) +
  geom_smooth(aes(y=msemean_norm, colour="mean"),se=0) +
  geom_smooth(aes(y=mse95_norm, colour="q95"),se=0) +
  geom_smooth(aes(y=mseBeta_norm, colour="beta"),se=0) +
  ylab("Standard error (as percent of max)") +
  scale_color_manual(values=c("beta"="forestgreen","mean"="red","q95"="blue")) +
  guides(colour=guide_legend(title=NULL))

print(plot_mse)

print(object.size(listPlotsArticle), units="Mb")

# save(listPlotsArticle, custom_trans,
#      df_mse, plot_mse, real_alpha, real_alpha, real_values,
#      file="listPlots_model.RData")
