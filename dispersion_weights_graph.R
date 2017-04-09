library(ggplot2)

set.seed(1005192119)

X <- rpareto(N,location_pareto,dispersion_pareto)
Y <- real_beta*X + rnorm(N,0,sigma_norm) + real_alpha

df_dispersion <- data.frame(X,Y)

muPoints <- seq(0,1,by = 0.01)
disp_weights <- rep(0,length(muPoints))

for(k in 1:length(muPoints)) {
  currentMu <- muPoints[k]
  piks <- currentMu * n * X / sum(X) + (1-currentMu) * n/N
  disp_weights[k] <- var(piks)
}

df_disp_weights <- data.frame(muPoints, disp_weights)

plot_disp_weights <- ggplot(df_disp_weights) +
  geom_smooth(aes(x=muPoints,y=disp_weights)) +
  ylab("Dipsersion of the weights") +
  xlab("mu")

print(plot_disp_weights)
