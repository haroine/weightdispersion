library(ggplot2)

set.seed(1005192119)

X <- rpareto(N,location_pareto,dispersion_pareto)
Y <- real_beta*X + rnorm(N,0,sigma_norm) + real_alpha

df_dispersion <- data.frame(X,Y)

muPoints <- seq(0,1,by = 0.005)
disp_weights <- rep(0,length(muPoints))

for(k in 1:length(muPoints)) {
  currentMu <- muPoints[k]
  piks <- currentMu * n * X / sum(X) + (1-currentMu) * n/N
  disp_weights[k] <- var(1/piks)
}

df_disp_weights <- data.frame(muPoints, disp_weights)

plot_disp_weights <- ggplot(df_disp_weights) +
  # geom_smooth(aes(x=muPoints,y=disp_weights), se=0) +
  geom_point(aes(x=muPoints,y=disp_weights)) +
  ylab("Dipsersion of the weights") +
  xlab("mu") +
  scale_y_log10()

print(plot_disp_weights)
