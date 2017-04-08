
## Under the model probability
graph_mixte_model <- function(alpha, df, nSimus=1000) {
  
df$piks <- 0
## Compute the piks
for(k in 1:N_simus_models) {
  df[(1:N)+(k-1)*N,"piks"] <- alpha * n * df[(1:N)+(k-1)*N,"X"] / sum(df[(1:N)+(k-1)*N,"X"]) + (1-alpha) * n/N
}

simus_pps_model <- foreach(kSimus_model=1:N_simus_models, .combine=rbind) %do% {
  df_k <- df[df$k_sim == kSimus_model,]
    
  ## Simulations mixte
  df_k$sampled <- 0
  # df_k$piks <- alpha * n * df_k$X / sum(df_k$X) + (1-alpha) * n/N
  
  simus_pps <- foreach(kSimus=1:nSimus, .combine=rbind) %do% {
    
    sampledId <- which(UPpivotal(df_k$piks) == 1)
    
    df_k[sampledId,"sampled"] <- df_k[sampledId,"sampled"]+1
    
    df_k_sample <- df_k[sampledId,]
    modele_estime <- lm(Y ~ X, data = df_k_sample, weights = 1/df_k_sample$piks)
    beta_estime <- unname(modele_estime$coefficients[2])
    alpha_estime <- unname(modele_estime$coefficients[1])
    
    est_mean_X <- 1/N*sum(df_k_sample$X / df_k_sample$piks)
    est_q95_X <- unname(wtd.quantile(df_k_sample$X, 
                                     weights = 1/df_k_sample$piks, probs = c(0.95)))
    
    c(alpha_estime, beta_estime, est_mean_X, est_q95_X)
  }
simus_pps
}

simus_pps_model <- data.frame(simus_pps_model, stringsAsFactors = F)
row.names(simus_pps_model) <- NULL
colnames(simus_pps_model) <- c("alpha_estime", "beta_estime", 
                         "est_mean_X", "est_q95_X")


## Parameters of the confidence region
beta_1 <- unname(quantile(simus_pps_model$beta_estime, probs=c(0.95)))
beta_2 <- unname(quantile(simus_pps_model$beta_estime, probs=c(0.05)))
alpha_1 <- unname(quantile(simus_pps_model$alpha_estime, probs=c(0.95)))
alpha_2 <- unname(quantile(simus_pps_model$alpha_estime, probs=c(0.05)))

## Estimators of X : mean and quantile
X_mean_1 <- unname(quantile(simus_pps_model$est_mean_X, probs=c(0.95)))
X_mean_2 <- unname(quantile(simus_pps_model$est_mean_X, probs=c(0.05)))
X_q95_1 <- unname(quantile(simus_pps_model$est_q95_X, probs=c(0.95)))
X_q95_2 <- unname(quantile(simus_pps_model$est_q95_X, probs=c(0.05)))

sd_X_mean <- sd(simus_pps_model$est_mean_X)
sd_X_q95 <- sd(simus_pps_model$est_q95_X)
sd_beta <- sd(simus_pps_model$beta_estime)
sd_alpha <- sd(simus_pps_model$alpha_estime)

real_X_mean <- real_values[1]
real_X_q95 <- real_values[2]

## Mean squared errors
mse_X_mean <- sd_X_mean**2 + (mean(simus_pps_model$est_mean_X) - real_X_mean)**2
mse_X_q95 <- sd_X_q95**2 + (mean(simus_pps_model$est_q95_X) - real_X_q95)**2
mse_beta <- sd_beta**2 + (mean(simus_pps_model$beta_estime) - real_beta)**2
mse_alpha <- sd_alpha**2 + (mean(simus_pps_model$alpha_estime) - real_alpha)**2

df$sampled <- df$piks
# print(sum(df$piks))
df_reduced <- df[UPpivotal(df$piks) == 1,]

plot_pps_sims_model <- ggplot(df_reduced, aes(x=X, y=Y, colour=sampled)) +
  geom_point(shape=21) +
  geom_abline(slope = real_beta, intercept = real_alpha) +
  geom_abline(slope = beta_1, intercept = alpha_1, 
              colour="red", linetype="dashed") +
  geom_abline(slope = beta_2, intercept = alpha_2, 
              colour="red", linetype="dashed") +
  # geom_vline(xintercept=c(X_mean_1,X_mean_2),
  #            colour="blue", linetype="dashed") +
  # geom_vline(xintercept=X_mean_2, colour="blue", linetype="dashed") +
  # geom_vline(xintercept=X_q95_1, colour="blue", linetype="dashed") +
  # geom_vline(xintercept=c(X_q95_1,X_q95_2),
  #            colour="blue", linetype="dashed") +
  # geom_vline(xintercept=real_X_mean, colour="blue") +
  # geom_vline(xintercept=real_X_q95, colour="blue") +
  scale_color_continuous(low="#ffffff", high = "#ff0000")
  # ylim(c(0.85*vrai_alpha,vrai_alpha/0.85))
  # scale_x_continuous(trans=custom_trans, 
  #                    breaks = c(500,1000,2000,5000,10000,20000))

  returnList <- list(plot_pps_sims_model, c(X_mean_1,X_mean_2), 
                     c(X_q95_1,X_q95_2), sd_X_mean, sd_X_q95, c(sd_alpha, sd_beta),
                     mse_X_mean, mse_X_q95, c(mse_alpha, mse_beta))
  names(returnList) <- c("plot","ci_mean","ci_q95","sd_mean","sd_95","sd_param",
                         "mse_mean","mse_95","mse_param")
  return(returnList)

}
