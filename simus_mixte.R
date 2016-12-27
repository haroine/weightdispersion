
graph_mixte <- function(alpha) {

## Simulations mixte
df <- df_copy
df$piks <- alpha * n * df$X / sum(df$X) + (1-alpha) * n/N

nSimus <- 1000
simus_pps <- foreach(kSimus=1:nSimus, .combine=rbind) %do% {
  
  sampledId <- which(UPpivotal(df$piks) == 1)
  
  df[sampledId,"sampled"] <- df[sampledId,"sampled"]+1
  
  df_sample <- df[sampledId,]
  modele_estime <- lm(Y ~ X, data = df_sample, weights = 1/df_sample$piks)
  beta_estime <- unname(modele_estime$coefficients[2])
  alpha_estime <- unname(modele_estime$coefficients[1])
  
  est_mean_X <- 1/N*sum(df_sample$X / df_sample$piks)
  est_q95_X <- unname(wtd.quantile(df_sample$X, 
                                   weights = 1/df_sample$piks, probs = c(0.95)))
  
  c(alpha_estime, beta_estime, est_mean_X, est_q95_X)
}

df$sampled <- df$sampled / nSimus
simus_pps <- data.frame(simus_pps, stringsAsFactors = F)
colnames(simus_pps) <- c("alpha_estime", "beta_estime", 
                         "est_mean_X", "est_q95_X")

## Parameters of the confidence region
beta_1 <- unname(quantile(simus_pps$beta_estime, probs=c(0.95)))
beta_2 <- unname(quantile(simus_pps$beta_estime, probs=c(0.05)))
alpha_1 <- unname(quantile(simus_pps$alpha_estime, probs=c(0.95)))
alpha_2 <- unname(quantile(simus_pps$alpha_estime, probs=c(0.05)))

## Estimators of X : mean and quantile
X_mean_1 <- unname(quantile(simus_pps$est_mean_X, probs=c(0.95)))
X_mean_2 <- unname(quantile(simus_pps$est_mean_X, probs=c(0.05)))
X_q95_1 <- unname(quantile(simus_pps$est_q95_X, probs=c(0.95)))
X_q95_2 <- unname(quantile(simus_pps$est_q95_X, probs=c(0.05)))

sd_X_mean <- sd(simus_pps$est_mean_X)
sd_X_q95 <- sd(simus_pps$est_q95_X)
sd_beta <- sd(simus_pps$beta_estime)
sd_alpha <- sd(simus_pps$alpha_estime)

vrai_mean_X <- mean(df$X)
vrai_q95_X <- unname(quantile(df$X, probs=c(0.95)))

plot_pps_simus <- ggplot(df, aes(x=X, y=Y, colour=sampled)) +
  geom_point(shape=21) +
  geom_abline(slope = vrai_beta, intercept = vrai_alpha) +
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
  # geom_vline(xintercept=vrai_mean_X, colour="blue") +
  # geom_vline(xintercept=vrai_q95_X, colour="blue") +
  scale_color_continuous(low="#ffffff", high = "#ff0000")
  # ylim(c(0.85*vrai_alpha,vrai_alpha/0.85))
  # scale_x_continuous(trans=custom_trans, 
  #                    breaks = c(500,1000,2000,5000,10000,20000))

  returnList <- list(plot_pps_simus, c(X_mean_1,X_mean_2), 
                     c(X_q95_1,X_q95_2), sd_X_mean, sd_X_q95, c(sd_alpha, sd_beta))
  names(returnList) <- c("plot","ci_mean","ci_q95","sd_mean","sd_95","sd_param")
  return(returnList)

}