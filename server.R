
shinyServer(function(input, output) {
  

  output$plot_simus <- renderPlot({
    
    currentAlpha <- alpha()
    
    indexList <- which.min(abs(seq(0,1,length.out = length(listPlotsArticle)) - currentAlpha))
    currentList <- listPlotsArticle[[indexList]]
    currentPlot_data <- currentList$plot$data
    
    real_beta <- currentList$real_beta
    real_alpha <- currentList$real_alpha
    
    print(real_beta)
    print(real_alpha)
    
    currentPlot <- ggplot(currentPlot_data, aes(x=X, y=Y, colour=sampled)) +
      geom_point(shape=21) +
      geom_abline(slope = real_beta, intercept = real_alpha) +
      # geom_abline(slope = currentList$beta_1, intercept = currentList$alpha_1, 
      #             colour="red", linetype="dashed") +
      # geom_abline(slope = currentList$beta_2, intercept = currentList$alpha_2, 
      #             colour="red", linetype="dashed") +
      # geom_vline(xintercept=c(X_mean_1,X_mean_2),
      #            colour="blue", linetype="dashed") +
      # geom_vline(xintercept=c(X_q95_1,X_q95_2),
      #            colour="blue", linetype="dashed") +
      # geom_vline(xintercept=real_X_mean, colour="blue") +
      # geom_vline(xintercept=real_X_q95, colour="blue") +
      scale_color_continuous(low="mistyrose", high = "#ff0000", limits=c(0,0.65))
    # ylim(c(0.85*vrai_alpha,vrai_alpha/0.85))
    # scale_x_continuous(trans=custom_trans, 
    #                    breaks = c(500,1000,2000,5000,10000,20000))
    

    if(input$include_CI_mean) {
      currentPlot <- currentPlot +
        geom_vline(xintercept=listPlotsArticle[[indexList]]$ci_mean,
                   colour="blue", linetype="dashed")
    }

    if(input$include_CI_95) {
      currentPlot <- currentPlot +
        geom_vline(xintercept=listPlotsArticle[[indexList]]$ci_q95,
                   colour="blue", linetype="dashed")
    }

    if(input$scale) {
      currentPlot <- currentPlot +   scale_x_continuous(trans=custom_trans,
           breaks = c(500,1000,2000,5000,10000,20000)) +
        ylim(c(0.85*real_alpha,real_alpha/0.85))
    }

    currentPlot
  })
  
  output$plot_mse <- renderPlot({

    currentAlpha <- alpha()
    
    plot_mse_local <- ggplot(plot_mse$data, aes(x=mu)) +
      geom_point(aes(y=msemean_norm, colour="mean")) +
      geom_point(aes(y=mse95_norm, colour="q95")) +
      geom_point(aes(y=mseBeta_norm, colour="beta1")) +
      geom_point(aes(y=mseAlpha_norm, colour="beta0")) +
      geom_smooth(aes(y=msemean_norm, colour="mean"),se=0) +
      geom_smooth(aes(y=mse95_norm, colour="q95"),se=0) +
      geom_smooth(aes(y=mseBeta_norm, colour="beta1"),se=0) +
      geom_smooth(aes(y=mseAlpha_norm, colour="beta0"),se=0) +
      ylab("Standard error (as percent of max)") +
      scale_color_manual(values=c("beta1"="aquamarine","mean"="red",
                                  "q95"="blue","beta0"="aquamarine4")) +
      guides(colour=guide_legend(title=NULL)) +
      geom_vline(xintercept = currentAlpha, size=1)
    
    # plot_mse_local <- plot(df_mse$mu, df_mse$mse95)
    plot_mse_local
    
  })
  
  alpha <- reactive({
    input$alpha
  })
  
})