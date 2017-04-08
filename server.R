library(shiny)


shinyServer(function(input, output) {
  

  output$plot_simus <- renderPlot({
    
    indexList <- which.min(abs(seq(0,1,length.out = length(listPlotsArticle)) - input$alpha))
    currentPlot <- listPlotsArticle[[indexList]]$plot
    
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
    
    print(currentPlot)
  })
  
  output$plot_mse <- renderPlot({
    
    currentAlpha <- input$alpha
    # point_df <- data.frame(rep(currentAlpha,3),
    #                        c(df_mse[df_mse$alpha==currentAlpha,c("msemean_norm","mse95_norm","mseBeta_norm")]))
    # names(point_df)[1] <- "alpha"
    
    plot_mse <- plot_mse +
      # geom_point(data=point_df, aes(x=alpha,y=msemean_norm)) +
      # geom_point(data=point_df, aes(x=alpha,y=mse95_norm)) +
      # geom_point(data=point_df, aes(x=alpha,y=mseBeta_norm)) +
      geom_vline(xintercept = currentAlpha, size=1)
    
    print(plot_mse)
  })
  
})