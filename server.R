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
        ylim(c(0.85*vrai_alpha,vrai_alpha/0.85))
    }
    
    print(currentPlot)
  })
  
  output$plot_sd <- renderPlot({
    
    currentAlpha <- input$alpha
    # point_df <- data.frame(rep(currentAlpha,3),
    #                        c(df_sd[df_sd$alpha==currentAlpha,c("sdmean_norm","sd95_norm","sdBeta_norm")]))
    # names(point_df)[1] <- "alpha"
    
    plot_sd <- plot_sd +
      # geom_point(data=point_df, aes(x=alpha,y=sdmean_norm)) +
      # geom_point(data=point_df, aes(x=alpha,y=sd95_norm)) +
      # geom_point(data=point_df, aes(x=alpha,y=sdBeta_norm)) +
      geom_vline(xintercept = currentAlpha, size=1)
    
    print(plot_sd)
  })
  
})