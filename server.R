
# This is the server logic for a Shiny web application.

library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output) {
  
  # Load Data from file
  data <- read.csv("basedata.csv")
  # Add some random jitter
  for(filter1 in c("a", "b")){
    for(filter2 in c("a", "b")){
      data <- data %>%
        mutate(measure=ifelse(factor1==filter1 & factor2==filter2,
                              measure + runif(1, -0.1, 0.1),
                              measure))
    }
  }
  

  output$distPlot <- renderPlot({

    # Create the desired effects
    if("mainEffect1" %in% input$effects){
      data <- data %>% mutate(measure=ifelse(factor1=="b", measure + 4.2, measure))
    }
    if("mainEffect2" %in% input$effects){
      data <- data %>% mutate(measure=ifelse(factor2=="b", measure + 4.2, measure))
    }
    if("interact12" %in% input$effects){
      data <- data %>%
        mutate(measure=ifelse(factor2=="b" & factor1=="b", measure + 2.5, measure)) %>%
        mutate(measure=ifelse(factor2=="a" & factor1=="a", measure + 2.5, measure))
    }

    
    means.interaction <- data %>% group_by(factor1, factor2) %>% summarise_each(measure, funs="mean")
    means.main1 <- data %>% group_by(factor1) %>% summarise_each(measure, funs="mean")
    means.main2 <- data %>% group_by(factor2) %>% summarise_each(measure, funs="mean")
    means.between <- data %>% group_by(factor1, factor2) %>% summarise_each(measure, funs="mean")
    grandmean <- data %>% summarise(mean=mean(measure))
    
    a <- ggplot(data, aes(x=factor2, y=measure, color=factor1),
                environment=environment()) +
      # nice white theme
      theme_bw(base_size=20) +
      # Add the labels
      labs(y="Dependent Variable", x="Factor 2", colour="Factor 1") +
      # Set the y axis scale
      ylim(0, 16) +
      # plot individual points
      geom_point(aes(group=interaction(factor1, factor2)),
                 position=position_dodge(width=0.5), size=2, shape=1) +
      # plot means of each group
      geom_point(aes(group=factor1),
                 stat="summary", fun.y="mean",
                 size=4, shape=16, show.legend=FALSE,
                 position=position_dodge(width=0.5)) +
      # Add a line connecting the two factor1 sets
      geom_line(aes(group=factor1),
                stat="summary", fun.y="mean",
                position=position_dodge(width=0.5),
                show.legend=FALSE)
    
    # Plot the comparison means
    if(input$squares=="error"){
      # a <- a + 
    } else if(input$squares=="total"){
      a <- a + geom_segment(data=grandmean, aes(x=0.5, xend=2.5, y=mean, yend=mean),
                            linetype="dashed", colour="black", group="Grand Mean", size=1)
    } else if(input$squares=="between"){
      a <- a +
        geom_segment(data=means.interaction, linetype="dashed",
                     aes(x=c(0.8, 1.8, 1, 2), xend=c(1, 2, 1.2, 2.2), y=measure,
                         yend=measure, colour=factor1)) +
        geom_segment(data=grandmean, aes(x=0.5, xend=2.5, y=mean, yend=mean),
                     linetype="dashed", colour="black", group="Grand Mean", size=1)
    }else if(input$squares=="main1"){
      a <- a +
        geom_segment(data=means.main1, linetype="dashed",
                     aes(x=c(0.8, 1), xend=c(2, 2.2), y=measure,
                         yend=measure, group=factor1, colour=factor1)) +
        geom_segment(data=grandmean, aes(x=0.5, xend=2.5, y=mean, yend=mean),
                     linetype="dashed", colour="black", group="Grand Mean", size=1)
    } else if(input$squares=="main2"){
      a <- a +
        geom_segment(data=means.main2, linetype="dashed", colour="black", size=0.5,
                     aes(x=c(0.8, 1.8), xend=c(1.2, 2.2), y=measure, yend=measure)) +
        geom_segment(data=grandmean, aes(x=0.5, xend=2.5, y=mean, yend=mean),
                     linetype="dashed", colour="black", group="Grand Mean", size=1)
    } else if(input$squares=="none" | input$squares=="interaction"){
      # Do nothing
    }
    
    # Add in the difference indicators
    for(filter1 in c("a", "b")){
      for(filter2 in c("a", "b")){
        subdata <- filter(data, factor1==filter1 & factor2==filter2)
        # Ugly hack because I don't know how to get xvalues after ggplot dodge
        if(input$squares=="total"){
          subdata <- filter(data, factor1==filter1 & factor2==filter2)
          comparison <- data %>%
            summarise(measure=mean(measure))
          if(filter2=="a" & filter1=="a"){xval=0.87}
          if(filter2=="a" & filter1=="b"){xval=1.13}
          if(filter2=="b" & filter1=="a"){xval=1.87}
          if(filter2=="b" & filter1=="b"){xval=2.13}
        }else if(input$squares=="main1"){
          subdata <- means.main1 %>% filter(factor1==filter1)
          comparison <- data %>%
            summarise(measure=mean(measure))
          if(filter1=="a"){xval=1.4}
          if(filter1=="b"){xval=1.6}
        }else if(input$squares=="main2"){
          subdata <- means.main2 %>% filter(factor2==filter2)
          comparison <- data %>%
            summarise(measure=mean(measure))
          if(filter2=="a"){xval=1}
          if(filter2=="b"){xval=2}
        }else if(input$squares=="error"){
          subdata <- filter(data, factor1==filter1 & factor2==filter2)
          # Create the comparison mean
          comparison <- data %>%
            group_by(factor1, factor2) %>%
            summarise_each(measure, funs="mean") %>%
            filter(factor1==filter1 & factor2==filter2)
          if(filter2=="a" & filter1=="a"){xval=0.87}
          if(filter2=="a" & filter1=="b"){xval=1.13}
          if(filter2=="b" & filter1=="a"){xval=1.87}
          if(filter2=="b" & filter1=="b"){xval=2.13}
        }else if(input$squares=="between"){
          subdata <- means.interaction %>% filter(factor1==filter1 & factor2==filter2)
          comparison <- data %>%
            summarise(measure=mean(measure))
          if(filter2=="a" & filter1=="a"){xval=0.87}
          if(filter2=="a" & filter1=="b"){xval=1.13}
          if(filter2=="b" & filter1=="a"){xval=1.87}
          if(filter2=="b" & filter1=="b"){xval=2.13}
        } else if(input$squares=="none" | input$squares=="interaction"){
          # Do nothing
          break
        }
        
        for(point in subdata$measure){
          a <- a +
            geom_curve(x=xval, xend=xval,
                       y=min(comparison$measure, point),
                       yend=max(comparison$measure, point),
                       curvature=-0.5, colour="gray", size=0.2,
                       linetype="dashed")
        }
      }
    }
    
    a
    

  })
  
  output$errorExplanation <- renderText({
    if(input$squares=="between"){
      paste("The Model SS is calculated by subtracting each group mean from the grand mean, ",
            "squaring the differences, then summing them up.")
    } else if(input$squares=="total"){
      paste("The total SS is calculated by subtracing each point from ",
            "the grand mean, squaring the differences, and summing them.")
    } else if(input$squares=="main1"){
      paste("The SS for Factor 1 is calculated by subtracting the mean ",
            "of each level in Factor 1 from the grand mean, squaring ",
            "the differences, then summing them.")
    } else if(input$squares=="main2"){
      paste("The SS for Factor 2 is calculated by subtracting the mean ",
            "of each level in Factor 2 from the grand mean, squaring ",
            "the differences, then summing them.")
    } else if(input$squares=="none"){
      ""  # empty string
    } else if(input$squares=="error"){
      paste("The Error SS is calculated by subtracting each point from ",
            "its group's mean, squaring it and summing it.")
    } else if(input$squares=="interaction"){
      paste("The SS for the interaction is calculated by first calculating ",
            "the Between-Group SS (as if you are doing a one-way ANOVA), and ",
            "then subtracting the SS of Factor 1 and Factor 2. To visualise ",
            "this, simply first click Model, then Effect of Factor 1, ",
            "then Effect of Factor 2 - and imagine subtracting them in your head.")
    }
  })
  
})
