library(shiny)

shinyServer(function(input, output, session){
  
  plate <- reactive ({
    matrix(table()[[1]]$value, ncol = 12, nrow = 8, byrow = T, dimnames = list(LETTERS[1:8], 1:12))
  })
  
  table <- reactive ({
    validate(
      need(input$measurements != "", "Enter measurement data"),
      need(input$background.position != "", "Enter background measurement wells"),
      need(input$sample.dilution.factor != "", "Enter sample dilution factor value"),
      need(input$std.curve.position.1 %>% string2vector %>% length > 1 | input$std.curve.position.2 %>% string2vector %>% length > 1, "Enter at least 2 standard curve well positions")
    )
    
    pico.green(value = input$measurements,
               background.position = input$background.position,
               sample.dilution.factor = input$sample.dilution.factor,
               std.curve.position.1 = input$std.curve.position.1,
               std.curve.position.2 = input$std.curve.position.2,
               starting.conc.1 = input$starting.conc.1,
               starting.conc.2 = input$starting.conc.2,
               serial.dilution.factor.1 = input$serial.dilution.factor.1,
               serial.dilution.factor.2 = input$serial.dilution.factor.2,
               group1 = input$group1, group1.pos = input$group1.pos,
               group2 = input$group2, group2.pos = input$group2.pos,
               group3 = input$group3, group3.pos = input$group3.pos,
               group4 = input$group4, group4.pos = input$group4.pos,
               group5 = input$group5, group5.pos = input$group5.pos,
               group6 = input$group6, group6.pos = input$group6.pos
    )
  })
  
  # standard curve ggplot
  std.curve <- reactive ({
    a <- table()$std.curve # %>% mutate(x = value, y = conc)
    b <- a[str_count(a$sample, "Std Curve") == 1, ]
    
    lm_eqn <- function(df){
      df <- df %>% mutate(x = value, y = conc)
      m <- lm(y ~ x, df);
      eq <- substitute(#italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                       italic(r)^2~"="~r2, 
                       list(#a = format(coef(m)[1], digits = 2), 
                            #b = format(coef(m)[2], digits = 2), 
                            r2 = format(summary(m)$r.squared, digits = 3)))
      as.character(as.expression(eq));                 
    }
    
    ggplot(b, aes(x = value, y = conc)) + 
      geom_smooth(method = "lm", color = "black", size = 0.5) + geom_point(alpha = 0.5, size = 4, shape = 1) +
      annotate("text", x = min(b$value), y = max(b$conc), hjust = 0, label = lm_eqn(b), colour = "black", size = 5, parse=TRUE) +
      theme_bw()
  })
  
  # sample summary statistics ggplot
  sample.plot <- reactive ({
    a <- table()$table
    ggplot(a[str_count (a$sample, "(Std Curve)|(unknown)") == 0, ],
           aes(x = sample, y = conc, color = sample)) + stat_summary(fun.y = mean, fun.ymin = function(x){mean(x)-sd(x)}, fun.ymax = function(x){mean(x)+sd(x)}) +
      geom_point(alpha = 0.5) +
      theme_bw()
  })
  
  # output variables
  output$plate <- renderPrint({ round(plate(), 1) })
  output$table <- renderTable({ table()$sample_table })
  output$summary_stats <- renderTable({ table()$summary_table })
  output$std_curve <- renderPlot({ std.curve() })
  output$sample_plot <- renderPlot({ sample.plot() })
  
})


