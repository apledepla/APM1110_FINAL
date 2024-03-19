library(shiny)
library(ggplot2)
library(shinydashboard)
library(shinyalert)

ui <- dashboardPage(
  dashboardHeader(title = "SA1"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Probability for defective rates (#1)", tabName = "def_rates", icon = icon("globe")),
      menuItem("Analysis for Univariate (#2)", tabName = "analysis_univariate", icon = icon("calculator")),
      menuItem("Analysis for Bivariate (#2)", tabName = "analysis_bivariate", icon = icon("calculator")),
      menuItem("Markov Memoryless (#3)", tabName = "markov", icon = icon("bar-chart"))
      
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "analysis_bivariate",
        fluidRow(
          box(
            title = "Input Parameters",
            titleStyle="color:red",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            textInput("size_input_bi", "Sample Size (n): "),
            textInput("probx_input_bi","Probability of x (p_x): "),
            textInput("proby_input_bi", "Probability of y (p_y): "),
            actionButton("calculate_btn_bi", "Calculate")
          ),
          box(
            title = "Results",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            textOutput("samples_bi"),
            textOutput("mean_output_bi"),
            textOutput("variance_output_bi"),
            plotOutput("pdf_bi"),
            plotOutput("cdf_bi"),
            plotOutput("pdf_marginal_x"),
            plotOutput("pdf_marginal_y"),
            plotOutput("pdf_conditional_x"),
            plotOutput("pdf_conditional_y")
          )
        )
      ),
      tabItem(
        tabName = "analysis_univariate",
        fluidRow(
          box(
            title= "Input Parameters",
            status="primary",
            solidHeader = TRUE,
            width=4,
            textInput("size_input_uni", "Sample Size (n): "),
            textInput("prob_input_uni","Probability(p): "),
            actionButton("calculate_btn_uni", "Calculate")
            
          ),
          box(
            title = "Results",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            textOutput("samples_uni"),
            textOutput("mean_output_uni"),
            textOutput("variance_output_uni"),
            plotOutput("pdf_uni"),
            plotOutput("cdf_uni")
          )
        )
      ),
      tabItem(
        tabName = "markov",
        fluidRow(
          box(
            title="Input Parameters",
            status="primary",
            solidHeader = TRUE,
            width=4,
            textInput("markov_input","Probability(p): "),
            actionButton("calculate_btn_markov", "Calculate")
            
          ),
          box(
            title = "Results",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            textOutput("mean_output_markov"),
            textOutput("variance_output_markov"),
            plotOutput("pdf_markov"),
            textOutput("con_res"),
            textOutput("meancon_output_markov"),
            textOutput("variancecon_output_markov"),
            textOutput("p_4_3"),
            textOutput("p_1"),
            textOutput("p_5_3"),
            textOutput("p_2")
            
          )
        )
      ),
      tabItem(
        tabName = "def_rates",
        fluidRow(
          box(
            title="Input Parameters",
            status="primary",
            solidHeader = TRUE,
            width=4,
            textInput("x1","Probability that the product is from Factory 1 (x1): "),
            textInput("x2","Probability that the product is from Factory 2 (x2): "),
            textInput("x3","Probability that the product is from Factory 3 (x3): "),
            textInput("y1","Probability that the defect is from Factory 1 (y1): "),
            textInput("y2","Probability that the defect is from Factory 2 (y2): "),
            textInput("y3","Probability that the defect is from Factory 3 (y3): "),
            actionButton("calculate_btn_def", "Calculate")
          ),
          box(
            title="Results",
            status="primary",
            solidHeader = TRUE,
            width=4,
            textOutput("defective")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calculate_btn_bi, {
    n <- as.numeric(input$size_input_bi)
    px <- as.numeric(input$probx_input_bi)
    py <- as.numeric(input$proby_input_bi)
    
    samplesx <- rgeom(n , px)
    samplesy <- rgeom(n,py)
    join_samples <- table(samplesx, samplesy) / n
    join_samples_vec <- as.vector(join_samples)
    mean_val <- mean(join_samples_vec)
    var_val <- var(join_samples_vec)
    
    output$samples_bi <- renderPrint(paste("Random generated variables: ", paste(join_samples, collapse= ", ")))
    output$mean_output_bi <- renderPrint(paste("Mean:", mean_val))
    output$variance_output_bi <- renderPrint(paste("Variance:", var_val))
    
    # Prob density function and cum density function (pdf and cdf plots)
    df <- data.frame(x = join_samples_vec)
    
    output$pdf_bi <- renderPlot({
      ggplot(df, aes(x)) +
        geom_density()+
        labs(title = "Probability Density Function (PDF)")
    })
    
    output$cdf_bi <- renderPlot({
      ggplot(df, aes(x = x)) +
        stat_function(fun = pnorm) +
        labs(title = "Cumulative Distribution Function (CDF)") +
        xlab("Value") +
        ylab("Cumulative probability")
    })
    
    marginal_x <- margin.table(join_samples, 1)  
    marginal_y <- margin.table(join_samples, 2)  
    
    output$pdf_marginal_x <- renderPlot({
      plot(names(marginal_x), marginal_x, type = "h", main = "Marginal PDF of X", xlab = "X", ylab = "Density")
    })
    
    output$pdf_marginal_y <- renderPlot({
      plot(names(marginal_y), marginal_y, type = "h", main = "Marginal PDF of Y", xlab = "Y", ylab = "Density")
    })
    
    conditional_x <- join_samples / sum(join_samples[, "1"])  # Conditional distribution of X given Y = 1
    conditional_y <- join_samples / sum(join_samples["1", ])  # Conditional distribution of Y given X = 1
    
    output$pdf_conditional_x <- renderPlot({
      plot(names(conditional_x[, "1"]), conditional_x[, "1"], type = "h", main = "Conditional PDF of X given Y=1", xlab = "X", ylab = "Density")
    })
    
    output$pdf_conditional_y <- renderPlot({
      plot(names(conditional_y["1", ]), conditional_y["1", ], type = "h", main = "Conditional PDF of Y given X=1", xlab = "Y", ylab = "Density")
    })
  })
  
  observeEvent(input$calculate_btn_uni,{
    
    n <- as.numeric(input$size_input_uni)
    p <- as.numeric(input$prob_input_uni)
    
    samples <- rgeom(n , p)
    
    mean_val <- mean(samples)
    var_val <- var(samples)
    
    output$samples_uni <- renderText(paste("Random generated variables: ", paste(samples, collapse= ", ")))
    output$mean_output_uni <- renderText(paste("Mean:", mean_val))
    output$variance_output_uni <- renderText(paste("Variance:", var_val))
    
    
    #prob density function and cum density function (pdf and cdf plots)
    library (ggplot2) 
    
    df <- data.frame(x = samples)
    
    output$pdf_uni <- renderPlot({
      ggplot(df, aes(x)) +
        geom_density()+
        labs(title = "Probability Density Function (PDF)")
    })
    
    cdf_g<-ecdf(samples)
    
    output$cdf_uni<- renderPlot({
      ggplot(df, aes(x = x)) + 
        stat_function(fun = pnorm)+
        labs(title = "Cumulative Distribution Function (CDF)")+
        xlab("Value") +
        ylab("Cumulative probability")
    })
    
    
  })
  observeEvent(input$calculate_btn_markov,{
    n <- 10000
    p <- as.numeric(input$markov_input)
    
    x<-rgeom(n,prob=p)
    
    mean_x<-round(mean(x), 2)
    var_x<-round(var(x),2)
    
    output$mean_output_markov <- renderText(paste("Mean:", mean_x))
    output$variance_output_markov <- renderText(paste("Variance:", var_x))
    
    output$pdf_markov<- renderPlot({
      hist(x, breaks = seq(min(x) - 0.5, max(x) + 0.5, by = 1),
         main = "Number of Searches needed for the 1st Success",
         xlab = "Number of Searches", ylab = "Frequency",
         col = "purple")
      
    })
    
    conditional_results <- x[x > 3]
    
    mean_con<-round(mean(conditional_results), 2)
    var_con<-round(var(conditional_results),2)
    output$con_res<- renderText(paste("Conditional distribution: ", paste(conditional_results, collapse= ", ")))
    output$meancon_output_markov <- renderText(paste("Mean of conditional distribution:", mean_con))
    output$variancecon_output_markov <- renderText(paste("Variance of conditional distribution:", var_con))
    
    
    #bullet points
    
    p_b<-0.6
    
    x<-rgeom(n,prob=p_b)
    
    conditional_results_4 <- x[x == 4]
    
    sum_con_res_4 <- sum(conditional_results_4==4)
    
    p_4 <- sum_con_res_4/n
    

    
    conditional_results_3 <- x[x > 3]
    sum_con_res_3 <- sum(conditional_results_3>3)
    p_3 <- sum_con_res_3/n

    conditional_results_5 <- x[x == 5]
    sum_con_res_5 <- sum(conditional_results_5==5)
    p_5 <- sum_con_res_5/n

    
    conditional_results_1 <- x[x == 1]
    sum_con_res_1 <- sum(conditional_results_1==1)
    p_1 <- sum_con_res_1/n

    
    conditional_results_2 <- x[x == 2]
    sum_con_res_2 <- sum(conditional_results_2==2)
    p_2 <- sum_con_res_2/n
    

    ## P(x=4|x>3):

    p_4_3 <-(p_4*p_3)/p_3
    
    
    
    ##P(x=5|x>3):
    p_5_3 <-(p_5*p_3)/p_3
    
    output$p_4_3 <- renderText(paste("Probability of X=4 given X>3:", p_4_3*100))
    output$p_1 <- renderText(paste("Probability of X=1:", p_1*100))
    output$p_5_3 <- renderText(paste("Probability of X=5 given X>3:", p_5_3*100))
    output$p_2 <- renderText(paste("Probability of X=2:", p_2*100))
    
  })
  observeEvent(input$calculate_btn_def,{
    x1<-as.numeric(input$x1)
    x2<-as.numeric(input$x2)
    x3<-as.numeric(input$x3)
    y1<-as.numeric(input$y1)
    y2<-as.numeric(input$y2)
    y3<-as.numeric(input$y3)
    
    checker <- 0
    
    
    if ((0.1 <= x1 && x1 <=0.4) &&(0.01<= y1 && y1<= 0.05)){
      cat("Both x1 and y1 are within their specified ranges.\n")
      checker <- checker +1
    } else{
      if (!(0.1<= x1 && x1<= 0.4)){
        shinyalert(title = "Error", text = "x1 is not in between the ranges of 10% and 40%")
      }
      if (!(0.01<= y1 && y1<= 0.05)) {
        shinyalert(title = "Error", text = "y1 is not in between the ranges of 1% and 5%")

      }
    }
    
    
    
    if ((0.1 <= x2 && x2 <=0.4) &&(0.01<= y2 && y2<= 0.05)){
      cat("Both x2 and y2 are within their specified ranges.\n")
      checker <- checker +1
      
    } else{
      if (!(0.1<= x2 && x2<= 0.4)){
        shinyalert(title = "Error", text = "x2 is not in between the ranges of 10% and 40%")
      }
      if (!(0.01<= y2&& y2<= 0.05)) {
        shinyalert(title = "Error", text = "y2 is not in between the ranges of 1% and 5%")
        
      }
    }
    
    
    if ((0.1 <= x3 && x3 <=0.4) &&(0.01<= y3 && y3<= 0.05)){
      cat("Both x3 and y3 are within their specified ranges.\n")
      checker <- checker +1
      
    } else{
      if (!(0.1<= x3 && x3<= 0.4)){
        shinyalert(title = "Error", text = "x3 is not in between the ranges of 10% and 40%")
      }
      if (!(0.01<= y3&& y3<= 0.05)) {
        shinyalert(title = "Error", text = "y2 is not in between the ranges of 1% and 5%")
        
      }
    }
    
    #getting the prob of defect 
    #checker is for validating that all are within conditinons. 
    pr_defect <- (x1*y1) + (x2*y2) + (x3*y3)
    if(checker == 3){
      output$defective<- renderText(paste("Probability of getting a defect is:", pr_defect*100))
    }else{
      shinyalert(title = "Error", text = "There is a problem with the conditions")
    }
    
  })
}

shinyApp(ui = ui, server = server)


