# Load descriptions about H0 of tests Hypotheses:
dados_desc_te <- read.csv("desc_testes_estatisticos.csv", sep = ',')

# Server configurations: 
server <- function(input, output, session) {
  
  # Tema do shiny
  thematic::thematic_shiny()
  
  #===========================================================================
   # Load input file
  data <- reactive({
    file1 <- input$file_input
    if(is.null(file1)){
      return(NULL)
    }
    read.csv(file1$datapath, header = TRUE)
  })
  
  # Renderização da tabela
  output$table <- renderTable({
    if(is.null(data())){
      return(NULL)
    }
    head(data(),3)
  })
  
  # Interface for print table about data:
  output$table <- renderUI({
    'Data Head:'
    tableOutput("table")    
  })
  
  # Calculate the summary of the data and store it in an output object
  output$dim <- renderPrint({
    dim(data())
  })
  
  output$colmeans <- renderPrint({
    colMeans(data())
  })
  #===========================================================================
  
  # Vetor dos testes de uma amostra
  #===========================================================================
  output$select_test <- renderUI ({
    group_map <- list('For compare Means'=c('t de Student', 
                                            'ANOVA', 'Kruskal-Wallis', 
                                            'Mann-Whitney U'),
                      
                      'For compare Distributions'=c('Kolmogorov-Smirnov (K-S)', 
                                                    'chi-quadrado',
                                                    'Mantel-Haenszel',
                                                    'Wilcoxon-Mann-Whitney',
                                                    'Kruskal-Wallis'),
                      
                      'For compare Variations'=c('Kruskal-Wallis',
                                                 'Fligner-Killeen',
                                                 'Bartlett'))
    
    selectInput("select_test",'Select the Hypothese Tests', choices = group_map[[input$group_test]])
  })
  
  output$config_test <- renderUI({
    group_config_test <- list('t de Student'= c('For one', 'For two - independent', 'For two - dependent'),
                              'Other_test_here'=c('pass'))
    if(input$select_test %in% names(group_config_test)){
      selectInput('config_test','Select Type', choices = group_config_test[[input$select_test]])
    }
  })
  
  output$mu <- renderUI({
    if(input$config_test == 'For one')
    numericInput(
      inputId = "mu",
      label = "Populational Mean estimated",
      value = 0
    )
  })
  
  output$select_data <- renderUI({
    selectInput('select_data','Select Column', choices= colnames(data()))
  })
  #===========================================================================
  output$confidencelevel <- renderUI({
    confidencelevel <- c("t de Student", "Teste de Wilcoxon Signed Rank", "Teste t Para Duas Amostras")
    
    # Se o teste selecionado estiver na lista anterior, mostra a caixa solicitando o intervalo de confiança
    if(input$select_test %in% confidencelevel){
      selectInput(
        inputId = "conf.level",
        label = "Select the Confidence Interval:",
        choices = list("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
        selected = 0.95
      )
    }
  })
  
  # Execute the Hypothese test:
  stat_test <- eventReactive(input$generate, {
    
    # Data
    primeira_amostra <- as.numeric(data()[[input$select_data]])
    segunda_amostra <- as.numeric(unlist(str_split(input$segunda_amostra, pattern = ",")))
    conf.level <- as.numeric(input$conf.level)
    # Execute the select tests:
    if(input$select_test == "t de Student") {
      test_result <- t.test(primeira_amostra, mu = input$mu, conf.level = conf.level) %>% tidy() 
      print(input$mu)
    } 
    else if (input$nometeste == "Teste t Para Duas Amostras") {
      test_result <- t.test(x = primeira_amostra, y = segunda_amostra, mu = input$mu, conf.level = conf.level) %>% tidy()
    } 
    else if (input$nometeste == "Teste de Wilcoxon Signed Rank") {
      test_result <- wilcox.test(primeira_amostra, mu = input$mu, conf.level = conf.level) %>% tidy()
    } 
    else if (input$nometeste == "Teste de Shapiro-Wilk") {
      test_result <- shapiro.test(primeira_amostra) %>% tidy()
    } 
    else if (input$nometeste == "Teste Kolmogorov-Smirnov") {
      test_result <- ks.test(x = primeira_amostra, y = segunda_amostra) %>% tidy()
    } 
    
    # Organize results about tests:
    test_result_tidy <- test_result %>% 
      mutate(result = ifelse(p.value <= 0.05, "Statistically Significant. We reject the  H0", 
                             "Statistically Insignificant. We failed to reject H0")) %>% 
      t() %>% 
      tibble(Parameter = rownames(.), Value = .[,1]) %>% 
      select(-1) %>% 
      mutate(Parameter = str_to_title(Parameter))
    
    return(test_result_tidy)
    
  })
  
  # Formating the table about results tests
  output$testresult <- renderDT({
    datatable(
      stat_test(),
      rownames = FALSE,
      options = list(
        dom = 't',
        columnDefs = list(
          list(
            className = "dt-center",
            targets = "_all"
          )
        )
      )
    )
  })
  
  # formating the data for make histogram
  hist_vector <- eventReactive(input$generate, {
    
    # density function
    primeira_amostra <- density(as.numeric(data()[[input$select_data]]))
    
    return(primeira_amostra)
    
  })
  
  # Plot do histogram
  output$hist <- renderPlotly({
    hist_vector <- hist_vector()
    plot_ly(x = ~hist_vector$x, 
            y = ~hist_vector$y, 
            type = "scatter", 
            mode = "lines", 
            fill = "tozeroy") %>%  
      layout(xaxis = list(title = "Data"), 
             yaxis = list(title = "Density"))
  })
  
  # Results: 
  testresulttitle <- eventReactive(input$generate, {"Test Result"})
  histogramtitle <- eventReactive(input$generate, {"Histogram"})
  output$testresulttitle <- renderText({paste(testresulttitle())})
  output$histogramtitle <- renderText({paste(histogramtitle())})
  testdescription <- eventReactive(input$generate, {"About the Hypothese null (H0)"})
  output$descriptiontitle <- renderText({paste(testdescription())})
  
  nometestedesc <- eventReactive(input$generate, {
    print('before')
    nometestedesc <- dados_desc_te %>% 
      dplyr::filter(nometeste == input$select_test)
    print('after')
  })
  
  output$nometestedesc <- renderText({paste(nometestedesc()[['desc']])})
}
