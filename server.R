library(shiny)
library(ggplot2)
library(shinyjs)
library(cowplot)
library(dplyr)
elementos <- sort(round(runif(20, min = 1, max = 10)))

values <- reactiveValues()
values$showMedia <- FALSE
normal_params <- reactiveValues()
normal_params$u <- 0
normal_params$dp <- 1
normal_params$min <- 1
normal_params$max <- 4
normal_params$sit <- 1

testeT1_params <- reactiveValues()
testeT1_params$u <- 0
testeT1_params$dp <- 1
testeT1_params$X <- 0.18
testeT1_params$n <- 10
testeT1_params$alpha <- 0.05

testeT2_params <- reactiveValues()
testeT2_params$n <- 12
testeT2_params$u1 <- 10
testeT2_params$u2 <- 9
testeT2_params$dp1 <- 2.2
testeT2_params$dp2 <- 2.2
p1 <- rnorm(12, 10, 2.2)
p2 <- rnorm(12, 9, 2.2)
testeT2_params$p1 <- p1
testeT2_params$p2 <- p2
testeT2_params$D <- p1 - p2

testeQui_params <- reactiveValues()
testeQui_params$matrix <- rbind(c(48, 8, 56), c(30, 21, 51), c(78, 29, 107))
testeQui_params$expected <- rbind(c(41, 15, 56), c(37, 14, 51), c(78, 29, 107))
testeQui_params$colnames <- c("Melhora", "Não", "Total")
testeQui_params$rownames <- c("Droga", "Placebo", "Total")
testeQui_params$chi <- 9.3


blue <- c("#A1D2CE", "#8AD1CB", "#78CAD2", "#62A8AC", "#4CA6AA", "#5497A7", "#50858B", "#254A4F")
colorful <- c("#20BFB2", "#63BE76", "#A8B245", "#E49C45", "#FE8A77", "#F48BAE", "#BCA0D8", "#64B5DA", "#c9e5f2")
medium_cyan <- '#3db9bf'
dark_cyan <- '#077e84'
medium_ocre <- "#bf9f56"
dark_orange <- "#8c6a1c"
brown <- "#4c380c"
red_nail <- '#ce4b37'
red_broken_nail <- '#300802'

  
getmode <- function(v) {
  uv <- unique(v)
  tab <- tabulate(match(v, uv))
  uv[tab == max(tab)]
}

vectorToString <- function(v) {
  string <- v[1]
  if (length(v) > 1) {
    string <- "{"
    for (i in (1:length(v))) {
      if (i != length(v)) {
        string <- paste0(string, v[i], ", ")
      }
      else {
        string <- paste0(string, v[i], "}")
      }
    }
  }
  return (string)
}

function(input, output, session) {
  
  # output$imagemLogo <- renderImage({
  #   return(list(
  #     src = "www/images/garu_3.png",
  #     filetype = "image/png",
  #     alt = "Garu"
  #   ))
  # }, deleteFile = FALSE)
  
  output$tabelaExemploVariaveis <- renderTable(example_dataframe, bordered = TRUE)
  
  output$imagemTiposVariaveis <- renderImage({
    return(list(
      src = "www/images/tipos_variaveis.png",
      filetype = "image/png",
      alt = "Tipos de Variaveis"
    ))
  }, deleteFile = FALSE)
  
  output$calcFrequencia <- renderText({
    "Frequência: Número de ocorrências de determinado valor"
  })
  
  output$calcProporcao <- renderText({
    "Proporção: Frequência dividida pelo número total de ocorrências"
  })
  
  output$tabelaFreqRelacionamento <- renderTable(tab_frequencia_relacionamento,
                                                 bordered = TRUE, striped = TRUE)
  
  output$tabelaFreqAnoLetivo <- renderTable(tab_frequencia_ano_letivo,
                                                 bordered = TRUE, striped = TRUE)
  
  output$tabelaFreqPeso <- renderTable(tab_frequencia_peso,
                                       bordered = TRUE, striped = TRUE)
  
  observeEvent(input$geraElementos, {
    elementos <<- sort(round(runif(input$slider_qtd_elementos, min = 1, max = input$slider_max_valor)))
  })
  
  getElementos <- reactive({
    return(elementos)
  })
  
  output$printElementos <- renderText({
    input$geraElementos
    elementos
  })
  
  #Média
  
  observeEvent(input$mediaMostrarMais, {
    shinyjs::hide("mediaMostrarMais")
    shinyjs::show("mediaMostrarMenos")
    shinyjs::show("mediaTexto")
    shinyjs::show("mediaExplain")
    shinyjs::show("htMedia")
    shinyjs::show("exMedia")
  })
  
  observeEvent(input$mediaMostrarMenos, {
    shinyjs::hide("mediaMostrarMenos")
    shinyjs::show("mediaMostrarMais")
    shinyjs::hide("mediaTexto")
    shinyjs::hide("mediaExplain")
    shinyjs::hide("htMedia")
    shinyjs::hide("exMedia")
  })
  
  output$mediaTitle <- renderText({
    input$geraElementos
    paste0("<h4> Média: <strong> ", mean(elementos), "</strong> </h4>")
  })
  
  output$showMedia <- reactive({
    return(values$showMedia)
  })
  
  output$mediaExplain <- renderUI({
    withMathJax(helpText("Média:  $$\\frac{1}{n} \\sum_{i=1}^{n} x_{i}$$"))
  })
  
  output$exMedia <- renderText({
    input$geraElementos
    text <- "Média = ("
    for (i in (1:length(elementos))) {
      if (i != length(elementos)) {
        text <- paste0(text, elementos[i], " + ")
      } else {
        text <- paste0(text, elementos[i])
      }
        
    }
    
    text <- paste0(text, ")/", length(elementos), " = ", mean(elementos))
    text
  })
  
  #Mediana
  
  observeEvent(input$medianaMostrarMais, {
    shinyjs::hide("medianaMostrarMais")
    shinyjs::show("medianaMostrarMenos")
    shinyjs::show("medianaTexto")
    shinyjs::show("medianaExplain")
    shinyjs::show("htMediana")
    shinyjs::show("exMediana")
  })
  
  observeEvent(input$medianaMostrarMenos, {
    shinyjs::hide("medianaMostrarMenos")
    shinyjs::show("medianaMostrarMais")
    shinyjs::hide("medianaTexto")
    shinyjs::hide("medianaExplain")
    shinyjs::hide("htMediana")
    shinyjs::hide("exMediana")
  })
  
  output$medianaTitle <- renderText({
    input$geraElementos
    paste0("<h4> Mediana: <strong> ", median(elementos), "</strong> </h4>")
  })
  
  output$medianaExplain <- renderUI({
    withMathJax(helpText("Mediana (n ímpar): $$x_{\\frac{n+1}{2}}$$"),
                helpText("Mediana (n par): $$\\frac{x_{\\frac{n}{2}} + x_{\\frac{n}{2} + 1}}{2}$$")
                )
  })
  
  output$exMediana <- renderText({
    input$geraElementos
    text <- "Mediana = "
    if (length(elementos)%%2 == 1) {
      text <- paste0(text, median(elementos))
    } else {
      text <- paste0(text, "(", elementos[length(elementos)/2], " + ", elementos[(length(elementos)/2) + 1], ")/2 = ", median(elementos))
    }
    text
  })
  
  #Moda
  
  observeEvent(input$modaMostrarMais, {
    shinyjs::hide("modaMostrarMais")
    shinyjs::show("modaMostrarMenos")
    shinyjs::show("modaTexto")
    shinyjs::show("modaExplain")
    shinyjs::show("htModa")
    shinyjs::show("exModa")
  })
  
  observeEvent(input$modaMostrarMenos, {
    shinyjs::hide("modaMostrarMenos")
    shinyjs::show("modaMostrarMais")
    shinyjs::hide("modaTexto")
    shinyjs::hide("modaExplain")
    shinyjs::hide("htModa")
    shinyjs::hide("exModa")
  })
  
  output$modaTitle <- renderText({
    input$geraElementos
    paste0("<h4> Moda: <strong> ", vectorToString(getmode(elementos)), "</strong> </h4>")
  })
  
  output$exModa <- renderText({
    input$geraElementos
    text <- paste0("Moda = ", vectorToString(getmode(elementos)))
    text
  })
  
  #Mínimo
  
  observeEvent(input$minimoMostrarMais, {
    shinyjs::hide("minimoMostrarMais")
    shinyjs::show("minimoMostrarMenos")
    shinyjs::show("minimoTexto")
    shinyjs::show("minimoExplain")
    shinyjs::show("htMinimo")
    shinyjs::show("exMinimo")
  })
  
  observeEvent(input$minimoMostrarMenos, {
    shinyjs::hide("minimoMostrarMenos")
    shinyjs::show("minimoMostrarMais")
    shinyjs::hide("minimoTexto")
    shinyjs::hide("minimoExplain")
    shinyjs::hide("htMinimo")
    shinyjs::hide("exMinimo")
  })
  
  output$minimoTitle <- renderText({
    input$geraElementos
    paste0("<h4> Mínimo: <strong> ", min(elementos), "</strong> </h4>")
  })
  
  output$exMinimo <- renderText({
    input$geraElementos
    text <- paste0("Mínimo = ", min(elementos))
    text
  })
  
  #Máximo
  
  observeEvent(input$maximoMostrarMais, {
    shinyjs::hide("maximoMostrarMais")
    shinyjs::show("maximoMostrarMenos")
    shinyjs::show("maximoTexto")
    shinyjs::show("maximoExplain")
    shinyjs::show("htMaximo")
    shinyjs::show("exMaximo")
  })
  
  observeEvent(input$maximoMostrarMenos, {
    shinyjs::hide("maximoMostrarMenos")
    shinyjs::show("maximoMostrarMais")
    shinyjs::hide("maximoTexto")
    shinyjs::hide("maximoExplain")
    shinyjs::hide("htMaximo")
    shinyjs::hide("exMaximo")
  })
  
  output$maximoTitle <- renderText({
    input$geraElementos
    paste0("<h4> Máximo: <strong> ", max(elementos), "</strong> </h4>")
  })
  
  output$exMaximo <- renderText({
    input$geraElementos
    text <- paste0("Máximo = ", max(elementos))
    text
  })
  
  #Variância
  
  observeEvent(input$varianciaMostrarMais, {
    shinyjs::hide("varianciaMostrarMais")
    shinyjs::show("varianciaMostrarMenos")
    shinyjs::show("varianciaTexto")
    shinyjs::show("varianciaExplain")
    shinyjs::show("htVariancia")
    shinyjs::show("exVariancia")
  })
  
  observeEvent(input$varianciaMostrarMenos, {
    shinyjs::hide("varianciaMostrarMenos")
    shinyjs::show("varianciaMostrarMais")
    shinyjs::hide("varianciaTexto")
    shinyjs::hide("varianciaExplain")
    shinyjs::hide("htVariancia")
    shinyjs::hide("exVariancia")
  })
  
  output$varianciaTitle <- renderText({
    input$geraElementos
    paste0("<h4> Variância: <strong> ", round(var(elementos), 4), "</strong> </h4>")
  })
  
  output$varianciaExplain <- renderUI({
    withMathJax(helpText("Variância: $$\\frac{\\sum_{i=1}^{n} (x_{i} - \\bar{x})^{2}}{n}$$")
    )
  })
  
  output$exVariancia <- renderText({
    input$geraElementos
    text <- "Variância = ("
    for (i in (1:length(elementos))) {
      if (i != length(elementos)) {
        text <- paste0(text, "(",elementos[i], " - ", mean(elementos), ")^2 + ")
      }
      else {
        text <- paste0(text, "(",elementos[i], " - ", mean(elementos), ")^2)/", length(elementos), " = ", var(elementos))
      }
    }
    text
  })
  
  #Desvio Padrão
  
  observeEvent(input$dpMostrarMais, {
    shinyjs::hide("dpMostrarMais")
    shinyjs::show("dpMostrarMenos")
    shinyjs::show("dpTexto")
    shinyjs::show("dpExplain")
    shinyjs::show("htDp")
    shinyjs::show("exDp")
  })
  
  observeEvent(input$dpMostrarMenos, {
    shinyjs::hide("dpMostrarMenos")
    shinyjs::show("dpMostrarMais")
    shinyjs::hide("dpTexto")
    shinyjs::hide("dpExplain")
    shinyjs::hide("htDp")
    shinyjs::hide("exDp")
  })
  
  output$dpTitle <- renderText({
    input$geraElementos
    paste0("<h4> Desvio Padrão: <strong> ", round(sd(elementos), 4), "</strong> </h4>")
  })
  
  output$dpExplain <- renderUI({
    withMathJax(helpText("Desvio Padrão: $$\\sqrt{var(X)}$$")
    )
  })
  
  output$exDp <- renderText({
    input$geraElementos
    text <- paste0("Desvio Padrão = sqrt(", var(elementos), ") = ", sd(elementos))
    text
  })
  
  #Erro Padrão
  
  observeEvent(input$epMostrarMais, {
    shinyjs::hide("epMostrarMais")
    shinyjs::show("epMostrarMenos")
    shinyjs::show("epTexto")
    shinyjs::show("epExplain")
    shinyjs::show("htEp")
    shinyjs::show("exEp")
  })
  
  observeEvent(input$epMostrarMenos, {
    shinyjs::hide("epMostrarMenos")
    shinyjs::show("epMostrarMais")
    shinyjs::hide("epTexto")
    shinyjs::hide("epExplain")
    shinyjs::hide("htEp")
    shinyjs::hide("exEp")
  })
  
  output$epTitle <- renderText({
    paste0("<h4> Erro Padrão </h4>")
  })
  
  output$epExplain <- renderUI({
    withMathJax(helpText("Erro Padrão: $$\\frac{\\sigma}{\\sqrt{n}}$$")
    )
  })
  
  output$exEp <- renderText({
    input$geraElementos
    tam_amostra <- ceiling(sqrt(length(elementos)))
    amostra <- sample(elementos, tam_amostra, replace = FALSE)
    text <- paste0("Separando uma amostra exemplo dos elementos gerados: \n", vectorToString(amostra),
                   "\nCalcula-se o seu desvio padrão = sqrt(var(", vectorToString(amostra), ")) = ", 
                   sd(amostra), "\nE divide-se pela raiz do tamanho da amostra: \n", 
                   sd(amostra), "/sqrt(", tam_amostra, ") = ", (sd(amostra)/sqrt(tam_amostra)))
    text
  })
  
  #Quantis
  
  observeEvent(input$quantilMostrarMais, {
    shinyjs::hide("quantilMostrarMais")
    shinyjs::show("quantilMostrarMenos")
    shinyjs::show("quantilTexto")
    shinyjs::show("quantilExplain")
    shinyjs::show("htQuantil")
    shinyjs::show("exQuantil")
  })
  
  observeEvent(input$quantilMostrarMenos, {
    shinyjs::hide("quantilMostrarMenos")
    shinyjs::show("quantilMostrarMais")
    shinyjs::hide("quantilTexto")
    shinyjs::hide("quantilExplain")
    shinyjs::hide("htQuantil")
    shinyjs::hide("exQuantil")
  })
  
  output$quantilTitle <- renderText({
    paste0("<h4> Quantis </h4>")
  })
  
  output$exQuantil <- renderText({
    input$geraElementos
    isEven <- (length(elementos)%%2 == 0)
    text <- paste0("Temos: ", vectorToString(elementos), "\n")
    
    if (isEven) {
      text <- paste0(text, 
                     paste0("0.25 quantil = ", elementos[ceiling(0.25*length(elementos))], ", o "),
                     ceiling(0.25*length(elementos)), "º elemento, pois divide 25% dos elementos abaixo dele.\n",
                     "0.5 quantil (mediana) = (", elementos[length(elementos)/2] , " + ", elementos[(length(elementos)/2) + 1], 
                     ")/2 = ", median(elementos), "\n",
                     "0.75 quantil = ", elementos[ceiling(0.75*length(elementos))], ", o ",
                      ceiling(0.75*length(elementos)), "º elemento, pois divide 75% dos elementos abaixo dele.\n"
                     )
    } else {
      text <- paste0(text, 
                     "0.25 quantil = (",  elementos[floor(0.25*length(elementos))] , " + ", elementos[ceiling(0.25*length(elementos))], 
                     ")/2 = ", as.vector(quantile(elementos))[2], "\n",
                     "0.5 quantil (mediana) = ", elementos[ceiling(0.5*length(elementos))], ", o ",
                     ceiling(0.5*length(elementos)), "º elemento, pois divide metade dos elementos abaixo dele.\n",
                     "0.75 quantil = (",  elementos[floor(0.75*length(elementos))] , " + ", elementos[ceiling(0.75*length(elementos))],
                     ")/2 = ", as.vector(quantile(elementos))[4], "\n"
                     )
    }
    
    text
  })
  
  
  
  

  
  output$graficoElementos <- renderPlot({
    input$geraElementos
    tab <- as.data.frame(table(elementos))
    tab$elementos <- as.numeric(as.character(tab$elementos))
    mean <- round(mean(elementos), 2)
    mode <- getmode(elementos)
    tab$isMode <- ifelse(tab$elementos %in% mode, TRUE, FALSE)
    ggplot(data=tab, aes(x=elementos, y=Freq, fill = isMode)) + 
      geom_bar(stat="identity", position=position_dodge(), color = "black", width = 0.9) +
      geom_text(aes(label=Freq), vjust=-0.5, size=3.5)+
      annotate("segment", x = mean, xend = mean, y = 0, yend = max(tab$Freq) + 0.5, colour = "black", size=1) +
      #geom_segment(aes(x=mean, xend=mean, y=0, yend=max(tab$Freq) + 0.5)) +
      geom_label(aes(x=mean, y=max(tab$Freq) + 0.5, label = paste0("Média = ", mean))) +
      annotate("text", x = mode, y = 0.5, label = "Moda" , color="black", size=4 , angle=0) +
      #geom_label(aes(x=mode, y=0, label = paste0("Moda"))) +
      theme_classic() +
      scale_fill_manual(values = c("steelblue", "red"),
                        labels = c("", "Moda")) +
      scale_x_continuous(breaks = round(seq(min(tab$elementos), max(tab$elementos), by = 1))) +
      theme(legend.position="none") +
      theme(legend.title = element_blank()) +
      labs(x="Valores", y = "Quantidade de elementos com esse valor")
  })
  
  ##################################################################################################
  #Página Gráficos
  ##################################################################################################
  
  output$graficosQual <- renderUI({
    tags <- NULL
    tags <- 
    
    
    if (input$varGrafQual %in% c("Sexo", "Trabalha", "Relacionamento", "Pratica esportes", "Toma vitaminas", "Culinária favorita")) {
      tags <- tagList(
                 column(6,h4(strong("Gráfico de Barras")),
                        plotOutput("grafBarras")),
                 column(6, h4(strong("Gráfico de Pizza")),
                        plotOutput("grafPizza")))
    } else {
      tags <- tagList(
                 h4(strong("Gráfico de Barras")),
                 plotOutput("grafBarras"),
                 helpText("O gráfico de pizza não é exibido pois a variável é qualitativa ordinal.")
               )
    }
    tags
    
  })
  
  getQualPlotData <- reactive({
    return (data[input$varGrafQual])
  })
  
  getQuantPlotData <- reactive({
    return (data[input$varGrafQuant])
  })
  
  output$grafBarras <- renderPlot({
    selData <- getQualPlotData()
    selData <- as.data.frame(table(selData))
    colnames(selData) <- c("cat", "freq")
    print(selData)
    
    g <- ggplot(data = selData, aes(x=cat, y=freq, fill = cat)) +
      geom_bar(stat="identity", colour = "black", width = 0.8) + 
      geom_text(aes(x = cat, y = freq, label = freq), colour = "black", vjust = -2) + 
      scale_fill_manual(values=colorful) +
      guides(fill=FALSE) +
      theme_classic() +
      scale_x_discrete(name = input$varGrafQual) +
      scale_y_continuous(name = "Frequência absoluta", limits = c(0, max(selData$freq) + 5)) + 
      theme(axis.title.y = element_text(size = 16), 
            axis.text.y = element_text(size = 12))

    if (input$varGrafQual == "Culinária favorita") {
      g <- g + theme(axis.text.x = element_text(angle = -90, hjust = 0.5))
    } else {
      g <- g + theme(axis.title.x = element_text(face = "bold", size = 16), 
                     axis.text.x = element_text(size = 12))
    }
    
    g
  })
  
  output$grafPizza <- renderPlot({
    selData <- getQualPlotData()
    selData <- as.data.frame(table(selData))
    colnames(selData) <- c("cat", "freq")
    
    g <- ggplot(selData, aes(x="", y=freq, fill = cat)) +
      geom_bar(width = 1, stat = "identity", colour = "black") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = colorful, name = input$varGrafQual) + 
      xlab(" ") +
      ylab(" ") + 
      theme_classic() + 
      theme(
        axis.ticks = element_blank(),
        axis.text.x=element_blank()
      )
    
    if (length(levels(selData$cat)) <= 5) {
      g <- g + geom_text(aes(label = scales::percent(freq/sum(freq))), position = position_stack(vjust = 0.5))
    }
    
    g
  })
  
  output$histograma <- renderPlot({
    selData <- getQuantPlotData()
    g <- NULL
    if (input$varGrafQuant %in% c("Ano letivo", "Percepção de Saúde")) {
      
      selData <- as.data.frame(table(selData))
      g <- NULL
      colnames(selData) <- c("Var")
    
      
      g <- ggplot(selData, aes(x=Var)) +
        geom_histogram()

      max <- max(data[input$varGrafQuant])
      colnames(selData) <- c("cat", "freq")
      selData$cat <- factor(as.character(selData$cat), levels = c(1:max))
      g <- ggplot(data = selData, aes(x=cat, y=freq, fill = cat)) +
        geom_bar(stat="identity", colour = "black", width = 0.8) +
        geom_text(aes(x = cat, y = freq, label = freq), colour = "black", vjust = -2) +
        guides(fill=FALSE) +
        theme_classic() +
        scale_x_discrete(name = input$varGrafQuant) +
        scale_y_continuous(name = "Frequência absoluta", limits = c(0, max(selData$freq) + 5))
      
    } else {
      g <- NULL
      max <- max(selData)
      colnames(selData) <- c("Var")
      
      
      if (input$varGrafQuant == "Altura") {
        x_breaks <-  seq(1.4, 2.2, by=0.1)
        g <- ggplot(data = selData, aes(x=Var)) + 
          geom_histogram(breaks=x_breaks, aes(y =..density../10), colour = "black", fill = "#4cA6AA") + 
          theme_classic() + 
          scale_x_continuous(breaks = x_breaks) + 
          ylab("Frequência Relativa") + 
          xlab(input$varGrafQuant)
        
        #print(ggplot_build(g)$data)
        
        g <- g + geom_text(data=as.data.frame(ggplot_build(g)$data),
                           aes(x=x, y=density/10 + 0.007, label=scales::percent(density/10)), size = 3.8)
        
      } else {
        x_breaks <- seq(40, 120, by=10)
        g <- ggplot(data = selData, aes(x=Var)) + 
          geom_histogram(breaks=x_breaks, aes(y = 1000*..density..), colour = "black", fill = "#4cA6AA") + 
          theme_classic() + 
          scale_x_continuous(breaks = x_breaks) + 
          ylab("Frequência Relativa") + 
          xlab(input$varGrafQuant)
        
        #print(ggplot_build(g)$data)
        
        g <- g + geom_text(data=as.data.frame(ggplot_build(g)$data),
                           aes(x=x, y=1000*density + 0.7, label=scales::percent(10*density)), size = 3.8)
      }
      
      
    }
    g
    
  })
  
  output$tituloGraf1 <- renderText({
    text <- ifelse((input$varGrafQuant %in% c("Ano letivo", "Percepção de Saúde")),
                   "<h4><strong>Gráfico de Barras</strong></h4>",
                   "<h4><strong>Histograma</strong></h4>")
    text 
  })
  
  output$htGraf1 <- renderText({
    ifelse((input$varGrafQuant %in% c("Ano letivo", "Percepção de Saúde")), paste0("<span class = 'help-block'> Pode-se utilizar o gráfico de barras pois a variável ", 
                           input$varGrafQuant, " é quantitativa discreta.</span>"),
           paste0("<span class = 'help-block'> Utiliza-se somente o histograma pois a variável ", 
                  input$varGrafQuant, " é quantitativa contínua.</span>"))
  })
  
  output$boxplot <- renderPlot({
    selData <- getQuantPlotData()
    selData <- as.data.frame(selData)
    colnames(selData) <- c("cat")
    p <- ggplot(selData, aes(x="",y=cat)) + 
      geom_boxplot(fill = "#4CA6AA", colour = "black") + 
      xlab("") +
      ylab(input$varGrafQuant)
    p
  })
  
  output$histogramaAlt <- renderPlot({
    selData <- getQuantPlotData()
    g <- NULL
    colnames(selData) <- c("Var")
    
    if (input$varGrafQuant == "Ano letivo") {
      selData$Var <- cut(selData$Var, breaks=c(-Inf, 2.5, Inf), labels = c("Início", "Fim"))
      selData <- as.data.frame(table(selData))
      colnames(selData) <- c("Var", "Freq")
      selData$Freq <- selData$Freq/sum(selData$Freq)
      print(selData)
      g <- ggplot(selData, aes(x=Var, y=Freq)) + 
        geom_bar(stat="identity", colour = "black", fill = "#4cA6AA") +
        geom_text(aes(x = Var, y = Freq, label = scales::percent(Freq)), colour = "black", vjust = -2) +
        scale_y_continuous(limits = c(0, max(selData$Freq) + 0.05)) +
        theme_classic() + 
        ylab("Frequência Relativa") + 
        xlab(input$varGrafQuant)
    } else {
      selData$Var <- cut(selData$Var, breaks=c(-Inf, 2, 4, 6, 8, Inf), labels = c("Muito ruim", "Ruim", "Regular", "Bom", "Muito bom"))
      selData <- as.data.frame(table(selData))
      colnames(selData) <- c("Var", "Freq")
      selData$Freq <- selData$Freq/nrow(selData)
      selData$Freq <- selData$Freq/sum(selData$Freq)
      print(selData)
      g <- ggplot(selData, aes(x=Var, y=Freq)) + 
        geom_bar(stat="identity", colour = "black", fill = "#4cA6AA") +
        geom_text(aes(x = Var, y = Freq, label = scales::percent(Freq)), colour = "black", vjust = -2) +
        scale_y_continuous(limits = c(0, max(selData$Freq) + 0.05)) +
        theme_classic() + 
        ylab("Frequência Relativa") + 
        xlab(input$varGrafQuant)
    }
    g
  })
  
  output$histAlt <- renderUI({
    tags <- NULL
    if (input$varGrafQuant %in% c("Ano letivo", "Percepção de Saúde")) {
      tags <- tagList(fluidRow(
        column(6, 
               h4(strong(("Histograma")),
                  plotOutput("histogramaAlt"),
                  helpText("Mesmo sendo uma variável quantitativa discreta, pode-se utilizar o 
                               histograma além do gráfico de barras."))
               ), 
        column(6,
               p("Separação de valores em intervalos"),
               tableOutput("tabHist")
               )
      ))
    }
    tags
  })
  
  output$tabHist <- renderTable({
    tab <- NULL
    if (input$varGrafQuant == "Ano letivo") {
      tab <- as.data.frame(cbind(c("[1, 2]", "[3, 4]"), c("Início", "Fim")))
    } else if (input$varGrafQuant == "Percepção de Saúde") {
      tab <- as.data.frame(cbind(c("[1, 2]", "[3, 4]", "[5, 6]", "[7, 8]", "[9, 10]"), 
                                 c("Muito ruim", "Ruim", "Regular", "Boa", "Muito boa")))
    }
    tab
  }, striped = TRUE, colnames = FALSE, bordered = TRUE)
  
  output$tabGrafVarQual <- renderTable({
    tab <- table(data[,colnames(data) == input$varGrafBiQual1], data[,colnames(data) == input$varGrafBiQual2])
    tab <- as.data.frame.matrix(tab)
    tab <- tab/sum(tab)
    tab
  }, rownames = TRUE, bordered = TRUE, striped = TRUE)
  
  output$grafBarraBiQual <- renderPlot({
    selData <- as.data.frame(table(data[,colnames(data) == input$varGrafBiQual1], data[, colnames(data) == input$varGrafBiQual2]))
    selData <- selData %>% 
      group_by(Var1) %>%
      arrange(Var1, desc(Var2)) %>%
      mutate(y_pos = cumsum(Freq) - 0.5*Freq)
    g <- ggplot(selData, aes(x=Var1, y=Freq, fill=Var2)) + 
      geom_bar(stat="identity", color = "black", width = 0.9) +
      geom_text(aes(y = y_pos, label = ifelse(Freq > 0, Freq, ""), group = Var2)) +
      labs(x=input$varGrafBiQual1, y="Frequência absoluta") + 
      scale_fill_discrete(name=input$varGrafBiQual2) +
      theme_classic()
    g
    
  })
  
  output$scatterGraf <- renderPlot({
    selData <- data.frame(data[,colnames(data) == input$varGrafBiQuant1], data[,colnames(data) == input$varGrafBiQuant2])
    colnames(selData) <- c("Var1", "Var2")
    g <- ggplot(selData, aes(x=Var1, y=Var2)) + 
      geom_point(color = "#5497A7", size = 3L) +
      theme_classic() + 
      labs(x=input$varGrafBiQuant1, y=input$varGrafBiQuant2)
    g
  })
  
  output$multiBoxplot <- renderPlot({
    selData <- data.frame(data[,colnames(data) == input$varQualBoxplot1], data[,colnames(data) == input$varQualBoxplot2])
    colnames(selData) <- c("Var1", "Var2")
    if (input$varQualBoxplot1 == "Ano letivo") {
      selData$Var1 <- as.factor(as.character(selData$Var1))
    }
    print(selData)
    g <- ggplot(selData, aes(x=Var1, y=Var2, fill=Var1)) + 
      geom_boxplot() + 
      theme_classic() + 
      labs(x="", y = input$varQualBoxplot2) + 
      scale_fill_discrete(name = input$varQualBoxplot1)
    g
    
  })
  
  output$tabelaProbDef <- renderTable({
    tb
  }, striped = TRUE, bordered = TRUE, digits = 0, width = "100%")
  
  output$caixaContaProb <- renderText({
    letter1 <- substr(input$prob_in1, 1, 2)
    letter2 <- "NO_INPUT"
    freq1 <- ifelse((input$prob_in1 %in% c("Masculino", "Feminino")), 
                    tb[9, input$prob_in1], 
                    tb[tb$`Culinária/Sexo` == input$prob_in1, "Total"])
    freq2 <- 0
    freq3 <- 0
    if (input$probOp != "only") {
      letter2 <- substr(input$prob_in2, 1, 2)
      if (letter2 == letter1) {
        letter2 <- substr(input$prob_in2, 1, 4)
      }
      
      freq2 <- ifelse((input$prob_in2 %in% c("Masculino", "Feminino")), 
                      tb[9, input$prob_in2], 
                      tb[tb$`Culinária/Sexo` == input$prob_in2, "Total"])
      
      if (!xor((input$prob_in1 %in% c("Masculino", "Feminino")),(input$prob_in2 %in% c("Masculino", "Feminino")))) { #são iguais
        freq3 <- ifelse(input$prob_in1 == input$prob_in2, freq1, 0)
      } else {
        if (input$prob_in1 %in% c("Masculino", "Feminino")) {
          freq3 <- tb[tb$`Culinária/Sexo` == input$prob_in2, input$prob_in1]
        } else {
          freq3 <- tb[tb$`Culinária/Sexo` == input$prob_in1, input$prob_in2]
        }
      }
    }
    text <- paste0(letter1, " = ", input$prob_in1, "\n")
    text <- paste0(text, "P(", letter1, ") = ", 
                   freq1,
                   "/", tb[9, "Total"], " = ", round((freq1/tb[9, "Total"]), 4), " = ", 
                   scales::percent(round((freq1/tb[9, "Total"]), 4)),
                   "\n"
    )
    
    if (letter2 != "NO_INPUT") {
      text <- paste0(text, "\n", letter2, " = ", input$prob_in2, "\n",
                     "P(", letter2, ") = ", freq2,
                     "/", tb[9, "Total"], " = ", round((freq2/tb[9, "Total"]), 4), " = ", 
                     scales::percent(round((freq2/tb[9, "Total"]), 4)),"\n"
                     
                     )
      
      text <- paste0(text, "\nP(", letter1, " ∩ ", letter2, ") = ", freq3, "/", tb[9, "Total"], 
                     " = ", round((freq3/tb[9, "Total"]), 4), " = ", 
                     scales::percent(round((freq3/tb[9, "Total"]), 4)), "\n")
      
      if (input$probOp == "uniao") {
        text <- paste0(text, "\nP(", letter1, " ∪ ", 
                       letter2, ") = ", "P(", letter1, ") + P(", letter2, ") - P(", letter1, " ∩ ", letter2,
                       ") = ", round((freq1/tb[9, "Total"]), 4), " + ", round((freq2/tb[9, "Total"]), 4), 
                       " - ", round((freq3/tb[9, "Total"]), 4), " = ", 
                       round(((freq1 + freq2 - freq3)/tb[9, "Total"]), 4), " = ",
                       scales::percent(round(((freq1 + freq2 - freq3)/tb[9, "Total"]), 4)), "\n"
                       )
      }
    }
    
    text
                   
  })
  
  
  output$formulasProb <- renderUI({
    formula <- ifelse(input$probOp == "uniao", 
                      "P(A ∪ B) = $$ P(A) + P(B) - P(A ∩ B) $$",
                      ifelse(input$probOp == "intersec", 
                             "P(A ∩ B) = $$\\frac{Freq(A, B)}{Total}$$", 
                             "P(A) = $$\\frac{Freq(A)}{Total}$$")
                      )
    withMathJax(helpText(formula))
  })
  
  bernoulliPlot <- reactive({
    p <- input$bernoulli_p
    bern <- data.frame(c(0, 1), c(1-p, p))
    colnames(bern) <- c("x", "p(x)")
    bern$x <- as.factor(as.character(bern$x))
    g <- ggplot(data = bern, aes(x=x, y=`p(x)`)) + 
      geom_bar(stat="identity", fill = medium_cyan, color = "white", width = 0.3) + 
      labs(title = paste0("Distribuição Bernoulli de Probabilidades com p = ", p),
           y = "Probabilidade", 
           x = "x") + 
      scale_y_continuous(breaks = seq(0, 1, by=0.1), limits = c(0, 1)) + 
      theme_classic() + 
      theme(axis.ticks.x = element_blank(),
            axis.title.x = element_text(size = 20),
            axis.text.x = element_text(size = 14), 
            axis.title.y = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            title = element_text(size = 16))
    
    g
  })
  
  binomPlot <- reactive({
    p <- input$binom_p
    n <- input$binom_n
    x_breaks <- c(0:n)
    if (n > 25)
      x_breaks <- seq(5, n, by = 5)
    
    values <- data.frame(x = 0:n, y = dbinom(0:n, n, p))
    print(x_breaks)
    g <- ggplot(data = values, aes(x=x, y=y)) + 
      geom_bar(stat = "identity", width = 0.5, color = "white", fill = medium_cyan) + 
      labs(title = paste0("Distribuição Binomial de Probabilidades com p = ", p, " e n = ", n)) + 
      ylab("Probabilidade") + 
      scale_x_continuous(name = "k", breaks = x_breaks) +
      theme_classic() + 
      theme(axis.ticks.x = element_blank(),
            axis.title.x = element_text(size = 20),
            axis.text.x = element_text(size = 14), 
            axis.title.y = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            title = element_text(size = 16))
    
    return(g)
  })
  
  buildBernoulliTable <- reactive({
    p <- input$bernoulli_p
    bern <- data.frame(c(0, 1), c(1-p, p), c("Probabilidade de que seja 0 (fracasso)", "Probabilidade de que seja 1 (sucesso)"))
    colnames(bern) <- c("x", "p(x)", "Explicação")
    bern$`p(x)` <- scales::percent(bern$`p(x)`)
    bern
  })
  
  buildBinomTable <- reactive({
    p <- input$binom_p
    n <- input$binom_n
    binomTab <- data.frame(0:n, round(dbinom(0:n, n, p), 4))
    colnames(binomTab) <- c("k", "p(k)")
    if (n > 25) {
      binomTab <- binomTab[binomTab$`p(k)` > 0,]
    }
    binomTab$exp <- ""
    binomTab$`p(k)` <- scales::percent(binomTab$`p(k)`)
    for (i in 1:nrow(binomTab)) {
      binomTab$exp[i] <- paste0("Probabilidade de ter ", binomTab$k[i], " sucesso(s) em ", n, " tentativas.")
    }
    colnames(binomTab)[3] <- "Explicação"
    return (binomTab)
  })
  
  buildPoissonTable <- reactive({
    u <- input$poisson_u
    min <- input$poisson_minmax[1]
    max <- input$poisson_minmax[2]
    
    poisTab <- data.frame(min:max, dpois(min:max, u))
    colnames(poisTab) <- c("k", "p(k)")
    poisTab$exp <- ""
    poisTab$`p(k)` <- scales::percent(poisTab$`p(k)`)
    for (i in 1:nrow(poisTab)) {
      poisTab$exp[i] <- paste0("Probabilidade de ter ", poisTab$k[i], " acontecimento(s) no intervalo de tempo.")
    }
    colnames(poisTab)[3] <- "Explicação"
    return (poisTab)
  })
  
  output$distrTable <- renderTable({
    switch (input$distribuicao,
            "bernoulli" = buildBernoulliTable(),
            "binomial" = buildBinomTable(),
            "poisson" = buildPoissonTable(),
            "normal" = buildBinomTable()
    )
  }, bordered = TRUE, striped = TRUE, colnames = TRUE, width = "100%", align = "c")
  
  poissonPlot <- reactive({
    u <- input$poisson_u
    min <- input$poisson_minmax[1]
    max <- input$poisson_minmax[2]
    x_breaks <- c(min:max)
    if (max - min > 20)
      x_breaks <- seq(min, max, by = 5)
    values <- data.frame(x = min:max, y = dpois(min:max, u))
    g <- ggplot(data = values, aes(x=x, y=y)) + 
      geom_bar(stat = "identity", width = 0.5, color = "white", fill = medium_cyan) +
      ylab("Probabilidade") + 
      scale_x_continuous(name = "k", breaks = x_breaks) +
      theme_classic() + 
      theme(axis.ticks.x = element_blank(),
            axis.title.x = element_text(size = 20),
            axis.text.x = element_text(size = 14), 
            axis.title.y = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            title = element_text(size = 16))
      
    
    return(g)
  })
  
  observeEvent(input$distribuicao, {
    if (input$distribuicao == "normal") {
      req(input$normal_p)
      click("normal_refresh")
      print("triggered")
    }
  })
  
  observeEvent(input$normal_refresh, {
    normal_params$u <- input$normal_media
    normal_params$dp <- input$normal_dp
    normal_params$min <- input$normal_range[1]
    normal_params$max <- input$normal_range[2]
    min <- normal_params$u - 4*normal_params$dp
    max <- normal_params$u + 4*normal_params$dp
    normal_params$sit <- 1
    if (normal_params$min > min) {
      if (normal_params$max < max) {
        normal_params$sit <- 3
      } else {
        normal_params$sit <- 4
      }
    } else {
      if (normal_params$max < max) {
        normal_params$sit <- 2
      } else {
        normal_params$sit <- 1
      }
    }
    
  })
  
  output$normal_params <- renderUI({
    u <- input$normal_media
    dp <- input$normal_dp
    min <- u - 4*dp
    max <- u + 4*dp
    tags <- tagList(
      sliderInput("normal_range", label = "Intervalo de interesse", 
                  min = min, max = max, value = c(u + dp, max), round = -2, step = 0.05)
      
      
      
    )
    tags
  })
  
  normalPlot <- reactive({
    req(input$normal_range)
    u <- normal_params$u
    dp <- normal_params$dp
    min_p <- normal_params$min
    max_p <- normal_params$max
    print(min_p)
    print(max_p)
    print(normal_params$sit)
    yend <- switch (normal_params$sit,
      dnorm(u + 2*dp, mean=u, sd=dp),
      dnorm(max_p, mean=u, sd=dp),
      dnorm(max_p, mean=u, sd=dp),
      dnorm(min_p, mean=u, sd=dp)
    ) 
    xpos <- switch(normal_params$sit,
                   u+2*dp + 1.5,
                   ifelse(max_p > u, max_p + 1.5, max_p - 1.5),
                   ifelse(max_p > u, max_p + 1.5, max_p - 1.5),
                   ifelse(min_p > u, min_p + 1.5, min_p - 1.5)
                   )
    
    min <- u - 6*dp
    max <- u + 6*dp
    
    prob <- round(pnorm(max_p, u, dp) - pnorm(min_p, u, dp), 3)
    enum <- switch (normal_params$sit,
      paste0("P(", min_p, " <= x <= ", max_p, ") = "),
      paste0("P(x <= ", max_p, ") = "),
      paste0("P(", min_p, " <= x <= ", max_p, ") = "),
      paste0("P(x >= ", min_p, ") = ")
    )
    enum <- paste0(enum, scales::percent(prob))
    x <- seq(min_p, max_p, length = 300)
    y <- dnorm(x, mean=u, sd=dp)
    area <- data.frame(x, y)
    
    g <- ggplot(data = area, aes(x=x, y=y)) + 
      geom_area(stat="identity", fill = medium_cyan, alpha = 0.7) + 
      stat_function(fun=dnorm, n=101, args = list(mean=u, sd=dp), color=dark_cyan) + 
      scale_x_continuous(limits = c(min, max)) + 
      geom_segment(aes(x=u, xend=u, y=0, yend=dnorm(u, u, dp)), color = "gray", linetype=2, alpha=0.3) + 
      annotate(geom="text", x=u, y=dnorm(u,u,dp) + 0.05, label=paste0("μ = ", u), size=5, color=dark_cyan) + 
      annotate(geom="text", x=xpos, y=yend + 0.01, label=enum) + 
      labs(x = "x", y = "f(x)")
    
    g <- switch (normal_params$sit,
      g,
      g + geom_segment(aes(x=max_p, xend=max_p, y=0, yend=dnorm(max_p, u, dp)), color=dark_cyan, linetype=2),
      g + geom_segment(aes(x=max_p, xend=max_p, y=0, yend=dnorm(max_p, u, dp)), color=dark_cyan, linetype=2) + 
        geom_segment(aes(x=min_p, xend=min_p, y=0, yend=dnorm(min_p, u, dp)), color=dark_cyan, linetype=2),
      g + geom_segment(aes(x=min_p, xend=min_p, y=0, yend=dnorm(min_p, u, dp)), color=dark_cyan, linetype=2)
    )
    g
    
    
  })
  
  output$normalExemplo <- renderText({
    req(input$normal_range)
    u <- normal_params$u
    dp <- normal_params$dp
    min_p <- normal_params$min
    max_p <- normal_params$max
    z1 <- (min_p - u)/dp
    z2 <- (max_p - u)/dp
    text <- c("P(", min_p, " <= x <= ", max_p, ") = \nP((", min_p, " - μ)/σ <= (x- μ)/σ <= (", max_p, " - μ)/σ) = \n
              P((", min_p, " - ", u, ")/", dp, " <= Z <= (", max_p, " - ", u, ")/", dp, ") = \n
              P(", z1, " <= Z <= ", z2, ")\n\n
              P(Z >= ", z1, ") = 1 - P(Z <= ", z1, ") = 1 - ", round(pnorm(min_p, u, dp), 3), " = ", round(pnorm(min_p, u, dp, lower.tail=TRUE), 3), "\n
              P(Z <= ", z2, ") = ", round(pnorm(max_p, u, dp), 3), "\n\n
              P(", z1, " <= Z <= ", z2, ") = ", round(pnorm(max_p, u, dp), 3), " - ", round(pnorm(min_p, u, dp, lower.tail=TRUE), 3), " = ", round(pnorm(max_p, u, dp), 3) - round(pnorm(min_p, u, dp, lower.tail=TRUE), 3), " = \n
              ", scales::percent(round(pnorm(max_p, u, dp), 3) - round(pnorm(min_p, u, dp, lower.tail=TRUE), 3)))
    text
  })
  
  output$expExemplo <- renderUI({
    b <- input$exp_b
    min <- input$exp_val[1]
    max <- input$exp_val[2]
    prob <- pexp(max, b) - pexp(min, b)
    withMathJax(paste0("P(", min, " <= x <= ", max, ") = $$\\int_{", min, "}^{", max, "}
                       2e^{-2x}dx = ", prob, "$$"))
  })
  
  expPlot <- reactive({
    b <- input$exp_b
    min <- input$exp_val[1]
    max <- input$exp_val[2]
    x <- seq(min, max, length = 300)
    y <- dexp(x, b)
    area <- data.frame(x, y)
    prob <- pexp(max, b) - pexp(min, b)
    text <- paste0("P(", min, " <= x <= ", max, ") = ", scales::percent(round(prob, 4)))
    g <- ggplot(data = area, aes(x=x, y=y)) + 
      geom_area(stat="identity", fill = medium_ocre) + 
      stat_function(fun=dexp, n=101, args = list(rate = b), color="black") + 
      scale_x_continuous(name="x", limits = c(0, 3)) + 
      scale_y_continuous(name="f(x)", limits=c(0, b + 1)) + 
      geom_segment(aes(x=max, xend=max, y=0, yend=dexp(max, b)), color = brown, alpha= 0.7, linetype=2) +
      annotate(geom="text", x = 2, y=b+0.1, label=text, color=brown, size = 6) 
    
    if (min > 0) {
      g <- g + geom_segment(aes(x=min, xend=min, y=0, yend=dexp(min, b)), color = "brown", alpha=0.7, linetype=2)
    }
    g
  })
  
  quiPlot <- reactive({
    qx <- input$qui_x
    min <- input$qui_values[1]
    max <- input$qui_values[2]
    x <- seq(min, max, length = 300)
    y <- dchisq(x, qx)
    area <- data.frame(x, y)
    prob <- pchisq(max, qx) - pchisq(min, qx)
    text <- paste0("P(", min, " <= x <= ", max, ") = ", scales::percent(round(prob, 4)))
    g <- ggplot(data = area, aes(x=x, y=y)) + 
      geom_area(stat="identity", fill = red_nail) + 
      stat_function(fun=dchisq, n=101, args = list(df = qx), color=red_broken_nail) + 
      scale_x_continuous(name="x", limits = c(0, 16)) + 
      ylab("f(x)") + 
      geom_segment(aes(x=max, xend=max, y=0, yend=dchisq(max, qx)), color = red_broken_nail, alpha= 0.7, linetype=2) +
      annotate(geom="text", x = 12, y=dchisq(max, qx) + 0.1, label=text, color=red_broken_nail, size = 6) 
    
    if (min > 0) {
      g <- g + geom_segment(aes(x=min, xend=min, y=0, yend=dchisq(min, qx)), color = red_broken_nail, alpha=0.7, linetype=2)
    }
    g
  })
  
  tPlot <- reactive({
    gl <- input$t_gl
    min <- -4
    max <- 4
    x <- seq(min, max, length=300)
    y <- dt(x, gl)
    area <- data.frame(x, y)
    
    g <- ggplot(data=area, aes(x=x, y=y)) + 
      stat_function(fun=dnorm, n=101, args=list(mean=0, sd=1), color = medium_cyan, alpha = 0.7) +
      stat_function(fun=dt, n=101, args=list(df=gl), color = "black") + 
      annotate(geom="text", x=2, y=dnorm(1, 0, 1), color=medium_cyan, label = "Distribuição Normal") + 
      annotate(geom="text", x=-0.1, y=dt(-1, gl) - 0.15, color="black", label = paste0("Distribuição T com ", gl, " graus de liberdade")) + 
      labs(x="x", y="f(x)")
    g
  })
  
  
  
  distributionInput <- reactive({
    switch (input$distribuicao,
      "bernoulli" = bernoulliPlot(),
      "binomial" = binomPlot(),
      "poisson" = poissonPlot(),
      "normal" = normalPlot(),
      "exp" = expPlot(),
      "qui" = quiPlot(),
      "t" = tPlot()
    )
  })
  
  output$distribuicao <- renderPlot({
    g <- distributionInput()
    g
  })
  
  
  #create frequency table
  output$tabelaProbCond <- renderTable({
    var1 <- input$condTabVar1
    var2 <- input$condTabVar2
    
    tab <- table(data[,colnames(data) == var1], data[,colnames(data) == var2])
    tab <- as.data.frame.matrix(tab)
    tab <- cbind(rownames(tab), tab)
    rownames(tab) <- NULL
    colnames(tab)[1] <- paste0(var1, "/", var2)
    tab$Total <- rowSums(tab[-1])
    tab[,1] <- as.character(tab[,1])
    tab <- rbind(tab, c("Total", colSums(tab[-1])))
    tab[,1] <- as.factor(tab[,1])
    tab
  }, striped = TRUE, bordered = TRUE)
  
  output$caixaContaProbCond <- renderText({
    
    req(input$prob_cond1)
    req(input$prob_cond2)
    
    var1 <- input$condTabVar1
    var2 <- input$condTabVar2
    att1 <- input$prob_cond1
    att2 <- input$prob_cond2
    tot <- nrow(data)
    
    freq1 <- nrow(data[data[!is.na(data[var1]),colnames(data) == var1] == att1,])
    freq2 <- nrow(data[data[!is.na(data[var2]),colnames(data) == var2] == att2,])
    freq12 <- nrow(data[data[!is.na(data[var1]) & !is.na(data[var2]),colnames(data) == var1] == att1 & 
                          data[!is.na(data[var1]) & !is.na(data[var2]),colnames(data) == var2] == att2,])
    
    text <- paste0("A: ", var1, " é ", att1, "\n")
    text <- paste0(text, "B: ", var2, " é ", att2, "\n\n")
    
    text <- paste0(text, "P(A) = ", freq1, "/", tot, " = ", (freq1/tot), "\n")
    text <- paste0(text, "P(B) = ", freq2, "/", tot, "= ", (freq2/tot), "\n")
    text <- paste0(text, "P(A ∩ B) = ", freq12, "/", tot, " = ", (freq12/tot), "\n\n")
    
    text <- paste0(text, "P(A | B) = P(A ∩ B)/P(B)\n")
    text <- paste0(text, "P(A | B) = ", (freq12/tot), "/", (freq2/tot), "\n")
    f <- ((freq12/tot)/(freq2/tot))
    text <- paste0(text, "P(A | B) = ", f, "\n")
    text <- paste0(text, "P(A | B) = ", round(f*100, 2), "%\n")
    
    text
  })
  
  output$selectProbCondVar <- renderUI({
    tagList(
      selectInput("prob_cond1", "Probabilidade de: ", levels((data[,colnames(data) == input$condTabVar1]))),
      selectInput("prob_cond2", "dado que: ", levels((data[,colnames(data) == input$condTabVar2])))
    )
  })
  
  
  ######################################################################
  ####################---I N F E R Ê N C I A---#########################
  ######################################################################
  
  observeEvent(input$testeT1Refresh, {
    testeT1_params$u <- input$testeT1MediaPop
    testeT1_params$dp <- input$testeT1DPPop
    testeT1_params$X <- input$testeT1MediaA
    testeT1_params$n <- input$testeT1TamanhoA
    testeT1_params$alpha <- input$testeT1Alpha
  })
  
  output$testeT1Plot <- renderPlot({
    u <- testeT1_params$u
    dp <- testeT1_params$dp
    X <- testeT1_params$X
    n <- testeT1_params$n
    S <- dp/sqrt(n)
    alpha <- testeT1_params$alpha
    x_min <- u - 4*dp
    x_max <- u + 4*dp
    rc1 <- qnorm(alpha/2, mean=u, sd=S)
    rc2 <- qnorm(1-alpha/2, mean=u, sd=S)
    accept <- FALSE
    if (X > rc1 && X < rc2) {
      accept <- TRUE
    }
    x1 <- c(seq(x_min, rc1, length = 200))
    y1 <- dnorm(x1, mean=u, sd=S)
    data1 <- as.data.frame(cbind(x1, y1))
    colnames(data1) <- c("x", "y")
    
    x2 <- c(seq(rc2, x_max, length=200))
    y2 <- dnorm(x2, mean=u, sd=S)
    data2 <- as.data.frame(cbind(x2, y2))
    colnames(data2) <- c("x", "y")
    
    point <- data.frame(X, dnorm(X, u, S)/2)
    colnames(point) <- c("x", "y")
    
    g <- ggplot(data=data1, aes(x=x, y=y))  + 
      geom_area(stat="identity", fill="red", alpha = 0.4) + 
      geom_area(data=data2, aes(x=x, y=y), stat="identity", fill="red", alpha=0.4) +
      stat_function(fun=dnorm, n=300, args = list(mean=u, sd=S), color = "brown") + 
      scale_x_continuous(limits = c(x_min, x_max)) + 
      geom_segment(aes(x=u, xend=u, y=0, yend=dnorm(u, u, S)), color = "gray", linetype=2, alpha=0.6) + 
      annotate(geom="text", x=u, y=1.1*dnorm(u,u,S), label=paste0("μ = ", u), size=5, color=dark_cyan) + 
      geom_point(data=point, aes(x=x, y=y), 
                 color = ifelse(accept, "green", "red"), size = 8, shape = 8)
    g
  })
  
  output$testeT1Texto <- renderText({
    u <- testeT1_params$u
    dp <- testeT1_params$dp
    X <- testeT1_params$X
    n <- testeT1_params$n
    S <- dp/sqrt(n)
    alpha <- testeT1_params$alpha
    rc1 <- qnorm(alpha/2, mean=u, sd=S)
    rc2 <- qnorm(1-alpha/2, mean=u, sd=S)
    accept <- FALSE
    if (X > rc1 && X < rc2) {
      accept <- TRUE
    }
    p <- 2*pnorm(X, u, S, lower.tail = X < u)
    
    text <- paste0("H0: μ = ", u, "\nH1: μ ≠ ", u, 
                   "\n\nX̄ ~ N(μ, σ/sqrt(n))\n",
                   "X̄ ~ N(", u, ", ", round(S, digits = 4), ")\n",
                   "α = P(Cometer Erro Tipo I) = ", alpha, " = ", scales::percent(alpha), "\n",
                   "Região Crítica = RC = ]-∞, ", round(rc1, digits = 4), "] U [", round(rc2, digits = 4), ", ∞[\n"
                   )
    
    if (accept) {
      text <- paste0(text, "X̄̄ ∉ RC, logo H0 é aceita. μ = ", u, "\n")
    } else {
      text <- paste0(text, "X̄ ∈ RC, logo H0 é rejeitada e a hipótese alternativa H1 é aceita. 
                     μ ≠ ", u, "\n")
    }
    
    text <- paste0(text, "\nValor-p: ", p)
    text
    
  })
  
  
  observeEvent(input$testeT2Refresh, {
    testeT2_params$n <- input$testeT2TamanhoA
    testeT2_params$u1 <- input$testeT2MediaP1
    testeT2_params$u2 <- input$testeT2MediaP2
    testeT2_params$dp1 <- input$testeT2DPP1
    testeT2_params$dp2 <- input$testeT2DPP2
    p1 <- rnorm(testeT2_params$n, testeT2_params$u1, testeT2_params$dp1)
    p2 <- rnorm(testeT2_params$n, testeT2_params$u2, testeT2_params$dp2)
    testeT2_params$p1 <- p1
    testeT2_params$p2 <- p2
    testeT2_params$D <- p1 - p2
  })
  
  output$testeT2Table <- renderTable({
    tab <- as.data.frame(rbind(testeT2_params$p1, testeT2_params$p2, testeT2_params$D))
    tab <- round(tab, 2)
    rownames(tab) <- c("Amostra A1 de P1", "Amostra A2 de P2", "Diferença (A1 - A2)")
    tab
  }, colnames = FALSE, rownames = TRUE, bordered = TRUE, striped = TRUE, width = "100%")
  
  output$testeT2Graph1 <- renderPlot({
    n <- testeT2_params$n
    u1 <- testeT2_params$u1
    u2 <- testeT2_params$u2
    dp1 <- testeT2_params$dp1
    dp2 <- testeT2_params$dp2
    
    p1 <- testeT2_params$p1
    p2 <- testeT2_params$p2
    p <- as.data.frame(cbind(c(p1, p2), c(rep("População 1", n), rep("População 2", n))))
    colnames(p) <- c("x", "pop")
    p$x <- as.numeric(as.character(p$x))
    
    g <- ggplot(data=p, aes(x=x)) + 
      geom_density(aes(group = pop, color = pop)) + 
      scale_x_continuous(limits = c(min(u1 - 4*dp1, u2 - 4*dp2), max(u1 + 4*dp1, u2 + 4*dp2))) + 
      scale_color_discrete(name="População") + 
      xlab(" ") +
      ylab("Densidade")
    g
    
  })
  
  output$testeT2Graph2 <- renderPlot({
    u1 <- testeT2_params$u1
    u2 <- testeT2_params$u2
    dp1 <- testeT2_params$dp1
    dp2 <- testeT2_params$dp2
    D <- as.data.frame(testeT2_params$D)
    colnames(D) <- c("x")
    
    g <- ggplot(data=D, aes(x=x)) + 
      geom_density() + 
      scale_x_continuous(limits = c(min(u1 - u2, u2 - u1) - 4*(max(dp1, dp2)), max(u1 - u2, u2 - u1) + 4*max(dp1, dp2))) + 
      xlab("Diferença") + 
      ylab("Densidade")
    g
    
    
  })
  
  output$testeT2Calc <- renderText({
    D <- testeT2_params$D
    n <- testeT2_params$n
    t <- mean(D)/(sd(D)/sqrt(n))
    p <- dt(t, df=n-1)
    alpha <- input$testeT2Alpha
    print(t.test(D, alternative = "two.sided", mu = 0, conf.level = .95))
    text <- paste0("H0: µ1 = µ2\nH1:µ1 ≠ µ2\n\n")
    
    text <- paste0(text, "Valor do teste = T = ", round(mean(D), 4), "/(", round(sd(D), 4), "/sqrt(", n, ")) = \n
                   ", round(t, 4), "\n\n
                   p-valor = P(T > |", round(t, 4), "| + P(T < - |", round(t, 4), "|) = \n
                   ", round(p, 4))
    
    if (p < alpha) {
      text <- paste0(text, "\nComo p-valor = ", round(p, 4), " < ", "α = ", alpha, ", rejeita-se a hipótese nula H0 e aceita-se H1:\n
                     µ1 ≠ µ2")
    } else {
      text <- paste0(text, "\nComo p-valor = ", round(p, 4), " >= ", "α = ", alpha, ", aceita-se a hipótese nula H0:\n
                     µ1 = µ2")
    }
    text 
  })
  
  output$testeT2Graph3 <- renderPlot({
    D <- testeT2_params$D
    n <- testeT2_params$n
    t <- mean(D)/(sd(D)/sqrt(n))
    p <- dt(t, df=n-1)
    alpha <- input$testeT2Alpha
    accept <- p >= alpha
    
    lower_p <- qt(alpha/2, df=n-1)
    upper_p <- qt(1 - alpha/2, df=n-1)
    
    x1 <- seq(-6, lower_p, length = 200)
    x2 <- seq(upper_p, 6, length = 200)
    y1 <- dt(x1, df=n-1)
    y2 <- dt(x2, df=n-1)
    
    data1 <- data.frame(x1, y1)
    data2 <- data.frame(x2, y2)
    
    colnames(data1) <- c("x", "y")
    colnames(data2) <- c("x", "y")
    
    point <- data.frame(t, dt(t, df=n-1)/2)
    colnames(point) <- c("x", "y")
    g <- ggplot(data=data1, aes(x=x, y=y)) + 
      stat_function(fun=dt, n=101, args=list(df=n-1), color = "black") + 
      geom_area(data=data1, aes(x=x, y=y), fill="red", alpha=0.4) + 
      geom_area(data=data2, aes(x=x, y=y), fill="red", alpha=0.4) + 
      geom_point(data=point, aes(x=x, y=y), 
                 color = ifelse(accept, "green", "red"), size = 8, shape = 8) + 
      scale_x_continuous(limits = c(-6, 6)) + 
      ylab("f(x)")
      ggtitle(paste0("Distribuição T de Student com ", n-1, " graus de liberdade"))
    g
    
    
    
  })
  
  
  observeEvent(input$testeQuiRefresh, {
    testeQui_params$matrix <- rbind(c(input$testeQuiValor1, input$testeQuiValor2, input$testeQuiValor1 + input$testeQuiValor2), 
                                    c(input$testeQuiValor3, input$testeQuiValor4, input$testeQuiValor3 + input$testeQuiValor4), 
                                    c(input$testeQuiValor1 + input$testeQuiValor3, input$testeQuiValor2 + input$testeQuiValor4, 
                                      input$testeQuiValor1 + input$testeQuiValor2 + input$testeQuiValor3 + input$testeQuiValor4))
    testeQui_params$colnames <- c(input$testeQuiCol1, input$testeQuiCol2, "Total")
    testeQui_params$rownames <- c(input$testeQuiRow1, input$testeQuiRow2, "Total")
    p <- (input$testeQuiValor1 + input$testeQuiValor3)/
      (input$testeQuiValor1 + input$testeQuiValor2 + input$testeQuiValor3 + input$testeQuiValor4)
    testeQui_params$expected <- rbind(c(round(p*(input$testeQuiValor1 + input$testeQuiValor2)), 
                                        round((1-p)*(input$testeQuiValor1 + input$testeQuiValor2)),
                                        input$testeQuiValor1 + input$testeQuiValor2
                                        ),
                                      c(round(p*(input$testeQuiValor3 + input$testeQuiValor4)),
                                        round((1-p)*(input$testeQuiValor3 + input$testeQuiValor4)),
                                        input$testeQuiValor3 + input$testeQuiValor4
                                        ),
                                      c(input$testeQuiValor1 + input$testeQuiValor3, input$testeQuiValor2 + input$testeQuiValor4, 
                                        input$testeQuiValor1 + input$testeQuiValor2 + input$testeQuiValor3 + input$testeQuiValor4))
    testeQui_params$chi <- 
      ((input$testeQuiValor1 - round(p*(input$testeQuiValor1 + input$testeQuiValor2)))^2)/(round(p*(input$testeQuiValor1 + input$testeQuiValor2))) +
      ((input$testeQuiValor2 - round((1-p)*(input$testeQuiValor1 + input$testeQuiValor2)))^2)/(round((1-p)*(input$testeQuiValor1 + input$testeQuiValor2))) +
      ((input$testeQuiValor3 - round(p*(input$testeQuiValor3 + input$testeQuiValor4)))^2)/(round(p*(input$testeQuiValor3 + input$testeQuiValor4))) +
      ((input$testeQuiValor4 - round((1-p)*(input$testeQuiValor3 + input$testeQuiValor4)))^2)/(round((1-p)*(input$testeQuiValor3 + input$testeQuiValor4)))
  })
  
  output$testeQuiTabela <- renderTable({
    tab <- as.data.frame(testeQui_params$matrix)
    colnames(tab) <- testeQui_params$colnames
    rownames(tab) <- testeQui_params$rownames
    
    tab
  }, colnames = TRUE, rownames = TRUE, bordered = TRUE, striped = TRUE, width = "100%", digits = 0, align = "c")
  
  output$testeQuiEsperado <- renderTable({
    tab <- as.data.frame(testeQui_params$expected)
    colnames(tab) <- testeQui_params$colnames
    rownames(tab) <- testeQui_params$rownames
    
    tab
  }, colnames = TRUE, rownames = TRUE, bordered = TRUE, striped = TRUE, width = "100%", digits = 0, align = "c")
  
  output$testeQuiCalc <- renderUI({
    matrix <- testeQui_params$matrix
    exp <- testeQui_params$expected
    chi <- 0
    for (i in c(1, 2, 4, 5)) {
      chi <- chi + (matrix[i] - exp[i])^2/(exp[i])
    }
    withMathJax(helpText(paste0("$$\\chi^{2} = ", round(chi, 2), "$$")))
  })
  
  output$testeQuiPlot <- renderPlot({
    matrix <- testeQui_params$matrix
    exp <- testeQui_params$expected
    chi <- 0
    rc <- qchisq(1 - input$testeQuiAlpha, 1)
    for (i in c(1, 2, 4, 5)) {
      chi <- chi + (matrix[i] - exp[i])^2/(exp[i])
    }
    
    accept <- chi < rc
    
    x <- seq(rc, 8, length = 300)
    y <- dchisq(x, df=1)
    
    data <- data.frame(x, y)
    colnames(data) <- c("x", "y")
    
    point <- data.frame(chi, dchisq(chi, df=1)/2)
    colnames(point) <- c("x", "y")
    
    g <- ggplot(data, aes(x=x, y=y)) + 
      geom_area(stat="identity", fill = "red", alpha = 0.4) + 
      stat_function(fun=dchisq, n=101, args = list(df = 1), color="black") + 
      geom_point(data=point, aes(x=x, y=y), color = ifelse(accept, "green", "red"), size = 8, shape = 8) + 
      scale_x_continuous(limits = c(0, max(7, 1.2*chi))) + 
      xlab("x") +
      ylab("f(x)") + 
      ggtitle("Distribuição Qui-quadrado com 1 grau de liberdade")
      
    g
  })
  
  output$testeQuiConta <- renderText({
    matrix <- testeQui_params$matrix
    exp <- testeQui_params$expected
    chi <- 0
    rc <- qchisq(1 - input$testeQuiAlpha, 1)
    for (i in c(1, 2, 4, 5)) {
      chi <- chi + (matrix[i] - exp[i])^2/(exp[i])
    }
    
    accept <- chi < rc
    text <- paste0("H0: Não há influência (independente)\nH1: Há influência (não é independente)\n
    Valor-qui = ", round(chi, 4), "\nRegião Crítica RC = [", round(rc, 4), ", +∞[\n")
    if (accept) {
      text <- paste0(text, "Como o valor-qui está fora da região crítica, aceita-se H0 e portanto 
                     não há influência se ", input$testeQuiRow1, " ou ", input$testeQuiRow2, ".")
    } else {
      text <- paste0(text, "Como o valor-qui está dentro da região crítica, H0 é rejeitada e portanto H1 é aceita, ou seja,
                     há influência quando ", input$testeQuiRow1, " ou ", input$testeQuiRow2, ".")
    }
    text
  })
  
  getCorrPlotData <- reactive({
    return (as.data.frame(cbind(data[,colnames(data) == input$testeCorrVar1], data[,colnames(data) == input$testeCorrVar2])))
  })
  
  output$testeCorrPlot <- renderPlot({
    selData <- getCorrPlotData()
    colnames(selData) <- c("Var1", "Var2")
    g <- ggplot(selData, aes(x=Var1, y=Var2)) + 
      geom_point(color=dark_cyan, size = 2) + 
      geom_smooth(method=lm) + 
      xlab(input$testeCorrVar1) + 
      ylab(input$testeCorrVar2) + 
      theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12))
    g
  })
  
  output$testeCorrConta <- renderText({
    selData <- getCorrPlotData()
    colnames(selData) <- c("Var1", "Var2")
    correlacao <- cor(selData$Var1, selData$Var2)
    text <- paste0("Correlação(", input$testeCorrVar1, ", ", input$testeCorrVar2, ") = ", correlacao)
    text
  })
  
  
  output$tabelaInformativa <- renderTable({
    tab <- rbind(c("Sexo", "Sexo do aluno: Masculino ou Feminino."),
                 c("Ano letivo", "Ano do curso em que o aluno se encontra. 1 é o primeiro ano e 4, o último."),
                 c("Peso", "Peso do aluno, em kg."),
                 c("Trabalha", "Variável que indica se o aluno trabalha ou não, e se trabalha, se é meio período ou integral."),
                 c("Relacionamento", "Estado de relacionamento do aluno."),
                 c("Cozinha", "Frequência em que o aluno costuma cozinhar, de nunca até sempre."),
                 c("Come fora", "Frequência em que o aluno come fora de casa, em algum restaurante."),
                 c("Percepção de saúde", "Como o aluno classifica, de 1 a 10, sua saúde, sendo 1 péssima e 10 excelente."), 
                 c("Culinária favorita", "Tipo de culinária preferida do aluno"),
                 c("Vegetais nas refeições", "Frequência em que o aluno tem vegetais como parte de suas refeições"),
                 c("Pratica exercícios", "Frequência em que o aluno se exercita."),
                 c("Pratica esportes", "Se o aluno participa de algum esporte ou não."),
                 c("Toma vitaminas", "Se o aluno toma vitaminas."),
                 c("Altura", "Altura do aluno, em metros."))
    tab <- as.data.frame(tab)
    tab
  }, colnames = FALSE, striped = TRUE, bordered = TRUE, width = "100%", align = "c")
  
  #linkTexts
  
  
  observeEvent(input$linkTiposVariaveis, { 
    updateNavbarPage(session, "mainNav", selected = "tabTiposVariaveis")
    })
  observeEvent(input$linkDistrFreq, { 
    updateNavbarPage(session, "mainNav", selected = "tabTabFreqs")
  })
  observeEvent(input$linkMedidasResumo, { 
    updateNavbarPage(session, "mainNav", selected = "tabMedidasResumo")
  })
  observeEvent(input$linkGrafQual, { 
    updateNavbarPage(session, "mainNav", selected = "tabGrafQual")
  })
  observeEvent(input$linkGrafQuant, { 
    updateNavbarPage(session, "mainNav", selected = "tabGrafQuant")
  })
  observeEvent(input$linkGrafBi, { 
    updateNavbarPage(session, "mainNav", selected = "tabGrafBi")
  })
  observeEvent(input$linkDefProb, { 
    updateNavbarPage(session, "mainNav", selected = "tabDefProb")
  })
  observeEvent(input$linkProbCond, { 
    updateNavbarPage(session, "mainNav", selected = "tabProbCond")
  })
  observeEvent(input$linkDistrProb, { 
    updateNavbarPage(session, "mainNav", selected = "tabDistrProb")
  })
  observeEvent(input$linkTesteT1, { 
    updateNavbarPage(session, "mainNav", selected = "tabTesteT1")
  })
  observeEvent(input$linkTesteT2, { 
    updateNavbarPage(session, "mainNav", selected = "tabTesteT2")
  })
  observeEvent(input$linkTesteQui, { 
    updateNavbarPage(session, "mainNav", selected = "tabTesteQui")
  })
  observeEvent(input$linkTesteCorr, { 
    updateNavbarPage(session, "mainNav", selected = "tabTesteCorr")
  })
  
  
}