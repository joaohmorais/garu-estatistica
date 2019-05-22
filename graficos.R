graf_qualitativa <- sidebarLayout(
  sidebarPanel(
    h3(strong("Gráficos para variáveis qualitativas")),
    selectInput("varGrafQual", "Variável", choices = colnames(data)[-c(2, 3, 8)]),
    h5(strong("Gráfico de Barras")),
    p("O gráfico de barras se dá por retângulos (barras) em que em uma de suas direções (mais frequentemente 
      na vertical) representa-se a frequência absoluta (contagem) ou relativa (porcentagem) de uma 
      variável qualitativa. Cada barra representa um possível valor e todas são paralelas umas às outras."),
    h5(strong("Gráfico de Pizza")),
    p("O gráfico de setores, ou gráfico de pizza, consiste de um círculo dividido em fatias que são proporcionais 
      às frequências relativas da variável."),
    br(),
    helpText("Fonte: Morettin, P. and Bussab, W. (2000). Estatística Básica (7a. ed.). Editora Saraiva.")
  ),
  mainPanel(
    uiOutput("graficosQual"))
)
graf_quantitativa <- sidebarLayout(
  sidebarPanel(
    h3(strong("Gráficos para variáveis quantitativas")),
    selectInput("varGrafQuant", "Variável", choices = colnames(data)[c(2, 3, 8, 14)], selected = "Peso"),
    h5(strong("Gráfico de Barras")),
    p("Quando trabalhamos com variáveis quantitativas discretas, ou seja, que assumem uma quantidade limitada 
      de valores, podemos usar um gráfico similar ao de barras usado para variáveis qualitativas ordinais. 
      Dispomos os valores possíveis, em ordem, em barras e a altura é relativa a frequência."),
    h5(strong("Histograma")),
    p("Para variáveis quantitativas contínuas, fica inviável representar uma barra para cada observação. Imagine: 
      teria uma barra para o valor 4.82, uma para o 4.91, e assim em diante. Para facilitar isso, mapeamos os 
      valores em intervalos e representa, por meio de um histograma, essa quantidade limitada de intervalos."),
    h5(strong("Boxplot")),
    HTML("<p>O boxplot é um gráfico muito utilizado pois a partir dele pode-se obter muitas informações sobre a dispersão 
      e assimetria dos dados. Ele se dá por um retângulo, onde cada limite é um quantil (o limite inferior é o 
      primeiro quartil (q1), e o limite superior é o terceiro quartil (q3).) O traço no meio representa a <strong>mediana</strong> 
      dos dados (q2). A partir do retângulo, para cima, segue uma linha até o ponto mais remoto que não exceda 
      LS = q3 + 1,5dq, chamado de limite superior. Similarmente, segue uma linha abaixo até o ponto que não exceda 
      LI = q1 - 1,5dq, chamando limite inferior. Os valores acima do limite superior e abaixo do limite inferior 
      são plotados como pontos e chamados de outliers ou valores atípicos.</p>"),
    br(),
    helpText("Observação: dq é a distância interquartil, que se dá por dq = q3 - q1."),
    br(),
    helpText("Fonte: Morettin, P. and Bussab, W. (2000). Estatística Básica (7a. ed.). Editora Saraiva.")
  ),
  mainPanel(
    fluidRow(column(6,htmlOutput("tituloGraf1"),
                    plotOutput("histograma"),
                    htmlOutput("htGraf1")
    ),
    column(6, h4(strong("Boxplot")),
           plotOutput("boxplot"))),
    fluidRow(column(6,
                    uiOutput("histAlt")))
    
            )
)

graf_bidimensional <- fluidPage(
  h3(strong("Gráficos para Variáveis Qualitativas")),
  fluidRow(column(4, h4(strong("Duas variáveis qualitativas")),
                  selectInput("varGrafBiQual1", "Variável 1", choices = c("Sexo", "Ano letivo", "Trabalha", 
                                                                          "Relacionamento", "Cozinha", "Come fora", 
                                                                          "Vegetais nas refeições", "Pratica exercícios", 
                                                                          "Pratica esportes", "Toma vitaminas"), selected = "Sexo"),
                  selectInput("varGrafBiQual2", "Variável 2", choices = c("Sexo", "Ano letivo", "Trabalha", 
                                                                          "Relacionamento", "Cozinha", "Come fora", 
                                                                          "Vegetais nas refeições", "Pratica exercícios", 
                                                                          "Pratica esportes", "Toma vitaminas"), selected = "Ano letivo"),
                  p("Imagine que queiramos visualizar a distribuição das observações em 
                    relação a duas variáveis qualitativas. Podemos, primeiramente, visualizar 
                    a tabela de frequências relativas: "),
                  tableOutput("tabGrafVarQual"),
                  p("Podemos agora, construir um gráfico de barras muito similar ao visto 
                    anteriormente, mas com as barras divididas de acordo com a outra variável 
                    variável qualitativa escolhida"),
                  plotOutput("grafBarraBiQual")
                  ),
           column(4,
                  h4(strong("Duas variáveis quantitativas")),
                  selectInput("varGrafBiQuant1", "Variável 1", choices = c("Percepção de Saúde", "Peso", "Ano letivo", "Altura"), selected = "Altura"),
                  selectInput("varGrafBiQuant2", "Variável 2", choices = c("Percepção de Saúde", "Peso", "Ano letivo", "Altura"), selected = "Peso"),
                  p("Pode-se observar a distribuição de duas variáveis quantitativas também por meio de tabela. Porém, 
                    em alguns casos pode ser necessária a divisão em intervalos (casos de variável quantitativa contínua). Uma forma 
                    bem útil de visualização de distribuição e relação de duas variáveis quantitativas é o gráfico de dispersão. Nele, 
                    temos dois eixos, cada um representando uma variável, e assim colocam-se os pontos para representar cada observação."),
                  plotOutput("scatterGraf")
                  ),
           column(4,
                  h4(strong("Variáveis qualitativas e quantitativas")),
                  selectInput("varQualBoxplot1", "Variável Qualitativa", choices = c("Sexo", "Ano letivo", "Trabalha", 
                                                                                    "Relacionamento", "Cozinha", "Come fora", 
                                                                                    "Vegetais nas refeições", "Pratica exercícios", 
                                                                                    "Pratica esportes", "Toma vitaminas")),
                  selectInput("varQualBoxplot2", "Variável Quantitativa", choices = c("Peso", "Altura")),
                  p("Também é muito comum visualizar o que acontece com uma variável qualitativa dentro de cada categoria de uma 
                    variável qualitativa. Uma forma de realizar isso é através de vários boxplots, similares aos vistos anteriormente, mas onde há 
                    um dele para cada valor da variável qualitativa. Compare cada variável qualitativa com o peso ou altura nos boxplots abaixo:"),
                  plotOutput("multiBoxplot")
                  )
           )
)