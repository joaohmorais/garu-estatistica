inicio <- fluidPage(
  fluidRow(
    HTML('<center><img src="images/garu_3.png"></center>')),
  fluidRow(column(6, wellPanel(
    h3(strong('Funcionalidades e métodos estatísticos')),
    p("O conteúdo do app está dividido pelos tópicos (clique para explorar):"),
    strong("1. Estatística Descritiva"), br(),
    actionLink("linkTiposVariaveis", "1.1. Tipos de Variáveis"), br(),
    actionLink("linkDistrFreq", "1.2. Distribuição de Frequências"), br(),
    actionLink("linkMedidasResumo", "1.3. Medidas Resumo"), br(),
    br(), 
    strong("2. Gráficos"), br(),
    actionLink("linkGrafQual", "2.1. Gráficos para variáveis qualitativas"), br(),
    actionLink("linkGrafQuant", "2.2. Gráficos para variáveis quantitativas"), br(), 
    actionLink("linkGrafBi", "2.3. Gráficos bidimensionais"), br(), 
    br(),  
    strong("3. Probabilidade"), br(),
    actionLink("linkDefProb", "3.1. Definição de Probabilidade"), br(),
    actionLink("linkProbCond", "3.2. Probabilidade Condicional"), br(), 
    actionLink("linkDistrProb", "3.3 Distribuições de Probabilidade"), br(),
    br(), 
    strong("4. Inferência"), br(),
    actionLink("linkTesteT1", "4.1. Teste T para uma amostra"), br(),
    actionLink("linkTesteT2", "4.2. Teste T para duas amostras dependentes"), br(),
    actionLink("linkTesteQui", "4.3. Teste qui-quadrado de independência"), br(),
    actionLink("linkTesteCorr", "4.4. Teste de Correlação de Pearson"), br(), 
    br(), br(),
    HTML("<p><strong>Aluno participante:</strong> João Henrique de Araujo Morais</p>"),
    HTML("<p><strong>Professora Orientadora:</strong> Profa. Dra. Camila Bertini Martins</p>"),
    HTML("<p><strong>Contato: </strong><a> joao.morais@unifesp.br </a></p>"),
    helpText("GARU Estatística, 2019. Versão 1.0.1")
  )
  ), 
  column(6, wellPanel(
    h3(strong('Conjunto de dados utilizados')),
    HTML("<p>Em diversos exemplos, utilizamos para explicação dados extraídos de uma base do site <a href='https://www.kaggle.com'> Kaggle</a>. 
      A base se chama 'Food Choices', de domínio público, e consiste em um questionário aplicado na 
      Universidade de Mercyhurst, Pennsilvânia, EUA. A base inclui informações de preferências gastronômicas, 
      nutrição, e relação com a saúde dos alunos. Ela pode ser visualizada <a href='https://www.kaggle.com/borapajo/food-choices'> aqui</a>. A seguir contém uma breve 
      explicação das variáveis que vocês podem encontrar e usar no aplicativo:
</p>"),
    tableOutput("tabelaInformativa"),
    downloadButton("botaoBaixarDados", "Download dos dados"),
    helpText("Vale informar que a variável 'Altura' não existia na base de dados original e foi estimada a partir de 
             métodos de regressão treinados com a base de dados 'weight-height', também do Kaggle.")
  )
  )
  )
)
