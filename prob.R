def_prob <- fluidPage(
  h3(strong("Definição de Probabilidade")),
  fluidRow(column(6,
                  wellPanel(p("Um evento é um subconjunto de realizações de interesse entre todas as possíveis. As frequências relativas nos dão uma noção das ocorrências de um determinado evento em um estudo. Ou seja, elas são estimativas de probabilidade de tais eventos. Essa probabilidade se dá dividindo o número de casos em que o evento esperado ocorre pelo número total de observações.
                    "),
                  br(),
                  p("Ao lado, encontra-se uma tabela de frequências de nossa base de dados das variáveis 
                    'Sexo' e 'Culinária Favorita'. Assim, podemos calcular as probabilidades de, ao escolher um 
                    indivíduo dessa base ao acaso, este ser homem ou mulher. Ou de gostar 
                    de comida Mexicana. Ou de ser homem e gostar de comida mexicana. Explore essas probabilidades por meio da 
                    tabela e das entradas ao lado.")
                  )),
           column(6, "Tabela de Frequências",
                  tableOutput("tabelaProbDef"))),
  fluidRow(
    column(6, wellPanel("Processo de Cálculo",
           
           selectInput("prob_in1", "Probabilidade de: ", c("Feminino", "Masculino", levels(tb$`Culinária/Sexo`))),
           radioButtons("probOp", " ", choices = c("União" = "uniao", "Intersecção" = "intersec", 
                                                   "Apenas dela" = "only"), selected = "only"),
           conditionalPanel("input.probOp != 'only'",
                            selectInput("prob_in2", "com: ", c("Feminino", "Masculino", levels(tb$`Culinária/Sexo`)), 
                                        selected = "Africana")),
           uiOutput("formulasProb"))
    ),
    column(6, 
           h4(strong("Cálculo")),
           verbatimTextOutput("caixaContaProb"))
  )
)