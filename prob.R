def_prob <- fluidPage(
  h3(strong("Definição de Probabilidade")),
  fluidRow(column(2,
                  p("As frequências relativas, além de nos dar uma noção de como os dados estão distribuídos, pode ser 
                    uma estimativa para um cálculo de probabilidade. Ou seja, a partir delas podemos criar um modelo 
                    teórico para calcular a probabilidade de ocorrência de um certo evento de interesse. \n
                    No geral, essa probabilidade se dá dividindo o número de casos em que o evento é favorável (
                    ocasiões em que ocorre o evento esperado) pelo número total de possibilidades de ocorrência. 
                    "),
                  br(),
                  p("Ao lado, encontra-se uma tabela de frequências de nossa base de dados das variáveis 
                    'Sexo' e 'Culinária Favorita'. Assim, podemos calcular as probabilidades de, ao escolher um 
                    indivíduo dessa base ao acaso, qual a probabilidade deste ser homem, ou mulher. Ou de gostar 
                    de comida Mexicana. Ou de ser homem e gostar de comida mexicana. Explore essas por meio da 
                    tabela e das entradas ao lado.")
                  ),
           column(6, "Tabela de Frequências",
                  tableOutput("tabelaProbDef")),
           column(4, "Processo de Cálculo",
                  
                  selectInput("prob_in1", "Probabilidade de: ", c("Feminino", "Masculino", levels(tb$`Culinária/Sexo`))),
                  radioButtons("probOp", " ", choices = c("União" = "uniao", "Intersecção" = "intersec", 
                                                          "Apenas dela" = "only"), selected = "only"),
                  conditionalPanel("input.probOp != 'only'",
                                   selectInput("prob_in2", "com: ", c("Feminino", "Masculino", levels(tb$`Culinária/Sexo`)), 
                                               selected = "Africana")),
                  uiOutput("formulasProb"),
                  verbatimTextOutput("caixaContaProb")
                  ))
)