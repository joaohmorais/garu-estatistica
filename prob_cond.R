prob_cond <- fluidPage(
  h3(strong("Probabilidade Condicional")),
  fluidRow(column(2,
                  p("A probabilidade condicional refere-se à probabilidade de um evento A acontecer dado que 
                    um evento B já aconteceu. Ou seja, agora em vez de calcular a probabilidade de um determinado 
                    indivíduo ser homem ou gostar de um determinado tipo de comida, calculamos a probabilidade 
                    dele ser homem dado que o indivíduo gosta de tal tipo de comida. Ou seja, limitamos o nosso 
                    espaço amostral para um em que uma condição já ocorreu."),
                  selectInput("condTabVar1", "Variável 1", choices = colnames(data)[c(-3, -2, -8)], selected = "Percepção de Saúde"),
                  selectInput("condTabVar2", "Variável 2", choices = colnames(data)[c(-3, -2, -8)], selected = "Vegetais nas refeições")
                  ),
           column(6, "Tabela de Frequências",
                  tableOutput("tabelaProbCond")),
           column(4, "Processo de Cálculo",
                  
                  uiOutput("selectProbCondVar"),
                  
                  withMathJax("P(A | B) = $$\\frac{P(A ∩ B)}{P(B)} $$"),
                  verbatimTextOutput("caixaContaProbCond")
                  
                  
                  # selectInput("prob_in1", "Probabilidade de: ", c("Feminino", "Masculino", levels(tb$`Culinária/Sexo`))),
                  # radioButtons("probOp", " ", choices = c("União" = "uniao", "Intersecção" = "intersec", 
                  #                                         "Apenas dela" = "only"), selected = "only"),
                  # conditionalPanel("input.probOp != 'only'",
                  #                  selectInput("prob_in2", "com: ", c("Feminino", "Masculino", levels(tb$`Culinária/Sexo`)), 
                  #                              selected = "Africana")),
                  # uiOutput("formulasProb"),
                  # verbatimTextOutput("caixaContaProb")
                  ))
)