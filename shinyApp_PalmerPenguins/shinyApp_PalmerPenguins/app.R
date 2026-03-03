# --- Carregamento dos Pacotes ---
# Certifique-se de que os pacotes estão instalados:
# install.packages(c("shiny", "ggplot2", "dplyr", "dados"))

library(shiny)
library(ggplot2)
library(dplyr)
library(dados) # Pacote que contém o dataset pinguins

# --- Preparação dos Dados ---
# Carregamos o dataset e removemos as linhas com dados faltantes (NA)
# para evitar erros nos filtros e no gráfico.
pinguins_df <- dados::pinguins %>%
  na.omit()

# Criamos um vetor com os nomes das variáveis numéricas para usar nos inputs
opcoes_variaveis <- names(select_if(pinguins_df, is.numeric))


# --- Interface do Usuário (UI) ---
ui <- fluidPage(
  # Título do aplicativo
  titlePanel("Exploração de Dados dos Pinguins de Palmer"),
  
  # Layout com barra lateral
  sidebarLayout(
    # Painel da barra lateral com os filtros
    sidebarPanel(
      h4("Filtros do Gráfico"),
      
      # Seletor para a variável do eixo X
      selectInput(
        inputId = "var_x",
        label = "Selecione a variável do Eixo X:",
        choices = opcoes_variaveis,
        selected = "comprimento_bico_mm" # Valor inicial
      ),
      
      # Seletor para a variável do eixo Y
      selectInput(
        inputId = "var_y",
        label = "Selecione a variável do Eixo Y:",
        choices = opcoes_variaveis,
        selected = "massa_corporal_g" # Valor inicial
      ),
      
      # Checkbox para filtrar as espécies
      checkboxGroupInput(
        inputId = "especies",
        label = "Selecione as Espécies:",
        choices = unique(pinguins_df$especie),
        selected = unique(pinguins_df$especie) # Começa com todas selecionadas
      ),
      
      # Slider para filtrar pelo comprimento do bico
      sliderInput(
        inputId = "comprimento_bico",
        label = "Filtro por Comprimento do Bico (mm):",
        min = min(pinguins_df$comprimento_bico_mm),
        max = max(pinguins_df$comprimento_bico_mm),
        value = c(min(pinguins_df$comprimento_bico_mm), max(pinguins_df$comprimento_bico_mm))
      )
    ),
    
    # Painel principal onde o gráfico será exibido
    mainPanel(
      plotOutput("grafico_dispersao")
    )
  )
)


# --- Lógica do Servidor (Server) ---
server <- function(input, output) {
  
  # 1. Criação de um dataframe reativo
  # Este bloco de código irá re-executar sempre que um dos inputs mudar.
  dados_filtrados <- reactive({
    pinguins_df %>%
      # Filtra com base nas espécies selecionadas no checkbox
      filter(especie %in% input$especies) %>%
      # Filtra com base no intervalo do slider de comprimento do bico
      filter(comprimento_bico_mm >= input$comprimento_bico[1] & 
               comprimento_bico_mm <= input$comprimento_bico[2])
  })
  
  # 2. Renderização do gráfico de dispersão
  # Este bloco irá re-executar sempre que 'dados_filtrados()' for atualizado.
  output$grafico_dispersao <- renderPlot({
    
    ggplot(
      data = dados_filtrados(), 
      # Usamos .data[[input$var_x]] para que o ggplot entenda que
      # os nomes das variáveis vêm de um input.
      aes(
        x = .data[[input$var_x]], 
        y = .data[[input$var_y]],
        color = especie, # Cor baseada na espécie
        shape = especie  # Forma baseada na espécie
      )
    ) +
      geom_point(size = 3, alpha = 0.8) +
      # Títulos e legendas
      labs(
        title = "Gráfico de Dispersão dos Pinguins",
        x = str_to_title(str_replace_all(input$var_x, "_", " ")), # Formata o nome do eixo X
        y = str_to_title(str_replace_all(input$var_y, "_", " ")), # Formata o nome do eixo Y
        color = "Espécie",
        shape = "Espécie"
      ) +
      # Tema do gráfico
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
      )
    
  })
  
}


# --- Execução do Aplicativo ---
shinyApp(ui, server)