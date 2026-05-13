# ========================================
# ANÁLISE DE FOMENTO POR INSTITUIÇÃO
# ========================================
# Objetivo: Processar e visualizar dados de fomento (CNPQ)
# segregados por gênero, raça e grande área de pesquisa
#
# Saída: Gráficos e estatísticas por instituição
# ========================================

library(dplyr)      # Manipulação de dados
library(ggplot2)    # Visualizações
library(readr)      # Leitura de arquivos
library(stringr)    # Manipulação de strings

# ----------------------------------------
# FUNÇÃO PRINCIPAL
# ----------------------------------------
# Processa dados de fomento para uma instituição específica
# 
# Parâmetros:
#   df: dataframe com os dados
#   padrao_instituicao: padrão de texto para filtrar instituição (regex)
#   nome: nome da instituição (para títulos dos gráficos)
#
# Retorna: lista com totais, tabelas agregadas e gráficos
processar <- function(df, padrao_instituicao, nome) {
  
  # --- Padronizar nomes das colunas ---
  df <- df %>%
    rename(
      instituicao  = `01_Instituição`,
      modalidade   = `03_Modalidade`,
      sexo         = `08_Sexo`,
      raca         = `09_Cor ou Raça`,
      grande_area  = `06_Grande Área`,
      valor_R      = `Valor (R$)`
    )
  
  # --- Limpar e converter valores monetários para numérico ---
  # Remover símbolos, substituir vírgula por ponto, converter para número
  df <- df %>%
    mutate(
      valor_R = valor_R %>% 
        str_replace_all("[^0-9,]", "") %>%   # Remover tudo que não é número ou vírgula
        str_replace(",", ".") %>%               # Converter "1.000,00" em "1000.00"
        as.numeric(),                           # Converter para número
      instituicao = str_to_lower(instituicao),  # Minúsculas para comparação
      modalidade  = str_to_lower(modalidade)    # Minúsculas para comparação
    )
  
  # --- Aplicar filtros ---
  # 1. Remover valores vazios ou inválidos
  # 2. Filtrar apenas a instituição desejada
  # 3. Manter apenas mestrado e doutorado (cursos de pós-graduação)
  df <- df %>%
    filter(!is.na(valor_R)) %>%
    filter(str_detect(instituicao, padrao_instituicao)) %>%
    filter(str_detect(modalidade, "mestrado|doutorado"))
  
  # --- AGREGAÇÕES DE DADOS ---
  # Tabela: fomento cruzado por sexo e raça
  genero_raca <- df %>%
    group_by(sexo, raca) %>%
    summarise(valor_total = sum(valor_R), .groups = "drop")
  
  # Tabela: fomento total por grande área de pesquisa
  area <- df %>%
    group_by(grande_area) %>%
    summarise(valor_total = sum(valor_R), .groups = "drop")
  
  # Valor total de fomento
  total <- sum(df$valor_R)
  
  # --- GRÁFICOS ---
  # Gráfico 1: Barras agrupadas por sexo e raça
  # Permite comparação visual entre gêneros dentro de cada categoria racial
  plot_genero_raca <- ggplot(genero_raca, aes(raca, valor_total, fill = sexo)) +
    geom_col(position = "dodge") +  # Barras lado a lado
    labs(
      title = paste("Fomento por Gênero e Raça -", nome),
      x = "Raça", 
      y = "Valor total (R$)",
      fill = "Gênero"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Gráfico 2: Barras horizontais ordenadas por valor
  # Facilita a leitura de nomes longos de áreas
  plot_area <- ggplot(area, aes(reorder(grande_area, valor_total), valor_total)) +
    geom_col(fill = "#2E86AB") +
    coord_flip() +  # Inverte para ler melhor
    labs(
      title = paste("Fomento por Grande Área -", nome),
      x = "Grande Área", 
      y = "Valor total (R$)"
    ) +
    theme_minimal()
  
  list(
    total = total,
    genero_raca = genero_raca,
    area = area,
    plot_genero_raca = plot_genero_raca,
    plot_area = plot_area
  )
}

# ========================================
# CARREGAR DADOS
# ========================================
# Lê o arquivo CSV do painel de fomento (CNPQ)
# Nota: Arquivo usa encoding UTF-16 (padrão CNPQ)

dados <- read_delim(
  "Painel_Fomento(tab).csv",
  delim = "\t",                          # Delimitador é tabulação
  locale = locale(encoding = "UTF-16")   # Encoding padrão CNPQ
)

# A primeira linha contém os nomes das colunas
colnames(dados) <- as.character(unlist(dados[1, ]))
dados <- dados[-1, ]  # Remover a primeira linha de dados

# ========================================
# PROCESSAR DADOS POR INSTITUIÇÃO
# ========================================
# Aplica a função para UNICAMP e UFBA
# Cada padrão captura variações do nome da instituição

unicamp <- processar(
  dados, 
  "unicamp|estadual de campinas",  # Padrões para identificar UNICAMP
  "UNICAMP"
)

ufba <- processar(
  dados, 
  "ufba|federal da bahia",  # Padrões para identificar UFBA
  "UFBA"
)

# ========================================
# EXIBIR RESULTADOS
# ========================================
# Mostrar os gráficos gerados

cat("\n===== GRÁFICOS UNICAMP =====\n")
print(unicamp$plot_genero_raca)
print(unicamp$plot_area)

cat("\n===== GRÁFICOS UFBA =====\n")
print(ufba$plot_genero_raca)
print(ufba$plot_area)

# Exibir totais de fomento
cat("\n===== TOTAIS DE FOMENTO =====\n")
cat("UNICAMP: R$", format(unicamp$total, big.mark = "."), "\n")
cat("UFBA: R$", format(ufba$total, big.mark = "."), "\n")
