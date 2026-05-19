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
  df <- df %>%
    mutate(
      valor_R = valor_R %>% 
        str_replace_all("[^0-9,]", "") %>%   
        str_replace(",", ".") %>%               
        as.numeric(),                           
      instituicao = str_to_lower(instituicao),  
      modalidade  = str_to_lower(modalidade)    
    )
  
  # --- Aplicar filtros ---
  df <- df %>%
    filter(!is.na(valor_R)) %>%
    filter(str_detect(instituicao, padrao_instituicao))
  
  # --- AGREGAÇÕES DE DADOS ---
  genero_raca <- df %>%
    group_by(sexo, raca) %>%
    summarise(valor_total = sum(valor_R), .groups = "drop")
  
  area <- df %>%
    group_by(grande_area) %>%
    summarise(valor_total = sum(valor_R), .groups = "drop")
  
  total <- sum(df$valor_R)
  
  # --- GRÁFICOS ---
  plot_genero_raca <- ggplot(genero_raca, aes(raca, valor_total, fill = sexo)) +
    geom_col(position = "dodge") +  
    labs(
      title = paste("Fomento por Gênero e Raça -", nome),
      x = "Raça", 
      y = "Valor total (R$)",
      fill = "Gênero"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_area <- ggplot(area, aes(reorder(grande_area, valor_total), valor_total)) +
    geom_col(fill = "#2E86AB") +
    coord_flip() +  
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
dados <- read_delim(
  "Painel_Fomento(tab).csv",
  delim = "\t",                                  
  locale = locale(encoding = "UTF-16")   
)

# A primeira linha contém os nomes das colunas
colnames(dados) <- as.character(unlist(dados[1, ]))
dados <- dados[-1, ]  

# ========================================
# PROCESSAR DADOS POR INSTITUIÇÃO
# ========================================
unicamp <- processar(
  dados, 
  "unicamp|estadual de campinas",  
  "UNICAMP"
)

ufba <- processar(
  dados, 
  "ufba|federal da bahia",  
  "UFBA"
)

# ========================================
# EXIBIR RESULTADOS DOS GRÁFICOS
# ========================================
cat("\n===== GRÁFICOS UNICAMP =====\n")
print(unicamp$plot_genero_raca)
print(unicamp$plot_area)

cat("\n===== GRÁFICOS UFBA =====\n")
print(ufba$plot_genero_raca)
print(ufba$plot_area)

cat("\n===== TOTAIS DE FOMENTO =====\n")
cat("UNICAMP: R$", format(unicamp$total, big.mark = ".", decimal.mark = ","), "\n")
cat("UFBA: R$", format(ufba$total, big.mark = ".", decimal.mark = ","), "\n")

# ========================================
# 4. COMPARAÇÃO: BRANCOS VS RACIALIZADOS
# ========================================

# PREPARANDO OS DADOS DA UNICAMP ANTES DE COMPARAR
dados_unicamp <- dados %>%
  rename(
    instituicao  = `01_Instituição`,
    raca         = `09_Cor ou Raça`,
    valor_R      = `Valor (R$)`
  ) %>%
  mutate(
    valor_R = as.numeric(str_replace_all(str_replace_all(valor_R, "[^0-9,]", ""), ",", ".")),
    instituicao = str_to_lower(instituicao),
    grupo_raca = case_when(
      str_detect(str_to_lower(raca), "branca") ~ "Branca",
      str_detect(str_to_lower(raca), "não informad|nao informad") ~ "Não Informada",
      TRUE ~ "Não Branca"
    )
  ) %>%
  filter(!is.na(valor_R)) %>%
  filter(str_detect(instituicao, "unicamp|estadual de campinas")) %>%
  filter(grupo_raca != "Não Informada")

# FAZENDO OS CÁLCULOS
resumo_comp <- dados_unicamp %>%
  group_by(grupo_raca) %>%
  summarise(
    qtd = n(),
    valor_total = sum(valor_R),
    .groups = "drop"
  )

b_qtd <- sum(resumo_comp$qtd[resumo_comp$grupo_raca == "Branca"])
nb_qtd <- sum(resumo_comp$qtd[resumo_comp$grupo_raca == "Não Branca"])

b_val <- sum(resumo_comp$valor_total[resumo_comp$grupo_raca == "Branca"])
nb_val <- sum(resumo_comp$valor_total[resumo_comp$grupo_raca == "Não Branca"])

# Médias
media_b <- b_val / b_qtd
media_nb <- nb_val / nb_qtd

# EXIBINDO RESULTADOS FINAIS
cat("\n========================================\n")
cat("CENÁRIO COMPLETO UNICAMP (Todas as Bolsas)\n")
cat("========================================\n")
cat("Pessoas Brancas:     ", b_qtd, "bolsistas | R$", format(b_val, big.mark=".", decimal.mark=","), "\n")
cat("Pessoas Racializadas:", nb_qtd, "bolsistas | R$", format(nb_val, big.mark=".", decimal.mark=","), "\n\n")

cat("Média por pessoa Branca:      R$", format(round(media_b, 2), big.mark=".", decimal.mark=","), "\n")
cat("Média por pessoa Racializada: R$", format(round(media_nb, 2), big.mark=".", decimal.mark=","), "\n\n")

pct_media <- ((media_b / media_nb) - 1) * 100
cat(sprintf(">> Em média, ao considerar todas as modalidades, uma pessoa branca ganha %.1f%% A MAIS que uma pessoa racializada na Unicamp.\n", pct_media))
cat("========================================\n")