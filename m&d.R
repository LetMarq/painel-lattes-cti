# ========================================
# ANÁLISE MESTRADO + DOUTORADO (UNICAMP vs UFBA)
# ========================================
# Objetivo: Comparar formandos de mestrado e doutorado
# de duas instituições principais (UNICAMP e UFBA)
# Segmentados por sexo e cor/raça
#
# Fonte de dados: Plataforma Lattes (CNPq)
# ========================================

# --- Carregar bibliotecas ---
library(readr)      # Leitura de arquivos
library(dplyr)      # Manipulação de dados
library(janitor)    # Limpeza de nomes
library(ggplot2)    # Visualizações
library(stringr)    # Manipulação de strings

# ========================================
# 1. CARREGAR DADOS
# ========================================
# Importar tabelas de Doutorado e Mestrado
# Ambas usam encoding UTF-16LE (padrão Lattes)

dados_doutorado <- read_delim(
  file = "Download - Tabela Formacao.csv",
  delim = "\t",
  locale = locale(encoding = "UTF-16LE"),
  col_types = cols(.default = col_character())
) %>%
  clean_names()

dados_mestrado <- read_delim(
  file = "Download - Tabela - Mestrado.csv",
  delim = "\t",
  locale = locale(encoding = "UTF-16LE"),
  col_types = cols(.default = col_character())
) %>%
  clean_names()

cat("✓ Dados carregados:\n")
cat("  - Doutorado:", nrow(dados_doutorado), "registros\n")
cat("  - Mestrado:", nrow(dados_mestrado), "registros\n\n")

# ========================================
# 2. UNIFICAR DADOS
# ========================================
# Combina mestrado + doutorado em uma única tabela
# permitindo comparações combinadas (M + D)

dados_unificados <- bind_rows(dados_doutorado, dados_mestrado)

cat("  - Total (M + D):", nrow(dados_unificados), "registros\n\n")

# ========================================
# 3. FUNÇÃO PARA CRIAR GRÁFICOS PAREADOS
# ========================================
# Padroniza a criação de gráficos por instituição
#
# Parâmetros:
#   df: dataframe filtrado para a instituição
#   nome_instituicao: nome para exibição

gerar_analise <- function(df, nome_instituicao) {
  
  # Preparar dados: filtrar valores vazios
  df_resumo <- df %>%
    filter(
      !is.na(sexo), !is.na(cor_ou_raca),
      sexo != "", cor_ou_raca != ""
    ) %>%
    group_by(sexo, cor_ou_raca) %>%
    summarise(qtd = n(), .groups = "drop") %>%
    arrange(desc(qtd))
  
  # Gráfico 1: Por raça (agrupado por sexo)
  # Permite comparar gêneros dentro de cada categoria racial
  g1 <- ggplot(df_resumo, aes(x = cor_ou_raca, y = qtd, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(
      aes(label = qtd),
      position = position_dodge(width = 0.9),
      vjust = -0.3, size = 3
    ) +
    labs(
      title = paste(nome_instituicao, ": Distribuição por Raça"),
      x = "Cor ou Raça",
      y = "Número de pessoas",
      fill = "Sexo"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Gráfico 2: Por sexo (empilhado por raça)
  # Facilita ver distribuição total de gênero
  g2 <- ggplot(df_resumo, aes(x = sexo, y = qtd, fill = cor_ou_raca)) +
    geom_col() +
    geom_text(
      aes(label = qtd),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    labs(
      title = paste(nome_instituicao, ": Distribuição por Sexo"),
      x = "Sexo",
      y = "Número de pessoas",
      fill = "Cor ou Raça"
    ) +
    theme_minimal()
  
  # Retornar resultados
  return(list(
    total = nrow(df),
    resumo = df_resumo,
    grafico_raca = g1,
    grafico_sexo = g2
  ))
}

# ========================================
# 4. ANÁLISE UNICAMP (M + D)
# ========================================

dados_unicamp <- dados_unificados %>%
  filter(instituicao_formacao == "Universidade Estadual de Campinas UNICAMP")

resultado_unicamp <- gerar_analise(dados_unicamp, "UNICAMP (M + D)")

cat("\n===== UNICAMP (MESTRADO + DOUTORADO) =====\n")
cat("Total de formandos:", resultado_unicamp$total, "\n\n")

print(resultado_unicamp$grafico_raca)
print(resultado_unicamp$grafico_sexo)

# ========================================
# 5. ANÁLISE UFBA (M + D)
# ========================================

dados_ufba <- dados_unificados %>%
  filter(instituicao_formacao == "Universidade Federal da Bahia UFBA")

resultado_ufba <- gerar_analise(dados_ufba, "UFBA (M + D)")

cat("\n===== UFBA (MESTRADO + DOUTORADO) =====\n")
cat("Total de formandos:", resultado_ufba$total, "\n\n")

print(resultado_ufba$grafico_raca)
print(resultado_ufba$grafico_sexo)

# ========================================
# 6. COMPARAÇÃO RESUMIDA
# ========================================

cat("\n========================================")
cat("\nRESUMO COMPARATIVO\n")
cat("========================================\n\n")

cat("UNICAMP:\n")
cat("  Total (M + D):", resultado_unicamp$total, "\n")

cat("\nUFBA:\n")
cat("  Total (M + D):", resultado_ufba$total, "\n")

cat("\nDiferença:", abs(resultado_unicamp$total - resultado_ufba$total), "\n")
cat("Proporção UNICAMP/UFBA:", 
    round(resultado_unicamp$total / resultado_ufba$total, 2), "x\n")
