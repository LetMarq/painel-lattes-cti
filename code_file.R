# ========================================
# ANÁLISE DE DOUTORES DA UNICAMP - LATTES
# ========================================
# Objetivo: Analisar doutores que se formaram ou atuam na UNICAMP
# com segmentação por sexo e cor/raça
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
# 1. CARREGAR E PREPARAR DADOS
# ========================================
# Importar arquivo CSV do Lattes
# Nota: Encoding UTF-16LE é padrão do sistema Lattes

dados_lattes <- read_delim(
  file = "Download - Tabela Formacao.csv",
  delim = "\t",                                    # Delimitador é tabulação
  locale = locale(encoding = "UTF-16LE"),          # Encoding padrão Lattes
  col_types = cols(.default = col_character())    # Ler tudo como texto primeiro
)

# Padronizar nomes das colunas (remove espaços e caracteres especiais)
dados_lattes <- dados_lattes %>%
  clean_names()

# Informar estatísticas básicas do arquivo carregado
num_linhas <- nrow(dados_lattes)
num_colunas <- ncol(dados_lattes)
message(sprintf("✓ Dados carregados: %d linhas x %d colunas", num_linhas, num_colunas))

# ========================================
# 2. FUNÇÃO AUXILIAR PARA CRIAR GRÁFICOS
# ========================================
# Cria gráficos padronizados com 2 visualizações
# (uma por raça/gênero e outra por sexo)
#
# Parâmetros:
#   df: dataframe já filtrado
#   titulo_base: texto base para o título
#   label_contagem: rótulo para a contagem (ex: "Número de pessoas")

criar_graficos <- function(df, titulo_base, label_contagem = "Quantidade") {
  
  # Preparar dados: agrupar por sexo e raça
  df_resumo <- df %>%
    filter(
      !is.na(sexo), !is.na(cor_ou_raca),     # Remover valores vazios
      sexo != "", cor_ou_raca != ""           # Remover strings vazias
    ) %>%
    group_by(sexo, cor_ou_raca) %>%
    summarise(qtd = n(), .groups = "drop")
  
  # Gráfico 1: Barras agrupadas por raça e sexo
  # Permite comparar gêneros dentro de cada categoria racial
  g1 <- ggplot(df_resumo, aes(x = cor_ou_raca, y = qtd, fill = sexo)) +
    geom_col(position = "dodge") +
    geom_text(
      aes(label = qtd),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      size = 3
    ) +
    labs(
      title = paste(titulo_base, "(Distribuição por Raça)"),
      x = "Cor ou Raça",
      y = label_contagem,
      fill = "Sexo"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Gráfico 2: Barras por sexo com raça empilhada
  # Facilita ver distribuição total por gênero
  g2 <- ggplot(df_resumo, aes(x = sexo, y = qtd, fill = cor_ou_raca)) +
    geom_col() +
    geom_text(
      aes(label = qtd),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    labs(
      title = paste(titulo_base, "(Distribuição por Sexo)"),
      x = "Sexo",
      y = label_contagem,
      fill = "Cor ou Raça"
    ) +
    theme_minimal()
  
  return(list(g1 = g1, g2 = g2))
}

# ========================================
# 3. ANÁLISE 1: DOUTORES FORMADOS NA UNICAMP
# ========================================
# Filtra pessoas que se GRADUARAM na UNICAMP

dados_unicamp_formacao <- dados_lattes %>%
  filter(instituicao_formacao == "Universidade Estadual de Campinas UNICAMP")

cat("\n===== DOUTORES FORMADOS NA UNICAMP =====\n")
cat("Total:", nrow(dados_unicamp_formacao), "pessoas\n\n")

# Gerar gráficos para formandos da UNICAMP
graficos_formacao <- criar_graficos(
  dados_unicamp_formacao,
  "Doutores Formados na UNICAMP",
  "Número de pessoas"
)

# Exibir gráficos
print(graficos_formacao$g1)
print(graficos_formacao$g2)

# ========================================
# 4. ANÁLISE 2: PROFESSORES FORMADOS NA UNICAMP
# ========================================
# Filtra doutores formados na UNICAMP que atuam como professor

dados_prof_formacao <- dados_unicamp_formacao %>%
  filter(str_detect(enquadramento_atuacao, regex("professor", ignore_case = TRUE)))

cat("\n===== PROFESSORES (Formados na UNICAMP) =====\n")
cat("Total:", nrow(dados_prof_formacao), "professores(as)\n\n")

# Gerar gráficos
graficos_prof_formacao <- criar_graficos(
  dados_prof_formacao,
  "Professores Formados na UNICAMP",
  "Número de Professores(as)"
)

print(graficos_prof_formacao$g1)
print(graficos_prof_formacao$g2)

# ========================================
# 5. ANÁLISE 3: PESSOAS QUE ATUAM NA UNICAMP
# ========================================
# Filtra pessoas cujo local ATUAL de trabalho é UNICAMP

dados_atuacao_unicamp <- dados_lattes %>%
  filter(instituicao_atuacao == "Universidade Estadual de Campinas UNICAMP")

cat("\n===== PESSOAS QUE ATUAM NA UNICAMP =====\n")
cat("Total:", nrow(dados_atuacao_unicamp), "pessoas\n\n")

# Resumir por enquadramento (tipos de atuação)
# Opcional: mostrar frequência de cada tipo
atuacoes_contagem <- dados_atuacao_unicamp %>%
  filter(!is.na(enquadramento_atuacao), enquadramento_atuacao != "") %>%
  group_by(enquadramento_atuacao) %>%
  summarise(qtd = n(), .groups = "drop") %>%
  arrange(desc(qtd))

cat("\nTipos de atuação:\n")
print(atuacoes_contagem)

# Gerar gráficos para quem atua na UNICAMP
graficos_atuacao <- criar_graficos(
  dados_atuacao_unicamp,
  "Pessoas que Atuam na UNICAMP",
  "Número de pessoas"
)

print(graficos_atuacao$g1)
print(graficos_atuacao$g2)

# ========================================
# 6. ANÁLISE 4: PROFESSORES QUE ATUAM NA UNICAMP
# ========================================
# Filtra pessoas que atuam como professor NA UNICAMP
# (não necessariamente formadas lá)

dados_prof_atuacao <- dados_lattes %>%
  filter(
    instituicao_atuacao == "Universidade Estadual de Campinas UNICAMP",
    str_detect(enquadramento_atuacao, regex("professor", ignore_case = TRUE))
  )

cat("\n===== PROFESSORES QUE ATUAM NA UNICAMP =====\n")
cat("Total:", nrow(dados_prof_atuacao), "professores(as)\n\n")

# Gerar gráficos
graficos_prof_atuacao <- criar_graficos(
  dados_prof_atuacao,
  "Professores que Atuam na UNICAMP",
  "Número de Professores(as)"
)

print(graficos_prof_atuacao$g1)
print(graficos_prof_atuacao$g2)

# ========================================
# RESUMO FINAL
# ========================================
cat("\n========================================\n")
cat("RESUMO DAS ANÁLISES\n")
cat("========================================\n\n")
cat("1. Formados na UNICAMP:", nrow(dados_unicamp_formacao), "\n")
cat("2. Professores formados na UNICAMP:", nrow(dados_prof_formacao), "\n")
cat("3. Pessoas que atuam na UNICAMP:", nrow(dados_atuacao_unicamp), "\n")
cat("4. Professores que atuam na UNICAMP:", nrow(dados_prof_atuacao), "\n")



