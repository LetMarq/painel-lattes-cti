# --- Pacotes necessários ---
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(stringr)

# --- 1) Importar dados ---
dados_lattes <- read_delim(
  file = "Download - Tabela Formacao.csv",
  delim = "\t",
  locale = locale(encoding = "UTF-16LE"),
  col_types = cols(.default = col_character())
)
# --- Limpar nomes das colunas ---
dados_lattes <- dados_lattes %>%
  clean_names()

# Verificar dimensões
num_linhas <- nrow(dados_lattes)
num_colunas <- ncol(dados_lattes)
message(sprintf("Linhas: %d | Colunas: %d", num_linhas, num_colunas))

# --- 2) Filtrar dados ---
# Filtrar formações da UNICAMP
dados_unicamp <- dados_lattes %>%
  filter(instituicao_formacao == "Universidade Estadual de Campinas UNICAMP")

# Filtrar quem atua como professor(a)
dados_professores <- dados_unicamp %>%
  filter(str_detect(enquadramento_atuacao, regex("professor", ignore_case = TRUE)))

# Agrupar por sexo e cor/raça
dados_professores_resumo <- dados_professores %>%
  filter(!is.na(sexo), !is.na(cor_ou_raca), sexo != "", cor_ou_raca != "") %>%
  group_by(sexo, cor_ou_raca) %>%
  summarise(qtd = n(), .groups = "drop")

# --- 3) Gráficos ---

# Distribuição por cor/raça e sexo
ggplot(dados_professores_resumo, aes(x = cor_ou_raca, y = qtd, fill = sexo)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = qtd),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3) +
  labs(
    title = "Distribuição de doutores pela UNICAMP que atuam como professores(as) por sexo e cor/raça",
    x = "Cor ou Raça",
    y = "Número de Professores(as)",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Distribuição por sexo e cor/raça
ggplot(dados_professores_resumo, aes(x = sexo, y = qtd, fill = cor_ou_raca)) +
  geom_col() +
  labs(
    title = "Distribuição de doutores pela UNICAMP que atuam como professores(as) por sexo e cor/raça",
    x = "Sexo",
    y = "Número de Professores(as)",
    fill = "Cor ou Raça"
  ) +
  theme_minimal()


###########################################################################


# --- NOVA ANÁLISE: pessoas que atuam na UNICAMP ---
dados_atuacao_unicamp <- dados_lattes %>%
  filter(instituicao_atuacao == "Universidade Estadual de Campinas UNICAMP")

atuacoes_contagem <- dados_atuacao_unicamp %>%
  filter(!is.na(enquadramento_atuacao), enquadramento_atuacao != "") %>%
  group_by(enquadramento_atuacao) %>%
  summarise(qtd = n(), .groups = "drop") %>%
  arrange(desc(qtd))

# Resumo por sexo e cor/raça
dados_atuacao_resumo <- dados_atuacao_unicamp %>%
  filter(!is.na(sexo), !is.na(cor_ou_raca), sexo != "", cor_ou_raca != "") %>%
  group_by(sexo, cor_ou_raca) %>%
  summarise(qtd = n(), .groups = "drop")

# --- GRÁFICO 1: Distribuição por cor/raça e sexo ---
ggplot(dados_atuacao_resumo, aes(x = cor_ou_raca, y = qtd, fill = sexo)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = qtd),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3) +
  labs(
    title = "Distribuição de pessoas que atuam na UNICAMP por sexo e cor/raça",
    x = "Cor ou Raça",
    y = "Número de pessoas",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- GRÁFICO 2: Distribuição por sexo e cor/raça ---
ggplot(dados_atuacao_resumo, aes(x = sexo, y = qtd, fill = cor_ou_raca)) +
  geom_col() +
  labs(
    title = "Distribuição de pessoas que atuam na UNICAMP por sexo e cor/raça",
    x = "Sexo",
    y = "Número de pessoas",
    fill = "Cor ou Raça"
  ) +
  theme_minimal()


##############################################

# --- Filtrar pessoas que atuam na UNICAMP como professores(as) ---
dados_prof_unicamp_atuacao <- dados_lattes %>%
  filter(
    instituicao_atuacao == "Universidade Estadual de Campinas UNICAMP",
    str_detect(enquadramento_atuacao, regex("professor", ignore_case = TRUE))
  )

# --- Resumir por sexo e cor/raça ---
dados_prof_unicamp_resumo <- dados_prof_unicamp_atuacao %>%
  filter(!is.na(sexo), !is.na(cor_ou_raca), sexo != "", cor_ou_raca != "") %>%
  group_by(sexo, cor_ou_raca) %>%
  summarise(qtd = n(), .groups = "drop")

# --- GRÁFICO 1: Professores(as) na UNICAMP — distribuição por cor/raça e sexo ---
ggplot(dados_prof_unicamp_resumo, aes(x = cor_ou_raca, y = qtd, fill = sexo)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = qtd),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3) +
  labs(
    title = "Professores(as) que atuam na UNICAMP por sexo e cor/raça",
    x = "Cor ou Raça",
    y = "Número de Professores(as)",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- GRÁFICO 2: Professores(as) na UNICAMP — distribuição por sexo e cor/raça ---
ggplot(dados_prof_unicamp_resumo, aes(x = sexo, y = qtd, fill = cor_ou_raca)) +
  geom_col() +
  labs(
    title = "Professores(as) que atuam na UNICAMP por sexo e cor/raça",
    x = "Sexo",
    y = "Número de Professores(as)",
    fill = "Cor ou Raça"
  ) +
  theme_minimal()



