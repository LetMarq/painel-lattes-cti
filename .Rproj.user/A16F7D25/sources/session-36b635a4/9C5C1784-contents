# --- Pacotes necessários ---
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(stringr)

# --- 1) Importar as duas tabelas ---
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

# --- 2) Unir as bases (Mestrado + Doutorado) ---
dados_unificados <- bind_rows(dados_doutorado, dados_mestrado)


# ------------------------------------------

# --- 3) Filtrar apenas quem se formou na UNICAMP ---
dados_unicamp <- dados_unificados %>%
  filter(instituicao_formacao == "Universidade Estadual de Campinas UNICAMP")

# --- 4) Resumir por sexo e cor/raça ---
dados_unicamp_resumo <- dados_unicamp %>%
  filter(!is.na(sexo), !is.na(cor_ou_raca), sexo != "", cor_ou_raca != "") %>%
  group_by(sexo, cor_ou_raca) %>%
  summarise(qtd = n(), .groups = "drop") %>%
  arrange(desc(qtd))

# --- 5) Visualização 1: gráfico de barras por cor/raça e sexo ---
ggplot(dados_unicamp_resumo, aes(x = cor_ou_raca, y = qtd, fill = sexo)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = qtd),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3) +
  labs(
    title = "Formados na UNICAMP (Mestrado + Doutorado) por sexo e cor/raça",
    x = "Cor ou Raça",
    y = "Número de pessoas",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 6) Visualização 2: gráfico de barras por sexo e cor/raça ---
ggplot(dados_unicamp_resumo, aes(x = sexo, y = qtd, fill = cor_ou_raca)) +
  geom_col() +
  labs(
    title = "Distribuição de formados na UNICAMP (Mestrado + Doutorado) por sexo e cor/raça",
    x = "Sexo",
    y = "Número de pessoas",
    fill = "Cor ou Raça"
  ) +
  theme_minimal()



# --------------------------------- Universidade Federal da Bahia UFBA ---------------------------------------------------

# --- 3) Filtrar apenas quem se formou na UFBA ---
dados_ufba <- dados_unificados %>%
  filter(instituicao_formacao == "Universidade Federal da Bahia UFBA")

# --- 4) Resumir por sexo e cor/raça ---
dados_ufba_resumo <- dados_ufba %>%
  filter(!is.na(sexo), !is.na(cor_ou_raca), sexo != "", cor_ou_raca != "") %>%
  group_by(sexo, cor_ou_raca) %>%
  summarise(qtd = n(), .groups = "drop") %>%
  arrange(desc(qtd))

# --- 5) Visualização 1: gráfico de barras por cor/raça e sexo ---
ggplot(dados_ufba_resumo, aes(x = cor_ou_raca, y = qtd, fill = sexo)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = qtd),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3) +
  labs(
    title = "Formados na UFBA (Mestrado + Doutorado) por sexo e cor/raça",
    x = "Cor ou Raça",
    y = "Número de pessoas",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 6) Visualização 2: gráfico de barras por sexo e cor/raça ---
ggplot(dados_ufba_resumo, aes(x = sexo, y = qtd, fill = cor_ou_raca)) +
  geom_col() +
  labs(
    title = "Distribuição de formados na UFBA (Mestrado + Doutorado) por sexo e cor/raça",
    x = "Sexo",
    y = "Número de pessoas",
    fill = "Cor ou Raça"
  ) +
  theme_minimal()


# --- Total de formados na UNICAMP ---
total_unicamp <- nrow(dados_unicamp)
print(paste("Total de formados na UNICAMP:", total_unicamp))

# --- Total de formados na UFBA ---
total_ufba <- nrow(dados_ufba)
print(paste("Total de formados na UFBA:", total_ufba))