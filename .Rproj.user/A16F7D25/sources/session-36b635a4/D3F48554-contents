library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

# ------------------------------------------------------
# Função simples e totalmente explícita
# ------------------------------------------------------
processar <- function(df, padrao_instituicao, nome) {
  
  df <- df %>%
    rename(
      instituicao  = `01_Instituição`,
      modalidade   = `03_Modalidade`,
      sexo         = `08_Sexo`,
      raca         = `09_Cor ou Raça`,
      grande_area  = `06_Grande Área`,
      valor_R      = `Valor (R$)`
    ) %>%
    mutate(
      valor_R = valor_R %>% 
        str_replace_all("[^0-9,]", "") %>% 
        str_replace(",", ".") %>% 
        as.numeric(),
      instituicao = str_to_lower(instituicao),
      modalidade  = str_to_lower(modalidade)
    ) %>%
    filter(!is.na(valor_R)) %>%
    filter(str_detect(instituicao, padrao_instituicao)) %>%
    filter(str_detect(modalidade, "mestrado|doutorado"))
  
  # ---- Agregações ----
  genero_raca <- df %>%
    group_by(sexo, raca) %>%
    summarise(valor_total = sum(valor_R), .groups = "drop")
  
  area <- df %>%
    group_by(grande_area) %>%
    summarise(valor_total = sum(valor_R), .groups = "drop")
  
  total <- sum(df$valor_R)
  
  # ---- Gráficos ----
  plot_genero_raca <- ggplot(genero_raca, aes(raca, valor_total, fill = sexo)) +
    geom_col(position = "dodge") +
    labs(title = paste("Gênero x Raça -", nome),
         x = "Raça", y = "Valor total (R$)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_area <- ggplot(area, aes(reorder(grande_area, valor_total), valor_total)) +
    geom_col(fill = "#2E86AB") +
    coord_flip() +
    labs(title = paste("Fomento por Grande Área -", nome),
         x = "Grande Área", y = "Valor total (R$)") +
    theme_minimal()
  
  list(
    total = total,
    genero_raca = genero_raca,
    area = area,
    plot_genero_raca = plot_genero_raca,
    plot_area = plot_area
  )
}

# ------------------------------------------------------
# Carregar o arquivo uma única vez
# ------------------------------------------------------
dados <- read_delim(
  "Painel_Fomento(tab).csv",
  delim = "\t",
  locale = locale(encoding = "UTF-16")
)

colnames(dados) <- as.character(unlist(dados[1, ]))
dados <- dados[-1, ]

# ------------------------------------------------------
# Gerar UNICAMP e UFBA
# ------------------------------------------------------
unicamp <- processar(dados, "unicamp|estadual de campinas", "UNICAMP")
ufba    <- processar(dados, "ufba|federal da bahia", "UFBA")

# ------------------------------------------------------
# Imprimir gráficos como você pediu
# ------------------------------------------------------
print(unicamp$plot_genero_raca)
print(unicamp$plot_area)
print(ufba$plot_genero_raca)
print(ufba$plot_area)

# Totais opcionais
unicamp$total
ufba$total
