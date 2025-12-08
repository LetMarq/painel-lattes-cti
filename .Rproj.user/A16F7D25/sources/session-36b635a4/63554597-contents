# --- Pacotes necessários ---
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

# --- Ler o arquivo (usando UTF-16 porque o CSV do CNPq costuma usar esse encoding) ---
dados <- read_delim(
  "Painel_Fomento(tab).csv",
  delim = "\t",
  locale = locale(encoding = "UTF-16")
)

# --- Usar a primeira linha como cabeçalho ---
colnames(dados) <- as.character(unlist(dados[1, ]))
dados <- dados[-1, ]

# --- Renomear colunas principais ---
dados <- dados %>%
  rename(
    instituicao  = `01_Instituição`,
    programa     = `02_Programa`,
    modalidade   = `03_Modalidade`,
    area         = `05 _Área`,
    grande_area  = `06_Grande Área`,
    sexo         = `08_Sexo`,
    raca         = `09_Cor ou Raça`,
    uf           = `14_Sigla UF`,
    valor_R      = `Valor (R$)`
  )

# --- Limpar valores monetários ---
dados$valor_R <- dados$valor_R %>%
  str_replace_all("[^0-9,]", "") %>%
  str_replace(",", ".") %>%
  as.numeric()

# --- Remover valores vazios ---
dados <- dados %>% filter(!is.na(valor_R))

dados <- dados %>%
  filter(str_detect(tolower(instituicao), "ufba|universidade federal da bahia"))

# --- Total de fomento por sexo ---
fomento_genero <- dados %>%
  group_by(sexo) %>%
  summarise(valor_total = sum(valor_R, na.rm = TRUE)) %>%
  arrange(desc(valor_total))

print("Total de fomento por gênero:")
print(fomento_genero)

# --- Total de fomento por raça ---
fomento_raca <- dados %>%
  group_by(raca) %>%
  summarise(valor_total = sum(valor_R, na.rm = TRUE)) %>%
  arrange(desc(valor_total))

print("Total de fomento por raça:")
print(fomento_raca)

# --- Cruzamento gênero + raça ---
fomento_genero_raca <- dados %>%
  group_by(sexo, raca) %>%
  summarise(valor_total = sum(valor_R, na.rm = TRUE)) %>%
  arrange(desc(valor_total))

print("Total de fomento por gênero e raça:")
print(fomento_genero_raca)

# --- Fomento por grande área ---
fomento_area <- dados %>%
  group_by(grande_area) %>%
  summarise(valor_total = sum(valor_R, na.rm = TRUE)) %>%
  arrange(desc(valor_total))

print("Total de fomento por grande área:")
print(fomento_area)

# --- Visualização: fomento por gênero e raça ---
ggplot(fomento_genero_raca, aes(x = raca, y = valor_total, fill = sexo)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Feminino" = "#8E44AD", "Masculino" = "#16A085")) +
  theme_minimal() +
  labs(
    title = "Distribuição do fomento por gênero e raça",
    x = "Raça",
    y = "Valor total (R$)",
    fill = "Gênero"
  )

# --- Visualização: fomento por grande área ---
ggplot(fomento_area, aes(x = reorder(grande_area, valor_total), y = valor_total)) +
  geom_col(fill = "#2E86AB") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Fomento total por grande área do conhecimento",
    x = "Grande Área",
    y = "Valor total (R$)"
  )
