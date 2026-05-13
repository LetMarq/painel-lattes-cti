# ========================================
# ANÁLISE DE FOMENTO CNPQ - UFBA
# ========================================
# Objetivo: Analisar fomento da UFBA
# segregado por gênero, raça e área de pesquisa
#
# Fonte de dados: Painel de Fomento (CNPQ)
# ========================================

# --- Carregar bibliotecas ---
library(dplyr)      # Manipulação de dados
library(ggplot2)    # Visualizações
library(readr)      # Leitura de arquivos
library(stringr)    # Manipulação de strings

# ========================================
# 1. CARREGAR E PREPARAR DADOS
# ========================================
# Importar arquivo do painel de fomento CNPQ
# Nota: Usa encoding UTF-16 (padrão CNPQ)

dados <- read_delim(
  "Painel_Fomento(tab).csv",
  delim = "\t",                          # Delimitador é tabulação
  locale = locale(encoding = "UTF-16")   # Encoding padrão CNPQ
)

# A primeira linha contém os nomes das colunas
colnames(dados) <- as.character(unlist(dados[1, ]))
dados <- dados[-1, ]  # Remover a primeira linha de dados

# Padronizar nomes das colunas para trabalho mais fácil
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

# ========================================
# 2. LIMPAR E CONVERTER VALORES MONETÁRIOS
# ========================================
# Transformar valores de texto para números
# Remove símbolos, substitui vírgula por ponto

dados$valor_R <- dados$valor_R %>%
  str_replace_all("[^0-9,]", "") %>%    # Remover símbolos
  str_replace(",", ".") %>%              # Converter formato brasileiro para inglês
  as.numeric()                           # Converter para número

cat("✓ Dados carregados\n")
cat("  Registros totais:", nrow(dados), "\n")
cat("  Registros válidos (com valor):", sum(!is.na(dados$valor_R)), "\n\n")

# ========================================
# 3. FILTRAR APENAS UFBA
# ========================================
# Identificar variações do nome da instituição

dados <- dados %>%
  filter(!is.na(valor_R))  # Manter apenas registros com valor monetário

dados_ufba <- dados %>%
  filter(str_detect(tolower(instituicao), "ufba|universidade federal da bahia"))

cat("Registros UFBA encontrados:", nrow(dados_ufba), "\n\n")

# ========================================
# 4. AGREGAÇÕES POR DIFERENTES PERSPECTIVAS
# ========================================

# Totais por gênero
fomento_genero <- dados_ufba %>%
  group_by(sexo) %>%
  summarise(valor_total = sum(valor_R, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(valor_total))

# Totais por raça
fomento_raca <- dados_ufba %>%
  group_by(raca) %>%
  summarise(valor_total = sum(valor_R, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(valor_total))

# Cruzamento gênero + raça
fomento_genero_raca <- dados_ufba %>%
  group_by(sexo, raca) %>%
  summarise(valor_total = sum(valor_R, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(valor_total))

# Totais por grande área (para entender onde vai o investimento)
fomento_area <- dados_ufba %>%
  group_by(grande_area) %>%
  summarise(valor_total = sum(valor_R, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(valor_total))

# ========================================
# 5. EXIBIR RESULTADOS TABULARES
# ========================================

cat("===== FOMENTO POR GÊNERO =====\n")
print(fomento_genero)

cat("\n===== FOMENTO POR RAÇA =====\n")
print(fomento_raca)

cat("\n===== FOMENTO POR GÊNERO E RAÇA =====\n")
print(fomento_genero_raca)

cat("\n===== FOMENTO POR GRANDE ÁREA =====\n")
print(fomento_area)

# ========================================
# 6. VISUALIZAÇÕES GRÁFICAS
# ========================================

# Gráfico 1: Fomento por gênero e raça (barras agrupadas)
# Permite comparar gêneros dentro de cada categoria racial
ggplot(fomento_genero_raca, aes(x = raca, y = valor_total, fill = sexo)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("Feminino" = "#8E44AD", "Masculino" = "#16A085"),
    na.value = "#95A5A6"  # Cor para valores faltantes
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      scale = 1e-6,
      suffix = "M"
    )  # Mostrar em milhões
  ) +
  labs(
    title = "Fomento CNPQ na UFBA por Gênero e Raça",
    x = "Raça",
    y = "Valor total (R$ Milhões)",
    fill = "Gênero"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Gráfico 2: Fomento por grande área (barras horizontais)
# Facilita leitura de nomes longos e vê a importância relativa
ggplot(fomento_area, aes(x = reorder(grande_area, valor_total), y = valor_total)) +
  geom_col(fill = "#2E86AB") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_number(
      scale = 1e-6,
      suffix = "M"
    )
  ) +
  labs(
    title = "Fomento CNPQ na UFBA por Grande Área",
    x = "Grande Área",
    y = "Valor total (R$ Milhões)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# ========================================
# 7. RESUMO FINAL
# ========================================

valor_total_ufba <- sum(dados_ufba$valor_R, na.rm = TRUE)

cat("\n========================================\n")
cat("RESUMO FINAL - FOMENTO UFBA\n")
cat("========================================\n\n")
cat("Valor total:", 
    paste("R$", format(valor_total_ufba, big.mark = ".", decimal.mark = ",")), "\n")
cat("Número de registros:", nrow(dados_ufba), "\n")
cat("Valor médio por registro:", 
    paste("R$", format(mean(dados_ufba$valor_R, na.rm = TRUE), big.mark = ".", decimal.mark = ",")), "\n")

