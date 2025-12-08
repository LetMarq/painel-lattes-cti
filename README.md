---

# 📊 Análise dos Painéis Lattes e CTI (CNPq)

Este repositório reúne scripts em **R** para processamento e análise dos dados públicos dos painéis da **Plataforma Lattes** e do **Centro de Tecnologia da Informação (CTI/CNPq)**.
O objetivo é fornecer análises reprodutíveis sobre formação acadêmica, atuação profissional, produção científica e indicadores de fomento com base no livro Data Feminism.

---

## 🧰 Tecnologias Utilizadas

* **R**

  * dplyr
  * janitor
  * readr
  * ggplot2
  * stringr
* Encoding compatível com UTF-16, comum nos arquivos do CNPq.

---

## 🚀 Como Executar

1. Instale as dependências:

```r
install.packages(c("dplyr", "janitor", "readr", "ggplot2", "stringr"))
```

2. Carregue e execute os scripts:

```r
source("code_file.R")
source("fomento.R")
source("m&d.R")
```

## 📚 Dados Utilizados

### 🔹 Painel Lattes — Formação e Atuação

Fonte: [https://bi.cnpq.br/painel/formacao-atuacao-lattes/](https://bi.cnpq.br/painel/formacao-atuacao-lattes/)

Inclui:

* Perfil acadêmico e profissional de mestres e doutores
* Áreas de atuação
* Distribuição por instituição
* Modalidade, sexo, raça/cor
* Dados temporais sobre formação e atuação

### 🔹 Painel CTI — Fomento

Fonte: [https://bi.cnpq.br/painel/fomento/](https://bi.cnpq.br/painel/fomento/)

Inclui:

* Recursos investidos
* Distribuição por área e região
* Programas de fomento
* Número de bolsistas

---

## 📈 Exemplos de Análises Realizadas

* Evolução da formação de mestres e doutores
* Percentual de participação por sexo e raça/cor
* Comparação entre modalidades (acadêmica/profissional)
* Volume de investimento ao longo do tempo
* Correlação entre número de bolsistas e investimentos

---

## ✨ Contribuições

Contribuições são bem-vindas!
Sugestões, melhorias e issues podem ser abertas diretamente neste repositório.

---
