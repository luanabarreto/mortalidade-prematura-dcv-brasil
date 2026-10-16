# Mortalidade Prematura por Doenças Cardiovasculares no Brasil

Este repositório contém os códigos utilizados para análise da **mortalidade prematura por doenças cardiovasculares (DCV) no Brasil**, entre **2018 e 2025**, considerando óbitos de pessoas com **30 a 69 anos** registrados no Sistema de Informações sobre Mortalidade (SIM).

A análise contempla o perfil sociodemográfico dos óbitos, os principais grupos de doenças cardiovasculares e a presença de condições contribuintes registradas nas causas múltiplas de morte.

## Objetivo

Analisar a mortalidade prematura por doenças cardiovasculares no Brasil entre 2018 e 2025, considerando características sociodemográficas, grupos de causas básicas e condições contribuintes associadas aos óbitos.

## Fonte de dados

Os dados de mortalidade são provenientes do **Sistema de Informações sobre Mortalidade (SIM)**, disponibilizado pelo Ministério da Saúde do Brasil.

Foram selecionados:

- óbitos ocorridos entre 2018 e 2025;
- indivíduos com idade entre 30 e 69 anos;
- óbitos cuja causa básica pertence ao capítulo IX da CID-10 — **Doenças do aparelho circulatório (I00–I99)**.

## Variáveis analisadas

O perfil dos óbitos foi analisado segundo:

- sexo;
- faixa etária;
- raça/cor;
- escolaridade;
- região de residência;
- local de ocorrência;
- ano do óbito;
- grupo da causa básica.

A escolaridade foi classificada em **baixa (0–3 anos de estudo)**, **média (4–11 anos)** e **alta (12 anos ou mais)**.

As faixas etárias utilizadas foram **30–49**, **50–59** e **60–69 anos**.

## Grupos de doenças cardiovasculares

As causas básicas foram agrupadas em:

| Grupo | CID-10 |
|---|---|
| Doenças isquêmicas | I20–I25 |
| Doenças cerebrovasculares | I60–I69 |
| Doenças hipertensivas | I10–I15 |
| Outras doenças cardíacas | I30–I33 e I39–I52 |
| Outras doenças do aparelho circulatório | Demais códigos I00–I99 |

## Condições contribuintes

As causas múltiplas de morte foram utilizadas para identificar a presença das seguintes condições contribuintes:

| Condição | CID-10 |
|---|---|
| Hipertensão | I10–I13 e I15 |
| Diabetes mellitus | E10–E14 |
| Obesidade | E65–E68 |
| Alcoolismo | F10 |
| Tabagismo | F17 |
| Uso de drogas | F11–F16 e F18–F19 |

Para os óbitos cuja causa básica foi classificada como doença hipertensiva, a hipertensão não foi contabilizada novamente como condição contribuinte.

## Análises

O script realiza:

- preparação e tratamento dos registros do SIM;
- classificação das características sociodemográficas;
- agrupamento das causas básicas de morte;
- identificação das condições contribuintes nas causas múltiplas;
- análise descritiva dos óbitos;
- cálculo da frequência das condições contribuintes;
- regressão logística para presença de comorbidades;
- modelos de regressão logística específicos para cada condição contribuinte;
- modelo de Poisson para o número de comorbidades;
- cálculo de Odds Ratios (OR) e respectivos intervalos de confiança de 95%;
- geração de tabelas, gráficos e arquivos para exportação.

## Arquivo principal

`analise_mortalidade_prematura_dcv.R`

O script contém o fluxo de preparação dos dados, construção das variáveis, análises descritivas, modelos estatísticos e geração dos resultados.

## Principais pacotes R

Entre os principais pacotes utilizados estão:

`tidyverse`, `dplyr`, `ggplot2`, `broom`, `read.dbc`, `readxl`, `openxlsx`, `purrr`, `sf`, `geobr` e `biscale`.

## Reprodutibilidade

Os microdados utilizados nas análises não são armazenados neste repositório. O código foi desenvolvido para processamento dos arquivos do SIM em formato DBC.

Alguns caminhos locais presentes no script devem ser ajustados de acordo com o ambiente do usuário antes da execução.

## Autoria

**Luana Gonçalves Barreto**

## Licença

Este repositório é destinado a fins científicos e acadêmicos. Os dados utilizados são provenientes de fontes públicas oficiais do Ministério da Saúde do Brasil.
