rm(list = ls())
pacotes <- c(
  "read.dbc","lubridate","tidyverse","readxl","writexl","foreign",
  "dplyr","FactoMineR","factoextra","corrplot","amap","rgl","MASS",
  "ca","scales","rlang","janitor","rstatix","ggplot2","patchwork"
)

novos <- setdiff(pacotes, rownames(installed.packages()))
if(length(novos)) install.packages(novos, dependencies = TRUE)

suppressPackageStartupMessages(
  lapply(pacotes, require, character.only = TRUE)
)
# Caminhos possíveis
caminhos <- c(
  "D:/Luana Barreto/OneDrive/Documentos/Mestrado/Brasil/Dados",
  "C:/Users/luana.gbarreto/OneDrive/Documentos/Mestrado/Brasil/Dados"
)

# Identifica qual caminho existe neste computador
caminho_dados <- caminhos[dir.exists(caminhos)][1]

# Verifica se encontrou
if (is.na(caminho_dados)) {
  stop("A pasta de dados não foi encontrada neste computador.")
}

# Define o diretório de trabalho
setwd(caminho_dados)

# Confirma
cat("Diretório utilizado:", getwd(), "\n")

arquivo_dbc<-list.files(pattern = "\\.dbc$")


arquivo_dbc <- arquivo_dbc[ grepl("2018|2019|2020|2021|2022|2023|2024|2025", basename(arquivo_dbc))]
lista<-list()
arquivos<-NULL

for(i in 1:length(arquivo_dbc)){
  
  dados <- read.dbc(arquivo_dbc[i])
  lista[[i]]<-dados
  
  if(i==1){
    arquivos<-lista[[i]]
    
    
  }else{
    arquivos <- dplyr::bind_rows(arquivos, lista[[i]])
    
  }
  
  
}
dados<-arquivos

dados$ano<-substr(dados$DTOBITO, 5,8)
table(dados$ano)
nrow(dados)

table(dados$ano)

#2018    2019    2020    2021    2022    2023    2024    2025 
#1316719 1349801 1556824 1832649 1544266 1465610 1532015 1534588 
# nrow(dados)
# 12132472
arquivos<-dados
arquivos$IDADE <- as.numeric(as.character(arquivos$IDADE))

arquivos<-arquivos[,c("DTOBITO", "RACACOR", "ESTCIV", "ESC", "DTNASC","LOCOCOR" ,"IDADE","SEXO","CAUSABAS","CODMUNRES",  "LINHAA", "LINHAB", "LINHAC", "LINHAD", "LINHAII")]
arquivos<-arquivos%>%filter(IDADE>=430 & IDADE<470)
# nrow(arquivos) # primeiro filtro deu 4920452

arquivos<-arquivos%>%filter(str_detect(CAUSABAS, "I"))
# nrow(arquivos) # CARDIO filtro deu 1144325
arquivos$ANO<-substr(arquivos$DTOBITO, 5,8)
table(arquivos$ANO)

arquivos$cb<-substr(arquivos$CAUSABAS, 1,3)

arquivos <- arquivos %>%
  mutate(
    IDADE = as.numeric(as.character(IDADE))
  ) %>%
  select(
    DTOBITO, RACACOR, ESTCIV, ESC, DTNASC, IDADE, SEXO,
    LOCOCOR, CAUSABAS, CODMUNRES,
    LINHAA, LINHAB, LINHAC, LINHAD, LINHAII
  ) %>%
  filter(IDADE >= 430 & IDADE < 470) %>%
  filter(str_detect(CAUSABAS, "^I")) %>%
  mutate(
    ANO = substr(DTOBITO, 5, 8),
    cb = substr(CAUSABAS, 1, 3),
    
    IDADE_REAL = as.numeric(substr(as.character(IDADE), 2, 3)),
    
    faixa = case_when(
      IDADE_REAL >= 30 & IDADE_REAL <= 49 ~ "30-49",
      IDADE_REAL >= 50 & IDADE_REAL <= 59 ~ "50-59",
      IDADE_REAL >= 60 & IDADE_REAL <= 69 ~ "60-69",
      TRUE ~ NA_character_
    ),
    
    Sexo = case_when(
      SEXO == 1 ~ "Homens",
      SEXO == 2 ~ "Mulheres",
      TRUE ~ "Ignorado"
    ),
    
    RacaCor = case_when(
      RACACOR %in% c(2, 4, 5) ~ "PPI",
      RACACOR %in% c(1, 3) ~ "Não PPI",
      TRUE ~ "Ignorado"
    ),
    
    Escolaridade = case_when(
      ESC %in% c(1, 2) ~ "Baixa",
      ESC %in% c(3, 4) ~ "Média",
      ESC == 5 ~ "Alta",
      TRUE ~ "Ignorado"
    ),
    
    Local_ocorrencia = case_when(
      LOCOCOR %in% c(1, 2) ~ "Hospital/Unidade de saúde",
      LOCOCOR == 3 ~ "Domicílio",
      LOCOCOR %in% c(4, 5) ~ "Outro",
      TRUE ~ "Outro"
    ),
    
    uf_cod = as.numeric(substr(as.character(CODMUNRES), 1, 2)),
    
    Regiao = case_when(
      uf_cod %in% c(11, 12, 13, 14, 15, 16, 17) ~ "Norte",
      uf_cod %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29) ~ "Nordeste",
      uf_cod %in% c(31, 32, 33, 35) ~ "Sudeste",
      uf_cod %in% c(41, 42, 43) ~ "Sul",
      uf_cod %in% c(50, 51, 52, 53) ~ "Centro-Oeste",
      TRUE ~ "Ignorado"
    )
  )

arquivos$parte1<-toupper(paste0(arquivos$LINHAA, "*", arquivos$LINHAB, "*",arquivos$LINHAC, "*",arquivos$LINHAD))

arquivos<-arquivos[,c("ANO", "faixa","CODMUNRES","Regiao","Local_ocorrencia", "Escolaridade", "RacaCor", "Sexo","cb","parte1", "LINHAII")]





dic_subgrp<- read_excel("dicionario.xlsx")
icd_to_3d <- function(x){
  x <- toupper(trimws(as.character(x)))
  x <- gsub("[^A-Z0-9]", "", x)
  m <- regmatches(x, regexpr("^[A-Z][0-9]{2}", x))
  ifelse(nzchar(m), m, NA_character_)
}

seq_icd3 <- function(a, b){
  La <- substr(a,1,1); Na <- as.integer(substr(a,2,3))
  Lb <- substr(b,1,1); Nb <- as.integer(substr(b,2,3))
  out <- character()
  if(La == Lb){
    out <- sprintf("%s%02d", La, Na:Nb)
  } else {
    abc <- LETTERS[match(La, LETTERS):match(Lb, LETTERS)]
    for(L in abc){
      if(L == La) out <- c(out, sprintf("%s%02d", L, Na:99))
      else if(L == Lb) out <- c(out, sprintf("%s%02d", L, 0:Nb))
      else out <- c(out, sprintf("%s%02d", L, 0:99))
    }
  }
  out
}

expand_token <- function(tok){
  tok <- gsub("\\s+","", tok)
  if(grepl("^[A-Z][0-9]{2}-[A-Z][0-9]{2}$", tok)){
    p <- strsplit(tok, "-", fixed=TRUE)[[1]]
    return(seq_icd3(p[1], p[2]))
  } else if(grepl("^[A-Z][0-9]{2}$", tok)){
    return(tok)
  } else return(character(0))
}

expand_cid_field <- function(x){
  toks <- unlist(strsplit(x, ";", fixed=TRUE))
  unlist(lapply(trimws(toks), expand_token), use.names = FALSE)
}

# Função robusta: sempre retorna comprimento == length(x)
icd_to_3d <- function(x){
  x %>%
    as.character() %>%
    toupper() %>%
    str_replace_all("[^A-Z0-9]", "") %>%   # remove pontuação/espaços
    str_extract("^[A-Z][0-9]{2}")          # pega letra + 2 dígitos (ex.: B57)
}



# (re)gera o mapa expandido
dic3_map <- dic_subgrp %>%
  mutate(cid3 = map(CID, expand_cid_field)) %>%
  unnest(cid3) %>%
  distinct(cid3, .keep_all = TRUE) %>%
  dplyr::select(cid3, group_abb)


arquivos<- arquivos%>%
  mutate(CID3 = icd_to_3d(cb)) %>%
  left_join(dic3_map, by = c("CID3" = "cid3"))%>%filter(!is.na(group_abb))

names(arquivos)[names(arquivos) == "group_abb"] <- "group_abb_cb"
circulatorios<-arquivos

library(dplyr)

# Identifica as colunas categóricas (factor ou character)
vars_cat <- names(circulatorios)[sapply(circulatorios, function(x) is.factor(x) || is.character(x))]


# Opcional: dropar níveis não usados, se houver fatores
circulatorios[vars_cat] <- lapply(circulatorios[vars_cat], function(x) droplevels(factor(x)))



rm(list = setdiff(ls(), c("arquivos", "circulatorios","anos","dados","icd_to_3d", "dic3_map")))

arquivos<-circulatorios


criar_doencas_contribuintes <- function(df){
  
  lista_doencas <- list(
    Hipert = c("I10", "I11", "I12", "I13", "I15"),
    Diab   = c("E10", "E11", "E12", "E13", "E14"),
    Obes   = c("E65", "E66", "E67", "E68"),
    Alco   = c("F10"),
    Tab = c("F17"),
    Drog   = c("F11", "F12", "F13", "F14", "F15", "F16", "F18","F19")
  )
  
  nomes <- names(lista_doencas)
  
  for(nome in nomes){
    
    padrao <- paste(lista_doencas[[nome]], collapse = "|")
    
    p1 <- str_detect(coalesce(as.character(df$parte1), ""), padrao)
    p2 <- str_detect(coalesce(as.character(df$LINHAII), ""), padrao)
    
    df[[nome]] <- as.integer(p1 | p2)
    df[[paste0(nome, "_p1")]] <- as.integer(p1)
    df[[paste0(nome, "_p2")]] <- as.integer(p2)
  }
  
  df <- df %>%
    mutate(
      Hipert = if_else(group_abb_cb == "Hipert", 0L, Hipert),
      Hipert_p1 = if_else(group_abb_cb == "Hipert", 0L, Hipert_p1),
      Hipert_p2 = if_else(group_abb_cb == "Hipert", 0L, Hipert_p2),
      
      tem_comorbidade = as.integer(rowSums(across(all_of(nomes)), na.rm = TRUE) > 0),
      qtd_comorbidades = rowSums(across(all_of(nomes)), na.rm = TRUE)
    )
  
  cols_novas <- c(
    nomes,
    paste0(nomes, "_p1"),
    paste0(nomes, "_p2"),
    "tem_comorbidade",
    "qtd_comorbidades"
  )
  
  df %>%
    relocate(all_of(cols_novas), .after = last_col())
}



arquivos <- criar_doencas_contribuintes(arquivos)



arquivos <- arquivos %>%
  mutate(
    UF = substr(as.character(CODMUNRES), 1, 2)
  )
ufs <- c(
  "11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
  "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL","28"="SE","29"="BA",
  "31"="MG","32"="ES","33"="RJ","35"="SP",
  "41"="PR","42"="SC","43"="RS",
  "50"="MS","51"="MT","52"="GO","53"="DF"
)

arquivos <- arquivos %>%
  mutate(
    UF = ufs[substr(as.character(CODMUNRES), 1, 2)]
  )

arquivos %>%
  summarise(
    Hipert = sum(Hipert, na.rm = TRUE),
    Diab   = sum(Diab, na.rm = TRUE),
    Obes   = sum(Obes, na.rm = TRUE),
    Alco   = sum(Alco, na.rm = TRUE),
    Tab    = sum(Tab, na.rm = TRUE),
    Drog   = sum(Drog, na.rm = TRUE),
    Total  = n()
  ) %>%
  pivot_longer(
    cols = -Total,
    names_to = "comorbidade",
    values_to = "n"
  ) %>%
  mutate(
    perc = round(n / Total * 100, 2)
  )

nrow(arquivos)
base_teste <- arquivos %>% 
  select(any_of(c("ANO","faixa","Regiao","Escolaridade","RacaCor","Sexo", "causa","group_abb_cb",
                  "Hipert","Diab","Obes","Alco","Drog","Tab","tem_comorbidade","qtd_comorbidades")))%>%
  filter(    Sexo != "Ignorado",    RacaCor != "Ignorado",    Escolaridade != "Ignorado",    Regiao != "Ignorado",    !is.na(faixa)  )

vars <- c("ANO","faixa","Regiao","Escolaridade","RacaCor","Sexo","causa")

base_teste <- base_teste %>%
  mutate(across(any_of(c(vars, "group_abb_cb")), as.character))

library(dplyr)
library(tidyr)

vars <- c("ANO","faixa","Regiao","Escolaridade","RacaCor","Sexo", "group_abb_cb")
contrib <- c("Hipert","Diab","Obes","Alco","Tab","Drog")

resultado <- bind_rows(
  lapply(vars, \(v)
         base_teste %>%
           count(Variavel_Nome = v, Variavel = as.character(.data[[v]]), group_abb_cb, name = "n") %>%
           group_by(Variavel_Nome, Variavel) %>%
           mutate(pct = round(100*n/sum(n), 1)) %>%
           ungroup()
  ),
  lapply(contrib, \(v)
         base_teste %>%
           filter(.data[[v]] == 1) %>%
           count(Variavel_Nome = "Contribuintes", Variavel = v, group_abb_cb, name = "n") %>%
           mutate(pct = round(100*n/nrow(base_teste), 1))
  )
)

resultado_wide <- resultado %>%
  mutate(valor = paste0(n, " (", pct, "%)")) %>%
  select(Variavel_Nome, Variavel, group_abb_cb, valor) %>%
  pivot_wider(names_from = group_abb_cb, values_from = valor, values_fill = "0 (0%)")


perfil_vars <- c(  "Sexo",  "RacaCor","Escolaridade",  "faixa",  "ANO",  "Regiao",  "group_abb_cb")

contrib <- c(  "Hipert",  "Diab",  "Obes",  "Alco",  "Drog", "Tab")

perfil_comorb <- bind_rows(  lapply(perfil_vars, function(v){
    base_teste %>%      group_by(Categoria = .data[[v]]) %>%
      summarise(        N = n(),        across(          all_of(contrib),          ~ round(100 * mean(.x == 1, na.rm = TRUE), 1)
        ),        .groups = "drop"      ) %>%      mutate(Variavel = v)  }))

# write_xlsx(perfil_comorb, "perfil_comorb.xlsx")
# write_xlsx(resultado_wide, "perfil_basica.xlsx")



#### modelo logito #######################

base_logit <- base_teste %>%
  mutate(
    tem_comorbidade = factor(tem_comorbidade, levels = c(0,1)),
    Sexo = factor(Sexo),
    RacaCor = factor(RacaCor),
    Escolaridade = factor(Escolaridade),
    faixa = factor(faixa),
    ANO = factor(ANO),
    Regiao = factor(Regiao),
    group_abb_cb = factor(group_abb_cb),
  )

modelo <- glm(
  tem_comorbidade ~
    Sexo +
    RacaCor +
    Escolaridade +
    faixa +
    ANO +
    Regiao +
    group_abb_cb ,
  data = base_logit,
  family = binomial()
)

library(broom)

or <- broom::tidy(
  modelo,
  exponentiate = TRUE,
  conf.int = TRUE
)

or

### modelo DCV #############

modelo_dcv <- glm(
  tem_comorbidade ~
    Sexo +
    RacaCor +
    Escolaridade +
    faixa +
    Regiao +
    group_abb_cb,
  data = filter(base_logit,
                group_abb_cb %in% c("Isque","Cerebv","Hipert","OCorac","OCirc")),
  family = binomial()
)


or_dcv <- broom::tidy(
  modelo_dcv,
  exponentiate = TRUE,
  conf.int = TRUE
)

or_dcv

library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)

# PREPARAR RESULTADOS DO MODELO GERAL ####

or_grafico <- or %>%
  filter(term != "(Intercept)") %>%
  mutate(
    modelo = "Modelo geral",
    term = str_replace_all(term, "`", ""),
    term = str_replace(term, "Sexo", "Sexo: "),
    term = str_replace(term, "RacaCor", "Raça/cor: "),
    term = str_replace(term, "Escolaridade", "Escolaridade: "),
    term = str_replace(term, "faixa", "Faixa etária: "),
    term = str_replace(term, "ANO", "Ano: "),
    term = str_replace(term, "Regiao", "Região: "),
    term = str_replace(term, "group_abb_cb", "Grupo DCV: ")
  )

# GRÁFICO MODELO GERAL ####

ggplot(or_grafico, aes(x = estimate, y = fct_reorder(term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.2,
    color = "gray30"
  ) +
  geom_point(size = 2.5, color = "#1f78b4") +
  scale_x_log10() +
  labs(
    title = "Razões de chances para presença de comorbidade",
    subtitle = "Regressão logística ajustada — modelo geral",
    x = "Odds Ratio ajustado, escala logarítmica",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


## NUMERO DE COMORBIDADES ############






modelo_pois <- glm(
  qtd_comorbidades ~
    Sexo +
    RacaCor +
    Escolaridade +
    faixa +
    ANO +
    Regiao +
    group_abb_cb,
  data = base_teste,
  family = poisson()
)



or_pois <- broom::tidy(
  modelo_pois,
  exponentiate = TRUE,
  conf.int = TRUE
)

or_pois



library(dplyr)
library(openxlsx)

interpreta_or <- function(df, tipo){
  df %>%
    mutate(
      tipo_modelo = tipo,
      interpretacao = case_when(
        term == "(Intercept)" ~ "Intercepto do modelo; geralmente não é interpretado substantivamente.",
        estimate > 1 ~ paste0(
          "A categoria ", term, " apresenta ",
          round((estimate - 1) * 100, 1),
          "% maior chance/razão do desfecho em relação à categoria de referência."
        ),
        estimate < 1 ~ paste0(
          "A categoria ", term, " apresenta ",
          round((1 - estimate) * 100, 1),
          "% menor chance/razão do desfecho em relação à categoria de referência."
        ),
        estimate == 1 ~ paste0(
          "A categoria ", term, " não apresenta diferença em relação à categoria de referência."
        )
      )
    ) %>%
    mutate(
      estimate = round(estimate, 3),
      conf.low = round(conf.low, 3),
      conf.high = round(conf.high, 3),
      p.value = signif(p.value, 3)
    )
}

or_exp <- interpreta_or(
  or,
  "Regressão logística: desfecho binário, presença de comorbidade contribuinte."
)

or_pois_exp <- interpreta_or(
  or_pois,
  "Modelo de Poisson: desfecho de contagem, quantidade de comorbidades."
)

or_dcv_exp <- interpreta_or(
  or_dcv,
  "Regressão logística restrita às DCV: presença de comorbidade contribuinte entre causas cardiovasculares."
)

wb <- createWorkbook()

addWorksheet(wb, "OR_logistico")
writeData(wb, "OR_logistico", or_exp)

addWorksheet(wb, "OR_poisson")
writeData(wb, "OR_poisson", or_pois_exp)

addWorksheet(wb, "OR_DCV")
writeData(wb, "OR_DCV", or_dcv_exp)

addWorksheet(wb, "Descricao_modelos")
writeData(
  wb,
  "Descricao_modelos",
  data.frame(
    aba = c("OR_logistico", "OR_poisson", "OR_DCV"),
    interpretacao = c(
      "Modelo logístico geral. O estimate representa Odds Ratio: valores acima de 1 indicam maior chance de ter comorbidade; abaixo de 1 indicam menor chance.",
      "Modelo de Poisson. O estimate representa razão de taxas/contagens: valores acima de 1 indicam maior quantidade esperada de comorbidades; abaixo de 1 indicam menor quantidade.",
      "Modelo logístico apenas para causas cardiovasculares. O estimate representa Odds Ratio dentro do grupo de DCV."
    )
  )
)

saveWorkbook(
  wb,
  "resultados_modelos_comorbidades.xlsx",
  overwrite = TRUE
)
 

library(broom)
library(dplyr)
library(ggplot2)
library(stringr)

or_plot <- tidy(
  modelo,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(
      term,
      "SexoMulheres" = "Mulheres",
      "RacaCorNão PPI" = "Não PPI",
      "EscolaridadeBaixa" = "Baixa escolaridade",
      "EscolaridadeMédia" = "Média escolaridade",
      "faixa50-59" = "50-59 anos",
      "faixa60-69" = "60-69 anos",
      "ANO2020" = "Ano 2020",
      "ANO2021" = "Ano 2021",
      "ANO2022" = "Ano 2022",
      "ANO2023" = "Ano 2023",
      "ANO2024" = "Ano 2024",
      "ANO2025" = "Ano 2025",
      "RegiaoNordeste" = "Nordeste",
      "RegiaoNorte" = "Norte",
      "RegiaoSudeste" = "Sudeste",
      "RegiaoSul" = "Sul",
      "group_abb_cbHipert" = "Hipertensivas",
      "group_abb_cbIsque" = "Isquêmicas",
      "group_abb_cbOCirc" = "Outras circulatórias",
      "group_abb_cbOCorac" = "Outras cardíacas"
    )
  ) %>%
  arrange(estimate)

ggplot(
  or_plot,
  aes(
    x = estimate,
    y = reorder(term, estimate)
  )
) +
  geom_vline(
    xintercept = 1,
    linetype = 2,
    colour = "red"
  ) +
  geom_errorbarh(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    height = .2,
    colour = "#4E79A7"
  ) +
  geom_point(
    size = 3,
    colour = "#1F4E79"
  ) +
  labs(
    title = "Fatores associados à presença de comorbidades",
    subtitle = "Odds Ratios ajustadas (IC95%)",
    x = "Odds Ratio (OR)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )



library(dplyr)
library(purrr)
library(broom)
library(ggplot2)

comorbidades <- c("Hipert", "Diab", "Obes", "Alco", "Tab", "Drog")

resultados <- map_df(comorbidades, function(comp) {
  
  form <- as.formula(
    paste0(
      comp,
      " ~ Sexo + RacaCor + Escolaridade + faixa + ANO + Regiao + group_abb_cb"
    )
  )
  
  modelo <- glm(
    form,
    data = base_logit,
    family = binomial()
  )
  
  tidy(modelo) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      OR = exp(estimate),
      IC95_inf = exp(estimate - 1.96 * std.error),
      IC95_sup = exp(estimate + 1.96 * std.error),
      Comorbidade = comp
    ) %>%
    select(
      Comorbidade,
      term,
      OR,
      IC95_inf,
      IC95_sup,
      p.value
    )
})

# Renomeia as comorbidades
resultados$Comorbidade <- recode(
  resultados$Comorbidade,
  Hipert = "Hipertensão",
  Diab   = "Diabetes",
  Obes   = "Obesidade",
  Alco   = "Álcool",
  Tab    = "Tabagismo",
  Drog   = "Drogas"
)

# Renomeia os termos
resultados$term <- recode(
  resultados$term,
  "SexoMulheres" = "Mulheres",
  "RacaCorNão PPI" = "Não PPI",
  "EscolaridadeBaixa" = "Baixa escolaridade",
  "EscolaridadeMédia" = "Média escolaridade",
  "faixa50-59" = "50-59 anos",
  "faixa60-69" = "60-69 anos",
  "ANO2020" = "2020",
  "ANO2021" = "2021",
  "ANO2022" = "2022",
  "ANO2023" = "2023",
  "ANO2024" = "2024",
  "ANO2025" = "2025",
  "RegiaoNordeste" = "Nordeste",
  "RegiaoNorte" = "Norte",
  "RegiaoSudeste" = "Sudeste",
  "RegiaoSul" = "Sul",
  "group_abb_cbHipert" = "Hipertensivas",
  "group_abb_cbIsque" = "Isquêmicas",
  "group_abb_cbOCirc" = "Outras circulatórias",
  "group_abb_cbOCorac" = "Outras cardíacas"
)

ggplot(resultados,
       aes(x = estimate,
           y = reorder(term, estimate))) +
  geom_vline(xintercept = 1,
             linetype = 2,
             colour = "red") +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high),
                 height = .2,
                 colour = "#4E79A7") +
  geom_point(size = 2.8,
             colour = "#1F4E79") +
  facet_wrap(~Comorbidade, ncol = 3, scales = "free_y") +
  labs(
    x = "Odds Ratio (IC95%)",
    y = NULL,
    title = "Fatores associados a cada comorbidade contribuinte"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )




library(dplyr)
library(purrr)
library(broom)
library(ggplot2)
library(stringr)

# Variáveis dos modelos
comorbidades <- c("Hipert", "Diab", "Obes", "Alco", "Tab", "Drog")
preditores <- c("Sexo", "RacaCor", "Escolaridade", "faixa", "ANO", "Regiao", "group_abb_cb")

# Prepara a base e define explicitamente as categorias de referência
base_logit <- base_logit %>%
  mutate(
    across(all_of(comorbidades), ~ as.integer(as.character(.x))),
    Sexo = relevel(factor(Sexo), ref = "Homens"),
    RacaCor = relevel(factor(RacaCor), ref = "PPI"),
    Escolaridade = relevel(factor(Escolaridade), ref = "Alta"),
    faixa = relevel(factor(faixa), ref = "30-49"),
    ANO = relevel(factor(ANO), ref = "2018"),
    Regiao = relevel(factor(Regiao), ref = "Centro-Oeste"),
    group_abb_cb = factor(group_abb_cb)
  ) %>%
  filter(if_all(all_of(c(comorbidades, preditores)), ~ !is.na(.x)))

# Ajusta um modelo logístico para cada comorbidade
ajustes_logit <- map(comorbidades, function(comp) {
  
  modelo <- glm(
    reformulate(preditores, response = comp),
    data = base_logit,
    family = binomial(link = "logit"),
    control = glm.control(maxit = 100)
  )
  
  resultado <- broom::tidy(modelo) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Comorbidade = comp,
      OR = exp(estimate),
      IC_inf = exp(estimate - 1.96 * std.error),
      IC_sup = exp(estimate + 1.96 * std.error),
      convergiu = modelo$converged,
      iteracoes = modelo$iter
    )
  
  list(modelo = modelo, resultado = resultado)
})

# Guarda os modelos para consulta posterior
modelos_logit <- set_names(
  map(ajustes_logit, "modelo"),
  comorbidades
)

# Junta os resultados dos seis modelos
resultados_logit <- map_dfr(ajustes_logit, "resultado") %>%
  mutate(
    Comorbidade = recode(
      Comorbidade,
      Hipert = "Hipertensão",
      Diab = "Diabetes",
      Obes = "Obesidade",
      Alco = "Uso de álcool",
      Tab = "Tabagismo",
      Drog = "Uso de drogas"
    ),
    
    termo = case_when(
      term == "SexoMulheres" ~ "Mulheres",
      term == "RacaCorNão PPI" ~ "Não PPI",
      term == "EscolaridadeBaixa" ~ "Baixa escolaridade",
      term == "EscolaridadeMédia" ~ "Média escolaridade",
      term == "faixa50-59" ~ "50–59 anos",
      term == "faixa60-69" ~ "60–69 anos",
      str_detect(term, "^ANO") ~ str_remove(term, "^ANO"),
      str_detect(term, "^Regiao") ~ str_remove(term, "^Regiao"),
      str_detect(term, "^group_abb_cb") ~ str_remove(term, "^group_abb_cb"),
      TRUE ~ term
    ),
    
    significativo = if_else(
      IC_inf > 1 | IC_sup < 1,
      "Sim",
      "Não"
    )
  ) %>%
  filter(
    is.finite(OR),
    is.finite(IC_inf),
    is.finite(IC_sup),
    OR > 0,
    IC_inf > 0,
    IC_sup > 0
  )

# Mostra a convergência de cada modelo
resumo_modelos <- tibble(
  Comorbidade = names(modelos_logit),
  convergiu = map_lgl(modelos_logit, "converged"),
  iteracoes = map_int(modelos_logit, "iter"),
  AIC = map_dbl(modelos_logit, AIC)
)

print(resumo_modelos)

# Gráfico com os seis logitos lado a lado
grafico_logitos <- ggplot(
  resultados_logit,
  aes(
    x = OR,
    y = reorder(termo, OR)
  )
) +
  geom_vline(
    xintercept = 1,
    linetype = 2,
    linewidth = 0.6,
    colour = "red"
  ) +
  geom_errorbarh(
    aes(
      xmin = IC_inf,
      xmax = IC_sup,
      alpha = significativo
    ),
    height = 0.18,
    linewidth = 0.6
  ) +
  geom_point(
    aes(alpha = significativo),
    size = 2.4
  ) +
  facet_wrap(
    ~ Comorbidade,
    ncol = 3,
    scales = "free_y"
  ) +
  scale_x_log10() +
  scale_alpha_manual(
    values = c("Sim" = 1, "Não" = 0.4),
    guide = "none"
  ) +
  labs(
    title = "Fatores associados a cada comorbidade contribuinte",
    subtitle = "Modelos de regressão logística ajustados — Odds Ratios e IC95%",
    x = "Odds Ratio — escala logarítmica",
    y = NULL,
    caption = "Linha tracejada: OR = 1. Categorias de referência: homens, PPI, alta escolaridade, 30–49 anos, 2018 e Centro-Oeste."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    panel.spacing = grid::unit(1, "lines")
  )

print(grafico_logitos)

# Salva o gráfico
ggsave(
  filename = "logitos_comorbidades_lado_a_lado.png",
  plot = grafico_logitos,
  width = 16,
  height = 11,
  dpi = 300,
  bg = "white"
)




# ============================================================
# SALVAR RESULTADOS DOS MODELOS LOGÍSTICOS EM XLSX
# ============================================================

library(openxlsx)

# Tabela organizada para exportação
resultados_xlsx <- resultados_logit %>%
  transmute(
    Comorbidade,
    Variavel = termo,
    Coeficiente_logit = estimate,
    Erro_padrao = std.error,
    Estatistica_z = statistic,
    Valor_p = p.value,
    OR,
    IC95_inferior = IC_inf,
    IC95_superior = IC_sup,
    Significativo = significativo,
    Convergiu = convergiu,
    Iteracoes = iteracoes
  ) %>%
  mutate(
    across(
      c(
        Coeficiente_logit,
        Erro_padrao,
        Estatistica_z,
        Valor_p,
        OR,
        IC95_inferior,
        IC95_superior
      ),
      ~ round(.x, 4)
    )
  )

# Frequência das comorbidades
frequencia_comorbidades <- base_logit %>%
  summarise(
    across(
      all_of(comorbidades),
      list(
        Ausente = ~ sum(.x == 0, na.rm = TRUE),
        Presente = ~ sum(.x == 1, na.rm = TRUE),
        Percentual = ~ mean(.x == 1, na.rm = TRUE) * 100
      )
    )
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("Comorbidade", ".value"),
    names_pattern = "(.+)_(Ausente|Presente|Percentual)"
  ) %>%
  mutate(
    Comorbidade = recode(
      Comorbidade,
      Hipert = "Hipertensão",
      Diab = "Diabetes",
      Obes = "Obesidade",
      Alco = "Uso de álcool",
      Tab = "Tabagismo",
      Drog = "Uso de drogas"
    ),
    Percentual = round(Percentual, 2)
  )

# Criação do arquivo
wb <- createWorkbook()

# Estilo do cabeçalho
estilo_cabecalho <- createStyle(
  fontColour = "white",
  fgFill = "#1F4E79",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom"
)

# Estilo para valores significativos
estilo_significativo <- createStyle(
  fgFill = "#D9EAD3"
)

# Aba com todos os resultados
addWorksheet(wb, "Todos_resultados")

writeData(
  wb,
  sheet = "Todos_resultados",
  x = resultados_xlsx,
  headerStyle = estilo_cabecalho,
  withFilter = TRUE
)

freezePane(
  wb,
  sheet = "Todos_resultados",
  firstRow = TRUE
)

setColWidths(
  wb,
  sheet = "Todos_resultados",
  cols = 1:ncol(resultados_xlsx),
  widths = "auto"
)

# Destaca associações significativas
linhas_significativas <- which(
  resultados_xlsx$Significativo == "Sim"
) + 1

if (length(linhas_significativas) > 0) {
  addStyle(
    wb,
    sheet = "Todos_resultados",
    style = estilo_significativo,
    rows = linhas_significativas,
    cols = 1:ncol(resultados_xlsx),
    gridExpand = TRUE,
    stack = TRUE
  )
}

# Aba com resumo dos modelos
addWorksheet(wb, "Resumo_modelos")

writeData(
  wb,
  sheet = "Resumo_modelos",
  x = resumo_modelos,
  headerStyle = estilo_cabecalho,
  withFilter = TRUE
)

freezePane(
  wb,
  sheet = "Resumo_modelos",
  firstRow = TRUE
)

setColWidths(
  wb,
  sheet = "Resumo_modelos",
  cols = 1:ncol(resumo_modelos),
  widths = "auto"
)

# Aba com frequência das comorbidades
addWorksheet(wb, "Frequencias")

writeData(
  wb,
  sheet = "Frequencias",
  x = frequencia_comorbidades,
  headerStyle = estilo_cabecalho,
  withFilter = TRUE
)

freezePane(
  wb,
  sheet = "Frequencias",
  firstRow = TRUE
)

setColWidths(
  wb,
  sheet = "Frequencias",
  cols = 1:ncol(frequencia_comorbidades),
  widths = "auto"
)

# Uma aba separada para cada comorbidade
nomes_abas <- c(
  "Hipertensão" = "Hipertensao",
  "Diabetes" = "Diabetes",
  "Obesidade" = "Obesidade",
  "Uso de álcool" = "Alcool",
  "Tabagismo" = "Tabagismo",
  "Uso de drogas" = "Drogas"
)

for (comp in names(nomes_abas)) {
  
  nome_aba <- nomes_abas[[comp]]
  
  tabela_comp <- resultados_xlsx %>%
    filter(Comorbidade == comp) %>%
    select(-Comorbidade)
  
  addWorksheet(wb, nome_aba)
  
  writeData(
    wb,
    sheet = nome_aba,
    x = tabela_comp,
    headerStyle = estilo_cabecalho,
    withFilter = TRUE
  )
  
  freezePane(
    wb,
    sheet = nome_aba,
    firstRow = TRUE
  )
  
  setColWidths(
    wb,
    sheet = nome_aba,
    cols = 1:ncol(tabela_comp),
    widths = "auto"
  )
  
  linhas_comp_significativas <- which(
    tabela_comp$Significativo == "Sim"
  ) + 1
  
  if (length(linhas_comp_significativas) > 0) {
    addStyle(
      wb,
      sheet = nome_aba,
      style = estilo_significativo,
      rows = linhas_comp_significativas,
      cols = 1:ncol(tabela_comp),
      gridExpand = TRUE,
      stack = TRUE
    )
  }
}

# Insere o gráfico no arquivo Excel
arquivo_grafico <- "logitos_comorbidades_lado_a_lado.png"

ggsave(
  filename = arquivo_grafico,
  plot = grafico_logitos,
  width = 16,
  height = 11,
  dpi = 300,
  bg = "white"
)

addWorksheet(wb, "Grafico")

insertImage(
  wb,
  sheet = "Grafico",
  file = arquivo_grafico,
  startRow = 2,
  startCol = 2,
  width = 16,
  height = 11,
  units = "in"
)

# Salva o arquivo Excel
saveWorkbook(
  wb,
  file = "Resultados_Logitos_Comorbidades.xlsx",
  overwrite = TRUE
)

message(
  "Arquivo salvo em: ",
  normalizePath("Resultados_Logitos_Comorbidades.xlsx")
)


# 

arrow::write_parquet(arquivos, "obitos_dcv_prematura_completo.xlsx")


# ============================================================
# MAPA BIVARIADO - % DE ÓBITOS MASCULINOS E FEMININOS POR UF
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(geobr)
library(biscale)

# ------------------------------------------------------------
# 1. Calcula quantidade e percentual por sexo em cada UF
# ------------------------------------------------------------

mortes_sexo_uf <- arquivos %>%
  filter(
    !is.na(UF),
    Sexo %in% c("Homens", "Mulheres")
  ) %>%
  count(UF, Sexo, name = "n") %>%
  group_by(UF) %>%
  mutate(
    total_uf = sum(n),
    pct = 100 * n / total_uf
  ) %>%
  ungroup() %>%
  select(UF, Sexo, n, pct) %>%
  pivot_wider(
    names_from = Sexo,
    values_from = c(n, pct)
  ) %>%
  rename(
    mortes_homens   = n_Homens,
    mortes_mulheres = n_Mulheres,
    pct_homens      = pct_Homens,
    pct_mulheres    = pct_Mulheres
  )

# Conferir
print(mortes_sexo_uf)

# ------------------------------------------------------------
# 2. Malha do Brasil por UF
# ------------------------------------------------------------

mapa_uf <- geobr::read_state(
  year = 2020,
  simplified = TRUE,
  showProgress = FALSE
)

# Junta os dados
mapa_sexo <- mapa_uf %>%
  left_join(
    mortes_sexo_uf,
    by = c("abbrev_state" = "UF")
  )


# ------------------------------------------------------------
# 3. Classificação bivariada
# ------------------------------------------------------------

mapa_sexo <- bi_class(
  mapa_sexo,
  x = pct_homens,
  y = pct_mulheres,
  style = "quantile",
  dim = 3
)

# ------------------------------------------------------------
# 4. Mapa bivariado
# ------------------------------------------------------------

mapa <- ggplot() +
  
  geom_sf(
    data = mapa_sexo,
    aes(fill = bi_class),
    color = "white",
    linewidth = 0.25,
    show.legend = FALSE
  ) +
  
  bi_scale_fill(
    pal = "GrPink",
    dim = 3
  ) +
  
  labs(
    title = "Distribuição percentual dos óbitos por sexo no Brasil",
    subtitle = "Percentual de óbitos masculinos e femininos por Unidade Federativa",
    caption = "SIM, 2018–2025 | Óbitos prematuros por doenças cardiovasculares, 30–69 anos"
  ) +
  
  bi_theme() +
  
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    plot.caption = element_text(
      size = 9,
      hjust = 0
    )
  )

mapa

# ------------------------------------------------------------
# 5. Legenda bivariada
# ------------------------------------------------------------

legenda <- bi_legend(
  pal = "GrPink",
  dim = 3,
  xlab = "Maior % de homens",
  ylab = "Maior % de mulheres",
  size = 8
)

legenda

library(cowplot)

mapa_final <- ggdraw() +
  draw_plot(
    mapa,
    0, 0, 1, 1
  ) +
  draw_plot(
    legenda,
    0.72, 0.08,
    0.22, 0.22
  )

mapa_final



