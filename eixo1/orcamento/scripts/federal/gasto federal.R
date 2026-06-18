#===============================================================================
# SCRIPT: EIXO 1 - OR?AMENTO (GASTO FEDERAL INTEGRADO)
#===============================================================================
library(tidyverse)
library(readxl)
library(scales)
library(janitor)
library(rbcb)

# 0. CONFIGURAÇÕES E MARCADORES ------------------------------------------------
options(scipen = 999) 
setwd("E:\\Pedro Buril\\Ministerio da Cultura\\Cultura em Numeros\\Eixo 1\\Orcamento\\SIOP")

tema_minC <- theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 10))

df_incumbentes <- data.frame(
  ano = c(2003, 2007, 2011, 2015, 2016, 2019, 2023),
  presidente = c("Lula I", "Lula II", "Dilma I", "Dilma II", "Temer", "Bolsonaro", "Lula III"))

# 1. CARREGAMENTO DAS BASES ----------------------------------------------------

# 1.1 SALIC (Incentivo Fiscal) - Teto
salic <- read_xlsx("salic_minc.xlsx") %>%
  clean_names() %>%
  mutate(across(c(vl_captado, vl_teto, vl_renunciado), as.numeric)) %>%
  group_by(ano = as.numeric(ano)) %>%
  summarise(teto       = sum(vl_teto, na.rm = TRUE),
            captado    = sum(vl_captado, na.rm = TRUE),
            renunciado = sum(vl_renunciado, na.rm = TRUE),
            .groups = "drop")

# 1.2 RCL (Receita Corrente L?quida)
rcl <- read_xlsx("RCL_2011_2025_Uniao_Resumido.xlsx") %>%
  clean_names() %>%
  transmute(ano = as.numeric(ano), rcl = as.numeric(rcl))

# 1.3 SIOP (Gasto Direto)
id_minc       <- "42000" 
ids_transicao <- c("54000", "55000") 
ids_fundos    <- c("73120", "74912", "73117") 

dados_siop <- read_xlsx("funcoes_orgaos_unidades_rp_20260402_v2.xlsx") %>% 
  clean_names() %>%
  mutate(across(c(ano, empenhado, liquidado), as.numeric)) %>%
  filter(ano != 2026)

anos_com_minc <- dados_siop %>%
  filter(str_sub(orgao_orcamentario, 1, 5) == id_minc) %>%
  pull(ano) %>% unique()

# 1.4 ANCINE (Ren?ncia fiscal)
ancine_raw <- read_xlsx("ANCINE - 2005 a 2026.xlsx") %>% 
  clean_names()

colunas_renuncia <- c("art_1a", "art_3a", "art1", "art18_lei_8_313_91_rouanet", 
  "art25_lei_8_313_91_rouanet", "art3", "art39_condecine", "art41_funcines")

ancine_anual <- ancine_raw %>%
  filter(!is.na(as.numeric(ano))) %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(across(all_of(colunas_renuncia), ~ as.numeric(as.character(replace_na(., 0))))) %>%
  mutate(ancine_captado = rowSums(select(., all_of(colunas_renuncia)), na.rm = TRUE)) %>%
  select(ano, ancine_captado)

# 2. PROCESSAMENTO E CONSOLIDACAO ----------------------------------------------
df_siop_consolidado <- dados_siop %>%
  filter(
    # FASE 1: 2019 (Regra H?brida) - MinC + Desenv. Social (F13/F28 s/ FNAS) + Fundos
    (ano == 2019 & (str_sub(orgao_orcamentario, 1, 5) == id_minc | 
        (str_sub(orgao_orcamentario, 1, 5) == "55000" & (str_detect(funcao, "13") | 
        (str_detect(funcao, "28") & str_sub(unidade_orcamentaria, 1, 5) != "55901"))) |
        str_sub(unidade_orcamentaria, 1, 5) %in% ids_fundos)) |
      # FASE 2: Anos com MinC Ativo (Captura o ?rg?o 42000 Integralmente + Fundos)
      (ano != 2019 & ano %in% anos_com_minc & (str_sub(orgao_orcamentario, 1, 5) == id_minc | 
          str_sub(unidade_orcamentaria, 1, 5) %in% ids_fundos)) |
      # FASE 3: Anos Sem MinC - Turismo/Desenv. Social (F13/F28 s/ FNAS) + Fundos
      (!(ano %in% anos_com_minc) & ano != 2019 & ((str_sub(orgao_orcamentario, 1, 5) %in% 
      ids_transicao & (str_detect(funcao, "13") | (str_detect(funcao, "28") & str_sub(unidade_orcamentaria, 1, 5) != "55901"))) | 
          str_sub(unidade_orcamentaria, 1, 5) %in% ids_fundos))) %>%
  group_by(ano, orgao_orcamentario, unidade_orcamentaria, resultado_primario) %>%
  summarise(valor_empenhado = sum(empenhado, na.rm = TRUE), .groups = "drop") %>%
  group_by(ano) %>%
  summarise(siop_direto = sum(valor_empenhado), .groups = "drop")


# 2.2 Uni?o Final para Gasto Pleno
# NOTA T?CNICA: Somamos Or?amento Direto (Tesouro) + Incentivo Rouanet (Teto) 
# + Incentivo ANCINE (Efetivo Captado). Isso ? o Gasto Federal Pleno.
df_final <- df_siop_consolidado %>%
  left_join(salic, by = "ano") %>%
  left_join(ancine_anual, by = "ano") %>%
  left_join(rcl, by = "ano") %>%
  mutate(total_incentivo = replace_na(teto, 0) + replace_na(ancine_captado, 0),
    total_pleno = siop_direto + total_incentivo,
    perc_rcl = (total_pleno / rcl) * 100)

#-------------------------------------------------------------------------------
# SEQU?NCIA DE GR?FICOS PARA APRESENTA??O
#-------------------------------------------------------------------------------
df_grafico_origem <- dados_siop %>%
  filter(
    !is.na(unidade_orcamentaria),
    # FASE 1: 2019 (MDS e MinC) - Exclui FNAS (55901) na Fun??o 28
    ((ano == 2019 & (str_sub(orgao_orcamentario, 1, 5) == id_minc | 
                       (str_sub(orgao_orcamentario, 1, 5) == "55000" & (str_detect(funcao, "13") | 
                                                                          (str_detect(funcao, "28") & str_sub(unidade_orcamentaria, 1, 5) != "55901"))) |
                       str_sub(unidade_orcamentaria, 1, 5) %in% ids_fundos)) |
       # FASE 2: Anos com MinC Ativo (?rg?o 42000 + Fundos)
       (ano != 2019 & ano %in% anos_com_minc & (str_sub(orgao_orcamentario, 1, 5) == id_minc | 
                                                  str_sub(unidade_orcamentaria, 1, 5) %in% ids_fundos)) |
       # FASE 3: Anos de Transi??o - Exclui FNAS (55901) na Fun??o 28
       (!(ano %in% anos_com_minc) & ano != 2019 & ((str_sub(orgao_orcamentario, 1, 5) %in% ids_transicao & 
                                                      (str_detect(funcao, "13") | (str_detect(funcao, "28") & str_sub(unidade_orcamentaria, 1, 5) != "55901"))) | 
                                                     str_sub(unidade_orcamentaria, 1, 5) %in% ids_fundos)))) %>%
  mutate(categoria_origem = case_when(
    str_sub(unidade_orcamentaria, 1, 5) == "73120" ~ "PNAB (UO 73120)",
    str_sub(unidade_orcamentaria, 1, 5) == "74912" ~ "FSA (UO 74912)",
    str_sub(unidade_orcamentaria, 1, 5) == "73117" & ano >= 2022 ~ "Lei Paulo Gustavo",
    str_sub(unidade_orcamentaria, 1, 5) == "73117" & ano < 2022 ~ "Lei Aldir Blanc 1",
    str_sub(orgao_orcamentario, 1, 5) == "42000" ~ "Ministério da Cultura (órgão 42000)",
    TRUE ~ "Outros órgãos (Cidadania/Turismo)")) %>% 
  filter(!is.na(categoria_origem)) %>%
  group_by(ano, categoria_origem) %>%
  summarise(valor_total = sum(empenhado, na.rm = TRUE), .groups = "drop")

# I. GASTO FEDERAL DIRETO
ggplot(df_grafico_origem, aes(x = factor(ano), y = valor_total, fill = categoria_origem)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(valor_total > 150000000, 
                               label_number(scale = 1e-9, suffix = "b", accuracy = 0.1, decimal.mark = ",")(valor_total), "")), 
            position = position_stack(vjust = 0.5), size = 2.5, fontface = "bold", color = "white") +
  stat_summary(fun = sum, aes(label = label_number(scale = 1e-9, suffix = " bi", accuracy = 0.1, decimal.mark = ",")(..y..), group = ano), 
               geom = "text", vjust = -0.7, size = 3, fontface = "bold") +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = " bi"), expand = expansion(mult = c(0, 0.4))) +
  scale_fill_manual(values = c(
    "PNAB (UO 73120)" = "#f1c40f",
    "FSA (UO 74912)" = "#27ae60",
    "Lei Aldir Blanc 1" = "#e67e22",
    "Lei Paulo Gustavo" = "#78281f",
    "Ministério da Cultura (órgão 42000)" = "#2980b9",
    "Outros órgãos (Cidadania/Turismo)" = "#95a5a6"),
    na.translate = FALSE) + 
  labs(title = "Evolu??o do Gasto Federal Direto em Cultura",
       x = "Ano", y = "R$ Bilh?es", fill = "Origem do Recurso",
       caption = "Fonte: SIOP. LPG (2022) e LAB 1 (2020-21) diferenciadas pela UO 73117.") +
  tema_minC + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# II. AN?LISE DE RIGIDEZ OR?AMENT?RIA - DETALHADO
df_rp_fontes <- dados_siop %>%
  filter(!is.na(unidade_orcamentaria),
    ((ano == 2019 & (str_sub(orgao_orcamentario, 1, 5) == id_minc | 
                       (str_sub(orgao_orcamentario, 1, 5) == "55000" & (str_detect(funcao, "13") | 
                                                                          (str_detect(funcao, "28") & str_sub(unidade_orcamentaria, 1, 5) != "55901"))) |
                       str_sub(unidade_orcamentaria, 1, 5) %in% ids_fundos)) |
       (ano != 2019 & ano %in% anos_com_minc & (str_sub(orgao_orcamentario, 1, 5) == id_minc | 
                                                  str_sub(unidade_orcamentaria, 1, 5) %in% ids_fundos)) |
       (!(ano %in% anos_com_minc) & ano != 2019 & ((str_sub(orgao_orcamentario, 1, 5) %in% ids_transicao & 
                                                      (str_detect(funcao, "13") | (str_detect(funcao, "28") & str_sub(unidade_orcamentaria, 1, 5) != "55901"))) | 
                                                     str_sub(unidade_orcamentaria, 1, 5) %in% ids_fundos)))) %>%
  mutate(categoria_origem = case_when(
      str_sub(unidade_orcamentaria, 1, 5) == "73120" ~ "PNAB (UO 73120)",
      str_sub(unidade_orcamentaria, 1, 5) == "74912" ~ "FSA (UO 74912)",
      str_sub(unidade_orcamentaria, 1, 5) == "73117" & ano >= 2022 ~ "Lei Paulo Gustavo",
      str_sub(unidade_orcamentaria, 1, 5) == "73117" & ano < 2022 ~ "Lei Aldir Blanc 1",
      str_sub(orgao_orcamentario, 1, 5) == "42000" ~ "Ministério da Cultura (órgão 42000)",
      TRUE ~ "Outros órgãos (Cidadania/Turismo)"),
    categoria_rp = case_when(str_detect(resultado_primario, "^0") ~ "0 - Financeiro",
      str_detect(resultado_primario, "^1") ~ "1 - RP obrigat?rio",
      str_detect(resultado_primario, "^2") ~ "2 - RP discricion?rio",
      str_detect(resultado_primario, "^3") ~ "3 - RP discricion?rio (PAC)",
      str_detect(resultado_primario, "^6") ~ "6 - RP discricion?rio (emenda individual)",
      str_detect(resultado_primario, "^7") ~ "7 - RP discricion?rio (emenda de bancada)",
      str_detect(resultado_primario, "^8") ~ "8 - RP discricion?rio (comiss?o)",
      str_detect(resultado_primario, "^9") ~ "9 - RP discricion?rio (emenda do relator)",
      TRUE ~ "Outros")) %>%
  mutate(categoria_rp = factor(categoria_rp, levels = c("Outros",
    "0 - Financeiro", 
    "1 - RP obrigat?rio", 
    "2 - RP discricion?rio", 
    "3 - RP discricion?rio (PAC)", 
    "6 - RP discricion?rio (emenda individual)", 
    "7 - RP discricion?rio (emenda de bancada)", 
    "8 - RP discricion?rio (comiss?o)", 
    "9 - RP discricion?rio (emenda do relator)"))) %>%
  filter(!is.na(categoria_origem)) %>%
  group_by(ano, categoria_origem, categoria_rp) %>%
  summarise(valor_empenhado = sum(empenhado, na.rm = TRUE), .groups = "drop")

df_rp_grafico <- df_rp_fontes %>%
  filter(valor_empenhado > 0) %>%
  filter(!(categoria_origem == "Minist?rio da Cultura (?rg?o 42000)" & ano %in% 2019:2022),
    !(categoria_origem == "FSA (UO 74912)" & ano == 2014)) %>%
  group_by(ano, categoria_origem) %>%
  mutate(pct_rp = valor_empenhado / sum(valor_empenhado, na.rm = TRUE)) %>%
  ungroup()

ggplot(df_rp_grafico, aes(x = as.character(ano), y = valor_empenhado, fill = categoria_rp)) +
  geom_col(position = "fill", width = 0.85, alpha = 0.95) +
  geom_text(aes(label = ifelse(pct_rp > 0.05, percent(pct_rp, accuracy = 1, decimal.mark = ","), "")), 
            position = position_fill(vjust = 0.5), 
            size = 2.6, fontface = "bold", color = "white") +
  facet_wrap(~ categoria_origem, scales = "free_x", ncol = 3) +
  scale_y_continuous(labels = percent_format(decimal.mark = ","), expand = c(0, 0)) +
  scale_fill_manual(values = c("0 - Financeiro" = "#bdc3c7",                         
    "1 - RP obrigat?rio" = "#2c3e50",                     
    "2 - RP discricion?rio" = "#2980b9",                  
    "3 - RP discricion?rio (PAC)" = "#1abc9c",            
    "6 - RP discricion?rio (emenda individual)" = "#f1c40f", 
    "7 - RP discricion?rio (emenda de bancada)" = "#e67e22", 
    "8 - RP discricion?rio (comiss?o)" = "#8e44ad",          
    "9 - RP discricion?rio (emenda do relator)" = "#c0392b", 
    "Outros" = "#7f8c8d")) +
  labs(title = "Composi??o do Resultado Prim?rio (RP) por Fonte de Recurso",
       subtitle = "O peso das Despesas Obrigat?rias (1), Discricion?rias do Executivo (2 e 3) e Emendas Parlamentares (6 a 9)",
       x = "Ano de Execu??o", y = "Propor??o do Empenho (%)", fill = "Marcador de RP") +
  tema_minC +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5),
    strip.text = element_text(face = "bold", size = 9, color = "#1b2631"),
    strip.background = element_rect(fill = "#f2f4f4", color = "white"),
    legend.position = "right",
    legend.text = element_text(size = 8.5),
    legend.title = element_text(face = "bold", size = 9),
    panel.spacing.x = unit(1.5, "lines"),
    panel.spacing.y = unit(1, "lines"))

# II. AN?LISE DE RIGIDEZ OR?AMENT?RIA - GERAL
df_rp_geral <- df_rp_fontes %>%
  filter(valor_empenhado > 0) %>%
  filter(!(categoria_origem == "Ministério da Cultura (órgão 42000)" & ano %in% 2019:2022),
    !(categoria_origem == "FSA (UO 74912)" & ano == 2014)) %>%
  group_by(ano, categoria_rp) %>%
  summarise(valor_empenhado = sum(valor_empenhado, na.rm = TRUE), .groups = "drop")

df_rp_geral_grafico <- df_rp_geral %>%
  group_by(ano) %>%
  mutate(pct_rp = valor_empenhado / sum(valor_empenhado, na.rm = TRUE)) %>%
  ungroup()

ggplot(df_rp_geral_grafico, aes(x = factor(ano), y = valor_empenhado, fill = categoria_rp)) +
  geom_col(position = "fill", width = 0.85, alpha = 0.95) +
  geom_text(aes(label = ifelse(pct_rp > 0.04, percent(pct_rp, accuracy = 1, decimal.mark = ","), "")), 
            position = position_fill(vjust = 0.5), 
            size = 3.2, fontface = "bold", color = "white") +
  scale_y_continuous(labels = percent_format(decimal.mark = ","), expand = c(0, 0)) +
  scale_fill_manual(values = c(
    "0 - Financeiro" = "#bdc3c7",                         
    "1 - RP obrigat?rio" = "#2c3e50",                     
    "2 - RP discricion?rio" = "#2980b9",                  
    "3 - RP discricion?rio (PAC)" = "#1abc9c",            
    "6 - RP discricion?rio (emenda individual)" = "#f1c40f", 
    "7 - RP discricion?rio (emenda de bancada)" = "#e67e22", 
    "8 - RP discricion?rio (comiss?o)" = "#8e44ad",          
    "9 - RP discricion?rio (emenda do relator)" = "#c0392b", 
    "Outros" = "#7f8c8d")) +
  labs(title = "Evolu??o da Rigidez do Gasto Direto Federal em Cultura (2003-2025)",
       subtitle = "Distribui??o percentual consolidada de todos os ?rg?os e fundos por Resultado Prim?rio",
       x = "Ano de Execu??o", y = "Propor??o do Empenho (%)", fill = "Marcador de RP",
       caption = "Fonte: SIOP. Base de dados abrange MinC, transi??es institucionais, PNAB e FSA.") +
  tema_minC +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
    legend.position = "right",
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10))

# III. Gasto Indireto
df_grafico_indireto <- df_final %>%
  filter(ano >= 2006 & ano <= 2025) %>%
  select(ano, `Lei Rouanet` = teto, 
         `Incentivo (ANCINE)` = ancine_captado) %>%
  pivot_longer(cols = -ano, names_to = "origem", values_to = "valor") %>%
  mutate(origem = factor(origem, levels = c("Lei Rouanet", "Incentivo (ANCINE)")))

ggplot(df_grafico_indireto, aes(x = factor(ano), y = valor, fill = origem)) +
  geom_col(position = "stack", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(valor > 100000000, 
                               label_number(scale = 1e-9, suffix = "b", accuracy = 0.1, decimal.mark = ",")(valor), "")), 
            position = position_stack(vjust = 0.5), 
            size = 2.5, fontface = "bold", color = "white") +
  stat_summary(fun = sum, aes(label = label_number(scale = 1e-9, suffix = " bi", accuracy = 0.1, decimal.mark = ",")(..y..), group = ano), 
               geom = "text", vjust = -0.7, size = 3.5, fontface = "bold", color = "black") +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = " bi"), 
                     expand = expansion(mult = c(0, 0.3))) +
  scale_fill_manual(values = c("Lei Rouanet" = "#1b2631",
                               "Incentivo (ANCINE)" = "#8e44ad")) +
  labs(title = "Evolu??o do Gasto Federal Indireto em Cultura",
       subtitle = "S?rie Hist?rica: Incentivos Fiscais via Lei Rouanet (Teto) e Setor Audiovisual (Capta??o)",
       x = "Ano", 
       y = "R$ Bilh?es", 
       fill = "Origem do Recurso",
       caption = "Fonte: SALIC e ANCINE. Valores nominais.") +
  tema_minC + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.justification = "left")

# IV. GASTO TOTAL PLENO
#SALIC + ANCINE + SIOP
df_detalhado_reais <- df_grafico_origem %>%
  rename(fonte = categoria_origem, valor = valor_total) %>%
  bind_rows(df_final %>%
              filter(ano >= 2006) %>%
              select(ano, `Lei Rouanet` = teto,
                     `Incentivo (ANCINE)` = ancine_captado) %>%
              pivot_longer(cols = -ano, names_to = "fonte", values_to = "valor")) %>%
  filter(!is.na(valor), valor > 0)

ggplot(df_detalhado_reais, aes(x = factor(ano), y = valor, fill = fonte)) +
  geom_col(position = "stack", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(valor > 150000000, 
                               label_number(scale = 1e-9, suffix = "b", accuracy = 0.1, decimal.mark = ",")(valor), "")), 
            position = position_stack(vjust = 0.5),  
            size = 3.2, 
            fontface = "bold", color = "white") + 
  stat_summary(fun = sum, aes(label = label_number(scale = 1e-9, suffix = " bi", accuracy = 0.1, decimal.mark = ",")(..y..), group = ano),  
               geom = "text", vjust = -0.7, 
               size = 4, 
               fontface = "bold", color = "black") + 
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = " bi"),  
                     expand = expansion(mult = c(0, 0.4))) + 
  scale_fill_manual(values = c(
    "Ministério da Cultura (órgão 42000)" = "#2980b9",
    "PNAB (UO 73120)" = "#f1c40f",
    "FSA (UO 74912)" = "#27ae60",
    "Lei Aldir Blanc 1" = "#e67e22",
    "Lei Paulo Gustavo" = "#78281f",
    "Outros órgãos (Cidadania/Turismo)" = "#95a5a6",
    "Lei Rouanet" = "#1b2631",
    "Incentivo (ANCINE)" = "#8e44ad"),
    na.translate = FALSE) + 
  labs(title = "Composição Detalhada do Gasto Federal Pleno", 
       subtitle = "Decomposição por Unidade Orçamentária (SIOP) e Renúncia Fiscal (Lei Rouanet/ANCINE)", 
       x = "Ano", y = "R$ Bilh?es", fill = "Fonte", 
       caption = "Fonte: SIOP, SALIC e ANCINE. Rótulos superiores indicam o total anual empenhado + teto de renúncia.") + 
  tema_minC + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 8))

# V. DETALHAMENTO DA MATRIZ POR FONTE (DECOMPOSTO)
df_fontes_detalhado <- df_grafico_origem %>%
  filter(ano >= 2006) %>%
  rename(fonte = categoria_origem, valor = valor_total) %>%
  bind_rows(df_final %>%
              filter(ano >= 2006) %>%
              select(ano, `Lei Rouanet` = teto, `Incentivo (ANCINE)` = ancine_captado) %>%
              pivot_longer(cols = -ano, names_to = "fonte", values_to = "valor")) %>%
  filter(!is.na(valor), valor > 0) %>%
  group_by(ano) %>%
  mutate(pct = valor / sum(valor, na.rm = TRUE)) %>%
  ungroup()

ggplot(df_fontes_detalhado, aes(x = factor(ano), y = valor, fill = fonte)) +
  geom_col(position = "fill", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(pct > 0.02, percent(pct, accuracy = 0.1, decimal.mark = ","), "")), 
            position = position_fill(vjust = 0.5), 
            size = 2.5, fontface = "bold", color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c(
    "Ministério da Cultura (órgão 42000)" = "#2980b9", 
    "PNAB (UO 73120)" = "#f1c40f",                       
    "FSA (UO 74912)" = "#27ae60",                       
    "Lei Aldir Blanc 1" = "#e67e22",
    "Lei Paulo Gustavo" = "#78281f",
    "Outros órgãos (Cidadania/Turismo)" = "#95a5a6", 
    "Lei Rouanet" = "#1b2631",                       
    "Incentivo (ANCINE)" = "#8e44ad"), 
    na.translate = FALSE) +
  labs(title = "Matriz de Financiamento por Fonte",
       x = "Ano", y = "Propor??o do Investimento (%)", fill = "Fonte",
       caption = "Fonte: SIOP, SALIC e ANCINE") + 
  tema_minC + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 8))

# VI. IMPACTO FISCAL DO GASTO DIRETO (% DA RCL)
ggplot(df_final %>% 
         mutate(perc_direto_rcl = (siop_direto / rcl) * 100) %>% 
         filter(!is.na(perc_direto_rcl)), 
       aes(x = ano, y = perc_direto_rcl)) +
  geom_line(color = "#2980b9", size = 1.2) + 
  geom_point(color = "#2980b9", size = 2) +
  geom_text(aes(label = paste0(round(perc_direto_rcl, 3), "%")), 
            vjust = -1.5, size = 3, fontface = "bold") +
  scale_x_continuous(breaks = seq(min(df_final$ano), 2025, 1)) +
  scale_y_continuous(labels = label_number(suffix = "%"), 
                     expand = expansion(mult = c(0, 0.5))) +
  labs(title = "Gasto Federal Direto em Cultura como % da RCL",
       subtitle = "Participação da Execução Orçamentária (SIOP) na Receita Corrente Líquida da União",
       x = "Ano", y = "Percentual",
       caption = "Fonte: SIOP e Tesouro Nacional. Valores nominais.") + 
  tema_minC +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############################ VOLUME FINANCEIRO REAL ######################################

ipca_bruto <- get_series(433, start_date = "2003-01-01")

ipca_acum <- ipca_bruto %>%
  rename(data = date, mensal = `433`) %>%
  mutate(indice = cumprod(1 + mensal/100))

media_indice_2024 <- ipca_acum %>%
  filter(format(data, "%Y") == "2024") %>%
  summarise(m = mean(indice)) %>%
  pull(m)

tabela_fator <- ipca_acum %>%
  mutate(ano = as.numeric(format(data, "%Y"))) %>%
  group_by(ano) %>%
  summarise(indice_medio_ano = mean(indice)) %>%
  mutate(fator_correcao = media_indice_2024 / indice_medio_ano)

df_anual_detalhado <- df_detalhado_reais %>%
  mutate(ano = as.numeric(ano)) %>%
  left_join(tabela_fator, by = "ano") %>%
  mutate(valor_real = valor * fator_correcao,
    valor_nominal = valor)

#VII. GASTO TOTAL PLENO POR INCUMBENTE (VALORES CORRENTES E REAIS) - LAB 1 e LPG
df_incumbente_ajustado <- df_detalhado_reais %>%
  mutate(ano = as.numeric(ano),
    fonte = trimws(fonte)) %>%
  mutate(incumbente = case_when(fonte == "Lei Paulo Gustavo" & ano == 2022 ~ "Lula III",
    ano == 2006 ~ "Lula I (Final)",
    ano >= 2007 & ano <= 2010 ~ "Lula II",
    ano >= 2011 & ano <= 2014 ~ "Dilma I",
    ano == 2015 ~ "Dilma II",
    ano >= 2016 & ano <= 2018 ~ "Temer",
    ano >= 2019 & ano <= 2022 ~ "Bolsonaro",
    ano >= 2023 ~ "Lula III")) %>%
  filter(!is.na(incumbente), ano >= 2006) %>%
  left_join(tabela_fator, by = "ano") %>%
  mutate(Real = valor * fator_correcao,
    Nominal = valor) %>%
  pivot_longer(cols = c(Nominal, Real), names_to = "tipo_valor", values_to = "valor_final") %>%
  group_by(incumbente, tipo_valor, fonte) %>%
  summarise(valor_final = sum(valor_final, na.rm = TRUE), .groups = "drop") %>%
  mutate(incumbente = factor(incumbente, levels = c(
      "Lula I (Final)", "Lula II", "Dilma I", "Dilma II", "Temer", "Bolsonaro", "Lula III")),
    tipo_valor = factor(tipo_valor, levels = c("Nominal", "Real")),
    fonte = factor(fonte, levels = c(
      "Outros ?rg?os (Cidadania/Turismo)", "Lei Aldir Blanc 1", "Lei Paulo Gustavo", 
      "PNAB (UO 73120)", "Minist?rio da Cultura (?rg?o 42000)", 
      "Incentivo (ANCINE)", "Lei Rouanet", "FSA (UO 74912)")))

ggplot(df_incumbente_ajustado, aes(x = tipo_valor, y = valor_final, fill = fonte)) +
  geom_col(position = "stack", width = 0.8, alpha = 0.9) +
  geom_text(aes(label = ifelse(valor_final > 3e8, 
                               label_number(scale = 1e-9, suffix = "b", accuracy = 0.1, decimal.mark = ",")(valor_final), "")),
            position = position_stack(vjust = 0.5), 
            color = "white", size = 2.4, fontface = "bold") +
  stat_summary(fun = sum, 
               aes(label = label_number(scale = 1e-9, suffix = " bi", accuracy = 0.1, decimal.mark = ",")(..y..), 
                   group = tipo_valor),
               geom = "text", vjust = -1, size = 3.2, fontface = "bold", color = "black") +
  facet_wrap(~incumbente, strip.position = "bottom", nrow = 1) +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = " bi"),
                     expand = expansion(mult = c(0, 0.4))) +
  scale_fill_manual(values = c(
    "Minist?rio da Cultura (?rg?o 42000)" = "#2980b9",
    "PNAB (UO 73120)" = "#f1c40f",
    "FSA (UO 74912)" = "#27ae60",
    "Lei Aldir Blanc 1" = "#d35400",
    "Lei Paulo Gustavo" = "#e67e22",
    "Outros ?rg?os (Cidadania/Turismo)" = "#95a5a6",
    "Lei Rouanet" = "#1b2631", 
    "Incentivo (ANCINE)" = "#8e44ad")) +
  labs(title = "Gasto Federal Pleno por Mandato: Nominal vs. Real",
       subtitle = "Teste Anal?tico: Lei Paulo Gustavo (2022) reclassificada para o ciclo de execu??o de Lula III",
       x = NULL, y = "R$ Bilh?es (Acumulado)", fill = "Fonte") +
  tema_minC +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.text = element_text(face = "bold", size = 9),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(size = 7.5, face = "italic"))


############################################# LPG EM 2023 ####################################

df_anual_ajustado <- df_detalhado_reais %>%
  mutate(ano = as.numeric(ano),
    ano = ifelse(fonte == "Lei Paulo Gustavo" & ano == 2022, 2023, ano)) %>%
  group_by(ano, fonte) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  left_join(tabela_fator, by = "ano") %>%
  mutate(valor_real = valor * fator_correcao,
    valor_nominal = valor) %>%
  mutate(fonte = factor(fonte, levels = c(
    "Outros órgãos (Cidadania/Turismo)", "Lei Aldir Blanc 1", "Lei Paulo Gustavo", 
    "PNAB (UO 73120)", "Ministério da Cultura (órgão 42000)", 
    "Incentivo (ANCINE)", "Lei Rouanet", "FSA (UO 74912)")))


#VIII. EVOLU??O ANUAL - VALORES NOMINAIS (LPG EM 2023)
df_anual_ajustado <- df_detalhado_reais %>%
  mutate(ano = as.numeric(ano),
    ano = ifelse(fonte == "Lei Paulo Gustavo" & ano == 2022, 2023, ano)) %>%
  group_by(ano, fonte) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  left_join(tabela_fator, by = "ano") %>%
  mutate(valor_real = valor * fator_correcao,
         valor_nominal = valor) %>%
  mutate(fonte = factor(fonte, levels = c(
    "Ministério da Cultura (órgão 42000)", 
    "PNAB (UO 73120)", 
    "Lei Paulo Gustavo", 
    "Lei Aldir Blanc 1", 
    "FSA (UO 74912)",
    "Lei Rouanet", 
    "Incentivo (ANCINE)", 
    "Outros órgãos (Cidadania/Turismo)")))

ggplot(df_anual_ajustado, aes(x = factor(ano), y = valor_nominal, fill = fonte)) +
  geom_col(position = "stack", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(valor_nominal > 4e8, 
                               label_number(scale = 1e-9, suffix = "b", accuracy = 0.1, decimal.mark = ",")(valor_nominal), "")),
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3.5, fontface = "bold") +
  stat_summary(fun = sum, 
               aes(label = label_number(scale = 1e-9, suffix = " bi", accuracy = 0.1, decimal.mark = ",")(..y..), group = ano),
               geom = "text", vjust = -0.7, size = 4.5, fontface = "bold", color = "black") +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = " bi"), 
                     expand = expansion(mult = c(0, 0.4))) +
  scale_fill_manual(values = c(
    "Ministério da Cultura (órgão 42000)" = "#2980b9",
    "PNAB (UO 73120)" = "#f1c40f",
    "FSA (UO 74912)" = "#27ae60", 
    "Lei Aldir Blanc 1" = "#e67e22",
    "Lei Paulo Gustavo" = "#78281f",
    "Outros órgãos (Cidadania/Turismo)" = "#95a5a6",
    "Lei Rouanet" = "#1b2631",
    "Incentivo (ANCINE)" = "#8e44ad"),
    na.translate = FALSE) +
  labs(title = "Evolução do Gasto Federal Pleno",
       subtitle = "Valores nominais",
       x = "Ano", y = "R$ Bilhões", fill = "Fonte",
       caption = "Fonte: SIOP, SALIC e ANCINE. Valores empenhados (direto) e teto/captadoo (indireto).") +
  tema_minC + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.text = element_text(size = 9))

#Matriz de Financiamento - LPG em 2023
df_percentual_ajustado <- df_detalhado_reais %>%
  mutate(ano = as.numeric(ano),
         ano = ifelse(fonte == "Lei Paulo Gustavo" & ano == 2022, 2023, ano)) %>%
  group_by(ano, fonte) %>%
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  group_by(ano) %>%
  mutate(pct = valor / sum(valor, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(fonte = factor(fonte, levels = c(
    "Ministério da Cultura (órgão 42000)", 
    "PNAB (UO 73120)", 
    "Lei Paulo Gustavo", 
    "Lei Aldir Blanc 1", 
    "FSA (UO 74912)",
    "Lei Rouanet", 
    "Incentivo (ANCINE)", 
    "Outros órgãos (Cidadania/Turismo)")))

ggplot(df_percentual_ajustado, aes(x = factor(ano), y = pct, fill = fonte)) +
  geom_col(position = "fill", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(pct > 0.03, scales::percent(pct, accuracy = 0.1, decimal.mark = ","), "")), 
            position = position_fill(vjust = 0.5), 
            color = "white", size = 2.8, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ","), 
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c(
    "Ministério da Cultura (órgão 42000)" = "#2980b9",
    "PNAB (UO 73120)" = "#f1c40f",
    "FSA (UO 74912)" = "#27ae60", 
    "Lei Aldir Blanc 1" = "#e67e22",
    "Lei Paulo Gustavo" = "#78281f",
    "Outros órgãos (Cidadania/Turismo)" = "#95a5a6",
    "Lei Rouanet" = "#1b2631",
    "Incentivo (ANCINE)" = "#8e44ad"),
    na.translate = FALSE) +
  labs(title = "Composição Percentual da Matriz de Financiamento Federal",
       subtitle = "Valores nominais",
       x = "Ano", y = "Percentual da Execução (%)", fill = "Fonte",
       caption = "Fonte: SIOP, SALIC e ANCINE. Obs: Gasto Federal Pleno") +
  tema_minC + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.text = element_text(size = 8))

#Percentual da RCL - LPG em 2023
df_ajustado_rcl <- df_final %>%
  mutate(valor_lpg_2022 = ifelse(ano == 2022, 
                            df_grafico_origem$valor_total[df_grafico_origem$ano == 2022 & 
                                                            df_grafico_origem$categoria_origem == "Lei Paulo Gustavo"], 0),
    siop_direto_ajustado = case_when(ano == 2022 ~ siop_direto - valor_lpg_2022,
      ano == 2023 ~ siop_direto + valor_lpg_2022,
      TRUE ~ siop_direto),
    perc_direto_rcl = (siop_direto_ajustado / rcl) * 100) %>%
  filter(!is.na(perc_direto_rcl))

ggplot(df_ajustado_rcl, aes(x = ano, y = perc_direto_rcl)) +
  geom_line(color = "#2980b9", size = 1.2) + 
  geom_point(color = "#2980b9", size = 2.5) +
  geom_text(aes(label = paste0(round(perc_direto_rcl, 2), "%")), 
            vjust = -1.5, size = 3.5, fontface = "bold") +
  scale_x_continuous(breaks = seq(min(df_ajustado_rcl$ano), 2025, 1)) +
  scale_y_continuous(labels = label_number(suffix = "%", decimal.mark = ","), 
                     expand = expansion(mult = c(0, 0.5))) +
  labs(title = "Gasto Federal Direto em Cultura como % da RCL",
       subtitle = "Participação da Execução Orçamentária (SIOP) na RCL",
       x = "Ano", y = "% da Receita Corrente Líquida",
       caption = "Fonte: SIOP e Tesouro Nacional. Nota: Valores nominais.") + 
  tema_minC +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#IX: EVOLU??O ANUAL - VALORES REAIS (LPG EM 2023)
ggplot(df_anual_ajustado, aes(x = factor(ano), y = valor_real, fill = fonte)) +
  geom_col(position = "stack", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(valor_real > 4e8, 
                               label_number(scale = 1e-9, suffix = "b", accuracy = 0.1, decimal.mark = ",")(valor_real), "")),
            position = position_stack(vjust = 0.5), 
            color = "white", size = 2.2, fontface = "bold") +
  stat_summary(fun = sum, 
               aes(label = label_number(scale = 1e-9, suffix = " bi", accuracy = 0.1, decimal.mark = ",")(..y..), group = ano),
               geom = "text", vjust = -0.7, size = 3, fontface = "bold", color = "black") +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = " bi"), expand = expansion(mult = c(0, 0.4))) +
  scale_fill_manual(values = c(
    "Minist?rio da Cultura (?rg?o 42000)" = "#2980b9",
    "PNAB (UO 73120)" = "#f1c40f",
    "FSA (UO 74912)" = "#27ae60",
    "Lei Aldir Blanc 1" = "#d35400",
    "Lei Paulo Gustavo" = "#e74c3c",
    "Outros ?rg?os (Cidadania/Turismo)" = "#95a5a6",
    "Lei Rouanet" = "#1b2631", 
    "Incentivo (ANCINE)" = "#8e44ad")) +
  labs(title = "Evolu??o do Gasto Federal Pleno: Valores Reais",
       subtitle = "S?rie Hist?rica corrigida (IPCA 2024) | LPG alocada em 2023",
       x = "Ano", y = "R$ Bilh?es (Pre?os de 2024)", fill = "Fonte") +
  tema_minC + theme(axis.text.x = element_text(angle = 45, hjust = 1))
