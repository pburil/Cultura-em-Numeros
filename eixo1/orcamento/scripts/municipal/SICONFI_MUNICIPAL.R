#-------------------------------------------------------------------------------
# 0. CARREGAMENTO DE PACOTES
#-------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(scales)
library(janitor)
library(stringr)
library(rbcb)
library(writexl)

# Configurações globais
options(scipen = 999) 

setwd("E:\\Pedro Buril\\Ministerio da Cultura\\Cultura em Numeros\\Eixo 1\\Orcamento\\SICONFI_MUNICIPAL")

#-------------------------------------------------------------------------------
# 1. FUNÇÕES AUXILIARES
#-------------------------------------------------------------------------------

parse_br <- function(x) {
  if(is.numeric(x)) return(x)
  x <- as.character(x)
  x <- gsub("\\.", "", x) 
  x <- gsub(",", ".", x)  
  as.numeric(x)
}

find_skip <- function(path) {
  linhas <- readLines(path, n = 30, warn = FALSE, encoding = "Latin1")
  skip_n <- which(str_detect(linhas, "Cod.IBGE|cod_ibge|Instituição"))[1] - 1
  if(is.na(skip_n)) return(5) 
  return(skip_n)
}

#-------------------------------------------------------------------------------
# 2. LEITURA DE DADOS (EMPENHADO - SÉRIE 2018-2025)
#-------------------------------------------------------------------------------
anos <- 2016:2025

ler_despesa_municipal <- function(ano){
  arquivo <- paste0(ano, " - Municipal.csv")
  if(!file.exists(arquivo)) {
    message(paste("Arquivo não encontrado:", arquivo))
    return(NULL)
  }
  
  # Mensagem atualizada para Empenhada
  message(paste("Processando Despesa Empenhada:", ano)) 
  skip_n <- find_skip(arquivo)
  
  read_delim(arquivo, delim = ";", locale = locale(encoding = "Latin1"),
             skip = skip_n, show_col_types = FALSE) %>%
    clean_names() %>%
    filter(conta == "Cultura", 
           coluna == "DESPESAS EMPENHADAS ATÉ O BIMESTRE (b)" ) %>%
    mutate(ano = ano,
           cod_ibge = str_pad(as.character(cod_ibge), 7, pad = "0"),
           valor_num = parse_br(valor)) %>%
    filter(!is.na(valor_num), valor_num > 0) %>%
    group_by(cod_ibge, ano) %>%
    summarise(gasto = max(valor_num, na.rm = TRUE), .groups = "drop")
}

gasto_municipal <- map_dfr(anos, ler_despesa_municipal)

# Leitura da Receita Corrente Líquida (RCL)
ler_rcl_municipal <- function(ano){
  arquivo <- paste0("RCL_", ano, "_Municipios.csv")
  if(!file.exists(arquivo)) return(NULL)
  
  message(paste("Processando RCL Municipal:", ano))
  skip_n <- find_skip(arquivo)
  
  read_delim(arquivo, delim = ";", locale = locale(encoding = "Latin1"),
             skip = skip_n, show_col_types = FALSE) %>%
    clean_names() %>%
    filter(conta == "RECEITA CORRENTE LÍQUIDA (III) = (I - II)",
           coluna == "TOTAL (ÚLTIMOS 12 MESES)") %>%
    mutate(ano = ano,
           cod_ibge = str_pad(as.character(cod_ibge), 7, pad = "0"),
           rcl_valor = parse_br(valor)) %>%
    group_by(cod_ibge, ano) %>%
    summarise(rcl = max(rcl_valor, na.rm = TRUE), .groups = "drop")
}

rcl_municipal <- map_dfr(anos, ler_rcl_municipal)

#-------------------------------------------------------------------------------
# 3. CONSOLIDAÇÃO
#-------------------------------------------------------------------------------
dados_municipios <- read_excel("MUNIC_FINAL.xlsx") %>%
  clean_names() %>%
  mutate(cod_ibge = str_pad(as.character(cod_ibge), 7, pad = "0")) %>%
  distinct(cod_ibge, .keep_all = TRUE)

base_rcl <- gasto_municipal %>%
  left_join(rcl_municipal, by = c("cod_ibge", "ano")) %>%
  left_join(dados_municipios, by = "cod_ibge") %>%
  filter(!is.na(rcl), rcl > 0) %>%
  mutate(perc_rcl = (gasto / rcl) * 100)

#-------------------------------------------------------------------------------
# 4. SÉRIE BRASIL E GRÁFICOS
#-------------------------------------------------------------------------------
serie_brasil <- base_rcl %>%
  group_by(ano) %>%
  summarise(
    gasto_total = sum(gasto, na.rm = TRUE),
    rcl_total = sum(rcl, na.rm = TRUE),
    perc = (gasto_total / rcl_total) * 100,
    .groups = "drop")

# Gráfico: Evolução Absoluta (Empenhado)
ggplot(serie_brasil, aes(x = factor(ano), y = gasto_total)) +
  geom_col(fill = "#2980b9", alpha = 0.9, width = 0.7) +
  geom_text(aes(label = label_number(scale = 1e-9, suffix = " bi", 
                                     decimal.mark = ",", accuracy = 0.01)(gasto_total)),
            vjust = -0.5, 
            fontface = "bold", 
            size = 3.5) +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = " bi", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.3))) + 
  labs(title = "Evolução do Empenho Cultural Municipal Total (Brasil)",
       subtitle = "Valores Empenhados (Fase b do SICONFI) - Série 2018-2025",
       y = "R$ (Bilhões)", 
       x = NULL,
       caption = "Fonte: SICONFI/Tesouro Nacional. Elaboração própria.") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

############################################## ANALISE PORTE POPULACIONAL ############################################
base_porte_analise <- gasto_municipal %>%
  filter(ano >= 2016 & ano <= 2019) %>%
  left_join(dados_municipios, by = "cod_ibge") %>%
  filter(!is.na(populacao)) %>%
  mutate(porte = case_when(
    populacao <= 20000 ~ "Pequeno Porte I",
    populacao > 20000 & populacao <= 50000 ~ "Pequeno Porte II",
    populacao > 50000 & populacao <= 100000 ~ "Médio Porte",
    populacao > 100000 ~ "Grande Porte")) %>%
  mutate(porte = factor(porte, levels = c("Pequeno Porte I", "Pequeno Porte II", 
                                          "Médio Porte", "Grande Porte")))

resumo_medio_grid <- base_porte_analise %>%
  group_by(ano, porte) %>%
  summarise(gasto_medio = mean(gasto, na.rm = TRUE),
            .groups = "drop")

#1. Gasto Médio Cultural por Porte Populacional / Valores nominais

ggplot(resumo_medio_grid, aes(x = factor(ano), y = gasto_medio, fill = porte)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = label_number(scale = 1e-6, suffix = " mi", 
                                     decimal.mark = ",", accuracy = 0.1)(gasto_medio)),
            vjust = -0.5, size = 3.2, fontface = "bold") +
  facet_wrap(~porte, scales = "free", ncol = 2) + 
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " mi", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.4))) +
  scale_fill_manual(values = c("Pequeno Porte I" = "#3498db", 
                               "Pequeno Porte II" = "#2ecc71", 
                               "Médio Porte" = "#f1c40f", 
                               "Grande Porte" = "#e74c3c")) +
  labs(title = "Evolução do Gasto Médio Cultural por Porte Populacional",
       subtitle = "Valores Médios em Milhões de R$ | Valores nominais",
       x = "Ano", 
       y = "Gasto Médio (R$ Milhões)", 
       caption = "Fonte: Relatório Resumido de Execução Orçamentária/SICONFI.") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(face = "bold", size = 9, angle = 0))

#2. Composição do Gasto Cultural por Porte

df_participacao_porte <- base_porte_analise %>%
  group_by(ano, porte) %>%
  summarise(gasto_total_porte = sum(gasto, na.rm = TRUE), .groups = "drop") %>%
  group_by(ano) %>%
  mutate(pct = gasto_total_porte / sum(gasto_total_porte)) %>%
  ungroup()

ggplot(df_participacao_porte, aes(x = factor(ano), y = pct, fill = porte)) +
  geom_col(position = position_fill(reverse = TRUE), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = percent(pct, accuracy = 0.1, decimal.mark = ",")), 
            position = position_fill(vjust = 0.5, reverse = TRUE), 
            color = "white",
            fontface = "bold", 
            size = 3.5) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("Pequeno Porte I" = "#3498db", 
                               "Pequeno Porte II" = "#2ecc71", 
                               "Médio Porte" = "#f1c40f", 
                               "Grande Porte" = "#e74c3c")) +
  labs(title = "Composição do Gasto Cultural Municipal por Porte",
       subtitle = "Participação percentual de cada categoria no investimento total (2016-2019)",
       x = "Ano", 
       y = "Participação no Gasto Total (%)", 
       fill = "Porte Populacional",
       caption = "Fonte: Relatório Resumido de Execução Orçamentária/SICONFI.") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.major.x = element_blank())

#3. Gasto per capita por porte populacional
df_per_capita_porte <- base_porte_analise %>%
  group_by(ano, porte) %>%
  summarise(gasto_total = sum(gasto, na.rm = TRUE),
            pop_total = sum(populacao, na.rm = TRUE),
            per_capita = gasto_total / pop_total,
            .groups = "drop")

ggplot(df_per_capita_porte, aes(x = factor(ano), y = per_capita, fill = porte)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_text(aes(label = label_number(prefix = "R$ ", 
                                     decimal.mark = ",", 
                                     accuracy = 0.01)(per_capita)),
            vjust = -1, 
            size = 3.5, 
            fontface = "bold") +
  facet_wrap(~porte, scales = "free_y", ncol = 2) + 
  scale_y_continuous(labels = label_number(prefix = "R$ ", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.5))) +
  scale_fill_manual(values = c("Pequeno Porte I" = "#3498db", 
                               "Pequeno Porte II" = "#2ecc71", 
                               "Médio Porte" = "#f1c40f", 
                               "Grande Porte" = "#e74c3c")) +
  labs(title = "Gasto Cultural Per Capita por Porte Populacional",
       subtitle = "2016-2019 | Valores Nominais",
       x = "Ano", 
       y = "Gasto Per Capita (R$)", 
       caption = "Fonte: Relatório Resumido de Execução Orçamentária/SICONFI.") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(face = "bold", size = 10),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1.5, "lines"))

####################################################### DEFLACIONANDO ############################################
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
  filter(ano %in% c(2016:2019, 2024)) %>%
  group_by(ano) %>%
  summarise(indice_medio_ano = mean(indice)) %>%
  mutate(fator_correcao = media_indice_2024 / indice_medio_ano)

print(tabela_fator)

base_real_final <- gasto_municipal %>%
  filter(ano >= 2016 & ano <= 2019) %>%
  left_join(tabela_fator, by = "ano") %>%
  left_join(dados_municipios, by = "cod_ibge") %>%
  mutate(gasto_real = gasto * fator_correcao) %>%
  filter(!is.na(populacao)) %>%
  mutate(porte = case_when(
    populacao <= 20000 ~ "Pequeno Porte I",
    populacao > 20000 & populacao <= 50000 ~ "Pequeno Porte II",
    populacao > 50000 & populacao <= 100000 ~ "Médio Porte",
    populacao > 100000 ~ "Grande Porte")) %>%
  mutate(porte = factor(porte, levels = c("Pequeno Porte I", "Pequeno Porte II", 
                                          "Médio Porte", "Grande Porte")))

resumo_medio <- base_real_final %>%
  group_by(ano, porte) %>%
  summarise(gasto_medio_real = mean(gasto_real, na.rm = TRUE), .groups = "drop")

ggplot(resumo_medio, aes(x = factor(ano), y = gasto_medio_real, fill = porte)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = paste0("R$ ", label_number(scale = 1e-6, decimal.mark = ",", accuracy = 0.1)(gasto_medio_real), " mi")),
            vjust = -1.2, size = 3.2, fontface = "bold") +
  facet_wrap(~porte, scales = "free", ncol = 2) + 
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " mi", decimal.mark = ","), 
                     expand = expansion(mult = c(0, 0.5))) +
  scale_fill_manual(values = c("#3498db", "#2ecc71", "#f1c40f", "#e74c3c")) +
  labs(title = "Evolução do Gasto Médio Cultural Real por Porte Populacional",
       subtitle = "Valores Médios em Milhões de R$ | Valores reais (Base: 2024)",
       x = "Ano", y = "Gasto Médio(R$ Milhões)",
       caption = "Fonte: Relatório Resumido de Execução Orçamentária/SICONFI.") +
  theme_minimal() + theme(legend.position = "none", strip.text = element_text(face="bold"))

resumo_per_capita <- base_real_final %>%
  group_by(ano, porte) %>%
  summarise(per_capita_real = sum(gasto_real, na.rm = TRUE) / sum(populacao, na.rm = TRUE), .groups = "drop")

ggplot(resumo_per_capita, aes(x = factor(ano), y = per_capita_real, fill = porte)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_text(aes(label = label_number(prefix = "R$ ", decimal.mark = ",", accuracy = 0.01)(per_capita_real)),
            vjust = -1.2, size = 3.5, fontface = "bold") +
  facet_wrap(~porte, scales = "free", ncol = 2) + 
  scale_y_continuous(labels = label_number(prefix = "R$ ", decimal.mark = ","), 
                     expand = expansion(mult = c(0, 0.5))) +
  scale_fill_manual(values = c("#3498db", "#2ecc71", "#f1c40f", "#e74c3c")) +
  labs(title = "Gasto Cultural Per Capita por Porte Populacional",
       subtitle = "2016-2019 | Valores Reais (Base 2024)",
       x = "Ano", y = "Gasto Per Capita (R$)",
       caption = "Fonte: Relatório Resumido de Execução Orçamentária/SICONFI.") +
  theme_minimal() + theme(legend.position = "none", strip.text = element_text(face="bold"))


write_xlsx(base_real_final, "Base_Cultura_Nominal_e_Real_2016_2019.xlsx")
