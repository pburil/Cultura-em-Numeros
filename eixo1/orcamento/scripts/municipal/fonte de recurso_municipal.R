#===============================================================================
# PROJETO CULTURA EM N?MEROS - EIXO 1 (MUNIC?PIOS)
#===============================================================================
library(tidyverse)
library(arrow)
library(scales)
library(gt)
library(janitor)
library(purrr)
library(rbcb)

path_base <- "E:/Pedro Buril/Ministerio da Cultura/Cultura em Numeros/Eixo 1/Orcamento/SICONFI_MUNICIPAL/municipios_parquet"

paleta_cores <- c(
  "Recurso Próprio (Municipal)"      = "#1b2631", 
  "Emendas Parlamentares (Cultura)" = "#27ae60", 
  "Lei Aldir Blanc 1 (LAB 1)"       = "#8e44ad",
  "Lei Paulo Gustavo (LPG)"          = "#e67e22", 
  "PNAB (Aldir Blanc 2)"            = "#f1c40f")

codigos_emendas <- c("3101", "3110", "3111", "3120", "3121", "3130", "3140", 
                     "3201", "3202", "3210", "3211", "3220", "3221")

# 1. DEFLATOR IPCA -------------------------------------------------------------
ipca_mensal <- rbcb::get_series(433, start_date = "2019-01-01", end_date = "2025-12-31")
fatores_ipca <- ipca_mensal %>%
  clean_names() %>% rename(var_mensal = x433) %>%
  mutate(ano = as.numeric(format(date, "%Y")),
         indice_encadeado = cumprod(1 + (var_mensal / 100))) %>%
  group_by(ano) %>% summarise(indice_medio_ano = mean(indice_encadeado), .groups = "drop") %>%
  mutate(indice_base = indice_medio_ano[ano == 2024],
         fator_deflacao = indice_base / indice_medio_ano) %>%
  select(exercicio = ano, fator_deflacao)

# 2. PROCESSAMENTO (INCLUINDO TRATAMENTO ROBUSTO DE EMENDAS) --------------------
arquivos <- list.files(path_base, pattern = "\\.parquet$", full.names = TRUE)

df_municipios_bruto <- arquivos %>% map_dfr(function(arq) {
  tryCatch({
    read_parquet(arq) %>% clean_names() %>%
      mutate(across(any_of(c("fonte_recursos", "complemento_fonte", "conta_contabil", "funcao")), as.character)) %>%
      filter(natureza_conta == "C") %>%
      mutate(id_funcao = as.numeric(str_extract(funcao, "\\d+"))) %>%
      filter(id_funcao == 13) %>%
      select(exercicio, uf, fonte_recursos, conta_contabil, complemento_fonte, valor)
  }, error = function(e) return(NULL))
})

df_municipios_processed <- df_municipios_bruto %>%
  mutate(exercicio = as.numeric(exercicio),
         valor = abs(valor), 
         conta_limpa = str_remove_all(conta_contabil, "\\."),
         fonte_string = str_remove_all(as.character(fonte_recursos), "\\."),
         complemento_limpo = str_pad(str_remove_all(complemento_fonte, "[^0-9]"), 4, pad = "0")) %>%
  filter(str_starts(conta_limpa, "62213")) %>%
  separate(uf, into = c("municipio", "uf_sigla"), sep = "_(?=[^_]+$)", fill = "right") %>%
  filter(uf_sigla != "DF") %>%
  left_join(fatores_ipca, by = "exercicio") %>%
  mutate(valor_real = valor * fator_deflacao) %>%
  mutate(origem = case_when(
    str_detect(fonte_string, "^1719|^2719|^1720|^2720|^719|^720") ~ "PNAB (Aldir Blanc 2)",
    str_detect(fonte_string, "^1715|^2715|^1716|^2716|^715|^716") ~ "Lei Paulo Gustavo (LPG)",
    exercicio %in% c(2020, 2021) & str_detect(fonte_string, "^19|^29") & 
      !str_detect(fonte_string, "^19900000|^19200000|^19500000") ~ "Lei Aldir Blanc 1 (LAB 1)",
    complemento_limpo %in% codigos_emendas ~ "Emendas Parlamentares (Cultura)",
    TRUE ~ "Recurso Próprio (Municipal)")) %>%
  filter(!is.na(origem)) %>%
  mutate(origem = factor(origem, levels = names(paleta_cores)))

# 3. AGREGA??O -----------------------------------------------------------------
df_municipios_final <- df_municipios_processed %>%
  group_by(exercicio, municipio, uf_sigla, origem) %>%
  summarise(valor_nominal_final = sum(valor, na.rm = TRUE),
            valor_real_final = sum(valor_real, na.rm = TRUE), .groups = "drop")

# 2. GR?FICOS  ----------------------------------------------
# Gr?fico Nominal
df_resumo_grafico <- df_municipios_final %>%
  group_by(exercicio, origem) %>%
  summarise(valor = sum(valor_nominal_final, na.rm = TRUE), .groups = "drop")

ggplot(df_resumo_grafico, aes(x = factor(exercicio), y = valor, fill = origem)) +
  geom_col(position = position_stack(), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(valor > 0.05e9, label_number(scale = 1e-9, suffix = "b", accuracy = 0.1, decimal.mark = ",")(valor), "")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 2.8) +
  stat_summary(fun = sum, aes(label = label_number(scale = 1e-9, suffix = " bi", accuracy = 0.1, decimal.mark = ",")(after_stat(y)), group = exercicio), 
               geom = "text", vjust = -1.2, size = 3.8, fontface = "bold") +
  scale_y_continuous(labels = label_number(prefix = "R$ ", scale = 1e-9, suffix = " bi", accuracy = 0.1, decimal.mark = ","), expand = expansion(mult = c(0, 0.4))) +
  scale_fill_manual(values = paleta_cores, na.translate = FALSE) +
  labs(title = "Evolução do Investimento Cultural Municipal por Fonte | Valores nominais",
       subtitle = "Valores Empenhados Totais | R$ Bilhões", x = "Ano de Execução", y = "Valor Empenhado", fill = "Origem do Recurso",
       caption = "Fonte: MSC/SICONFI. An?lise: Cultura em N?meros (2026).") +
  theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(face="bold", size = 14), axis.text.x = element_text(face="bold"))

# Gr?fico Real (Base 2024)
df_resumo_real <- df_municipios_final %>%
  group_by(exercicio, origem) %>%
  summarise(valor = sum(valor_real_final, na.rm = TRUE), .groups = "drop")

ggplot(df_resumo_real, aes(x = factor(exercicio), y = valor, fill = origem)) +
  geom_col(position = position_stack(), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(valor > 0.01e9, label_number(scale = 1e-9, suffix = "b", accuracy = 0.01, decimal.mark = ",")(valor), "")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 2.8) +
  stat_summary(fun = sum, aes(label = label_number(scale = 1e-9, suffix = " bi", accuracy = 0.01, decimal.mark = ",")(after_stat(y)), group = exercicio), 
               geom = "text", vjust = -1.2, size = 3.8, fontface = "bold") +
  scale_y_continuous(labels = label_number(prefix = "R$ ", scale = 1e-9, suffix = " bi", accuracy = 0.1, decimal.mark = ","), expand = expansion(mult = c(0, 0.4))) +
  scale_fill_manual(values = paleta_cores, na.translate = FALSE) +
  labs(title = "Evolução do Investimento Cultural Municipal por Fonte (Valores Reais)",
       subtitle = "Valores Empenhados Corrigidos pela Inflação (Preços Médios de 2024) | R$ Bilhões",
       x = "Ano de Execução", y = "Valor Empenhado Real (R$ Bilhões)", fill = "Origem do Recurso",
       caption = "Fonte: MSC/SICONFI. Deflação: IPCA/SGS-BCB. Ano-base: 2024.") +
  theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(face="bold", size = 14), axis.text.x = element_text(face="bold"), panel.grid.minor = element_blank())

# Composi??o percentual
df_resumo_percentual <- df_municipios_final %>%
  group_by(exercicio, origem) %>%
  summarise(valor_ano_fonte = sum(valor_real_final, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(participacao = valor_ano_fonte / sum(valor_ano_fonte)) %>%
  ungroup()

ggplot(df_resumo_percentual, aes(x = factor(exercicio), y = participacao, fill = origem)) +
  geom_col(position = position_stack(), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(participacao > 0.02, 
                               label_percent(accuracy = 0.1, decimal.mark = ",")(participacao), "")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 2.8) +
  scale_y_continuous(labels = label_percent(decimal.mark = ","), 
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = paleta_cores, na.translate = FALSE) +
  labs(title = "Composição Percentual do Investimento Cultural Municipal por Fonte",
       subtitle = "Participação Relativa das Fontes sobre o Investimento Real Empenhado (Base 2024)",
       x = "Ano de Execução", y = "Percentual (%)", fill = "Origem do Recurso",
       caption = "Fonte: MSC/SICONFI. Deflação: IPCA/SGS-BCB (2024).") +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        plot.title = element_text(face="bold", size = 14),
        axis.text.x = element_text(face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

# INVESTIMENTO REAL MUNICIPAL POR UF E FONTE (BASE 2024)
df_tabela_uf_real <- df_municipios_final %>%
  group_by(uf_sigla, origem, exercicio) %>%
  summarise(valor_total = sum(valor_real_final, na.rm = TRUE), .groups = "drop") %>%
  complete(uf_sigla, origem, exercicio = 2019:2025, fill = list(valor_total = 0)) %>%
  mutate(valor_mi = valor_total / 1e6) %>%
  select(uf_sigla, origem, exercicio, valor_mi) %>%
  pivot_wider(names_from = exercicio, values_from = valor_mi) %>%
  arrange(uf_sigla, origem)

colunas_anos <- as.character(2019:2025)

tabela_uf_final <- df_tabela_uf_real %>%
  gt(groupname_col = "uf_sigla") %>%
  tab_header(title = md("**Investimento Cultural Municipal Real por UF e Fonte de Recurso**"),
             subtitle = "Valores Empenhados em **R$ Milhões** | Corrigidos pela Inflação (2024)") %>%
  fmt_currency(
    columns = any_of(colunas_anos),
    currency = "BRL",
    dec_mark = ",",
    sep_mark = ".",
    decimals = 1,
    pattern = "{x} mi") %>%
  summary_rows(groups = TRUE,
               columns = any_of(colunas_anos),
               fns = list(SUBTOTAL = "sum"),
               formatter = fmt_currency,
               currency = "BRL",
               dec_mark = ",",
               sep_mark = ".",
               decimals = 1,
               pattern = "{x} mi") %>%
  cols_label(origem = "Fonte de Financiamento") %>%
  tab_options(row_group.font.weight = "bold",
              row_group.background.color = "#f2f4f4",
              summary_row.background.color = "#eaeded",
              table.font.size = px(11),
              heading.title.font.size = px(14),
              heading.subtitle.font.size = px(11),
              stub.border.color = "#d5dbdb",
              table.border.top.color = "#1b2631")

tabela_uf_final

################################### RECEITA CORRENTE LIQUIDA ###################################
path_pai_rcl <- "E:/Pedro Buril/Ministerio da Cultura/Cultura em Numeros/Eixo 1/Orcamento/SICONFI_MUNICIPAL"

arquivos_rcl <- list.files(path_pai_rcl, pattern = "^RCL_.*\\.csv$", full.names = TRUE)

df_rcl_pnc <- arquivos_rcl %>%
  map_dfr(function(arq) {
    ano_arq <- as.numeric(str_extract(basename(arq), "\\d{4}"))
    if (ano_arq >= 2019 & ano_arq <= 2024) {
      linhas_topo <- read_lines(arq, n_max = 30, locale = locale(encoding = "Latin1"))
      linha_header <- which(str_detect(linhas_topo, "Cod\\.IBGE|Institui"))
      if (length(linha_header) == 0) return(NULL)
      read_delim(arq, delim = ";", skip = linha_header - 1, 
                 locale = locale(encoding = "Latin1", decimal_mark = ","), 
                 show_col_types = FALSE) %>%
        clean_names() %>%
        filter(str_detect(tolower(conta), "receita corrente l.quida \\(iii\\)"),
               str_detect(tolower(coluna), "total \\(.ltimos 12 meses\\)")) %>%
        mutate(exercicio = ano_arq,
               valor_rcl = as.numeric(valor),
               nome_municipio = instituicao %>% 
                 str_remove(regex("^prefeitura municipal de ", ignore_case = TRUE)) %>% 
                 str_remove(regex("^prefeitura de ", ignore_case = TRUE)) %>% 
                 str_remove(regex("^prefeitura municipal ", ignore_case = TRUE)) %>% 
                 str_remove(regex("^governo municipal de ", ignore_case = TRUE)) %>% 
                 str_remove(regex(" - [A-Z]{2}$", ignore_case = TRUE)) %>% 
                 str_trim(),
               uf_chave = paste0(nome_municipio, "_", str_trim(uf))) %>%
        select(exercicio, uf_chave, valor_rcl)
    }
  })

df_cultura_proprio_pnc <- df_municipios_final %>%
  filter(origem == "Recurso Próprio (Municipal)") %>%
  mutate(uf_chave = paste0(municipio, "_", uf_sigla)) %>%
  group_by(exercicio, uf_chave) %>%
  summarise(total_cultura_proprio = sum(valor_nominal_final, na.rm = TRUE), .groups = "drop")

df_meta_pnc <- df_cultura_proprio_pnc %>%
  inner_join(df_rcl_pnc, by = c("exercicio", "uf_chave")) %>%
  separate(uf_chave, into = c("municipio", "uf_sigla"), sep = "_(?=[^_]+$)", fill = "right") %>%
  mutate(percentual_rcl = (total_cultura_proprio / valor_rcl) * 100,
         atingiu_meta = ifelse(percentual_rcl >= 2.0, 1, 0),
         regiao = case_when(uf_sigla %in% c("AM", "PA", "AC", "RO", "RR", "AP", "TO") ~ "Norte",
                            uf_sigla %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "Nordeste",
                            uf_sigla %in% c("MT", "MS", "GO", "DF") ~ "Centro-Oeste",
                            uf_sigla %in% c("SP", "RJ", "MG", "ES") ~ "Sudeste",
                            uf_sigla %in% c("PR", "SC", "RS") ~ "Sul"))

df_resumo_regioes <- df_meta_pnc %>%
  filter(!is.na(regiao)) %>%
  group_by(exercicio, regiao) %>%
  summarise(municipios_totais = n(),
            municipios_na_meta = sum(atingiu_meta),
            percentual_sucesso = (municipios_na_meta / municipios_totais) * 100,
            .groups = "drop")

ggplot(df_resumo_regioes, aes(x = factor(exercicio), y = percentual_sucesso, color = regiao, group = regiao)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.8) +
  geom_text(aes(label = label_number(suffix = "%", accuracy = 0.1, decimal.mark = ",")(percentual_sucesso)),
            vjust = -1.3, fontface = "bold", size = 2.8, show.legend = FALSE) +
  scale_y_continuous(labels = label_number(suffix = "%"), 
                     expand = expansion(mult = c(0.1, 0.3))) +
  scale_color_manual(values = c("Nordeste"     = "#27ae60", 
                                "Sudeste"      = "#1b2631", 
                                "Sul"          = "#8e44ad", 
                                "Norte"        = "#e67e22", 
                                "Centro-Oeste" = "#f1c40f")) +
  labs(title = "Evolução do Cumprimento da Meta do PNC por Região (2019-2024)",
       subtitle = "Percentual de Municípios com Gasto Cultural Próprio >= 2% da RCL",
       x = "Ano", y = "Municípios em Conformidade (%)", color = "Região Geográfica",
       caption = "Fonte: MSC/SICONFI. Diretriz: Diretrizes e Metas do Plano Nacional de Cultura (PNC).") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face="bold", size = 13),
        axis.text.x = element_text(face="bold"),
        panel.grid.minor = element_blank())
