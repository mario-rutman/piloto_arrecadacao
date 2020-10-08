library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(readxl)
library(ggplot2)

# Importando o arquivo.

arrec_ger_17_a_20_raw <- read_excel("~/R projetos no Acer Aspire 3/piloto_arrecadacao/arrecadacao_geral_2017_a_2020.xlsx", 
                                            col_types = c("date", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric"))


write_rds(arrec_ger_17_a_20_raw, "arrec_ger_17_a_20_raw.rds")

arrec_ger_17_a_20_processada <- arrec_ger_17_a_20_raw %>%
  # Fazendo o pivot_longer.
  pivot_longer(-referencia, names_to = "tipo_de_arrecadacao",
               values_to = "valor") %>%
  # Criando a coluna com número do mês, nome do mês,.
  mutate(mes = month(referencia, locale = Sys.getlocale("LC_TIME")),
         nome_mes = month(referencia, label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME"))) %>%
  # Criando a coluna com número do ano.
  mutate(ano = year(referencia)) %>%
  # Criando duas colunas de trimestre.
  mutate(trimestre = quarter(referencia)) %>%
  mutate(nome_trimestre = case_when(
    trimestre == 1 ~ "1º trimestre",
    trimestre == 2 ~ "2º trimestre",
    trimestre == 3 ~ "3º trimestre",
    TRUE ~ "4º trimestre"))

write_rds(arrec_ger_17_a_20_processada, "arrec_ger_17_a_20_processada.rds")

# Dá a arrecadação em bilhões de R$.
arrec_tot <- sum((arrec_ger_17_a_20_processada %>% filter(ano == 2020))$valor, na.rm = TRUE)/1e9

# O objetivo agora é medir a diferença percentual entre as arrecadações 
# de 2020 e 2019, 2020 e 2018, 2020 e 2017.
# Desse modo serão 3 gráficos, um para cada comparação.

serie_temp_arrec <- arrec_ger_17_a_20_processada %>% #Selecionando as colunas.
  select(c(6,4,3)) %>%  #Agrupando para somar.
  group_by(ano, mes) %>% 
  summarise(tot_mes = sum(valor)) %>% ungroup() %>% 
  # Fazendo o pivot-wider para distribuir os valores por coluna de ano.
  pivot_wider(names_from = ano, values_from = tot_mes) %>%
  # Criando as colunas de acumulado
  mutate(acum_17 = cumsum(`2017`), acum_18 = cumsum(`2018`),
         acum_19 = cumsum(`2019`), acum_20 = cumsum(`2020`)) %>% 
  select(c(1,6,7,8,9)) %>% # Calculando as variações percentuais.
  mutate(Var_percentual_2017 = (acum_20-acum_17)/acum_20,
         Var_percentual_2018 = (acum_20-acum_18)/acum_20,
         Var_percentual_2019 = (acum_20-acum_19)/acum_20) %>% 
  select(c(1,6,7,8)) %>% 
  pivot_longer(!mes, names_to = "var_percentual", values_to = "percentual")

write_rds(serie_temp_arrec, "serie_temp_arrec.rds")
  
