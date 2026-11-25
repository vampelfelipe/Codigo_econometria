# =============================================================================
# 05_tabelas.R
# Exporta as tabelas formatadas do trabalho para Word (.docx):
#   1. tabelas/descritivas.docx  (estatísticas descritivas)
#   2. tabelas/resultados.docx   (modelos MQO 1 e 2)
#   3. tabelas/robustez.docx     (testes de robustez)
#
# Entradas : dados/base_final.rds, tabelas/descritivas.csv, tabelas/robustez.csv
# Pacotes  : dplyr, tidyr, modelsummary, flextable
# =============================================================================

library(dplyr)
library(tidyr)
library(modelsummary)
library(flextable)

options(OutDec = ",")   # vírgula como separador decimal

dir.create("tabelas", showWarnings = FALSE)

base <- readRDS("dados/base_final.rds") |>
  mutate(
    clube_valor = factor(clube_valor),
    minutos_mil = minutos / 1000
  )

# -----------------------------------------------------------------------------
# 1. Tabela de estatísticas descritivas (a partir de tabelas/descritivas.csv)
# -----------------------------------------------------------------------------
tab_desc <- read.csv("tabelas/descritivas.csv", check.names = FALSE)

ft_desc <- flextable(tab_desc) |>
  colformat_int(j = 2, big.mark = ".") |>
  colformat_double(j = 3:7, decimal.mark = ",", big.mark = ".", digits = 2) |>
  set_caption("Estatísticas descritivas das variáveis do modelo (Série A 2024)") |>
  add_footer_lines(
    "Fonte: elaboração própria, a partir de bases públicas do Kaggle (FBref e Transfermarkt). Estrangeiro é variável binária (1 = estrangeiro; 0 = brasileiro)."
  ) |>
  theme_booktabs() |>
  fontsize(size = 9, part = "all") |>
  autofit()

save_as_docx(ft_desc, path = "tabelas/descritivas.docx")

# -----------------------------------------------------------------------------
# 2. Tabela principal de resultados (Modelos 1 e 2)
# -----------------------------------------------------------------------------
f1 <- log_valor ~ gols + assistencias + estrangeiro +
  idade + idade2 + minutos_mil + posicao
m1 <- lm(f1, data = base)                                  # sem clube
m2 <- lm(update(f1, . ~ . + clube_valor), data = base)     # com clube

nomes <- c(
  "gols"                 = "Gols",
  "assistencias"         = "Assistências",
  "estrangeiro"          = "Estrangeiro (1 = sim)",
  "idade"                = "Idade",
  "idade2"               = "Idade ao quadrado",
  "minutos_mil"          = "Minutos jogados (milhares)",
  "posicaoMeio-campista" = "Meio-campista",
  "posicaoAtacante"      = "Atacante",
  "(Intercept)"          = "Constante"
)

gof <- data.frame(
  raw   = c("nobs", "r.squared", "adj.r.squared"),
  clean = c("Observações", "R²", "R² ajustado"),
  fmt   = c(0, 3, 3)
)

linha_clube <- data.frame(
  termo = "Efeitos fixos de clube",
  mod1  = "Não",
  mod2  = "Sim"
)

tabela <- modelsummary(
  list("(1) Sem clube" = m1, "(2) Com clube" = m2),
  vcov     = "HC1",
  coef_map = nomes,
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map  = gof,
  add_rows = linha_clube,
  title    = "Determinantes do valor de mercado dos jogadores da Série A 2024 (MQO)",
  notes    = c(
    "Variável dependente: logaritmo do valor de mercado (Transfermarkt, em euros).",
    "Erros-padrão robustos (HC1) entre parênteses. Categoria de referência da posição: defensor.",
    "Fonte: elaboração própria, a partir de bases públicas do Kaggle."
  ),
  output   = "flextable"
)

save_as_docx(tabela, path = "tabelas/resultados.docx")

# -----------------------------------------------------------------------------
# 3. Tabela de testes de robustez (a partir de tabelas/robustez.csv)
# -----------------------------------------------------------------------------
tab_rob <- read.csv("tabelas/robustez.csv") |>
  mutate(
    estrelas = case_when(
      p < 0.01 ~ "***",
      p < 0.05 ~ "**",
      p < 0.10 ~ "*",
      TRUE     ~ ""
    ),
    celula = paste0(
      formatC(pct, format = "f", digits = 1, decimal.mark = ","),
      "%",
      estrelas
    ),
    variavel = case_when(
      variavel == "gols"         ~ "Gols (+1)",
      variavel == "assistencias" ~ "Assistências (+1)",
      variavel == "estrangeiro"  ~ "Estrangeiro",
      variavel == "minutos_mil"  ~ "Minutos (+1.000)"
    )
  ) |>
  select(Especificação = teste, N = n, variavel, celula) |>
  pivot_wider(names_from = variavel, values_from = celula)

ft_rob <- flextable(tab_rob) |>
  colformat_int(j = "N", big.mark = ".") |>
  set_caption("Testes de robustez: variação percentual estimada no valor de mercado") |>
  add_footer_lines(c(
    "Nota: valores correspondem a 100 × (exp(β) - 1), com erros-padrão HC1 (exceto na linha de erros agrupados por clube). * p < 0,10, ** p < 0,05, *** p < 0,01.",
    "Fonte: elaboração própria, a partir de bases públicas do Kaggle."
  )) |>
  theme_booktabs() |>
  fontsize(size = 9, part = "all") |>
  autofit()

save_as_docx(ft_rob, path = "tabelas/robustez.docx")

message("Tabelas salvas em tabelas/descritivas.docx, tabelas/resultados.docx e tabelas/robustez.docx")