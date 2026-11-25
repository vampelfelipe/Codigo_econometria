# =============================================================================
# 05_tabelas.R
# Tabela final de resultados (modelos 1 e 2) exportada para Word.
#
# Entrada : dados/base_final.rds
# Saída   : tabelas/resultados.docx
#
# Pacotes (instalar uma vez): install.packages(c("modelsummary", "flextable"))
# =============================================================================

library(dplyr)
library(modelsummary)
library(flextable)

options(OutDec = ",")   # vírgula como separador decimal

base <- readRDS("dados/base_final.rds")
base$clube_valor <- factor(base$clube_valor)

# Mesmos modelos do script 04
f1 <- log_valor ~ gols + assistencias + estrangeiro +
  idade + idade2 + minutos + posicao
m1 <- lm(f1, data = base)                                  # sem clube
m2 <- lm(update(f1, . ~ . + clube_valor), data = base)     # com clube

# Nomes das variáveis na tabela. A ordem aqui é a ordem da tabela.
# Os coeficientes das dummies de clube não aparecem: ficam indicados na linha
# "Efeitos fixos de clube".
nomes <- c(
  "gols"                 = "Gols",
  "assistencias"         = "Assistências",
  "estrangeiro"          = "Estrangeiro (1 = sim)",
  "idade"                = "Idade",
  "idade2"               = "Idade ao quadrado",
  "minutos"              = "Minutos jogados",
  "posicaoMeio-campista" = "Meio-campista",
  "posicaoAtacante"      = "Atacante",
  "(Intercept)"          = "Constante"
)

# Estatísticas de ajuste mostradas no rodapé da tabela
gof <- data.frame(
  raw   = c("nobs", "r.squared", "adj.r.squared"),
  clean = c("Observações", "R²", "R² ajustado"),
  fmt   = c(0, 3, 3)
)

# Linha extra indicando em quais modelos o clube entra
linha_clube <- data.frame(
  termo = "Efeitos fixos de clube",
  mod1  = "Não",
  mod2  = "Sim"
)

dir.create("tabelas", showWarnings = FALSE)

# A tabela é montada como objeto flextable e gravada em Word pelo flextable.
# (Gravar o .docx direto pelo modelsummary exige o pandoc.)
tabela <- modelsummary(
  list("(1) Sem clube" = m1, "(2) Com clube" = m2),
  vcov     = "HC1",                                   # erros-padrão robustos
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

message("Tabela salva em tabelas/resultados.docx")