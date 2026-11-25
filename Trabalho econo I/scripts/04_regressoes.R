library(dplyr)
library(sandwich)   # erros-padrão robustos
library(lmtest)     # coeftest e teste de Breusch-Pagan
library(car)        # vif

base <- readRDS("dados/base_final.rds")
base$clube_valor <- factor(base$clube_valor)

# Modelo 1: sem clube
f1 <- log_valor ~ gols + assistencias + estrangeiro +
  idade + idade2 + minutos + posicao
m1 <- lm(f1, data = base)

# Modelo 2: com clube (20 dummies)
m2 <- lm(update(f1, . ~ . + clube_valor), data = base)

# Coeficientes com erros-padrão robustos (HC1)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))[1:9, ]

# Ajuste e diagnósticos
c(nobs(m1), summary(m1)$r.squared, summary(m1)$adj.r.squared)
c(nobs(m2), summary(m2)$r.squared, summary(m2)$adj.r.squared)
bptest(m1)
vif(m1)

# ============================================================
# ROBUSTEZ
# Pergunta de cada teste: o resultado do modelo 2 (com clube)
# muda se fizermos outra escolha razoável?
# ============================================================
library(tidyr)

f2   <- update(f1, . ~ . + clube_valor)   # fórmula do modelo 2
alvo <- c("gols", "assistencias", "estrangeiro", "minutos")

# Converte os coeficientes em variação % do valor de mercado e mostra o p-valor.
# Gols, assistências e estrangeiro: variação % por unidade (ou da dummy).
# Minutos: variação % por 1.000 minutos.
resume <- function(m, vc) {
  ct <- coeftest(m, vcov = vc)[alvo, ]
  b  <- ct[, "Estimate"]
  data.frame(
    pct = round(100 * (exp(c(b[1:3], b[4] * 1000)) - 1), 1),
    p   = round(ct[, "Pr(>|t|)"], 3)
  )
}

# Referências: modelo 1 (sem clube) e modelo 2 (com clube, principal)
r_m1 <- resume(m1, vcovHC(m1, type = "HC1"))
r_m2 <- resume(m2, vcovHC(m2, type = "HC1"))

# Teste 1: só jogadores com minutos mínimos maiores (menos ruído nas estatísticas)
m_r1a <- lm(f2, data = filter(base, minutos >= 450))
r1a   <- resume(m_r1a, vcovHC(m_r1a, type = "HC1"))

m_r1b <- lm(f2, data = filter(base, minutos >= 900))
r1b   <- resume(m_r1b, vcovHC(m_r1b, type = "HC1"))

# Teste 2: sem os 3 jogadores de maior valor de mercado (valores extremos)
base2 <- base |> arrange(desc(`Market Value`)) |> slice(-(1:3))
m_r2  <- lm(f2, data = base2)
r2    <- resume(m_r2, vcovHC(m_r2, type = "HC1"))

# Teste 3: erros-padrão agrupados por clube (jogadores do mesmo clube
# não são independentes). O modelo é o mesmo; só os p-valores mudam.
r3 <- resume(m2, vcovCL(m2, cluster = base$clube_valor))

# Teste 4: uma regressão por posição, sem dummies de clube
# (são 20 clubes para 120 a 160 jogadores por grupo)
f_pos <- update(f1, . ~ . - posicao)
m_pos <- lapply(levels(base$posicao),
                function(g) lm(f_pos, data = filter(base, posicao == g)))
names(m_pos) <- levels(base$posicao)
r4 <- lapply(m_pos, function(m) resume(m, vcovHC(m, type = "HC1")))

# Junta tudo em uma tabela
junta <- function(r, nome, n)
  data.frame(teste = nome, variavel = rownames(r), r, n = n, row.names = NULL)

tab_rob <- rbind(
  junta(r_m1, "Modelo 1 (sem clube)",         nobs(m1)),
  junta(r_m2, "Modelo 2 (principal)",         nobs(m2)),
  junta(r1a,  "Minutos >= 450",               nobs(m_r1a)),
  junta(r1b,  "Minutos >= 900",               nobs(m_r1b)),
  junta(r2,   "Sem os 3 maiores valores",     nobs(m_r2)),
  junta(r3,   "Erros agrupados por clube",    nobs(m2)),
  junta(r4[["Defensor"]],      "Só defensores (sem clube)",     nobs(m_pos[["Defensor"]])),
  junta(r4[["Meio-campista"]], "Só meio-campistas (sem clube)", nobs(m_pos[["Meio-campista"]])),
  junta(r4[["Atacante"]],      "Só atacantes (sem clube)",      nobs(m_pos[["Atacante"]]))
)

# Duas visões compactas: variação % e p-valor, uma linha por teste
tab_rob |> select(teste, variavel, pct) |>
  pivot_wider(names_from = variavel, values_from = pct) |> print()
tab_rob |> select(teste, variavel, p) |>
  pivot_wider(names_from = variavel, values_from = p) |> print()
tab_rob |> distinct(teste, n) |> print()

dir.create("tabelas", showWarnings = FALSE)
write.csv(tab_rob, "tabelas/robustez.csv", row.names = FALSE)