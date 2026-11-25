# 04_regressoes.R - Regressões MQO, diagnósticos e testes de robustez

library(dplyr)
library(tidyr)
library(sandwich)
library(lmtest)
library(car)

base <- readRDS("dados/base_final.rds") |>
  mutate(
    clube_valor = factor(clube_valor),
    minutos_mil = minutos / 1000
  )

# 1. Estimação principal (sem e com efeitos fixos de clube)
f1 <- log_valor ~ gols + assistencias + estrangeiro +
  idade + idade2 + minutos_mil + posicao
m1 <- lm(f1, data = base)

f2 <- update(f1, . ~ . + clube_valor)
m2 <- lm(f2, data = base)

# Erros-padrão robustos (HC1)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))[1:9, ]

c(n = nobs(m1), r2 = summary(m1)$r.squared, r2_adj = summary(m1)$adj.r.squared)
c(n = nobs(m2), r2 = summary(m2)$r.squared, r2_adj = summary(m2)$adj.r.squared)

# 2. Diagnósticos
bptest(m1)
bptest(m2)
vif(m1)
vif(m2)

# Significância conjunta das dummies de clube
dummies_clube <- grep("^clube_valor", names(coef(m2)), value = TRUE)
linearHypothesis(m2, dummies_clube, vcov. = vcovHC(m2, type = "HC1"))

# Ponto de máximo da idade
c(
  pico_idade_m1 = -coef(m1)["idade"] / (2 * coef(m1)["idade2"]),
  pico_idade_m2 = -coef(m2)["idade"] / (2 * coef(m2)["idade2"])
)

# 3. Testes de robustez
alvo <- c("gols", "assistencias", "estrangeiro", "minutos_mil")

# Converte coeficientes log-nível em variação percentual exata: 100 * (exp(b) - 1)
resume <- function(m, vc) {
  ct <- coeftest(m, vcov = vc)[alvo, ]
  b  <- ct[, "Estimate"]
  data.frame(
    pct = round(100 * (exp(b) - 1), 1),
    p   = round(ct[, "Pr(>|t|)"], 3)
  )
}

r_m1 <- resume(m1, vcovHC(m1, type = "HC1"))
r_m2 <- resume(m2, vcovHC(m2, type = "HC1"))

# Cortes alternativos de minutos
m_r1a <- lm(f2, data = filter(base, minutos >= 450))
r1a   <- resume(m_r1a, vcovHC(m_r1a, type = "HC1"))

m_r1b <- lm(f2, data = filter(base, minutos >= 900))
r1b   <- resume(m_r1b, vcovHC(m_r1b, type = "HC1"))

# Exclusão dos 3 maiores valores de mercado
base2 <- base |> arrange(desc(`Market Value`)) |> slice(-(1:3))
m_r2  <- lm(f2, data = base2)
r2    <- resume(m_r2, vcovHC(m_r2, type = "HC1"))

# Erros-padrão agrupados por clube
r3 <- resume(m2, vcovCL(m2, cluster = base$clube_valor))

# Regressões por posição (sem dummies de clube)
f_pos <- update(f1, . ~ . - posicao)
m_pos <- lapply(levels(base$posicao),
                function(g) lm(f_pos, data = filter(base, posicao == g)))
names(m_pos) <- levels(base$posicao)
r4 <- lapply(m_pos, function(m) resume(m, vcovHC(m, type = "HC1")))

junta <- function(r, nome, n) {
  data.frame(teste = nome, variavel = rownames(r), r, n = n, row.names = NULL)
}

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

tab_rob |> select(teste, variavel, pct) |>
  pivot_wider(names_from = variavel, values_from = pct) |> print()
tab_rob |> select(teste, variavel, p) |>
  pivot_wider(names_from = variavel, values_from = p) |> print()
tab_rob |> distinct(teste, n) |> print()

dir.create("tabelas", showWarnings = FALSE)
write.csv(tab_rob, "tabelas/robustez.csv", row.names = FALSE)