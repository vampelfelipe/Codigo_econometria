library(dplyr)
library(tidyr)
library(ggplot2)

base <- readRDS("dados/base_final.rds")
dir.create("tabelas", showWarnings = FALSE)
dir.create("figuras", showWarnings = FALSE)

# 1. Tabela descritiva das variáveis do modelo
vars <- base |>
  transmute(
    `Valor de mercado (€)` = `Market Value`,
    `log(valor de mercado)` = log_valor,
    Gols = gols,
    `Assistências` = assistencias,
    Minutos = minutos,
    Idade = idade,
    Estrangeiro = estrangeiro
  )

tab_desc <- vars |>
  pivot_longer(everything(), names_to = "Variável", values_to = "x") |>
  group_by(Variável) |>
  summarise(N = n(), Média = mean(x), `Desvio-padrão` = sd(x),
            Mínimo = min(x), Mediana = median(x), Máximo = max(x),
            .groups = "drop") |>
  arrange(factor(Variável, levels = names(vars)))

print(tab_desc)
write.csv(tab_desc, "tabelas/descritivas.csv", row.names = FALSE)

# 2. Por posição e por nacionalidade
tab_posicao <- base |>
  group_by(posicao) |>
  summarise(N = n(),
            `Valor mediano (€)` = median(`Market Value`),
            `Gols (média)` = mean(gols),
            `Minutos (média)` = mean(minutos),
            `% estrangeiros` = 100 * mean(estrangeiro))
print(tab_posicao)

tab_nac <- base |>
  group_by(estrangeiro) |>
  summarise(N = n(),
            `Valor mediano (€)` = median(`Market Value`),
            `Idade (média)` = mean(idade))
print(tab_nac)

write.csv(tab_posicao, "tabelas/por_posicao.csv", row.names = FALSE)
write.csv(tab_nac, "tabelas/por_nacionalidade.csv", row.names = FALSE)

# 3. Gráficos
g1 <- ggplot(base, aes(x = `Market Value` / 1e6)) +
  geom_histogram(bins = 30) +
  labs(x = "Valor de mercado (€ milhões)", y = "Jogadores")

g2 <- ggplot(base, aes(x = log_valor)) +
  geom_histogram(bins = 30) +
  labs(x = "log(valor de mercado)", y = "Jogadores")

g3 <- ggplot(base, aes(x = idade, y = log_valor)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  labs(x = "Idade (anos)", y = "log(valor de mercado)")

ggsave("figuras/hist_valor.png", g1, width = 6, height = 4, dpi = 300)
ggsave("figuras/hist_log_valor.png", g2, width = 6, height = 4, dpi = 300)
ggsave("figuras/idade_vs_valor.png", g3, width = 6, height = 4, dpi = 300)