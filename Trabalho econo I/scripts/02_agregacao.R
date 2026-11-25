# =============================================================================
# 02_agregacao.R
# Constrói a base final (um jogador por clube) a partir das duas bases brutas.
#
# Entradas : dados/estatisticas_brutas.rds e dados/valor_bruto.rds (script 01)
#            dados/dicionario_nomes.csv (correções manuais de nomes)
# Saídas   : dados/base_final.rds e dados/base_final.csv
# =============================================================================

# Decisões sobre as variáveis
#   Gols, assistências e minutos: soma das partidas do jogador no clube
#   Unidade de observação: jogador + clube
#   jogos: número de partidas do jogador em determinado clube
#   Todas as 25 colunas numéricas das estatísticas são somadas;
#   Cmp% é retirado antes da soma (é um percentual)
#   Nacionalidade: vem da base de estatísticas (coluna Nação)
#   Idade e posição: vêm da base de valor de mercado
#   Variável estrangeiro: criada no fim do script (nacionalidade != "BRA")

library(readr)
library(dplyr)
library(stringi)

estatisticas <- readRDS("dados/estatisticas_brutas.rds")
valor        <- readRDS("dados/valor_bruto.rds")

estat_jogador <- estatisticas |>
  select(-`Cmp%`) |>
  group_by(Jogador, Time) |>
  summarise(
    jogos = n(),
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    nacionalidade = first(Nação),
    .groups = "drop"
  )
dim(estat_jogador)   # esperado: 725 linhas x 29 colunas

# -----------------------------------------------------------------------------
# 2. Padronizar nomes dos clubes
# -----------------------------------------------------------------------------

sort(unique(estat_jogador$Time))
sort(unique(valor$Team))

# dicionário: nome do clube na base de estatísticas -> nome na base de valor
dicionario_clubes <- tibble(
  clube_estat = c("Athletico Paranaense", "Atlético Goianiense",
                  "Atlético Mineiro",     "Bahia",
                  "Botafogo (RJ)",        "Corinthians",
                  "Criciúma",             "Cruzeiro",
                  "Cuiabá",               "Flamengo",
                  "Fluminense",           "Fortaleza",
                  "Grêmio",               "Internacional",
                  "Juventude",            "Palmeiras",
                  "Red Bull Bragantino",  "São Paulo",
                  "Vasco da Gama",        "Vitória"),
  
  clube_valor = c("Athletico",           "Atletico Goianiense",
                  "Atletico Mineiro",    "Bahia",
                  "Botafogo",            "Corinthians",
                  "Criciuma",            "Cruzeiro",
                  "Cuiaba",              "Flamengo",
                  "Fluminense",          "Fortaleza",
                  "Gremio",              "Internacional",
                  "Juventude",           "Palmeiras",
                  "Red Bull Bragantino", "Sao Paulo",
                  "Vasco Da Gama",       "Vitoria")
)

# colar o dicionário na base
estat_jogador <- estat_jogador |>
  left_join(dicionario_clubes, by = c("Time" = "clube_estat"))

# todos os clubes precisam ter correspondência (esperado: 0 NA)
stopifnot(sum(is.na(estat_jogador$clube_valor)) == 0)

# -----------------------------------------------------------------------------
# 3. Padronizar nomes dos jogadores (minúsculas, sem acento, sem espaços extras)
# -----------------------------------------------------------------------------

estat_jogador <- estat_jogador |>
  mutate(nome = trimws(tolower(stri_trans_general(Jogador, "Latin-ASCII"))))

valor <- valor |>
  mutate(nome = trimws(tolower(stri_trans_general(Player, "Latin-ASCII"))))

# =============================================================================
# CRITÉRIOS DE AMOSTRA (definidos pelo grupo em 19/11/2026)
# =============================================================================

# 1. Goleiros: excluídos (nas duas bases).
#    Na base de estatísticas eles saem no pareamento, pois não existem na
#    base de valor depois deste filtro.

valor <- valor |>
  filter(Position != "Goalkeeper")

nrow(valor)   # esperado: 574

# 2. Minutos: mínimo de 180 minutos jogados (equivalente a duas partidas
#    completas). Robustez: reestimar com outro limite (script 04).

estat_jogador <- estat_jogador |>
  filter(Min. >= 180)

nrow(estat_jogador)   # esperado: 567

# 3. Valor de mercado ausente (NA): jogador excluído.

valor <- valor |>
  filter(!is.na(`Market Value`))

nrow(valor)   # esperado: 564

# 4. Nacionalidade ausente: o único caso (Felipe Vieira, Fluminense) foi
#    excluído pelo filtro de 180 minutos; nenhuma correção manual necessária.

# 5. Duplicatas na base de valor:
#    - Adriel (Bahia): linha idêntica repetida -> removida com distinct().
#    - Vitinho (Red Bull Bragantino): dois jogadores diferentes com o mesmo
#      nome -> separados manualmente pela idade.

valor <- valor |> distinct()

valor <- valor |>
  distinct(Player, Team, Age, Position, `Market Value`,
           .keep_all = TRUE)

valor <- valor |>
  mutate(nome = if_else(
    nome == "vitinho" & Team == "Red Bull Bragantino" & Age == 26,
    "vitinho (outro)",
    nome
  ))

nrow(valor)   # esperado: 562

# 6. Jogadores transferidos durante a temporada: usado o clube da base de
#    valor; consideradas apenas as estatísticas nesse clube.
#    (tratado automaticamente na junção por nome + clube: só casa a linha do
#    clube registrado na base de valor)

# 7. Nomes sem correspondência entre as bases: corrigidos manualmente os
#    jogadores com mais de 2.000 minutos; os demais excluídos.

sem_par <- anti_join(estat_jogador, valor,
                     by = c("nome" = "nome", "clube_valor" = "Team")) |>
  filter(`Min.` > 2000) |>
  arrange(desc(`Min.`))

# As correções feitas à mão ficam em dados/dicionario_nomes.csv, com as
# colunas Jogador (nome na base de estatísticas), valor (nome na base de
# valor) e clube (clube na base de valor, igual à coluna Team).

dicionario_nomes <- read_delim("dados/dicionario_nomes.csv", delim = ";",
                               show_col_types = FALSE) |>
  rename(nome_estat = Jogador, nome_valor = valor, clube_valor = clube) |>
  filter(!is.na(nome_valor)) |>
  mutate(across(c(nome_estat, nome_valor),
                ~ trimws(tolower(stri_trans_general(.x, "Latin-ASCII")))))

# Validação: toda correção precisa apontar para um jogador que exista na base
# de valor (nome E clube). Se algo estiver errado, o script mostra o problema
# e para aqui, em vez de descartar a correção em silêncio.
erros_dicionario <- anti_join(dicionario_nomes, valor,
                              by = c("nome_valor" = "nome",
                                     "clube_valor" = "Team"))
if (nrow(erros_dicionario) > 0) print(erros_dicionario)
stopifnot(nrow(erros_dicionario) == 0)

estat_jogador <- estat_jogador |>
  left_join(dicionario_nomes, by = c("nome" = "nome_estat",
                                     "clube_valor" = "clube_valor")) |>
  mutate(nome = coalesce(nome_valor, nome)) |>
  select(-nome_valor)

# -----------------------------------------------------------------------------
# 4. Juntando tudo na base final
# -----------------------------------------------------------------------------

base_final <- inner_join(estat_jogador, valor,
                         by = c("nome" = "nome",
                                "clube_valor" = "Team"))

dim(base_final)   # esperado: 410 linhas x 36 colunas

# cada jogador deve aparecer uma única vez por clube
stopifnot(anyDuplicated(base_final[, c("nome", "clube_valor")]) == 0)

# -----------------------------------------------------------------------------
# 5. Variáveis do modelo
# -----------------------------------------------------------------------------

base_final <- base_final |>
  mutate(
    log_valor    = log(`Market Value`),
    gols         = Gols,
    assistencias = Assis.,
    minutos      = Min.,
    idade        = Age,
    idade2       = Age^2,
    estrangeiro  = as.integer(nacionalidade != "BRA"),   # 1 = estrangeiro
    posicao = case_when(
      Position %in% c("Center Back", "Left Back", "Right Back") ~ "Defensor",
      Position %in% c("Defensive Midfield", "Central Midfield",
                      "Attacking Midfield") ~ "Meio-campista",
      Position %in% c("Left Winger", "Right Winger",
                      "Centre Forward") ~ "Atacante",
      # Posição "Unknown" na base de valor: classificado à mão como atacante,
      # pois a posição está escrita no próprio nome ("Seg. Atacante").
      Position == "Unknown" & nome == "luciano seg. atacante" ~ "Atacante"
    ),
    posicao = factor(posicao,
                     levels = c("Defensor", "Meio-campista", "Atacante"))
  )

# nenhuma posição pode ficar sem classificação
stopifnot(!anyNA(base_final$posicao))

saveRDS(base_final, "dados/base_final.rds")
write.csv(base_final, "dados/base_final.csv", row.names = FALSE)

message("base_final: ", nrow(base_final), " linhas x ",
        ncol(base_final), " colunas")