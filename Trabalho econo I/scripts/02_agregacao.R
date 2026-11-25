# 02_agregacao.R - Agregação por jogador/clube, filtros amostrais e junção das bases

library(readr)
library(dplyr)
library(stringi)

estatisticas <- readRDS("dados/estatisticas_brutas.rds")
valor        <- readRDS("dados/valor_bruto.rds")

# 1. Agregação por jogador + clube (Cmp% excluído por ser percentual)
estat_jogador <- estatisticas |>
  select(-`Cmp%`) |>
  group_by(Jogador, Time) |>
  summarise(
    jogos = n(),
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    nacionalidade = first(Nação),
    .groups = "drop"
  )
dim(estat_jogador)

# 2. Padronização dos nomes dos clubes
sort(unique(estat_jogador$Time))
sort(unique(valor$Team))

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

estat_jogador <- estat_jogador |>
  left_join(dicionario_clubes, by = c("Time" = "clube_estat"))

stopifnot(sum(is.na(estat_jogador$clube_valor)) == 0)

# 3. Padronização dos nomes dos jogadores
estat_jogador <- estat_jogador |>
  mutate(nome = trimws(tolower(stri_trans_general(Jogador, "Latin-ASCII"))))

valor <- valor |>
  mutate(nome = trimws(tolower(stri_trans_general(Player, "Latin-ASCII"))))

# 4. Critérios de amostra

# Excluir goleiros (na base de estatísticas saem no inner_join)
valor <- valor |>
  filter(Position != "Goalkeeper")
nrow(valor)

# Mínimo de 180 minutos jogados (também remove o único NA de nacionalidade: Felipe Vieira)
estat_jogador <- estat_jogador |>
  filter(Min. >= 180)
nrow(estat_jogador)

# Excluir jogadores sem valor de mercado
valor <- valor |>
  filter(!is.na(`Market Value`))
nrow(valor)

# Duplicatas na base de valor:
# Adriel (Bahia) já saiu no filtro de goleiros;
# Vitinho (RB Bragantino) tinha linhas duplicadas e dois homônimos (separados pela idade)
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
nrow(valor)

# Jogadores sem par com mais de 2.000 minutos: corrigidos via dados/dicionario_nomes.csv
sem_par <- anti_join(estat_jogador, valor,
                     by = c("nome" = "nome", "clube_valor" = "Team")) |>
  filter(`Min.` > 2000) |>
  arrange(desc(`Min.`))

dicionario_nomes <- read_delim("dados/dicionario_nomes.csv", delim = ";",
                               show_col_types = FALSE) |>
  rename(nome_estat = Jogador, nome_valor = valor, clube_valor = clube) |>
  filter(!is.na(nome_valor)) |>
  mutate(across(c(nome_estat, nome_valor),
                ~ trimws(tolower(stri_trans_general(.x, "Latin-ASCII")))))

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

# 5. Junção final (transferidos mantêm apenas o clube registrado na base de valor)
base_final <- inner_join(estat_jogador, valor,
                         by = c("nome" = "nome",
                                "clube_valor" = "Team"))
dim(base_final)
stopifnot(anyDuplicated(base_final[, c("nome", "clube_valor")]) == 0)

# 6. Criação das variáveis do modelo
base_final <- base_final |>
  mutate(
    log_valor    = log(`Market Value`),
    gols         = Gols,
    assistencias = Assis.,
    minutos      = Min.,
    idade        = Age,
    idade2       = Age^2,
    estrangeiro  = as.integer(nacionalidade != "BRA"),
    posicao = case_when(
      Position %in% c("Center Back", "Left Back", "Right Back") ~ "Defensor",
      Position %in% c("Defensive Midfield", "Central Midfield",
                      "Attacking Midfield") ~ "Meio-campista",
      Position %in% c("Left Winger", "Right Winger",
                      "Centre Forward") ~ "Atacante",
      # Luciano aparece como "Unknown", mas tem "Seg. Atacante" no campo Player
      Position == "Unknown" & nome == "luciano seg. atacante" ~ "Atacante"
    ),
    posicao = factor(posicao,
                     levels = c("Defensor", "Meio-campista", "Atacante"))
  )

stopifnot(!anyNA(base_final$posicao))

saveRDS(base_final, "dados/base_final.rds")
write.csv(base_final, "dados/base_final.csv", row.names = FALSE)

message("base_final: ", nrow(base_final), " linhas x ",
        ncol(base_final), " colunas")