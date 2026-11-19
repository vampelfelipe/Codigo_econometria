# 1. o que fazer com as variáveis? 

#   Gols: Soma
#   Assistências: Soma
#   Minutos jogados: Soma
#   Total de cartões: Soma
#   Nacionalidade: pendente (será uma dummy brasileiros x estrangeiros)
#   Idade: Idade da base de valor de mercado
#   Posição: Posição da base de valor de mercado
#   Unidade de observação: Jogador + Time
#   jogos:  soma do número de partidas jogadas por um jogador emd eterminado clube
#   Todas as 25 colunas numéricas foram somadas
#   Cmp% retirado antes da soma (é percentual)

library(dplyr)
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
dim(estat_jogador)

# 2. padronizar nomes dos clubes

sort(unique(estat_jogador$Time))
sort(unique(valor$Team))

# criar um dicionário
dicionario_clubes <- tibble(
  clube_estat = c("Athletico Paranaense", "Atlético Goianiense", 
                  "Atlético Mineiro",     "Bahia",              
                  "Botafogo (RJ)",       "Corinthians",       
                  "Criciúma",            "Cruzeiro",            
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


#colar o dicionário na base

estat_jogador <- estat_jogador |>
  left_join(dicionario_clubes, by = 
    c("Time" = "clube_estat"))

sum(is.na(estat_jogador$clube_valor))

# arrumar os nomes dos jogadores

library(stringi)

estat_jogador <- estat_jogador |>
  mutate(nome = 
    trimws(tolower(stri_trans_general(Jogador, "Latin-ASCII"))))

valor <- valor |>
  mutate(nome = 
    trimws(tolower(stri_trans_general(Player, "Latin-ASCII"))))


View(anti_join(estat_jogador, valor,
   by = c("nome"  = "nome",
    "clube_valor" = "Team")))

# =============================================================================
# CRITÉRIOS DE AMOSTRA (definidos pelo grupo em 19/11/2026)
# =============================================================================

# 1. Goleiros: excluídos (nas duas bases).

valor <- valor |>
  filter(Position != "Goalkeeper")

nrow(valor)


# 2. Minutos: mínimo de 180 minutos jogados (equivalente a duas partidas
#    completas). Robustez: reestimar com outro limite.

estat_jogador <- estat_jogador |>
  filter(Min. >= 180)

nrow(estat_jogador)

# 3. Valor de mercado ausente (NA): jogador excluído.

valor <- valor |>
  filter(!is.na(`Market Value`))

nrow(valor)

# 4. Nacionalidade ausente: o único caso (Felipe Vieira, Fluminense) foi
# excluído pelo filtro de 180 minutos; nenhuma correção manual necessária.


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

nrow(valor)

# 6. Jogadores transferidos durante a temporada: usado o clube da base de
#    valor; consideradas apenas as estatísticas nesse clube.
#    (tratado automaticamente na junção por
#    nome + clube (só casa a linha do clube registrado na base de valor))


# 7. Nomes sem correspondência entre as bases: corrigidos manualmente os
#    jogadores com mais de 2.000 minutos; os demais excluídos.

View(
  anti_join(estat_jogador, valor, by = 
              c("nome" = "nome", "clube_valor" = "Team")) |> 
    filter(`Min.` > 2000) |>
    arrange(desc(`Min.`))
)

sem_par <- anti_join(estat_jogador, valor, by = 
              c("nome" = "nome", "clube_valor" = "Team")) |> 
  filter(`Min.` > 2000) |>
  arrange(desc(`Min.`))

#depois de selecionar manualmente os jogadores, queremos adicioná-los à base
  
dicionario_nomes <- read_delim("dados/dicionario_nomes.csv", delim = ";") |>
  rename(nome_estat = Jogador, nome_valor = valor) |>
  filter(!is.na(nome_valor)) |>
  mutate(across(everything(),
                ~ trimws(tolower(stri_trans_general(.x, "Latin-ASCII")))))
          
dicionario_nomes <- dicionario_nomes |>
  left_join(sem_par |> select(nome, clube_valor),
            by = c("nome_estat" = "nome"))

dicionario_nomes <- dicionario_nomes |>
  semi_join(valor, by = c("nome_valor" = "nome",
                          "clube_valor" = "Team"))
estat_jogador <- estat_jogador |>
  left_join(dicionario_nomes, by = c("nome" = "nome_estat",
                                     "clube_valor" = "clube_valor")) |>
  mutate(nome = coalesce(nome_valor, nome)) |>
  select(-nome_valor)

#juntando tudo na base final

base_final <- inner_join(estat_jogador, valor,
                         by = c("nome" = "nome",
                                "clube_valor" = "Team"))

dim(base_final)
base_final |> count(nome, clube_valor) |> filter(n > 1)
