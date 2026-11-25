# =============================================================================
# 01_importacao.R
# Importa as bases brutas (estatísticas por partida e valor de mercado) da
# Série A do Campeonato Brasileiro de 2024 e salva os objetos em .rds.
#
# Entradas : dados/archive.zip (database.csv) e dados/archive_2.zip (BRA_players.csv)
# Saídas   : dados/estatisticas_brutas.rds e dados/valor_bruto.rds
# =============================================================================

if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
library(readr)

# -----------------------------------------------------------------------------
# 1. Caminhos e fontes
# -----------------------------------------------------------------------------
url_estatisticas <- "https://www.kaggle.com/datasets/eduardopalmieri/brasileiro-player-stats-2024"
url_valor        <- "https://www.kaggle.com/datasets/leandrowrzecionek/players-from-the-brazilian-football-league"

pasta_dados      <- "dados"
zip_estatisticas <- file.path(pasta_dados, "archive.zip")    # contém database.csv
zip_valor        <- file.path(pasta_dados, "archive_2.zip")  # contém BRA_players.csv

unzip(zip_estatisticas, exdir = pasta_dados, overwrite = TRUE)
unzip(zip_valor,        exdir = pasta_dados, overwrite = TRUE)

arq_estatisticas <- file.path(pasta_dados, "database.csv")
arq_valor        <- file.path(pasta_dados, "BRA_players.csv")

# -----------------------------------------------------------------------------
# 2. Base de estatísticas (uma linha = um jogador em uma partida)
# -----------------------------------------------------------------------------
# Leitura em UTF-8 para preservar acentos; Cmp% é lido como texto por usar
# vírgula decimal ("75,0") e convertido em seguida com parse_number().
estatisticas <- read_csv(
  arq_estatisticas,
  locale = locale(encoding = "UTF-8"),
  col_types = cols(
    Jogador  = col_character(),
    Time     = col_character(),
    `#`      = col_character(),
    `Nação`  = col_character(),
    `Pos.`   = col_character(),
    Idade    = col_character(),
    `Cmp%`   = col_character(),
    Data     = col_date(format = "%Y-%m-%d"),
    .default = col_double()
  )
)

estatisticas$`Cmp%` <- parse_number(
  estatisticas$`Cmp%`,
  locale = locale(decimal_mark = ",", grouping_mark = ".")
)

# -----------------------------------------------------------------------------
# 3. Base de valor de mercado (uma linha = um jogador em um elenco)
# -----------------------------------------------------------------------------
# Valores em euros (Transfermarkt), nomes sem acentos (ASCII).
valor <- read_csv(
  arq_valor,
  locale = locale(encoding = "UTF-8"),
  col_types = cols(
    Player         = col_character(),
    Team           = col_character(),
    Age            = col_double(),
    Position       = col_character(),
    `Market Value` = col_double(),
    SofaScore      = col_double()
  )
)

# -----------------------------------------------------------------------------
# 4. Conferência da importação
# -----------------------------------------------------------------------------
stopifnot(nrow(problems(estatisticas)) == 0)
stopifnot(nrow(problems(valor)) == 0)

dim(estatisticas)   # esperado: 11682 x 33
dim(valor)          # esperado: 644 x 6

colSums(is.na(estatisticas))
colSums(is.na(valor))

range(estatisticas$Data)
sum(duplicated(estatisticas))
sum(duplicated(valor))

# Combinações jogador + clube repetidas na base de valor (tratadas no script 02)
valor[duplicated(valor[, c("Player", "Team")]) |
        duplicated(valor[, c("Player", "Team")], fromLast = TRUE), ]

# -----------------------------------------------------------------------------
# 5. Salvar as bases importadas
# -----------------------------------------------------------------------------
saveRDS(estatisticas, file.path(pasta_dados, "estatisticas_brutas.rds"))
saveRDS(valor,        file.path(pasta_dados, "valor_bruto.rds"))