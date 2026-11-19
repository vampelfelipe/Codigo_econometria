# =============================================================================
# 01_importacao.R
# Trabalho Final de Econometria I
# Etapa: importação das bases (estatísticas por partida e valor de mercado)
# Série A do Campeonato Brasileiro, 2024. Fonte: Kaggle.
# =============================================================================
# O que este script faz:
#   1. Carrega os pacotes necessários.
#   2. Descompacta os .zip baixados do Kaggle.
#   3. Lê as duas bases com os tipos de coluna corretos.
#   4. Confere se a leitura funcionou (dimensões, tipos, ausentes, duplicatas).
# O que este script NÃO faz (são etapas de vocês): agregar por jogador,
# limpar nomes, juntar as bases, definir a amostra, criar variáveis.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Pacotes
# -----------------------------------------------------------------------------
# readr: leitura rápida de CSV com controle de tipos e de encoding.
# A linha abaixo instala o pacote só se ele ainda não estiver instalado.
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
library(readr)


# -----------------------------------------------------------------------------
# 1. Caminhos
# -----------------------------------------------------------------------------
# Coloquem os .zip baixados do Kaggle dentro de uma pasta "dados/" no
# diretório do projeto (de preferência um projeto do RStudio, .Rproj, para
# que os caminhos relativos funcionem no computador de qualquer integrante).
# Ajustem os nomes dos arquivos se os de vocês forem diferentes.
pasta_dados    <- "dados"
zip_estatisticas <- file.path(pasta_dados, "archive.zip")    # contém database.csv
zip_valor        <- file.path(pasta_dados, "archive_2.zip")  # contém BRA_players.csv

# unzip() extrai o conteúdo do .zip para a pasta indicada em exdir.
# overwrite = TRUE substitui arquivos já extraídos antes.
unzip(zip_estatisticas, exdir = pasta_dados, overwrite = TRUE)
unzip(zip_valor,        exdir = pasta_dados, overwrite = TRUE)

arq_estatisticas <- file.path(pasta_dados, "database.csv")
arq_valor        <- file.path(pasta_dados, "BRA_players.csv")

# Para o link de acesso à base exigido no trabalho, anotem aqui as URLs
# das páginas do Kaggle de onde vieram os arquivos:
# url_estatisticas <- "https://www.kaggle.com/..."
# url_valor        <- "https://www.kaggle.com/..."


# -----------------------------------------------------------------------------
# 2. Base de estatísticas (uma linha = um jogador em uma partida)
# -----------------------------------------------------------------------------
# Particularidades deste arquivo:
#   - Está em UTF-8 (tem acentos: "Éder", "Criciúma"). Ler com outro encoding
#     estraga os nomes e atrapalha a junção depois.
#   - O separador de colunas é a vírgula (CSV padrão), mas a coluna "Cmp%"
#     usa vírgula como separador DECIMAL e vem entre aspas ("75,0").
#     Por isso ela é lida como texto e convertida em seguida com
#     parse_number() e um locale brasileiro.
#   - "Idade" vem no formato "anos-dias" (ex.: "37-150"); é lida como texto.
#     Como transformá-la é uma decisão de criação de variável de vocês.
#   - "#" é o número da camisa: lido como texto, porque não é uma quantidade.
#   - "Data" é a data da partida.
estatisticas <- read_csv(
  arq_estatisticas,
  locale = locale(encoding = "UTF-8"),
  col_types = cols(
    Jogador = col_character(),
    Time    = col_character(),
    `#`     = col_character(),
    `Nação` = col_character(),
    `Pos.`  = col_character(),
    Idade   = col_character(),
    `Cmp%`  = col_character(),
    Data    = col_date(format = "%Y-%m-%d"),
    .default = col_double()   # todas as demais colunas são numéricas
  )
)

# Conversão do "Cmp%": "75,0" -> 75.0
# locale(decimal_mark = ",") diz ao R que a vírgula é o separador decimal.
estatisticas$`Cmp%` <- parse_number(
  estatisticas$`Cmp%`,
  locale = locale(decimal_mark = ",", grouping_mark = ".")
)


# -----------------------------------------------------------------------------
# 3. Base de valor de mercado (uma linha = um jogador em um elenco)
# -----------------------------------------------------------------------------
# Particularidades deste arquivo:
#   - Sem acentos (ASCII): "Sao Paulo", "Criciuma". Os nomes de clubes e
#     jogadores NÃO estão escritos igual à outra base.
#   - Os nomes das colunas têm espaço ("Market Value"); no R, usem crases
#     para se referir a elas: valor$`Market Value`.
#   - Moeda e data de coleta do valor de mercado: conferir na documentação
#     do dataset no Kaggle.
valor <- read_csv(
  arq_valor,
  locale = locale(encoding = "UTF-8"),  # ASCII é compatível com UTF-8
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
# problems() lista linhas que o readr não conseguiu converter para o tipo
# pedido. O esperado é zero linhas nas duas bases.
problems(estatisticas)
problems(valor)

# Dimensões (linhas x colunas). Esperado: 11682 x 33 e 644 x 6.
dim(estatisticas)
dim(valor)

# Estrutura: nome e tipo de cada coluna.
str(estatisticas)
str(valor)

# Quantidade de valores ausentes (NA) por coluna.
colSums(is.na(estatisticas))
colSums(is.na(valor))

# Cobertura da base de estatísticas: período e clubes.
range(estatisticas$Data)
sort(unique(estatisticas$Time))
sort(unique(valor$Team))

# Linhas duplicadas por completo (todas as colunas iguais).
sum(duplicated(estatisticas))
sum(duplicated(valor))

# Combinações jogador + clube repetidas na base de valor.
# (Na base de estatísticas a repetição é esperada: cada jogador aparece uma
# vez por partida.) Vejam essas linhas e decidam o que fazer com elas.
valor[duplicated(valor[, c("Player", "Team")]) |
      duplicated(valor[, c("Player", "Team")], fromLast = TRUE), ]


# -----------------------------------------------------------------------------
# 5. Salvar as bases importadas
# -----------------------------------------------------------------------------
# saveRDS() guarda o objeto do R com os tipos já corretos, para que os
# próximos scripts comecem com readRDS() sem repetir a importação.
saveRDS(estatisticas, file.path(pasta_dados, "estatisticas_brutas.rds"))
saveRDS(valor,        file.path(pasta_dados, "valor_bruto.rds"))