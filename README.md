# Trabalho Final — Econometria I

**Tema:** Determinantes do valor de mercado dos jogadores da Série A do Campeonato Brasileiro (2024)  
**Autores:** Felipe Camargos Azeredo Vampel e Rafael Garavello Kaloglian

Este repositório reúne as bases de dados, os scripts em R e os resultados (figuras e tabelas) utilizados na estimação do modelo econométrico do trabalho.

## Estrutura do repositório

- `Trabalho econo I/Trabalho econo I.Rproj`: arquivo de projeto do RStudio (define o diretório de trabalho para os caminhos relativos).
- `Trabalho econo I/dados/`: bases brutas do Kaggle (`archive.zip` e `archive_2.zip`), tabela de correções manuais de nomes (`dicionario_nomes.csv`) e bases intermediárias e final geradas pelo código (`.rds` e `.csv`).
- `Trabalho econo I/scripts/`: scripts em R numerados na ordem de execução (`01` a `05`).
- `Trabalho econo I/figuras/`: gráficos em `.png` gerados pelo script `03_descritivas.R`.
- `Trabalho econo I/tabelas/`: tabelas em `.csv` e formatadas em Word (`.docx`) geradas pelos scripts `03`, `04` e `05`.

## Fontes dos dados

1. **Estatísticas individuais por partida (FBref — Série A 2024):**  
   https://www.kaggle.com/datasets/eduardopalmieri/brasileiro-player-stats-2024
2. **Valor de mercado por elenco (Transfermarkt — Série A 2024):**  
   https://www.kaggle.com/datasets/leandrowrzecionek/players-from-the-brazilian-football-league

## Como reproduzir

1. Abra o arquivo `Trabalho econo I/Trabalho econo I.Rproj` no RStudio.
2. Caso ainda não tenha os pacotes instalados, execute no Console:

```r
install.packages(c(
  "readr", "dplyr", "tidyr", "stringi", "ggplot2",
  "sandwich", "lmtest", "car", "modelsummary", "flextable"
))
```

3. Execute os scripts na seguinte ordem (via **Source**):
   - `scripts/01_importacao.R`: extrai os arquivos `.zip` e importa as bases brutas.
   - `scripts/02_agregacao.R`: agrega estatísticas por jogador e clube, aplica os filtros amostrais, faz o pareamento entre as bases e cria as variáveis do modelo ($N = 410$).
   - `scripts/03_descritivas.R`: calcula as estatísticas descritivas e gera as figuras em `figuras/`.
   - `scripts/04_regressoes.R`: estima os modelos MQO (sem e com efeitos fixos de clube), executa os testes de diagnóstico (Breusch-Pagan, VIF, teste F conjunto das dummies de clube e ponto de máximo da idade) e os testes de robustez.
   - `scripts/05_tabelas.R`: exporta as tabelas finais formatadas em `.docx` (`tabelas/descritivas.docx`, `tabelas/resultados.docx` e `tabelas/robustez.docx`).