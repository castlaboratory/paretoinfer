# Retomada do trabalho: artigo e versão 0.2.0

Última atualização: 2026-10-24. Branch: `fix/strict-dominance` (no GitHub, sem PR).

## Onde paramos

- O artigo está completo:
  - `manuscript/main.pdf`, 25 páginas;
  - `manuscript/supplement.pdf`, 6 páginas;
  - carta de submissão em rascunho: `manuscript/cover_letter.tex`.
- Todos os experimentos terminaram, e os resultados estão em `results/`:
  - Estudo 1: 6 configurações × 200 réplicas;
  - Estudo 2: 15 células × 30 réplicas;
  - aplicações: 2 × 20 réplicas;
  - horizontes de planejamento.
- O pacote 0.2.0 passou no `R CMD check --as-cran` com 0 erros e 0 warnings. A única NOTE (HTML Tidy) é desta máquina.

## Decisões já tomadas (não reabrir)

- **Revista:** Technometrics. Critério do autor: sem APC e Q1.
- **Ordem dos autores:** Ospina, Leite, Ferraz.
- **Versão do pacote:** o artigo descreve a versão corrigida 0.2.0. A 0.1.0 será retirada do CRAN.
- **Inventário:** o valor verdadeiro é exato, calculado pela cadeia de Markov (`policy_exact()` em `code/app_inventory_model.R`). Com o Monte Carlo, a política (40,100) pareceria Pareto-ótima por erro.
- **Resultados reportados honestamente:**
  - H2 só é sustentada em parte: o t repetido errou 3,5%, abaixo de α;
  - H3 vale só em direção;
  - o horizonte com Hoeffding chega a milhões de avaliações;
  - a fase de cobertura não terminou dentro do orçamento.

## Próximos passos (dependem do autor)

1. Leitura de Leite e Ferraz. Ao revisar, seguir as regras de `paper/CLAUDE.md`.
2. Versão anônima: `\newcommand{\blind}{1}` em `manuscript/main.tex`.
3. Conferir se a Technometrics aceita floats e referências em espaço simples. Com espaço duplo em tudo, o texto passa de 25 páginas.
4. Abrir o PR de `fix/strict-dominance` e fazer o merge em `main`.
5. Escrever para cran-submissions@r-project.org pedindo a retirada da 0.1.0, que está em `incoming/newbies` desde 2026-10-15, e explicar o defeito de certificado. O texto está em `cran-comments.md`.
6. Submeter a 0.2.0 com `devtools::submit_cran()`, que atualiza o `CRAN-SUBMISSION`.
7. Submeter o artigo com o suplemento e um arquivo zip com `paper/code` e a versão do pacote.

## Como retomar rapidamente

```sh
git checkout fix/strict-dominance && git pull
Rscript paper/code/00_install.R                     # pacote em paper/.lib/current
cd paper/manuscript && latexmk -pdf supplement.tex && latexmk -pdf main.tex && latexmk -pdf supplement.tex
```

- Para refazer figuras e tabelas sem rodar os estudos: `fig_studies.R`, `fig_apps.R` e `supp_tables.R`. Esses scripts leem `results/`.
- Os estados únicos das aplicações ficam em `results/cache/`, que está no gitignore. Quem os regenera é `03_applications.R`, e a rodada única leva cerca de 30 min.
- A ordem completa dos scripts e os tempos estão em `paper/README.md`.
