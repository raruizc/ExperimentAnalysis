# 📐 Analysis of Experiments in R (UFLA)

![R](https://img.shields.io/badge/Language-R-276DC3)
![Topic](https://img.shields.io/badge/Topic-Experimental_Design-blue)

R scripts, datasets and solved exercise lists from the graduate course **Analysis of Experiments** at the Federal University of Lavras (UFLA). The repository covers the full path from R basics to multi-environment trial analysis, and includes the *monitoria* (teaching-assistant) scripts used to support students.

> Exercise statements, code comments and reports are in Portuguese.

## Contents

| Folder | Topic |
|---|---|
| `00_Introduction.R` | Introduction to the R language |
| `01_Iteration_Plots.R` | Loops, conditionals and plotting |
| `Lista 01` | Normal distribution and parameter estimation |
| `Lista 02` | ANOVA for basic designs (CRD, RCBD), assumptions and precision metrics (CV) |
| `Lista 03` | Model definition, multiple comparisons (Tukey, Scott-Knott) and contrasts |
| `Lista 04` | Variance components |
| `Lista 05` | Split-plot experiments and repeated measures |
| `Lista 06` | Incomplete block designs and groups of experiments |
| `Lista 07` | Augmented block and p-rep designs |
| `Lista 08` | Multi-environment trial analysis |
| `Listao 09` | Final review list (additional routines) |

Each folder contains the exercise data (`.csv`, `.xlsx`, `.txt`), the R scripts (`lista_XX.R`, `Monitoria_ListaX.R`) and the written solution report.

## How to use

```bash
git clone https://github.com/raruizc/ExperimentAnalysis.git
```

Open `ExperimentAnalysis.Rproj` in RStudio and run the scripts in each `Lista XX` folder. The scripts read their data from the same folder.

## Credits

The course routines (`ROTINA XX`) and exercise statements were provided by the course instructors at UFLA. Solutions and teaching-assistant scripts are by **Ricardo Antonio Ruiz-Cardozo**.

## Recommended reading (Lista 02)

- Eisenhart (1947). The assumptions underlying the analysis of variance. *Biometrics*.
- Mead & Curnow (1981). On the meaning of the error mean square.
- Pimentel-Gomes (1991). Índice de variação e coeficiente de variação. *Circular Técnica*.
- Resende & Duarte (2007). Precisão e controle de qualidade em experimentos de avaliação de cultivares (VCU).
- Cargnelutti Filho et al. (2009). Medidas da precisão experimental em feijão e soja. *Pesquisa Agropecuária Brasileira*.
- Gurgel et al. (2013). Uso do CVe em ensaios de VCU de feijão e milho. *Boletim*.
- Piepho et al. (2013). Why randomize agricultural experiments? *Journal of Agronomy and Crop Science*.
- Kozak & Piepho (2017). What's normal anyway? Residual plots are more telling than significance tests when checking ANOVA assumptions. *Journal of Agronomy and Crop Science*.

The article PDFs are not included here for copyright reasons.
