# AccidentPoisson

Analyse des comptages d'accidents de la route par la loi de Poisson, sur **n'importe quel
data.frame** : l'utilisateur indique les noms de colonnes.

```r
analyser_zones(data2, zone_col = "ville", count_col = "nombre")
```

## Fonctions

| Fonction | Role |
|---|---|
| `calculer_poisson()` | P(X = k), P(X <= k), P(X > k) |
| `table_poisson()` | Tableau de probabilites (+ jours attendus) |
| `estimer_lambda()` | lambda, IC exact, indice de dispersion |
| `tester_adequation_poisson()` | Test du khi-deux d'adequation |
| `aggreger_accidents()` | Comptages par periode et par groupe |
| `analyser_zones()` | Classement et test des zones a risque |
| `ajuster_modele_poisson()` | Regression de Poisson (offset d'exposition) |
| `predire_accidents()` | Predictions avec intervalle de confiance |
| `diagnostiquer_surdispersion()` | Pearson + Cameron-Trivedi |
| `visualiser_comptages()` | Graphiques ggplot2 |
| `calculer_seuil_alerte()` / `generer_alerte()` | Alertes preventives |
| `exporter_resultats()` | Export csv / xlsx / rds |
| `charger_exemple_rn3()` | Donnees **synthetiques** de demonstration |

## Installation et verification

```r
install.packages(c("devtools", "roxygen2", "testthat", "ggplot2", "rlang", "writexl", "readxl"))
devtools::document()   # genere man/ et NAMESPACE
devtools::load_all()
devtools::test()
devtools::check()
devtools::install()
```

Script de demonstration : `inst/scripts/demo_rn3.R`.

## Limites

* Les donnees fournies sont synthetiques (projet pedagogique), pas des donnees officielles.
* La regression exige des **comptages agreges** (utiliser `aggreger_accidents()`).
* Le test des zones suppose des observations independantes ; en cas de surdispersion,
  les p-values sont trop optimistes (voir `diagnostiquer_surdispersion()`).
