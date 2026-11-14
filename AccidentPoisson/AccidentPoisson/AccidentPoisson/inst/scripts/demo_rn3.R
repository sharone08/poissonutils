# Demonstration complete sur le jeu synthetique RN3
library(AccidentPoisson)

d <- charger_exemple_rn3()

# 1. Loi de Poisson et estimation
est <- estimer_lambda(d$Nb_Accidents); est
table_poisson(est$lambda, k_max = 10, jours = 365)
tester_adequation_poisson(d$Nb_Accidents)

# 2. Zones a risque
zones <- analyser_zones(d, zone_col = "Troncon", count_col = "Nb_Accidents")
zones

# 3. Analyse temporelle
freq <- aggreger_accidents(d, date_col = "Date", group_cols = "Troncon",
                           periode = "mois", count_col = "Nb_Accidents")
head(freq)

# 4. Regression de Poisson (donnees journalieres = comptages)
modele <- ajuster_modele_poisson(d, Nb_Accidents ~ Meteo + Trafic + Troncon)
summary(modele)
diagnostiquer_surdispersion(modele)

# 5. Prediction
nouveau <- data.frame(Meteo = "Pluie", Trafic = "Tres eleve", Troncon = "Douala-Edea")
predire_accidents(modele, nouveau)

# 6. Visualisation, alertes, export
visualiser_comptages(zones, "zone", "total_accidents", titre = "Accidents par troncon")
alertes <- generer_alerte(zones, "total_accidents",
                          seuil = quantile(zones$total_accidents, 0.75))
calculer_seuil_alerte(est$lambda, 0.95)
exporter_resultats(list(zones = zones, alertes = alertes), "resultats_rn3.xlsx")
