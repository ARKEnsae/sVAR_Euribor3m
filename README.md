# Les effets d’une hausse de l’Euribor 3-mois

Projet d'*Applied macroeconometrics* de Valentin Giust (@ValGIUST), Gautier Lenfant (@GautierLENFANT) et Alain Quartier-la-Tente (@AQLT).

L'objectif est d'étudier les effets d'une hausse de l’Euribor 3-mois sur les variables macroéconomiques classiques (PIB, chômage, IPCH, inflation sous-jacente) à l'aide de modèles sVAR. 
Cette étude a été fait sur des données au niveau de l'Union Européenne dans son ensemble et également au niveau de la France.

Le rapport est disponible : https://arkensae.github.io/sVAR_Euribor3m/Rapport/Rapport.pdf

Le code R, également disponible dans le rapport, est décomposé en 4 fichiers :

- [`Z - Fonctions.R`](https://github.com/ARKEnsae/sVAR_Euribor3m/blob/main/R/Z%20-%20Fonctions.R) contient l'ensemble des fonctions utilisées et fait (si besoin) l'installation des packages nécessaires.

- [`1-extraction_donnees.R`](https://github.com/ARKEnsae/sVAR_Euribor3m/blob/main/R/1-extraction_donnees.R) extrait les données et les sauvegarde dans dans un dossier `data`.

- [`2.1-estimation_modeles EA.R`](https://github.com/ARKEnsae/sVAR_Euribor3m/blob/main/R/2.1-estimation_modeles%20EA.R) estime les modèles sVAR au niveau de l'Union Européenne.

- [`2.2-estimation_modeles FR.R`](https://github.com/ARKEnsae/sVAR_Euribor3m/blob/main/R/2.2-estimation_modeles%20FR.R) estime les modèles sVAR au niveau de la France.
