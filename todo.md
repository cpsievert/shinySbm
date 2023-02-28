#### Tests

- Rajouter des tests bipartite ou simple en fonction du jeu de données en entrée (validate)
 

  #### Données en entrée

- Améliorer la partie chargement de ses propres données, spécifier le format etc. (a faire)

- Permettre la simulation d'un jeu de données d'entrées ()

- Permettre de rentrer les données soit sous forme de matrice soit sous forme de liste d'arêtes et noeuds
 

  #### Figures

- Rajouter des titres pour les figures

- Modifier les labels des dernières plots

- Permettre d'enlever la légende

- Eventuellement jouer sur les couleurs

  #### Divers

  - Ajouter des références (important)

- Permettre de récupérer le code du sbm (shinymeta::outputCodeButton associée à shinymeta::metaRender2)

- Permettre de récupérer les paramètres et éditer une table qui bouge en fonction des paramètres (nb blocs)


- Proposer des issues pour le package sbm

### Suite à discussion avec Saint-Clair 01/09/2022

- Faire un package avec test contenant l'application sbmshiny sur le modèle de FactoShiny
- Avec une partie exploration des sorties, notamment une liste des noeuds avec les clusters + d'éventuelles métadonnées au choix
- Base ce code + code addins
- Paramètres nombre de blocs
- Regarder si on ne peut pas faire du 2 en 1. C'est-à-dire un package appli Shiny avec une fonction Addin utilisant le code de l'appli 
Lignes de code de l'ACP de PCAShiny


ideas from factoshiny : see on package files

Get Home Language : `strsplit(Sys.getlocale("LC_COLLATE"),"_")[[1]][1]`
Traduction : `gettext("Which graphs to use?",domain="R-Factoshiny")` 
Rapport automatique : `FactoInvestigate::Investigate` 
