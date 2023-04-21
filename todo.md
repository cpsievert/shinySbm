#### Tests

- Rajouter des tests bipartite ou simple en fonction du jeu de données en entrée (validate)
 

  #### Données en entrée

- information sur les modèles

- Permettre la simulation d'un jeu de données d'entrées ()




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
