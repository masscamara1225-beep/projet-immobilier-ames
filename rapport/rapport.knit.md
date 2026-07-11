---
title: "Analyse du Marché Immobilier d'Ames"
subtitle: "Rapport de projet · Visualisation de Données · Data Science / IA UFHB.UFRMI"
author: |
  | CAMARA Massaram & LOGBO Axelle · Groupe 4
  | \vspace{2pt} \small Sous l'encadrement du **Dr.KACOUTCHY Jean Ayikpa**
date: "Juillet 2026"
output:
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: true
    latex_engine: xelatex
lang: fr
fontsize: 11pt
geometry: margin=1.9cm
urlcolor: orangeames
linkcolor: marineames
header-includes:
  - \usepackage{setspace}
  - \setstretch{1.05}
  - \usepackage{float}
  - \usepackage{xcolor}
  - \definecolor{orangeames}{HTML}{E8721C}
  - \definecolor{marineames}{HTML}{102A43}
  - \usepackage{sectsty}
  - \sectionfont{\color{marineames}}
  - \subsectionfont{\color{marineames}}
  - \usepackage{fancyhdr}
  - \pagestyle{fancy}
  - \setlength{\headheight}{13pt}
  - \fancyhf{}
  - \fancyhead[L]{\small\color{gray}Marché immobilier d'Ames · Master 1 Groupe 4}
  - \fancyhead[R]{\small\color{gray}UFHB · UFRMI · 2025-2026}
  - \fancyfoot[C]{\thepage}
  - \renewcommand{\headrulewidth}{0.4pt}
  - \renewcommand{\headrule}{\color{orangeames}\hrule width\headwidth height 0.8pt}
  - \usepackage{titling}
  - \pretitle{\begin{center}\color{marineames}\Huge\bfseries}
  - \posttitle{\par\vskip 4pt{\color{orangeames}\rule{0.35\textwidth}{2pt}}\end{center}}
---



\newpage

# Présentation du projet

## Contexte

Ames est une ville universitaire de l'Iowa d'environ 65 000 habitants, structurée autour de l'Iowa State University (fondée en 1858). Son marché immobilier, organisé en 25 quartiers aux profils très contrastés, constitue un terrain d'étude idéal pour comprendre la formation des prix.

Notre analyse porte sur 1 460 transactions réalisées entre 2006 et 2010 (dataset House Prices, Kaggle). Cette période est marquée par la crise des subprimes, la plus grande crise immobilière depuis 1929 — et cette crise est lisible dans nos données : le prix médian passe de 167 000 $ en 2007 à 155 000 $ en 2010, soit une baisse de 7,2 % en trois ans.

## Problématique

Sur ce marché, une maison peut valoir 21 fois plus qu'une autre (de 34 900 $ à 755 000 $). 
Quels sont les véritables déterminants du prix de vente ? 
La qualité prime-t-elle sur la superficie ? 
Le quartier efface-t-il les défauts du bien ? 
L'environnement immédiat — parc, route artérielle — crée-t-il une prime ou une décote mesurable ? 
La crise de 2008 a-t-elle reconfiguré les règles du marché ?

## Objectifs

Cinq objectifs ont structuré notre travail :

1. **Explorer** — conduire une analyse exploratoire complète du marché ;
2. **Identifier** — quantifier les facteurs influençant le prix via des visualisations adaptées à chaque type de données ;
3. **Analyser l'environnement** — mesurer l'impact des conditions de proximité ;
4. **Modéliser** — valider les résultats par régression linéaire, Random Forest et une soumission Kaggle ;
5. **Recommander** — formuler cinq recommandations stratégiques par profil d'acteur du marché.

## Démarche et choix techniques

L'analyse est construite comme une enquête narrative en cinq actes — Quoi ? Où ? Environnement ? Pourquoi ? Quand ? — restituée dans une application interactive accompagnée de 24 visualisations.

Le dashboard est construit avec Shiny (shinydashboard) et déployé publiquement sur shinyapps.io. Il offre une interactivité réelle : filtres par quartier, comparateur de quartiers, simulateur de prix connecté au modèle Random Forest. L'obligation des deux outils de visualisation est satisfaite : ggplot2 pour le statique, plotly et highcharter pour l'interactif.
L'application est accessible à l'adresse : https://cam-s.shinyapps.io/ames-immobilier


# Présentation des données

## Le jeu de données

Le dataset *Ames Housing*, publié par *Dean De Cock en 2011*, est une référence de la data science au même titre que Titanic ou Iris. 
La compétition fournit trois fichiers. Le fichier principal, train.csv, contient 1 460 observations décrites par 80 variables explicatives, 
plus la variable cible SalePrice (prix de vente en dollars) : c'est lui qui sert à l'entraînement des modèles. Le fichier test.csv (1 459 observations sans prix) 
sert à produire les prédictions de la validation externe. Enfin, sample_submission.csv fournit le format attendu du fichier de soumission à Kaggle.

Chaque observation est une transaction immobilière réalisée à Ames entre 2006 et 2010. Les 80 variables couvrent toutes les dimensions d'un bien : localisation (quartier, zonage), dimensions (surfaces habitable, sous-sol, garage, terrain), qualité (notes globales et par équipement), équipements (cheminée, piscine, clôture), et circonstances de la vente (année, mois, condition de vente).

## Classification des variables

La première étape méthodologique consiste à classer chaque variable selon sa nature, car le type de données détermine à la fois le traitement dans R et la visualisation adaptée :

\begingroup\fontsize{9}{11}\selectfont

\begin{longtable}[t]{>{\raggedright\arraybackslash}p{3.8cm}r>{\raggedright\arraybackslash}p{5.2cm}>{\raggedright\arraybackslash}p{3.6cm}}
\toprule
Type & Nb & Exemples & Traitement R\\
\midrule
Qualitatives nominales & 38 & Neighborhood, BldgType, Condition1 & as.factor()\\
Qualitatives ordinales & 10 & ExterQual, KitchenQual, BsmtQual & factor(ordered = TRUE)\\
Quantitatives continues & 15 & SalePrice, GrLivArea, LotArea & Directement\\
Quantitatives discrètes & 9 & BedroomAbvGr, GarageCars, Fireplaces & Directement\\
Temporelles & 5 & YearBuilt, YrSold, MoSold & as.factor() ou cut()\\
\addlinespace
Faussement numériques & 5 & MSSubClass, OverallQual, MoSold & Conversion en facteur\\
\bottomrule
\end{longtable}
\endgroup{}
Le dernier groupe mérite attention : des variables comme `MSSubClass` (codes de types de logement) ou `OverallQual` (échelle ordinale de 1 à 10) sont stockées comme des entiers, mais les traiter comme des nombres serait une erreur méthodologique — le code 60 ne vaut pas « trois fois » le code 20, et une note de qualité est une échelle ordonnée, pas une mesure métrique. Leur conversion en facteurs conditionne la validité de toutes les visualisations de l'Acte 1.

## Distribution de la variable cible


\begin{center}\includegraphics[width=0.7\linewidth]{rapport_files/figure-latex/fig_distribution_cible-1} \end{center}

*Figure 1 — Distribution du prix de vente : une asymétrie à droite marquée.*
La distribution de `SalePrice` est asymétrique à droite : le gros du marché se concentre entre 80 000 et 300 000 dollars, tandis qu'une longue queue de biens de luxe s'étire jusqu'à 755 000 dollars. Cette poignée de maisons très chères tire la moyenne (180 921 $) au-dessus de la médiane (163 000 $, ligne pointillée). C'est pourquoi la médiane, insensible aux valeurs extrêmes, sert de référence chiffrée dans l'ensemble de ce rapport.

\newpage



# Nettoyage des données

## Audit des valeurs manquantes

Avant toute analyse, nous avons compté les trous dans le tableau : 19 variables sur 80 présentent des valeurs manquantes. Les situations sont très inégales — la variable `Electrical` n'a qu'une seule valeur absente sur 1 460, alors que `PoolQC` (qualité de la piscine) est vide dans 99,5 % des cas.

Face à cela, la solution de facilité aurait été d'appliquer la même recette partout : supprimer les lignes incomplètes, ou remplacer chaque trou par une moyenne. Nous ne l'avons pas fait, car ces deux réflexes auraient abîmé les données. Supprimer les lignes incomplètes aurait fait disparaître la quasi-totalité du dataset. Remplir avec des moyennes aurait inventé des piscines à des maisons qui n'en ont pas. Avant de corriger un trou, il faut d'abord comprendre pourquoi il existe.

## Le vide n'est pas toujours un manque

C'est le principe qui a guidé tout notre nettoyage. La documentation officielle du dataset est claire : pour 11 variables, une case vide ne veut pas dire « information perdue », mais « cet équipement n'existe pas dans cette maison ». Une case vide sur la qualité de la piscine signifie simplement : pas de piscine. Même logique pour la clôture, la cheminée, l'allée ou le garage.

Autrement dit, le vide est ici une réponse en soi. Nous avons donc remplacé ces cases vides par une catégorie explicite, « Absent », au lieu de deviner une valeur. Ce choix n'est pas un détail : le modèle de prédiction de la section 5 apprend à partir de ces données. Lui fournir des valeurs inventées reviendrait à lui apprendre des choses fausses — et ses prédictions de prix en auraient hérité.

\begingroup\fontsize{9}{11}\selectfont

\begin{longtable}[t]{>{\raggedright\arraybackslash}p{4.6cm}>{\raggedright\arraybackslash}p{2.2cm}>{\raggedright\arraybackslash}p{3.4cm}>{\raggedright\arraybackslash}p{4.2cm}}
\toprule
Variable & NA détectés & Stratégie & Justification\\
\midrule
PoolQC, MiscFeature, Alley, Fence, FireplaceQu & 47 \% à 99,5 \% & Catégorie « Absent » & NA = absence de l'équipement\\
GarageType / Finish / Qual / Cond & 81 (5,5 \%) & Catégorie « Absent » & Maisons sans garage\\
GarageYrBlt & 81 (5,5 \%) & Année de construction & Garage construit avec la maison\\
BsmtQual / Cond / Exposure / FinType & 37-38 (2,5 \%) & Catégorie « Absent » & Maisons sans sous-sol\\
LotFrontage & 259 (17,7 \%) & Médiane du quartier & Vraie donnée manquante\\
\addlinespace
MasVnrType / Area & 8 (0,5 \%) & « Absent » / 0 & Pas de parement maçonné\\
Electrical & 1 (0,07 \%) & Mode (SBrkr) & Cas isolé, impact négligeable\\
\bottomrule
\end{longtable}
\endgroup{}

## L'exception LotFrontage : une vraie imputation

`LotFrontage` (largeur de façade du terrain) suit une logique inverse : la façade existe bel et bien, mais n'a pas été mesurée pour 259 biens. C'est ici une véritable donnée manquante, qui justifie une imputation. Nous avons choisi la **médiane par quartier** plutôt que la médiane globale : deux maisons voisines ont des terrains comparables, issus des mêmes plans d'urbanisme. Cette imputation contextuelle préserve la structure géographique des données.

## Conversions de types

Cinq variables faussement numériques ont été converties en facteurs, conformément à la classification de la section 2 : `MSSubClass` (codes arbitraires), `OverallQual` et `OverallCond` (échelles ordinales 1-10, converties en facteurs ordonnés), `YrSold` et `MoSold` (catégories temporelles). Les 38 variables textuelles restantes ont été converties en facteurs pour la modélisation.

## Deux variables créées pour l'enquête

Deux variables synthétiques, absentes du dataset brut, ont été construites :

- **ScoreEnv** (score environnemental) : recode la variable `Condition1` (9 modalités textuelles peu exploitables) en échelle numérique ordonnée, de −2 (route artérielle) à +2 (adjacent à un parc). Cette variable rend possible toute l'analyse environnementale de l'Acte 3.
- **Era** (ère de construction) : regroupe les 112 années de construction distinctes en 5 périodes historiques interprétables (Pré-1920, 1920-1945, 1946-1970, 1971-1990, 1991-2010). Cette variable structure l'analyse temporelle de l'Acte 5.

## Retrait de deux valeurs aberrantes

Deux transactions affichent plus de 4 000 ft² habitables pour un prix inférieur à 300 000 $ — une anomalie documentée publiquement par la communauté ayant étudié ce dataset (ventes atypiques ou erreurs de saisie). Ces deux observations ont été écartées au moment de la modélisation (section 5), ce qui a amélioré la cohérence des prédictions. Le fichier `train_clean.csv`, socle de toutes les visualisations, conserve quant à lui l'intégralité des 1 460 observations, décrites par 83 variables (81 d'origine + 2 dérivées), sans aucune valeur manquante — comme le vérifie le tableau ci-dessous.


Table: Vérification du jeu de données nettoyé, calculée en direct sur train_clean.csv

|Indicateur         | Valeur|
|:------------------|------:|
|Observations       |   1460|
|Variables          |     83|
|Valeurs manquantes |      0|

Ce tableau est calculé en direct sur le fichier `train_clean.csv` au moment de la génération du rapport, garantissant la cohérence entre le rapport et les données réellement utilisées.


# Analyse exploratoire : l'enquête en cinq actes

L'analyse est structurée comme une enquête. Chaque acte pose une question, y répond par des visualisations, et passe le relais au suivant. Nous présentons ici les résultats clés de chaque acte ; l'intégralité des 24 visualisations est consultable dans l'application interactive.

## Les cinq indicateurs de référence

Cinq KPIs résument le marché et servent de repères tout au long de l'analyse :

| Indicateur | Valeur | Lecture |
|---|---|---|
| Prix médian global | 163 000 $ | Référence marché |
| Quartier le plus cher | Northridge Heights | 315 000 $ médian |
| Surface habitable médiane | 1 515 ft² | Maison typique |
| Qualité moyenne | 6,1 / 10 | OverallQual moyen |
| Amplitude du marché | ×21 | De 34 900 $ à 755 000 $ |

## Portraits de trois individus

Pour rendre l'amplitude du marché concrète, trois transactions réelles du dataset :

| Critère | Maison la moins chère | Maison médiane | Maison la plus chère |
|---|---|---|---|
| Prix de vente | 34 900 $ | 163 000 $ | 755 000 $ |
| Quartier | IDOTRR (voie ferrée) | North Ames | Northridge |
| Surface habitable | 720 ft² | 1 261 ft² | 4 316 ft² |
| Qualité globale | 4/10 | 6/10 | 10/10 |
| Année de construction | 1920 | 1958 | 1994 |
| Places de garage | Aucune | 2 | 3 |
| Condition de vente | Anormale (saisie) | Normale | Normale |
| Année de vente | 2009 (crise) | 2008 | 2007 (avant crise) |

La maison la plus chère vaut 21 fois la moins chère : surface six fois supérieure, qualité maximale contre qualité médiocre, quartier haut de gamme contre bordure de voie ferrée, et vente avant la crise contre saisie en pleine crise. Ce portrait croisé annonce déjà les facteurs que l'analyse confirmera,
Avant d'ouvrir les cinq actes, posons les repères chiffrés du marché et trois transactions réelles qui incarnent son amplitude.


## Acte 1 — Quoi se vend sur ce marché ?

Le marché d'Ames est dominé à 83 % par des maisons individuelles ; les appartements et maisons mitoyennes restent minoritaires. Cette répartition reflète le modèle d'étalement urbain américain, la ville s'étant développée horizontalement autour de son pôle universitaire.

Le prix des maisons individuelles couvre tous les budgets, tandis que celui des duplex se bloque autour de 200 000 $ : un duplex s'achète dans une logique d'investissement locatif, où le prix est plafonné par la rentabilité des loyers, alors qu'une maison individuelle répond à un achat de vie où les acheteurs paient une prime émotionnelle pour le confort et le standing.


\begin{center}\includegraphics[width=0.5\linewidth]{rapport_files/figure-latex/acte1_donut-1} \end{center}

*Figure — Composition du marché : 8 maisons vendues sur 10 sont des maisons individuelles.*

La qualité globale (`OverallQual`) crée une progression quasi mathématique : chaque niveau supplémentaire vaut environ 25 000 $ de plus. Et la dispersion s'élargit avec la qualité — sur le segment du luxe, la moindre variation de finition se traduit par des dizaines de milliers de dollars d'écart.


\begin{center}\includegraphics[width=0.75\linewidth]{rapport_files/figure-latex/acte1_violon-1} \end{center}

*Figure — Prix selon la qualité globale : la qualité 10/10 vaut 7 fois la qualité 1/10, et la dispersion s'élargit avec le niveau.*

## Acte 2 — Où sont les maisons les plus chères ?

L'écart de prix médian entre quartiers va du simple au quadruple : moins de 80 000 $ à Briardale, plus de 315 000 $ à Northridge Heights. Les prix ne progressent pas de façon fluide mais par paliers marqués : Ames n'est pas un marché uniforme, c'est une ville découpée en micro-marchés relativement étanches, chacun avec sa réputation, ses écoles et ses barrières financières à l'entrée.


\begin{center}\includegraphics[width=0.65\linewidth]{rapport_files/figure-latex/acte2_lollipop-1} \end{center}

*Figure — Prix médian par quartier : un rapport de 1 à 3,9 entre les extrêmes, dans la même ville.*

Une segmentation statistique (clustering hiérarchique sur cinq dimensions : prix, surface, qualité, garage, année de construction) confirme cette lecture : les 25 quartiers se regroupent naturellement en trois segments — Premium, Moyen, Abordable. Cette partition, obtenue par une méthode objective indépendante de l'observation visuelle, montre que les quartiers ne se répartissent pas au hasard mais selon une logique de marché cohérente et mesurable. La cartographie révèle en outre une fracture géographique nette : les quartiers premium se concentrent au nord et au nord-ouest de la ville, des développements résidentiels planifiés des années 1990-2000, construits avec parcs intégrés et positionnés loin de l'agitation du campus.

## Acte 3 — L'environnement crée-t-il une prime ou une décote ?

C'est l'un des résultats les plus spectaculaires de l'enquête : il existe plus de 115 000 $ d'écart entre une maison adjacente à un parc (235 000 $ médian, l'adjacence étant mesurée sur les deux variables de condition du dataset, Condition1 et Condition2) et une maison en bordure de route artérielle (119 550 $). Le bruit, la pollution et le danger des voies rapides provoquent une décote immédiate de 28 % ; le calme et la verdure d'un espace vert créent une prime de 41 %.


\begin{center}\includegraphics[width=0.6\linewidth]{rapport_files/figure-latex/acte3_barres-1} \end{center}

*Figure — Prix médian selon la condition environnementale : prime parc en vert, décotes routes et rail en rouge, référence du marché en pointillé.*

Détail révélateur : la voie ferrée décote bien moins qu'une grande route, car le passage d'un train est intermittent alors que le flux automobile est une nuisance continue. Ce n'est pas la présence d'une infrastructure qui pénalise le prix, mais la fréquence de la gêne qu'elle génère.

Cette prime environnementale s'explique par la rareté : moins de 2 % des maisons de la ville bénéficient d'un accès direct à un espace vert. La nature en ville est une ressource rare et figée — on ne crée pas un grand parc au milieu d'une zone déjà construite — et les acheteurs fortunés entrent en concurrence pour ces biens d'exception, ce qui sanctuarise leur valeur.

\begin{center}\includegraphics[width=0.5\linewidth]{rapport_files/figure-latex/acte3_waffle-1} \end{center}

*Figure — Sur 100 maisons, à peine 2 bénéficient d'un espace vert : la rareté fonde la prime de  %.*

## Acte 4 — Quels facteurs influencent vraiment le prix ?

La matrice de corrélation identifie trois forces majeures : la qualité globale (r = 0,79), la surface habitable (r = 0,71) et le garage (r = 0,64). Mais ces trois piliers sont fortement liés entre eux — les grandes maisons ont presque toujours de grands garages et de bonnes finitions. Un bien doit donc s'analyser comme un équilibre entre facteurs interconnectés, jamais comme une somme de critères indépendants.


\begin{center}\includegraphics[width=0.55\linewidth]{rapport_files/figure-latex/acte4_corr-1} \end{center}

*Figure — Matrice de corrélation des variables clés : qualité (r = 0,79), surface (r = 0,71) et garage (r = 0,64) dominent la relation au prix. Les cercles verts foncés hors diagonale révèlent l'interdépendance des facteurs entre eux.*

L'analyse des maisons premium (plus de 300 000 $) révèle une loi stricte : toutes respectent simultanément une surface supérieure à 2 000 ft² et une qualité d'au moins 8/10. Surface et qualité sont deux verrous non substituables — un immense garage ne compensera jamais des finitions médiocres. Aucune maison premium de l'échantillon ne déroge à cette double exigence.


\begin{center}\includegraphics[width=0.72\linewidth]{rapport_files/figure-latex/acte4_importance-1} \end{center}

*Figure — Importance des 12 variables les plus prédictives selon le Random Forest : la qualité globale domine, loin devant la surface habitable.*

Le Random Forest de la section 5 tranchera la hiérarchie : la qualité globale domine le pouvoir prédictif, loin devant la surface. Le standing perçu compte davantage que la taille brute.


## Acte 5 — Comment les prix évoluent-ils dans le temps ?


\begin{center}\includegraphics[width=0.72\linewidth]{rapport_files/figure-latex/acte5_evolution-1} \end{center}

*Figure — Évolution du prix médian 2006-2010 : le pic de 2007 puis la chute post-Lehman Brothers, -7,2 % en trois ans.*

Le prix médian culmine à 167 000 $ en 2007 puis chute à 155 000 $ en 2010, en pleine crise des subprimes. Fait notable : le volume de ventes reste stable (300 à 340 transactions par an de 2006 à 2009). La chute apparente de 2010 (175 ventes) est un artefact : le fichier s'arrête en juillet 2010. Ramené à un rythme annuel, le volume reste comparable — le marché d'Ames a encaissé la crise par un ajustement des prix, pas par un arrêt des transactions. Cette résistance s'explique par l'Iowa State University, qui maintient un flux constant d'étudiants et de personnel à loger, quel que soit le climat économique.

Les maisons construites après 1991 valent nettement plus que les anciennes : elles intègrent des standards de confort inexistants autrefois — plans ouverts, isolation performante, suites parentales, garages multiples. C'est cette rupture de conception, plus que l'âge des bâtiments, qui explique la prime sur la construction récente.

Enfin, la saisonnalité est d'une régularité spectaculaire : mai, juin et juillet concentrent le pic de ventes chaque année, crise ou pas. Ce rythme n'est pas météorologique mais sociologique : dans une ville universitaire, déménagements et mutations se calent sur le calendrier académique. Un vendeur qui met son bien sur le marché en juin profite d'une concurrence maximale entre acheteurs.


\begin{center}\includegraphics[width=0.75\linewidth]{rapport_files/figure-latex/acte5_saison-1} \end{center}

*Figure — Saisonnalité des ventes : mai-juillet concentrent le pic chaque année, crise ou pas. La ligne 2010 s'arrête en juillet, date de fin de la collecte.*

La crise a réduit les prix, mais n'a pas changé les règles du jeu : qualité, surface, quartier.


# Modélisation prédictive

L'enquête visuelle a identifié les facteurs du prix. Cette section les met à l'épreuve : si qualité, surface et quartier expliquent vraiment le prix, un modèle nourri de ces variables doit pouvoir le prédire avec précision.

## Protocole de validation

Le jeu de données nettoyé a été scindé aléatoirement en deux parties (graine aléatoire fixée pour la reproductibilité) : 80 % des observations pour l'entraînement, 20 % pour la validation. Les modèles sont évalués sur ces données de validation qu'ils n'ont jamais vues, avec le RMSE (racine de l'erreur quadratique moyenne) comme métrique : il s'interprète directement en dollars, comme l'erreur de prédiction typique du modèle.

Un choix de préparation mérite mention : la variable `Utilities` a été écartée avant modélisation. Elle est quasi constante (1 459 observations sur 1 460 partagent la même valeur) — aucun pouvoir prédictif réel, mais une source d'instabilité technique pour les modèles pénalisés dès qu'un découpage isole la valeur rare d'un seul côté.

## Cinq modèles en compétition

Cinq approches de complexité croissante ont été comparées :

1. **Régression linéaire simple** — six variables choisies sur des critères métier (qualité, surface, quartier, garage, sous-sol, année) : le modèle de référence, interprétable coefficient par coefficient.
2. **Régression stepwise (AIC)** — part d'un modèle à onze variables et laisse l'algorithme sélectionner la combinaison optimale au sens du critère d'information d'Akaike.
3. **Ridge** — régression pénalisée qui rétrécit les coefficients pour limiter le surapprentissage, en conservant toutes les variables.
4. **Lasso** — régression pénalisée qui pousse les coefficients inutiles exactement à zéro, opérant une sélection de variables automatique.
5. **Random Forest** — 500 arbres de décision agrégés, exploitant l'ensemble des variables et capturant les non-linéarités et interactions que les modèles linéaires ignorent.

## Résultats


\begin{longtable}[t]{>{\raggedright\arraybackslash}p{6cm}ll}
\caption{\label{tab:tab_modeles}Comparaison des cinq modèles sur les données de validation}\\
\toprule
Modèle & RMSE (validation) & En \% du prix médian\\
\midrule
Random Forest & 28 324 \$ & 17,4 \%\\
Régression stepwise (AIC) & 32 649 \$ & 20,0 \%\\
Ridge & 33 507 \$ & 20,6 \%\\
Lasso & 34 584 \$ & 21,2 \%\\
Régression linéaire simple & 34 833 \$ & 21,4 \%\\
\bottomrule
\end{longtable}


\begin{center}\includegraphics[width=0.75\linewidth]{rapport_files/figure-latex/fig_rmse_modeles-1} \end{center}

*Figure — Comparaison des cinq modèles : le Random Forest (orange) réduit l'erreur d'environ 6 500 $ par rapport au meilleur modèle linéaire.*

Le Random Forest domine nettement, avec une erreur typique de 28 324 $ — soit 17,4 % du prix médian — et 87,8 % de la variance des prix expliquée sur des données jamais vues. Sa supériorité sur les modèles linéaires (environ 6 500 $ d'écart avec le meilleur d'entre eux) confirme un enseignement de l'analyse exploratoire : le prix ne résulte pas d'une addition de facteurs indépendants, mais de leurs interactions — la surface ne prend sa pleine valeur qu'associée à la qualité, et le même bien ne vaut pas le même prix selon le quartier. C'est précisément ce type d'interactions qu'une forêt aléatoire capture et qu'une régression linéaire ignore.

La hiérarchie de l'importance des variables du Random Forest (figure de l'Acte 4) confirme le verdict de l'enquête : la qualité globale domine le pouvoir prédictif, devant la surface habitable — le standing perçu compte davantage que la taille brute.

## Validation externe : la soumission Kaggle

Pour éprouver notre chaîne de traitement au-delà de nos propres données de validation, des prédictions ont été soumises sur les 1 459 observations du fichier `test.csv` de la compétition Kaggle, dont les prix réels ne sont pas publics.

Un sixième modèle a été mobilisé pour cette soumission : XGBoost, un algorithme de gradient boosting réputé pour ses performances en compétition. Ce choix reflète une distinction classique en data science entre deux objectifs : le Random Forest reste notre modèle de référence — interprétable, analysé en détail et intégré au simulateur de l'application — tandis que XGBoost joue le rôle de modèle de compétition, optimisé pour la seule performance prédictive sur la métrique officielle.

Le score obtenu — 0,12714 (RMSE sur le logarithme des prix, métrique officielle de la compétition) — constitue une validation entièrement externe et indépendante de la qualité de l'ensemble de notre chaîne, du nettoyage à la modélisation.

## Du modèle à l'outil : le simulateur

Le modèle Random Forest n'est pas resté un objet d'étude : il est intégré à l'application Shiny sous forme de simulateur de prix. L'utilisateur saisit six caractéristiques (surface, qualité, quartier, garage, sous-sol, année de construction) et obtient instantanément une estimation. Un module complémentaire de type k-NN (plus proches voisins) affiche les cinq biens réels du dataset les plus comparables à la demande, chacun accompagné du critère sur lequel il s'en rapproche le plus. Le modèle est pré-entraîné et chargé au démarrage de l'application, conformément au cycle de vie standard d'un modèle en production : on n'entraîne pas à chaque utilisation, on entraîne une fois et on sert les prédictions.


# L'application interactive : du récit au produit

L'enquête est restituée dans une application web interactive, développée avec Shiny (shinydashboard) et déployée sur shinyapps.io — accessible par le lien : **https://cam-s.shinyapps.io/ames-immobilier**

## Architecture de l'application

L'application est organisée en neuf pages accessibles depuis un menu latéral,
qui reprennent exactement la structure de l'enquête : une page d'accueil
(indicateurs parcours guidé), l'Acte 0 (méthodologie et
données nettoyées, téléchargeables), les cinq actes de
l'analyse, une page Prédiction et une page Recommandations. À l'intérieur de
chaque acte, les visualisations sont regroupées en sous-onglets par question
posée.


\begin{center}\includegraphics[width=0.6\linewidth]{images/dashboard_accueil} \end{center}

*Figure — Page d'accueil de l'application : les cinq indicateurs de
référence, le carrousel des segments de marché et l'ouverture narrative
de l'enquête.*

Trois mécanismes distinguent cette application d'un document statique :

- **Un filtre global par quartier**, dans le menu latéral, recalcule en direct
  les graphiques des Actes 1 et 3 : chaque graphique est branché sur une
  expression réactive qui filtre les données à la demande, permettant de rejouer l'analyse sur un seul quartier.
- **Un comparateur de quartiers** (Acte 2) superpose les profils de deux quartiers au choix sur cinq dimensions — prix, surface, qualité, garage, récence — et génère automatiquement une lecture textuelle de la comparaison.
- **Un simulateur de prix** (page Prédiction) interroge en direct le modèle Random Forest de la section 5 : l'utilisateur décrit un bien en six caractéristiques et obtient instantanément une estimation, accompagnée des cinq biens réels les plus comparables du dataset (module k-NN).


\begin{center}\includegraphics[width=0.6\linewidth]{images/dashboard_simulateur} \end{center}

*Figure — Page Prédiction : comparaison des cinq modèles et simulateur de
prix connecté au Random Forest pré-entraîné.*


## Une charte visuelle unique

L'application et le rapport partagent une même charte graphique, construite
sur les principes du cours de visualisation. Elle repose sur deux couleurs
d'identité — l'orange (#E8721C) et le bleu marine (#102A43) — déclinées selon
la nature des variables : dégradés séquentiels pour les valeurs ordonnées
(qualité, prix), palettes divergentes pour les corrélations, couleurs
catégorielles distinctes pour les variables nominales comme les types de
biens. Les conventions culturelles complètent ce code : le rouge signale les
décotes et les nuisances (route artérielle, chute de 2008), le vert les
primes (parc, segment Premium), le bleu les références neutres. Enfin, chaque
graphique porte un titre formulé comme un insight, est suivi d'un encadré
« Ce que ça révèle », et sa forme visuelle découle de la classification des
variables établie en section 2 — la forme suit la donnée, jamais l'inverse.


# Recommandations stratégiques et conclusion

L'enquête a livré ses conclusions. Reste à les rendre actionnables : cinq recommandations, une par profil d'acteur du marché, chacune ancrée dans un résultat chiffré de l'analyse.

## Cinq recommandations par profil d'acteur

**R1 — Pour l'acheteur individuel : cibler les quartiers sous-évalués.** Timberland (228 950 $) et Somerset (226 000 $) présentent des profils multicritères quasi identiques à Northridge Heights (315 000 $) — qualité, surface, récence — pour des prix inférieurs de 30 %. Une maison de qualité 7 ou plus, dépassant 1 500 ft² dans ces quartiers, offre le meilleur rapport qualité-prix du marché. *Fondement : classement des quartiers et profils radar par segment (Acte 2).*

**R2 — Pour le vendeur : rénover la cuisine et l'extérieur, vendre en mai-juin.** La qualité de la cuisine et la qualité extérieure figurent dans le top 5 des variables les plus prédictives du Random Forest : passer d'une finition moyenne à bonne peut générer 15 000 à 20 000 $ de plus-value. Et le calendrier compte : mai et juin concentrent le pic annuel de ventes, avec une concurrence maximale entre acheteurs. *Fondement : importance des variables (Acte 4) et saisonnalité (Acte 5).*

**R3 — Pour l'investisseur : acheter en condition de vente anormale.** Les ventes anormales (saisies notamment) affichent une décote d'environ 18 % par rapport aux ventes classiques. Ciblées sur des biens de qualité 6 ou plus dans les quartiers médians (Mitchell, Gilbert, Northwest Ames), ces opportunités constituent un arbitrage systématique. *Fondement : analyse des conditions de vente et évolution temporelle (Acte 5).*

**R4 — Pour le promoteur immobilier : construire en zone résidentielle avec espaces verts.** Une maison adjacente à un parc se vend 235 000 $ en médiane, soit une prime de 41 % par rapport à la référence du marché — et 115 000 $ de plus qu'en bordure de route artérielle. Un programme résidentiel intégrant un parc dès la conception se positionne d'emblée sur le segment 200 000-250 000 $. *Fondement : analyse environnementale complète (Acte 3).*

**R5 — Pour l'urbaniste et la collectivité : les espaces verts créent une valeur mesurable.** L'écart de 115 000 $ par maison entre proximité d'un parc et bordure d'artère, appliqué à un quartier de 100 maisons, représente 5 à 10 millions de dollars de valorisation du patrimoine immobilier local. La création d'espaces verts n'est pas une dépense d'agrément : c'est un investissement dont le retour est directement lisible dans les prix de vente. *Fondement : prime environnementale et rareté des espaces verts (Acte 3).*

## Limites de l'étude

Trois limites bornent la portée de ces conclusions. D'abord, la fenêtre temporelle : les données couvrent 2006-2010, et aucune extrapolation directe vers la valeur actuelle du marché n'est possible sans données récentes. Ensuite, 2010 est une année partielle — le fichier s'arrête en juillet — ce qui impose la prudence sur les statistiques de cette année-là, dont la baisse apparente de volume est un artefact de collecte. Enfin, les résultats décrivent le marché d'une ville universitaire américaine moyenne : la hiérarchie des facteurs (qualité, surface, quartier) est vraisemblablement générale, mais leurs poids exacts ne se transposent pas tels quels à d'autres contextes.

La perspective naturelle de ce travail serait d'actualiser l'analyse avec des transactions récentes, et d'enrichir le modèle de variables macro-économiques locales (taux d'intérêt, emploi universitaire).

## Conclusion

Au terme de cette enquête, la question initiale — qu'est-ce qui fait le prix d'une maison ? — a trouvé une réponse étayée à chaque étape : par l'exploration visuelle des 24 graphiques, par la segmentation statistique des quartiers, et par la validation prédictive de cinq modèles concurrents.

La qualité globale domine tout : c'est la variable la plus corrélée au prix (r = 0,79) et la plus prédictive du Random Forest. La surface vient ensuite, mais ne vaut pleinement qu'associée à la qualité — aucune maison premium du dataset ne déroge à la double exigence qualité 8+ et surface 2 000+ ft². Le quartier, enfin, agit comme un multiplicateur : le même bien ne vaut pas le même prix selon l'endroit où il se trouve, dans un rapport allant jusqu'à 3,9 entre les extrêmes de la ville. Autour de ce trio, l'environnement immédiat module la valeur — prime de 41 % pour un parc, décote de 28 % pour une artère — et la crise de 2008 a démontré que si les prix peuvent chuter, la hiérarchie des facteurs, elle, résiste.

1 460 transactions. 80 variables. 5 actes. La réponse tient en trois mots : qualité, surface, quartier. Mais comprendre comment ces trois facteurs interagissent — c'est ça, la valeur ajoutée de l'analyse de données.

\newpage


# Organisation et collaboration

## Répartition du travail

Le projet a été conduit en binôme, avec une répartition par domaines et une
révision croisée systématique :

| Membre | Responsabilités principales |
|---|---|
| CAMARA Massaram | Visualisations des Actes 1 et 2 · application Shiny (architecture, design, déploiement) · rédaction et mise en forme du rapport |
| LOGBO Axelle | Nettoyage des données · visualisations des Actes 3 à 5 · modélisation (cinq modèles, XGBoost, soumission Kaggle) |

Le fil narratif, les recommandations et la relecture finale ont été menés en commun. Avant le lancement, un cahier des charges interne d'une trentaine de pages a été rédigé : classification des variables, plan des visualisations, principes de design, planning jour par jour sur deux semaines. Ce document a servi de contrat d'équipe tout au long du sprint.

## Versionnement avec Git et GitHub

L'ensemble du projet est versionné sur GitHub. Chaque domaine de travail a
fait l'objet d'une branche dédiée , fusionnée dans `main`
après revue par l'autre membre. Les messages de commit suivent une convention par préfixes qui rend l'historique lisible comme un journal de bord du projet.

Ce fonctionnement a eu un bénéfice concret : les deux membres ont travaillé
en parallèle pendant deux semaines, sur les mêmes fichiers de données, sans jamais se bloquer mutuellement ni perdre une version.

# Références {-}

- De Cock, D. (2011). *Ames, Iowa: Alternative to the Boston Housing Data as
  an End of Semester Regression Project*. Journal of Statistics Education, 19(3).
- Kaggle. *House Prices — Advanced Regression Techniques*.
  https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques
- Documentation officielle du dataset Ames Housing (codebook des 80 variables).
- R Core Team (2025). *R: A Language and Environment for Statistical Computing*.
  Packages mobilisés : ggplot2, dplyr, plotly, highcharter, leaflet, visNetwork,
  ggridges, GGally, corrplot, fmsb, randomForest, xgboost, shiny, shinydashboard,
  kableExtra.
- Application déployée : https://cam-s.shinyapps.io/ames-immobilier ·
  Code source : https://github.com/masscamara1225-beep/projet-immobilier-ames

\newpage


# Annexe — Catalogue des visualisations

Les consignes du projet exigeaient un minimum de 15 visualisations
différentes, dont sept formes obligatoires : histogramme, diagramme en
barres, diagramme circulaire, carte géographique, courbe temporelle,
heatmap et dashboard interactif. Notre cahier des charges interne, rédigé
en début de projet, avait relevé cette barre à 24 visualisations planifiées.
L'application en livre finalement 25 analytiques — toutes les formes
obligatoires y figurent, dont deux heatmaps (corrélations et saisonnalité)
et une carte géographique interactive — auxquelles s'ajoutent 3 graphiques
d'aperçu sur la page d'accueil.
Deux évolutions par rapport au plan initial sont assumées : la
heatmap des valeurs manquantes a été remplacée par un tableau d'audit
détaillé accompagné du jeu de données téléchargeable (plus exploitable
qu'une image), et le nuage de points avec droite de régression a été
absorbé par le diagramme à bulles et la vue 3D, qui montrent la même
relation surface-prix enrichie de dimensions supplémentaires. Trois
visualisations ont été ajoutées en cours de projet : le comparateur
interactif de quartiers, le dendrogramme de segmentation et la carte
géographique.

\begingroup\fontsize{8}{10}\selectfont

\begin{longtable}[t]{>{\raggedright\arraybackslash}p{2.6cm}>{\raggedright\arraybackslash}p{5.2cm}>{\raggedright\arraybackslash}p{2.6cm}>{\raggedright\arraybackslash}p{5.0cm}}
\toprule
Acte & Visualisation & Package & Question répondue\\
\midrule
\endfirsthead
\multicolumn{4}{@{}l}{\textit{(continued)}}\\
\toprule
Acte & Visualisation & Package & Question répondue\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
 & Histogramme du prix de vente & plotly & Quelle est la distribution des prix ?\\
\cmidrule{2-4}\nopagebreak
 & Courbes de densité par type de bien & ggplot2 & Le type de bien détermine-t-il le segment ?\\
\cmidrule{2-4}\nopagebreak
 & Anneau — composition du marché & plotly & Quelle est la composition du marché ?\\
\cmidrule{2-4}\nopagebreak
 & Sucettes — prix par quartier & ggplot2 & Quels quartiers sont les plus chers ?\\
\cmidrule{2-4}\nopagebreak
\multirow[t]{-5}{2.6cm}[4\dimexpr\aboverulesep+\belowrulesep+\cmidrulewidth]{\raggedright\arraybackslash Acte 1 · Marché} & Violons — prix par qualité & ggplot2 & La qualité détermine-t-elle le prix ?\\
\cmidrule{1-4}\pagebreak[0]
 & Boîtes à moustaches par quartier & highcharter & Quelle dispersion des prix par quartier ?\\
\cmidrule{2-4}\nopagebreak
 & Treemap — volume et prix & highcharter & Quels quartiers dominent le marché ?\\
\cmidrule{2-4}\nopagebreak
 & Barres empilées 100 \% — typologie & plotly & Quel quartier est le plus diversifié ?\\
\cmidrule{2-4}\nopagebreak
 & Radar — profils des segments & fmsb & Quel profil type par segment de marché ?\\
\cmidrule{2-4}\nopagebreak
 & Radar — comparateur de quartiers (+) & fmsb & Comment deux quartiers se comparent-ils ?\\
\cmidrule{2-4}\nopagebreak
 & Dendrogramme — segmentation (+) & stats + ggplot2 & Quels segments naturels de quartiers ?\\
\cmidrule{2-4}\nopagebreak
\multirow[t]{-7}{2.6cm}[6\dimexpr\aboverulesep+\belowrulesep+\cmidrulewidth]{\raggedright\arraybackslash Acte 2 · Géographie} & Carte géographique des prix (+) & leaflet & Où se situent les prix dans la ville ?\\
\cmidrule{1-4}\pagebreak[0]
 & Barres — prix par condition & highcharter & L'environnement influence-t-il le prix ?\\
\cmidrule{2-4}\nopagebreak
 & Nuage de points coloré par condition & plotly & La condition modifie-t-elle surface/prix ?\\
\cmidrule{2-4}\nopagebreak
 & Rose de Nightingale — fréquences & ggplot2 & Quelle rareté des espaces verts ?\\
\cmidrule{2-4}\nopagebreak
\multirow[t]{-4}{2.6cm}[3\dimexpr\aboverulesep+\belowrulesep+\cmidrulewidth]{\raggedright\arraybackslash Acte 3 · Environnement} & Gaufre — proportions & ggplot2 & Quelle proportion bénéficie d'un parc ?\\
\cmidrule{1-4}\pagebreak[0]
 & Matrice de corrélation & corrplot & Quelles variables sont liées au prix ?\\
\cmidrule{2-4}\nopagebreak
 & Réseau de corrélations & visNetwork & Quels groupes de variables co-évoluent ?\\
\cmidrule{2-4}\nopagebreak
 & Bulles — surface, prix, qualité & plotly & Surface + qualité = prix premium ?\\
\cmidrule{2-4}\nopagebreak
 & Nuage 3D — surface, qualité, prix & plotly 3D & Les trois facteurs co-évoluent-ils ?\\
\cmidrule{2-4}\nopagebreak
 & Coordonnées parallèles — premium & GGally & Quel profil pour les maisons chères ?\\
\cmidrule{2-4}\nopagebreak
\multirow[t]{-6}{2.6cm}[5\dimexpr\aboverulesep+\belowrulesep+\cmidrulewidth]{\raggedright\arraybackslash Acte 4 · Facteurs} & Importance des variables (RF) & randomForest + ggplot2 & Quelles variables prédisent le mieux ?\\
\cmidrule{1-4}\pagebreak[0]
 & Courbe — évolution 2006-2010 & highcharter & La crise de 2008 a-t-elle changé les prix ?\\
\cmidrule{2-4}\nopagebreak
 & Crêtes — prix par ère & ggridges & Les maisons récentes valent-elles plus ?\\
\cmidrule{2-4}\nopagebreak
\multirow[t]{-3}{2.6cm}[2\dimexpr\aboverulesep+\belowrulesep+\cmidrulewidth]{\raggedright\arraybackslash Acte 5 · Temporel} & Heatmap — saisonnalité des ventes & ggplot2 & Y a-t-il un meilleur moment pour vendre ?\\*
\end{longtable}
\endgroup{}

*Les visualisations marquées (+) sont des ajouts réalisés en cours de
projet, absents du cahier des charges initial. S'y ajoutent les trois
graphiques d'aperçu de la page d'accueil (prix par segment, composition
du marché, volume mensuel) et les outils de la page Prédiction
(comparaison des modèles, simulateur, biens comparables).*
