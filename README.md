# InterpolationTools

<!-- badges: start -->
<!-- badges: end -->

L'application Shiny est accessible via une interface utilisateur graphique en ligne. 
Les utilisateurs peuvent charger leur propre jeu de données en format CSV, choisir parmi différents algorithmes de modélisation pour interpoler les valeurs manquantes, et afficher les résultats sous forme de graphique.   

## Installation

Vous pouvez installer le package InterpolationTools à partir de [GitHub](https://github.com/) :

``` r
# install.packages("devtools")
devtools::install_github("AlexisMayer/InterpolationTools")
```

## ShinyApp

Le package inclut une application R-Shiny qui vous permet d'utiliser l'outil depuis votre navigateur web favori. 

``` r
library(InterpolationTools)
launch_app()
```
