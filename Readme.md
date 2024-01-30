# Projet de Programmation : Un interpréteur pour les automates à pile déterministes

## Membres

MAZELET Florent     flomazelet@gmail.com 21958561
WALLISER Marie      marie.walliser@free.fr  21979046

## Compilation

```make```

## Lancement

```./parser -affiche    MonFichier          -> ré-affiche l'automate tel qu'il a été analysé.```
```./parser -conforme   MonFicher           -> vérification de la bonne formation de l'automate.```
```./parser -lit        MonFichier MonMot   -> teste si le mot est reconnu.```

```./parser -affiche3    MonFichier          -> ré-affiche le programme tel qu'il a été analysé. ```
```./parser -lit3        MonFichier MonMot   -> teste si le mot est reconnu.```

-affiche3 n'est pas très utile car l'affichage du programme n'affiche pas les Begin et end car ils ne sont pas stockés. 

## Fonctionnalités implémentées

### Partie 1

- analyse d'un automate décrit dans un fichier (parser.mly et lexer.mll)
- construction de l'arbre de syntaxe (types dans ast.ml)
- interpréteur de l'arbre pour lire un mot (ast.ml)
- gestion des erreurs lors d'une lecture

### Partie 2

- vérification de la bonne formation de l'automate (état initial et symbole de pile initial)
- vérification que l'automate est déterministe

### Partie 3

- analyse d'un automate décrit dans un fichier (parserPhase3.mly et lexerPhase3.mll)
- interpréteur de l'arbre pour lire un mot
-Il n'y a pas de vérification sur le programme comme nous devions les faire dans la partie 2

## Répartition du travail

### Florent

- partie 1 : lecture d'un mot
- partie 2 : vérifications
- partie 3 : lexeur et parseur, lecture d'un mot

### Marie

- partie 1 : lexeur, construction de l'automate, lecture d'un mot, messages d'erreurs 
- partie 2 : correction de bugs
