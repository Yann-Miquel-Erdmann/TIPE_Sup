# déroulé
- intro
- problématique
- plan
- définition transpileur (et ses parties)
- montrer l'exemple et la sortie que l'on souhaite obtenir
exemple pour toute la présentation serait un hello world (avec un if?)

  - analyse lexicale
  - analyse syntaxique
  - traduction

- explication des différents paramètres du transpileur
  le language d'entrée n'a un effet que sur les deux premières parties, et la troisième partie ne dépend que du language de sortie
  donc il serait possible de brancher une troisième partie différente au deux premières et faire de la transpilation de fortran vers python ou même de fortran vers fortran. On construit donc une machine à l'aide de ces trois parties.

- analyse lexicale
  but transformer le code en une liste de lexèmes montrer le code et la liste obtenue

  automates, explication pas trop en détails
  - montrer un automate qui reconnaît un lexème   
    (on choisit le dernier automate qui n'est pas dans un puits)

  <!-- - tableau qui reconnaît un petit programme ex hello world avec les automates de chaque lexème utilisé -->

- analyse syntaxique, en détails 
  but transformer la liste de lexèmes en un arbre syntaxique (abstrait plus tard après avoir parlé de LL1 et des problèmes dûs à la dérivation gauche)

  - grammaire non contextuelle du fortran exemple d'une grammaire qui n'est pas LL1 A*B 
  
  - algorithme LL1:
  
    - avantages lecture en temps linéaire, comment? on sait à chaque lexème lu quels sont les lexèmes qui peuvent suivre et il n'y a pas d’ambiguïté


    - pour cela on garde pour chaque symbole non terminal, le liste des lexèmes qui peuvent débuter dans la dérivation gauche.  la liste des lexèmes qui peuvent le suivre
    exemple avec un tableau comme [ici](https://www.geeksforgeeks.org/construction-of-ll1-parsing-table/)

    - une partie du pseudo code 

  - on obtient une arbre de syntaxe qui représente le code mais ses noeuds représentent les symboles de la grammaire. Il est judicieux de transformer cet arbre des syntaxe en un arbre de syntaxe abstraite l'idée est de ne pas avoir des nœuds qui ne dépendent ni du language d'entrée ni du language de sortie. on simplifie grandement l'arbre de syntaxe (pas moi qui l'ai fait)


- traduction 
  on parcourt l'arbre de la racine vers les feuilles mais on fait pas que concaténer en effet print -> "Hello World"  se traduit en print( "Hello World" )
  on a donc une fonction qui pour chaque lexème traduit un noeud qu'il étiquette    



- conclusion
  
