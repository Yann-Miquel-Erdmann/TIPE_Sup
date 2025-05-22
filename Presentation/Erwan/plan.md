# déroulé

- intro
- problématique
- plan
- définition transpileur (et ses parties)

# détail important

- on suppose que la syntaxe fortran utilisée est correcte, c'est-à-dire qu'elle compile et s'exécute correctement
  => la grammaire n'est pas parfaite et ne prend pas en compte le nom des variables par exemple

## définitions (à définir tout au long de la présentation?)

- transpileur (comment ça fonctionne, les différentes étapes pour passer d'un langage à l'autre, dire que deux étapes disjointes avec début et fin interchangeables)

- présenter le Fortran (rapidement, utilisations actuelles, montrer que ça existait avant le C)

- expressions régulières (définition + comment ça fonctionne: les différents termes utilisés)

- automates (définir les différents types + leur utilité dans le programme)

- grammaire (définir => pas trop rentrer dans les détails, boulot de Yann + dire utilisé dans la construction automatique des automates pour trouver les terminaux)

- arbre de la syntaxe abstraite => abres qui peuvent représenter n'importe quel langage sans garder d'information qui lui est propre

## objet de l'étude

- syntaxe réduite de Fortran qui permet tout de même de créer de courts programmes

- Sa transformation en C

=> la création d'un transpileur de Frotran vers C

## éléments principaux à présenter

### analyse lexicale

- montrer comment automate reconnait les mots
  => 2 exemples, un simple pour comprendre, l'autre pour montrer un peu plus en détail le fonctionnement

- déterminisation permet avoir analyse en O(n) plutôt que d'avoir des problèmes de backtracking
  => représentation des automates

- même avec une syntaxe

### analyse syntaxique

=> Yann avec LL1, présenter rapidement sans rentrer dans les détails

=> analyse syntaxique et lexicale automatiquement créée à partir de la grammaire
=> gagner beaucoup de temps

### transformation de l'arbre de syntaxe en arbre de syntaxe abstraite

- permet d'avoir un langage de sortie modulaire

- présentation d'un exemple simple mais en réalité beaucoup plus compliqué à cause de LL1, arbre massif => arbre de sortie compact

- montrer que ce n'est pas possible de l'automatiser comme la création de la grammaire pour LL1 et l'automate

- interet: avoir un arbre qui se généralise pour tout langage et avec lequel on peut retrouver la syntaxe de sortie
  => on pourrait faire la même chose avec le C vers le fortran (était prévu à la base mais manque de temps)

### conversion de l'arbre de syntaxe abstraite vers le langage de sortie

- monter que cette étape est utile, pas que de la concaténation, il y a aussi un peu de remaniement de la syntaxe + un peu plus de calcul pour certains aspect

## un peu de détail sur l'automatisation

- pour créer la première partie de l'analyse, avec l'automate et LL1, il suffit d'avoir une grammaire valide, compatible avec LL1 (yann s'en est chargé) de n'importe quel langage. On y ajoute quelques règles sur les terminaux, pour la création et l'exécution de l'automate, et qui sert à donner ce qui est utile pour LL1 / la réécriture plus tard.

- pas possible d'automatiser la création de l'arbre de syntaxe abstraite, procédé pas trop difficile mais long et très peu lisible une fois formatté par ocamlformat

- pas d'automatisation non plus de la syntaxe abstraite vers le langage de sortie
