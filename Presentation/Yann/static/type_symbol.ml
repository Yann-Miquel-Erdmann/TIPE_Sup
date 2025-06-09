type terminal =
  | PrintMC
  | ProgramMC
  | EndProgramMC
  | Virgule
  | Asterisque
  | Espace
  | NouvelleLigne
  | NomProgramme
  | Chaine

type non_terminal = Programme | Print | Param_liste
type symbol = Terminal of terminal | NonTerminal of non_terminal
