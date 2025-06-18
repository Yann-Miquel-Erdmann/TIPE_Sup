type symbol = 
  | Terminal of terminal 
  | NonTerminal of non_terminal

type motif = symbol list
type regle = symbol * motif list
type regle_hashtable = 
  (symbol, motif list) Hashtbl.t
type grammaire = {
  regles_htbl : regle_hashtable; 
  start_symbol : symbol 
}
