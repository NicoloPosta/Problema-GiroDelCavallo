exception NonTrovato;;


let salto_del_cavallo inizio n algoritmo =

    (*Lista delle mosse che il cavallo può compiere *)
    let mosse_cavallo = [(-1,-2); (-2,-1); (1,-2); (-2,1); (-1,2); (2,-1); (1,2); (2,1)] in

    (*Questa funzione calcola la casa nella quele arriva il cavallo data una certa casa iniziale ed una mossa da eseguire *)
    let esegui_mossa (a,b) (c,d) = (a+c,b+d) in

    (*Lunghezza massima del cammino in una matrice n x n, ovvero la grandezza della scacchiera *)
    let max_cammino = (n * n) in

    (*Controlla se la mossa eseguita dal cavallo è una mossa valida all'interno della scacchiera,
    ovvero controlla se il cavallo non esce fuori dalla scacchiera *)
    let mossa_in_scacchiera (a,b) =
        (a >= 0) && (a < n) &&
        (b >= 0) && (b < n) in

    (*Ritorna la lista delle mosse valide partendo da una casa (x,y) posizione_cavallo *)
    let mosse_valide posizione_cavallo =
        List.filter (mossa_in_scacchiera)
                (List.map (esegui_mossa posizione_cavallo) mosse_cavallo) in

    (*Implementazione dell'euristica di Warnsdorff:
    L'euristica funziona andando ad posizionare in ordine crescente le nuove mosse possibili del cavallo
    in base a quante mosse potrà compiere nello step successivo a quello preso in esame.
    Questo mi porta a dare priorità alle celle esterne della scacchiera perchè al lato di essa varò meno mosse da poter compiere 
    evitando così di creare punti irraggiungibili e permettendo di risolvere il problema, in un tempo ragionevole, anche con matrici di medio/grandi dimensioni;
    al contrario della DFS senza euristica la quale non è in grado di risolvere nemmeno una matrice 8x8 *)
    let confronto elem1 elem2 = 
        let successivi_1 = List.filter (fun (x,y) -> not(List.mem (x,y) elem1)) 
                                                        (mosse_valide (List.hd elem1)) in

        let successivi_2 = List.filter (fun (x,y) -> not(List.mem (x,y) elem2)) 
                                                        (mosse_valide (List.hd elem2)) in 

        let conto_1 = List.length(successivi_1) in

        let conto_2 = List.length(successivi_2) in

        (*Serve come funzione per l'ordinamento di List.sort*)
        if conto_1 > conto_2 then 1
        else if conto_1 = conto_2 then 0
        else -1 in

    (*Funzioni utilizzate per abbellire l'output del programma, servono per disegnare una matrice con indicato i numeri delle mosse del cavallo *)
    (*Controlla iterativamente il numero da inserire nella cella della matrice che sto stampando grazie al cammino trovato come soluzione.
    Decrementa il numero da stampare fino a che non lo trova nella lista della soluzione *)
    let rec aggiunta_indice numero_stampa posizione soluzione =
      if (List.hd soluzione) = posizione
      then numero_stampa
      else aggiunta_indice (numero_stampa - 1) posizione (List.tl soluzione) in

    (*Trova usando la funzione aggiunta_indice il numero da stampare nella matrice  *)
    let indice posizione soluzione = aggiunta_indice max_cammino posizione soluzione in

    (*Stampa una matrice a schermo che mostra i passi del cavallo come soluzione *)
    let stampa_soluzione soluzione =
        (*Stampa la riga superiore della matrice *)
        for i = 1 to n do
            Printf.printf "-----";
        done;
        Printf.printf "-\n";
        (*Stampa le righe intermedie e gli elementi della matrice *)
        for i = 0 to n-1 do
            for j = 0 to n-1 do
                Printf.printf "| %2i " (indice (i,j) soluzione);
            done;
            Printf.printf "|\n";
            (*Stampa la riga inferiore della matrice *)
            for j = 1 to n do
                Printf.printf "-----";
            done;
            Printf.printf "-\n";
        done in
    
    (*DFS con implementata l'euristica di Warnsdorff*)
    let dfs_Warnsdorff inizio successivi = 
        let estendi cammino = 
                List.map (function (x,y) -> (x,y)::cammino)
                    (List.filter (fun (x,y) -> not (List.mem (x,y) cammino))
                                (successivi (List.hd cammino)))
        in let rec ricerca_ausiliaria = function
                [] -> raise NonTrovato
                | cammino::resto -> if List.mem (List.hd cammino) (successivi inizio) && max_cammino = List.length cammino
                    then stampa_soluzione cammino
                    else ricerca_ausiliaria ((List.sort confronto (estendi cammino)) @ resto)
        in ricerca_ausiliaria [[inizio]] in

    (*DFS base *)
    let dfs inizio successivi = 
        let estendi cammino = 
                List.map (function (x,y) -> (x,y)::cammino)
                    (List.filter (fun (x,y) -> not (List.mem (x,y) cammino))
                                (successivi (List.hd cammino)))
        in let rec ricerca_ausiliaria = function
                [] -> raise NonTrovato
                | cammino::resto -> if List.mem (List.hd cammino) (successivi inizio) && max_cammino = List.length cammino
                    then stampa_soluzione cammino
                    else ricerca_ausiliaria ((estendi cammino) @ resto)
        in ricerca_ausiliaria [[inizio]] in

    (*Scelta dell'algoritmo da utilizzare *)
    if algoritmo = "DFS"
    then dfs inizio mosse_valide
    else if algoritmo = "Warnsdroff"
    then dfs_Warnsdorff inizio mosse_valide;;

(*Funzioone che styampa gli esempi *)
let stampa_esempi stampa =
    if stampa = "si"
    then
    (Printf.printf "Esempio scacchiera 6x6 ed inizio nella casa (1,2) con suluzione trovata dalla DFS\n";
    salto_del_cavallo (1,2) 6 "DFS";
    Printf.printf "Esempio scacchiera 6x6 ed inizio nella casa (3,3) con suluzione trovata usando l'euristica di Warnsdorff\n";
    salto_del_cavallo (3,3) 6 "Warnsdroff";
    Printf.printf "Esempio scacchiera 8x8 ed inizio nella casa (4,4) con suluzione trovata usando l'euristica di Warnsdorff\n";
    salto_del_cavallo (4,4) 8 "Warnsdroff");;

(*Funzione main per il controllo degli imput da riga di comando *)
let main = 
    if Array.length Sys.argv = 2
    (*Stampa degli esempi *)
    then (if Sys.argv.(1) = "-e"
         then stampa_esempi "si"
         else Printf.printf "Per stampare gli esempi usare il comando \"-e\"\nSe invece si vuole eseguire il programma è possibile farlo inserendo \nla casa di partenza e la grandezza della scacchiera così: 2 3 7 \"algoritmo\"\ndove 2 è la riga, 3 la colonna, 7 la n della scacchiera nxn.\nGli algoritmi implementati sono DFS e Warnsdroff")
    else if (Array.length Sys.argv = 5)
        (*Inserimento dei parametri tramite riga di comando
        Esempio: 3 3 6 "Warnsdorff"
        inizio nella casa (3,3) in una scacchiera 6x6 usando come emtodo risolutivo la DFS con l'euristica di Warnsdorff*)
         then if (Sys.argv.(4) = "DFS" || Sys.argv.(4) = "Warnsdroff") 
              then if((int_of_string Sys.argv.(3)) < 6 || (int_of_string Sys.argv.(3)) mod 2 = 1)
                    then Printf.printf "Per essere risolvibile la scacchiera deve essere di grandezza maggiore di 5 e con numero di righe e colonne pari.\nAd esempio 6 o 8 o 10..."
                    else (Printf.printf "Scacchiera %dx%d ed inizio nella casa (%d,%d) con suluzione trovata usando %s\n" (int_of_string Sys.argv.(3)) (int_of_string Sys.argv.(3)) (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2)) Sys.argv.(4);
                    salto_del_cavallo ((int_of_string Sys.argv.(1)), (int_of_string Sys.argv.(2))) (int_of_string Sys.argv.(3)) Sys.argv.(4))
              else Printf.printf "Inserire un algoritmo fra DFS o Warnsdroff"
         else Printf.printf "Per stampare gli esempi usare il comando \"-e\"\nSe invece si vuole eseguire il programma è possibile farlo inserendo \nla casa di partenza e la grandezza della scacchiera così: 2 3 7 \"algoritmo\"\ndove 2 è la riga, 3 la colonna, 7 la n della scacchiera nxn.\nGli algoritmi implementati sono DFS e Warnsdroff";;

main;;