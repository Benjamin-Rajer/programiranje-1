(*----------------------------------------------------------------------------*
 # Podatkovni tipi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Valute

 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute. Oglejmo si dva pristopa k izboljšavi
 varnosti pri uporabi valut.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte tipa `euro` in `dollar`, kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število. Nato napišite funkciji
 `euro_to_dollar` in `dollar_to_euro`, ki primerno pretvarjata valuti (točne
 vrednosti pridobite na internetu ali pa si jih izmislite).

 Namig: Občudujte informativnost tipov funkcij.
[*----------------------------------------------------------------------------*)

type euro = Euro of float (*to je koristno delati, da vemo, kaj je evro in kaj je dolar*)

type dollar = Dollar of float

let dollar_to_euro (Dollar x) = Euro (x *. 0.89)
(*tako povemo s konstruktorji, kaj uporabljamo - x bo zdaj tipa float in lahko z njim manipuliramo *)
(*torej nam ni treba napisati funkcije float -> float, ampak z našimi tipi*)
(*tukaj lahko še razbijemo stvar v argumentu*)
let euro_to_dollar (Euro x) = Dollar (x *. 1.12)

(* let primer_valute_1 = dollar_to_euro (Dollar 0.5) *)
(* val primer_valute_1 : euro = Euro 0.4305 *)

(*----------------------------------------------------------------------------*
 Definirajte tip `currency` kot en vsotni tip z konstruktorji za jen, funt in
 švedsko krono. Nato napišite funkcijo `to_pound`, ki primerno pretvori valuto
 tipa `currency` v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
 Ocaml sam opozori, da je potrebno popraviti funkcijo `to_pound`.
[*----------------------------------------------------------------------------*)

type currency =
| Yen of float
| Pound of float
| Krona of float
| Franc of float

let to_pound x =
  match x with
  | Yen a -> Pound (a *. 0.0048)
  | Pound a -> Pound a
  | Krona a -> Pound (a *. 0.075)
  | Franc a -> Pound (a *. 0.91)


(* let primer_valute_2 = to_pound (Yen 100.) *)
(* val primer_valute_2 : currency = Pound 0.700000000000000067 *)

(*----------------------------------------------------------------------------*
 ## Mešani seznami

 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip `list` predstavimo s konstruktorjem za prazen seznam
 `Nil`(oz. `[]` v Ocamlu) in pa konstruktorjem za člen `Cons(x, xs)` (oz. `x ::
 xs` v Ocamlu).
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte tip `intbool_list` z konstruktorji za:

 - prazen seznam,
 - člen s celoštevilsko vrednostjo,
 - člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal `[5; true; false; 7]`.
[*----------------------------------------------------------------------------*)

type intbool_list =
| Nil
| Integer of int * intbool_list (*sestavljen iz glave in repa preostanka lista*)
| Boolean of bool * intbool_list
(*sezname definiramo rekurzivno tako kot so rekurzivno definirani običajni seznami*)

let test = Integer (5, Boolean (true, Boolean (false, Integer (7, Nil))))

(*----------------------------------------------------------------------------*
 Funkcija `intbool_map f_int f_bool ib_list` preslika vrednosti `ib_list` v nov
 `intbool_list` seznam, kjer na elementih uporabi primerno od funkcij `f_int`
 oz. `f_bool`.
[*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool ib_list = 
  match ib_list with
  | Nil -> Nil
  | Integer (x, xs) -> Integer (f_int x, intbool_map f_int f_bool xs)
  | Boolean (x, xs) -> Boolean (f_bool x, intbool_map f_int f_bool xs)
(*----------------------------------------------------------------------------*
 Funkcija `intbool_reverse` obrne vrstni red elementov `intbool_list` seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let rec intbool_reverse l =
  let rec reverse_aux acc l =
    match l with
    | Nil -> acc
    | Integer (x, xs) -> reverse_aux (Integer (x, acc)) xs
    | Boolean (y, ys) -> reverse_aux (Boolean (y, acc)) ys
  in
  reverse_aux Nil l



  


(*----------------------------------------------------------------------------*
 Funkcija `intbool_separate ib_list` loči vrednosti `ib_list` v par `list`
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let rec intbool_separate l = 
  let rec separate_aux acc1 acc2 s =
    match s with
    | Nil -> (acc1, acc2)
    | Integer (x, xs) -> separate_aux (acc1 @ [x]) acc2 xs
    | Boolean (x, xs) -> separate_aux acc1 (acc2 @ [x]) xs
  in
  separate_aux [] [] l

(*probaj še brez aux napisati, je možno*)

(*----------------------------------------------------------------------------*
 ## Čarodeji

 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 `magic`, ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire, frost
 in arcane. Ko se čarodej zaposli na akademiji, se usmeri v zgodovino,
 poučevanje ali raziskovanje oz. historian, teacher in researcher. Definirajte
 tip `specialisation`, ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic =
| Fire
| Frost
| Arcane


type specialisation =
| Historian
| Teacher
| Researcher

(*----------------------------------------------------------------------------*
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent, na
 koncu pa SE lahko tudi zaposli. Definirajte tip `status`, ki določa ali je
 čarodej:

 - začetnik `Newbie`,
 - študent `Student` (in kateri vrsti magije pripada in koliko časa študira),
 - zaposlen `Employed` (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip `wizard` z poljem za ime in poljem za trenuten
 status ter dodajte primer `professor`, ki je zaposlen učitelj magije ognja, in
 `jaina`, ki je četrto leto študentka magije ledu.
[*----------------------------------------------------------------------------*)

type status =
| Newbie
| Student of magic * int
| Employed of magic * specialisation

type wizard = {name: string; status: status}

let professor = {name = "Harold"; status = Employed (Fire, Teacher)}

let jaina = {name = "Jaina"; status = Student (Frost, 4)}

(*----------------------------------------------------------------------------*
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip `magic_counter`, ki v posameznem polju hrani število
 uporabnikov magije. Nato definirajte funkcijo `update counter magic`, ki vrne
 nov števec s posodobljenim poljem glede na vrednost `magic`.
[*----------------------------------------------------------------------------*)

type magic_counter = {fire: int; frost: int; arcane: int}

let izpis {fire; frost; arcane} = fire (*stvari se shranjujejo v imena teh spremenljivk*)
let izpis2 {fire = f; frost; arcane} = f (*tudi lahko definiramo novo ime*)
let izpis3 {fire = f; arcane} = arcane (*ni nam treba vsega navesti, ampak tudi ve, da mislimo magic counter*)


let update counter magic = 
  match magic with
  | Fire -> {counter with fire = counter.fire + 1}
  | Frost -> {counter with frost = counter.frost + 1}
  | Arcane -> {counter with arcane = counter.arcane + 1}

(* let primer_carovniki_1 = update {fire = 1; frost = 1; arcane = 1} Arcane *)
(* val primer_carovniki_1 : magic_counter = {fire = 1; frost = 1; arcane = 2} *)

(*----------------------------------------------------------------------------*
 Funkcija `count_magic` sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
[*----------------------------------------------------------------------------*)

let count_magic l =
  List.fold_left (fun counter a -> update counter a) {fire = 0; frost = 0; arcane = 0} l

(* let primer_carovniki_2 = count_magic [professor; professor; professor] *)
(* val primer_carovniki_2 : magic_counter = {fire = 3; frost = 0; arcane = 0} *)

(*----------------------------------------------------------------------------*
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija. Funkcija `find_candidate magic
 specialisation wizard_list` poišče prvega primernega kandidata na seznamu
 čarodejev in vrne njegovo ime, čim ustreza zahtevam za `specialisation` in
 študira vrsto `magic`. V primeru, da ni primernega kandidata, funkcija vrne
 `None`.
[*----------------------------------------------------------------------------*)

let rec find_candidate magic specialisation wizard_list = 
  match wizard_list with
  | [] -> None
  | x::xs ->
    match x.status with
    | Newbie | Employed _ -> find_candidate magic specialisation xs
    | Student (m, time) -> 
      let check t = if time >= t && magic = m then Some x.name else find_candidate magic specialisation xs in
      match specialisation with
      | Historian -> check 3
      | Researcher -> check 4
      | Teacher -> check 5

let find_candidate magic specialisation wizard_list =
  let aux wiz =
    match wiz.status with
    | Student (mag, time) -> 
      begin
       let check t = if time >= t && magic = mag then Some wiz.name else None in
      match specialisation with
      | Historian -> check 3
      | Researcher -> check 4
      | Teacher -> check 5 
      end
    | _ -> None
  in
  List.find_map aux wizard_list


(* let primer_carovniki_3 =
  find_candidate Frost Researcher [professor; jaina] *)
(* val primer_carovniki_3 : string option = Some "Jaina" *)
