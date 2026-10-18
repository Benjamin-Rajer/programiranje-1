(*----------------------------------------------------------------------------*
 # Uvod v OCaml
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Vektorji
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razteg : float -> float list -> float list`, ki vektor,
 predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

let razteg a sez = List.map (Float.mul a) sez
(*let razteg a sez = List.map (fun b -> a *. b) sez *)

let primer_vektorji_1 = razteg 2.0 [1.0; 2.0; 3.0]
(* val primer_vektorji_1 : float list = [2.; 4.; 6.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sestej : float list -> float list -> float list`, ki vrne
 vsoto dveh vektorjev.
[*----------------------------------------------------------------------------*)

let sestej sez1 sez2 = List.map2 (fun a b -> a +. b) sez1 sez2
let sestej2 sez1 sez2 = List.map2 ( +. ) sez1 sez2

let primer_vektorji_2 = sestej [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_2 : float list = [5.; 7.; 9.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `skalarni_produkt : float list -> float list -> float`, ki
 izračuna skalarni produkt dveh vektorjev. Pri tem si lahko pomagate s funkcijo
 `vsota_seznama : float list -> float`, definirano prek funkcije
 `List.fold_left`, ki jo bomo spoznali kasneje:
[*----------------------------------------------------------------------------*)

let vsota_seznama = List.fold_left (+.) 0.

let skalarni_produkt sez1 sez2 = vsota_seznama (List.map2 ( *. ) sez1 sez2)

(*potiskanje stvari v ukaze: uporabi se |> *)
let skalarni_produkt2 sez1 sez2 = List.map2 ( *. ) sez1 sez2
|> vsota_seznama

let primer_vektorji_3 = skalarni_produkt2 [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_3 : float = 32. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `norma : float list -> float`, ki vrne evklidsko normo
 vektorja.
[*----------------------------------------------------------------------------*)

let norma sez1 = sqrt (vsota_seznama (List.map (fun a -> a *. a) sez1)) (* ne vem a to dela, probaj še*)
let norma2 sez1 = sqrt (skalarni_produkt sez1 sez1)

let primer_vektorji_4 = norma [3.0; 4.0]
(* val primer_vektorji_4 : float = 5. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `vmesni_kot : float list -> float list -> float`, ki izračuna
 kot med dvema vektorjema v radianih.
[*----------------------------------------------------------------------------*)

let vmesni_kot sez1 sez2 = Float.acos (skalarni_produkt sez1 sez2 /. norma sez1 /. norma sez2)

let primer_vektorji_5 = vmesni_kot [1.0; 0.0] [0.0; 1.0]
(* val primer_vektorji_5 : float = 1.57079632679489656 *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `normirani : float list -> float list`, ki normira dani
 vektor.
[*----------------------------------------------------------------------------*)

let normirani sez1 = List.map (fun a -> a /. norma sez1) sez1

let primer_vektorji_6 = normirani [3.0; 4.0]
(* val primer_vektorji_6 : float list = [0.600000000000000089; 0.8] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `projeciraj : float list -> float list -> float list`, ki
 izračuna projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let projekcija vek1 vek2 = razteg (skalarni_produkt vek1 vek2 /. (norma vek2 *. norma vek2)) vek2

let primer_vektorji_7 = projekcija [3.0; 4.0] [1.0; 0.0]
(* val primer_vektorji_7 : float list = [3.; 0.] *)

(*----------------------------------------------------------------------------*
 ## Generiranje HTML-ja
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML
 oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.
[*----------------------------------------------------------------------------*)

let ovij znacka niz = "<" ^ znacka ^ ">" ^ niz ^ "</" ^ znacka ^ ">"

let primer_html_1 = ovij "h1" "Hello, world!"
(* val primer_html_1 : string = "<h1>Hello, world!</h1>" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število
 presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za
 ustrezno število presledkov.
[*----------------------------------------------------------------------------*)

let zamakni st niz = String.split_on_char '\n' niz
|> List.map (fun vrstica -> (String.make st ' ') ^ vrstica) (* to je zdaj funkcija narejena iz List.map, ki sprejme še en argument*)
|> String.concat "\n"

let primer_html_2 = zamakni 4 "Hello,\nworld!"
(* val primer_html_2 : string = "    Hello,\n    world!" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne
 niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:
[*----------------------------------------------------------------------------*)

let ul sez = List.map (fun b ->"<li>" ^ b ^ "</li>") sez
|> String.concat "\n"
|> zamakni 2
|> (fun a -> "<ul>\n" ^ a ^ "\n</ul>")

let primer_html_3 = ul ["ananas"; "banana"; "čokolada"]
(* val primer_html_3 : string =
  "<ul>\n  <li>ananas</li>\n  <li>banana</li>\n  <li>čokolada</li>\n</ul>" *)

(*----------------------------------------------------------------------------*
 ## Nakupovalni seznam
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme
 niz, ki vsebuje vejico, loči na del pred in del za njo.
[*----------------------------------------------------------------------------*)

let razdeli_vrstico niz =
  let indeks = String.index niz ',' in
  let levi = String.sub niz 0 indeks in (*argumenta sta začetek, dolžina*)
  let desni = String.sub niz (indeks + 2) (String.length niz - String.length levi - 2) in
  levi, desni (*ni treba dati oklepajev*)

let primer_seznam_1 = razdeli_vrstico "mleko, 2"
(* val primer_seznam_1 : string * string = ("mleko", "2") *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`,
 ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike `"izdelek,
 vrednost"`, in vrne seznam ustreznih parov.
[*----------------------------------------------------------------------------*)

let pretvori_v_seznam_parov niz = String.split_on_char '\n' niz
|> List.map razdeli_vrstico 

let primer_seznam_2 = pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"
(* val primer_seznam_2 : (string * string) list =
  [("mleko", "2"); ("kruh", "1"); ("jabolko", "5")] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list
 -> (string * 'b) list`, ki dano funkcijo uporabi na vseh drugih komponentah
 elementov seznama.
[*----------------------------------------------------------------------------*)

let pretvori_druge_komponente f sez = List.map (fun (a, b) -> (a, f b)) sez

let primer_seznam_3 =
  let seznam = [("ata", "mama"); ("teta", "stric")] in
  pretvori_druge_komponente String.length seznam
(* val primer_seznam_3 : (string * int) list = [("ata", 4); ("teta", 5)] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki
 sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni
 znesek nakupa.
[*----------------------------------------------------------------------------*)

let izracunaj_skupni_znesek cenik seznam = 
  let cenik_sez = pretvori_druge_komponente float_of_string (pretvori_v_seznam_parov cenik) in
  let seznam_sez = pretvori_druge_komponente float_of_string (pretvori_v_seznam_parov seznam) in
  let poglej a = List.fold_left ( +. ) 0. (List.map (fun (x, y) -> if x = a then y else 0.) cenik_sez) in
  List.fold_left ( +.) 0. (List.map (fun (a, b) -> (poglej a) *. b) seznam_sez)

  (*funkcija prvo pogleda cenik tako da "sešteje" skupaj vse cene za en predmet,
  kjer vzame ceno samo, ko se predmet ujema s predmetom v ceniku, drugače vzame 0,
  potem pa uporabi to funkcijo na vseh elemenith seznama, kjer še zmnoži cene,
  in sešteje skupaj vsote *)
  
let primer_seznam_4 = 
  let nakupovalni_seznam = "mleko, 2\njabolka, 5"
  and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
  izracunaj_skupni_znesek cenik nakupovalni_seznam
(* val primer_seznam_4 : float = 5.5 *)
