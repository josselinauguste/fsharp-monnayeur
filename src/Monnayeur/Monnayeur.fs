module Monnayeur

open System

type T = Monnayeur of DateTime * int list

let créer début = Monnayeur (début, [])

let ajoutePièce pièce (Monnayeur (début, pièces)) =
  let pièces = pièce :: pièces
  Monnayeur (début, pièces)

let heureLimite (Monnayeur (début, pièces)) =
  let montant = pièces |> List.sum
  let tarifs = [(3, TimeSpan.FromHours 2.); (2, TimeSpan.FromHours 1.); (1, TimeSpan.FromMinutes 20.)]
  let rec durée montant (duréeAccumulée: TimeSpan) =
    let tarifPlusAvantageux = tarifs |> List.tryFind (fun (m, d) -> montant >= m)
    match tarifPlusAvantageux with
    | Some (m, d) -> durée (montant - m) (duréeAccumulée.Add(d))
    | None -> duréeAccumulée
  let compensePauseDeMidi (début: DateTime) (durée: TimeSpan) =
    let fin = début.Add(durée)
    let débutPause = début.Date.AddHours 12.
    let finPause = début.Date.AddHours 14.
    if fin >= débutPause && début <= finPause then
      durée + (finPause - débutPause)
    else
      durée
  durée montant TimeSpan.Zero |> compensePauseDeMidi début |> début.Add
