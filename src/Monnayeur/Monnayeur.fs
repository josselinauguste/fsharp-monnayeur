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

  let (|Gratuit|_|) (débutGratuité, finGratuité) (début, fin) =
    match fin >= débutGratuité && début <= finGratuité with
    | true when début > débutGratuité -> (finGratuité - début) |> Some
    | true -> (finGratuité - débutGratuité) |> Some
    | false -> None

  let compensePériodesGratuites (début: DateTime) (durée: TimeSpan) =
    let pauseMidi = (début.Date.AddHours 12., début.Date.AddHours 14.)
    let nuit = (début.Date.AddHours 18., (début.Date.AddHours 18.).AddHours 14.)
    match (début, début.Add(durée)) with
    | Gratuit pauseMidi duréeGratuite -> durée + duréeGratuite
    | Gratuit nuit duréeGratuite -> durée + duréeGratuite
    | _ -> durée

  durée montant TimeSpan.Zero |> compensePériodesGratuites début |> début.Add
