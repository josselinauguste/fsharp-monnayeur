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
    | true when début > débutGratuité -> Some début
    | true -> Some débutGratuité
    | false -> None

  let compensePauseDeMidi (début: DateTime) (durée: TimeSpan) =
    let fin = début.Add(durée)
    let débutGratuité = début.Date.AddHours 12.
    let finGratuité = débutGratuité.AddHours 2.
    match (début, fin) with
    | Gratuit (débutGratuité, finGratuité) débutDécompte -> durée + (finGratuité - débutDécompte)
    | _ -> durée

  let compenseNuit (début: DateTime) (durée: TimeSpan) =
    let fin = début.Add(durée)
    let débutGratuité = début.Date.AddHours 18.
    let finGratuité = débutGratuité.AddHours 14.
    match (début, fin) with
    | Gratuit (débutGratuité, finGratuité) débutDécompte -> durée + (finGratuité - débutDécompte)
    | _ -> durée

  durée montant TimeSpan.Zero |> compensePauseDeMidi début |> compenseNuit début |> début.Add
