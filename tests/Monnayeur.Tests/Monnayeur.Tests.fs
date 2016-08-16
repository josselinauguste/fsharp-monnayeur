module Monnayeur.Tests

open System
open NUnit.Framework
open FsUnit
open Monnayeur

[<Test>]
let ``un lundi à 10h, ajouter 1€ renvoie 10h20`` () =
  let maintenant = new DateTime(2016, 8, 1, 10, 0, 0)
  let monnayeur = créer maintenant
  monnayeur |> ajoutePièce 1 |> heureLimite |> should equal (maintenant.AddMinutes 20.)

[<Test>]
let ``un lundi à 8h, ajouter 4€ renvoie 10h20`` () =
  let maintenant = new DateTime(2016, 8, 1, 8, 0, 0)
  let limite = maintenant.Add (new TimeSpan(2, 20, 0))
  let monnayeur = créer maintenant
  monnayeur |> ajoutePièce 1 |> ajoutePièce 2 |> ajoutePièce 1 |> heureLimite |> should equal limite

[<Test>]
let ``la pause de midi n'est pas décomptée`` () =
  let maintenant = new DateTime(2016, 8, 1, 11, 0, 0)
  let limite = maintenant.AddHours(4.)
  let monnayeur = créer maintenant
  monnayeur |> ajoutePièce 1 |> ajoutePièce 2 |> heureLimite |> should equal limite

[<Test>]
let ``commencer le stationnement durant la pause de midi repousse le début`` () =
  let maintenant = new DateTime(2016, 8, 1, 13, 0, 0)
  let limite = maintenant.AddHours(3.)
  let monnayeur = créer maintenant
  monnayeur |> ajoutePièce 1 |> ajoutePièce 2 |> heureLimite |> should equal limite

[<Test>]
let ``la nuit n'est pas décomptée`` () =
  let maintenant = new DateTime(2016, 8, 1, 17, 0, 0)
  let limite = new DateTime(2016, 8, 2, 9, 0, 0)
  let monnayeur = créer maintenant
  monnayeur |> ajoutePièce 1 |> ajoutePièce 2 |> heureLimite |> should equal limite
