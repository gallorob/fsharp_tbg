module TextBasedGame.Program

open System
open Types
open InitialValues
open GameLogic
open TextBasedGame

(*
Main program module
*)

let rec gameLoop currentWorld =
    let input = Console.ReadLine()
    match input with
        | "look around" -> getLookAroundResults currentWorld; gameLoop currentWorld
        | "look north" -> getLookResults "north" currentWorld; gameLoop currentWorld
        | "look south" -> getLookResults "south" currentWorld; gameLoop currentWorld
        | "look east" -> getLookResults "east" currentWorld; gameLoop currentWorld
        | "look west" -> getLookResults "west" currentWorld; gameLoop currentWorld
        | "move north" -> gameLoop (updateWorld (move north) currentWorld)
        | "move south" -> gameLoop (updateWorld (move south) currentWorld)
        | "move east" -> gameLoop (updateWorld (move east) currentWorld)
        | "move west" -> gameLoop (updateWorld (move west) currentWorld)
        | "exit" -> Environment.Exit 0 |> ignore; gameLoop currentWorld
        | _ -> printf "Command not recognized.\n" |> ignore; gameLoop currentWorld
    gameLoop currentWorld


[<EntryPoint>]
let main argv =
    gameWorld |> describeCurrentRoom |> displayResult|> ignore
    gameLoop gameWorld
    0