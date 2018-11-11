module TextBasedGame.GameLogic

open Types

(*
Game logic
*)

let bind processFunc lastResult =
    match lastResult with
    | Success s -> processFunc s
    | Failure f -> Failure f

let (>>=) x f =
    bind f x

let switch processFunc input =
    Success (processFunc input)

let extractDetailsFromRoom (room: Room) =
    room.Details

let extractItemsFromRoom (room: Room) =
    room.Items

let extractDetailsFromItem (item: Item) =
    item.Details

let getRoom world roomId =
    match world.Rooms.TryFind roomId with
    | Some room -> Success room
    | None -> Failure "Room does not exist!"

let describeDetails detail =
    sprintf "%s\n\n%s\n" detail.Name detail.Description

let describeCurrentRoom world =
    world.Player.Location
    |> getRoom world
    |> (bind (switch extractDetailsFromRoom) >> bind (switch describeDetails))
    
let updatePlayerInventory world items = {
    world with Player = {
            world.Player with Inventory = List.append world.Player.Inventory items
    }
}

let getCurrentRoom world =
    world.Player.Location
    |> getRoom world

let displayResult result =
    match result with
    | Success s -> printf "%s\n" s
    | Failure f -> printf "%s\n" f

let pickUpItems world =
    world
    |> getCurrentRoom
    >>= switch extractItemsFromRoom
    >>= switch (updatePlayerInventory world)

let north exits = exits.North
let south exits = exits.South
let east exits = exits.East
let west exits = exits.West

let getExit direction exits =
    match (direction exits) with
    | PassableExit (_, roomId) -> Success roomId
    | LockedExit (_, _, _) -> Failure "There is a locked door in that direction."
    | NoExit _ -> Failure "There is no room in that direction."

let setCurrentRoom world room = {
    world with Player = {
            world.Player with Location = room.Id
    }
}

let move direction world =
    world
    |> getCurrentRoom
    >>= switch (fun room -> room.Exits)
    >>= getExit direction
    >>= getRoom world
    >>= switch (setCurrentRoom world)

let updateWorld updateFunc (currentWorld: World) =
    let result = updateFunc currentWorld
    match result with
    | Success s -> s |> describeCurrentRoom |> displayResult |> ignore; s
    | Failure f -> printf "%s\n" f |> ignore; currentWorld