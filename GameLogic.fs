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

let extractExistsFromRoom room =
    room.Exits

let extractItemsFromRoom (room: Room) =
    match room.Items.Length with
    | 0 -> Failure "No items found!"
    | _ -> Success room.Items

let extractDetailsFromItem (item: Item) =
    item.Details

let getRoom world roomId =
    match world.Rooms.TryFind roomId with
    | Some room -> Success room
    | None -> Failure "Room does not exist!"

let getExitDescription direction exits =
    match direction with
    | "north" -> match exits.North with
                    | PassableExit(desc, _) -> Success desc
                    | LockedExit (desc, _, _) -> Success desc
                    | NoExit _ -> Failure "There is no room in that direction."
    | "south" -> match exits.South with
                    | PassableExit(desc, _) -> Success desc
                    | LockedExit (desc, _, _) -> Success desc
                    | NoExit _ -> Failure "There is no room in that direction."
    | "east" -> match exits.East with
                    | PassableExit(desc, _) -> Success desc
                    | LockedExit (desc, _, _) -> Success desc
                    | NoExit _ -> Failure "There is no room in that direction."
    | "west" -> match exits.West with
                    | PassableExit(desc, _) -> Success desc
                    | LockedExit (desc, _, _) -> Success desc
                    | NoExit _ -> Failure "There is no room in that direction."
    | _ -> Failure "Unknown direction."

let describeExit desc =
    sprintf "%s" desc

let describeDetails detail =
    sprintf "%s\n%s" detail.Name detail.Description

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

let lookAround world =
    world
    |> getCurrentRoom
    >>= switch extractItemsFromRoom
    >>= bind (switch (List.map extractDetailsFromItem))
    >>= switch (List.map describeDetails)

let look direction world =
    world
    |> getCurrentRoom
    >>= switch extractExistsFromRoom
    >>= getExitDescription direction
    >>= switch describeExit

let getLookAroundResults world =
    let resultList = lookAround world
    match resultList with
    | Success s -> List.iter (printf "%s\n") s |> ignore
    | Failure f -> printf "%s\n" f |> ignore

let getLookResults direction world =
    let result = look direction world
    match result with
    | Success s -> printf "%s\n" s |> ignore
    | Failure f -> printf "%s\n" f |> ignore

// to rewrite
(*let pickUpItems world =
    world
    |> getCurrentRoom
    |> extractItemsFromRoom
    >>= switch (updatePlayerInventory world) *)

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