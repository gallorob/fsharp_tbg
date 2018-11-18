module TextBasedGame.Types

(*
Types definitions
*)

// Result type (Railway Oriented Programming with generics)
type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure

// Description type    
type Details = {
    Name: string
    Description: string
}

// Item type
type Item = { Details: Details}

// ID of a room
type RoomId =
    | RoomId of string

// Exit enum type
type Exit = 
    | PassableExit of string * destination: RoomId 
    | LockedExit of string * key: Item * next: Exit
    | NoExit of string option

// Possible exits in a room    
type Exits = {
    North: Exit
    South: Exit
    East: Exit
    West: Exit
}

// Room type
type Room = {
    Id: RoomId
    Details: Details
    Items: Item list    // items that can be found in a room
    Exits: Exits
}

// Player type
type Player = {
    Details: Details
    Location: RoomId
    Inventory: Item list
}

// World type
type World = {
    Rooms: Map<RoomId, Room>
    Player: Player
}