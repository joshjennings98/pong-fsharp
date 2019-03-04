// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Pong

[<EntryPoint>]
let main argv = 
    use g = new Game1()
    g.Run()
    0