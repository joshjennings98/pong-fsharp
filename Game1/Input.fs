module Input

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Actors
open System

let handleInput (kbState:KeyboardState) actor =
    let rec handleKeys keys (currentVelocity:Vector2) =
        match keys with
        | [] -> currentVelocity
        | x :: xs -> 
            match x with
            | Keys.Up -> 
                let newSpeed = 
                    match actor.ActorType with
                    | Ball -> currentVelocity.Y
                    | _ -> -20.f
                let newVelocity = Vector2(currentVelocity.X, newSpeed)
                handleKeys xs (newVelocity)
            | Keys.Down -> 
                let newSpeed = 
                    match actor.ActorType with
                    | Ball -> currentVelocity.Y
                    | _ -> 20.f
                let newVelocity = Vector2(currentVelocity.X, newSpeed)
                handleKeys xs (newVelocity)
            | Keys.Space -> 
                let newVelocity = 
                    let rand = System.Random(System.DateTime.Now.Ticks |> int);
                    let direction = sign (rand.Next(-2, 1))
                    match actor.ActorType with
                    | Ball when actor.BodyType = Dynamic(Vector2(0.f,0.f)) -> Vector2(direction * 8 |> float32, direction * 8 |> float32) //Should be different each time but isn't...
                    | _ -> currentVelocity
                handleKeys xs (newVelocity)
            | _ -> match actor.ActorType with
                   | Ball -> handleKeys xs (currentVelocity)
                   | _ ->  handleKeys xs (Vector2(0.f,0.f))
    match actor.ActorType with
    | Paddle(IsPlayable) -> 
        let initialVelocity = 
            match actor.BodyType with
            | Dynamic(v) -> v
            | _ -> Vector2()
        let velocity = handleKeys (kbState.GetPressedKeys() |> Array.toList) (initialVelocity)
        {actor with BodyType = Dynamic(Vector2(0.f, 0.75f * velocity.Y))}
    | Ball -> 
        let initialVelocity = 
            match actor.BodyType with
            | Dynamic(v) -> v
            | _ -> Vector2()
        let velocity = handleKeys (kbState.GetPressedKeys() |> Array.toList) (initialVelocity)
        { actor with BodyType = Dynamic(velocity) }
    | _ -> actor