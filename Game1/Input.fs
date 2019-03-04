module Input

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Actors
open System

let HandleInput (kbState:KeyboardState) actor =
    let rec HandleKeys keys (currentVelocity:Vector2) =
        match keys with
        | [] -> currentVelocity
        | x :: xs -> match x with
                     | Keys.Up -> let newSpeed = match actor.ActorType with
                                                 | Ball -> currentVelocity.Y
                                                 | _ -> -20.f
                                  let newV = Vector2(currentVelocity.X, newSpeed)
                                  HandleKeys xs (newV)
                     | Keys.Down -> let newSpeed = match actor.ActorType with
                                                   | Ball -> currentVelocity.Y
                                                   | _ -> 20.f
                                    let newV = Vector2(currentVelocity.X, newSpeed)
                                    HandleKeys xs (newV)
                     | Keys.Space -> let newV = 
                                        let rand = System.Random();
                                        match actor.ActorType with
                                        | Ball when actor.BodyType = Dynamic(Vector2(0.f,0.f)) -> Vector2(rand.Next(6,8) |> float32, rand.Next(6,8) |> float32)
                                        | _ -> currentVelocity
                                     HandleKeys xs (newV)
                     | _ -> match actor.ActorType with
                            | Ball -> HandleKeys xs (currentVelocity)
                            | _ ->  HandleKeys xs (Vector2(0.f,0.f))
    match actor.ActorType with
    | Paddle(IsPlayable) -> 
        let initialVelocity = match actor.BodyType with
                              | Dynamic(v) -> v
                              | _ -> Vector2()
        let velocity = HandleKeys (kbState.GetPressedKeys() |> Array.toList) (initialVelocity)
        {actor with BodyType = Dynamic(Vector2(0.f, 0.75f * velocity.Y))}
    | Ball -> 
        let initialVelocity = match actor.BodyType with
                              | Dynamic(v) -> v
                              | _ -> Vector2()
        let velocity = HandleKeys (kbState.GetPressedKeys() |> Array.toList) (initialVelocity)
        if velocity = Vector2(0.f,0.f) 
        then { actor with BodyType = Dynamic(velocity) }
        else { actor with BodyType = Dynamic(velocity) }
    | _ -> actor