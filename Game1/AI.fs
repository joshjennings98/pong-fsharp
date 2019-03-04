module AI

open Microsoft.Xna.Framework
open Actors

let HandleAI (worldObjects : WorldActor list) = 
    let setAIDirection (ballPos : Vector2) (paddlePos : Vector2) actor =
        match actor.ActorType with
        | Paddle(NotPlayable) when ballPos.X < 300.f && ballPos.Y > paddlePos.Y + 17.f -> {actor with BodyType = Dynamic(Vector2(0.f, 30.f))}
        | Paddle(NotPlayable) when ballPos.X < 300.f && ballPos.Y < paddlePos.Y - 17.f -> {actor with BodyType = Dynamic(Vector2(0.f, -30.f))}
        | Paddle(NotPlayable) -> 
            match actor.BodyType with
            | Dynamic(v) -> {actor with BodyType = Dynamic(Vector2(0.f, 0.0f * v.Y))}
            | _ -> failwithf "Won't happen."
        | _ -> actor
    
    let rec HandleAI' (actors : WorldActor list) (ball : Vector2 option) (paddle : Vector2 option) = 
        match ball, paddle with
        | Some x, Some y -> 
            worldObjects 
            |> List.map (fun el -> setAIDirection x y el)
        | _, _ -> 
            match actors with
            | x :: xs when x.ActorType = Ball -> HandleAI' xs (Some x.Position) paddle
            | x :: xs when x.ActorType = Paddle(NotPlayable) -> HandleAI' xs ball (Some x.Position)
            | _ :: xs -> HandleAI' xs ball paddle
            | [] -> worldObjects

    HandleAI' worldObjects None None