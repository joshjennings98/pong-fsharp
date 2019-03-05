module AI

open Microsoft.Xna.Framework
open Actors

let handleAI (worldObjects : WorldActor list) = 
    let setAIDirection (ballPos : Vector2) (paddlePos : Vector2) actor =
        match actor.ActorType with
        | Paddle(NotPlayable) when ballPos.X < 300.f ->
            let distance = ballPos.Y - paddlePos.Y - 5.f
            if abs distance < 20.f
            then {actor with BodyType = Dynamic(Vector2(0.f, distance))}
            else {actor with BodyType = Dynamic(Vector2(0.f, (distance |> sign |> float32) * 20.f))}
        | Paddle(NotPlayable) -> 
            match actor.BodyType with
            | Dynamic(v) -> {actor with BodyType = Dynamic(Vector2(0.f, 0.0f * v.Y))}
            | _ -> failwithf "Paddle is always a dynamic actor." 
        | _ -> actor
    
    let rec handleAI' (actors : WorldActor list) (ball : Vector2 option) (paddle : Vector2 option) = 
        match ball, paddle with
        | Some x, Some y -> 
            worldObjects 
            |> List.map (fun el -> setAIDirection x y el)
        | _, _ -> 
            match actors with
            | x :: xs when x.ActorType = Ball -> handleAI' xs (Some x.Position) paddle
            | x :: xs when x.ActorType = Paddle(NotPlayable) -> handleAI' xs ball (Some x.Position)
            | _ :: xs -> handleAI' xs ball paddle
            | [] -> worldObjects

    handleAI' worldObjects None None