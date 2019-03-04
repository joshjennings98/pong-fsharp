module Physics

open Actors
open Microsoft.Xna.Framework

let isActorStatic actor =
    match actor.BodyType with
    | Static -> true
    | _ -> false

let partitionWorldObjects worldObjects =
    worldObjects
    |> List.partition isActorStatic

let handleCollisions worldObjects =
    let stc, dyn = partitionWorldObjects worldObjects
    
    let FindNewVelocity rect1 rect2 velocity actor1 =
        let inter = Rectangle.Intersect(rect1,rect2)
        let mutable (newVel:Vector2) = velocity
        if inter.Height > inter.Width then
            do match actor1.ActorType with 
               | Ball -> newVel.X <- -1.f * newVel.X
               | _ -> newVel.X <- 0.f
        if inter.Width > inter.Height then
            do match actor1.ActorType with 
               | Ball -> newVel.Y <- -1.f * newVel.Y
               | _-> newVel.Y <- 0.f
        newVel

    let findCollision a b =
        match a.ActorType,b.ActorType with
        | Paddle(_), Obstacle -> 
            match a.BodyType, b.BodyType with
            | Dynamic (s), Static -> { a with BodyType = Dynamic(Vector2(0.f, 0.f)) }
            | _ -> a
        | Ball, Obstacle ->  
            match a.BodyType, b.BodyType with
            | Dynamic (s), Static -> { a with BodyType = Dynamic((FindNewVelocity a.DesiredBounds b.CurrentBounds s a)) }
            | _ -> a
        | Ball, Goal -> 
            match a.BodyType, b.BodyType with
            | Dynamic (s), Static -> {a with Position = Vector2(384.f,240.f); BodyType = Dynamic(Vector2(0.f,0.f))}
            | _ -> a
        | Ball, Paddle(_) -> 
            match a.BodyType, b.BodyType with
            | Dynamic (s), Dynamic(_) -> { a with BodyType = Dynamic((FindNewVelocity a.DesiredBounds b.CurrentBounds s a)) }
            | _ -> a
        | _ -> a
    
    let rec sortCollisions (actor:WorldActor) (sortedActors:WorldActor list) =
        match sortedActors with
        | [] -> actor
        | x :: xs -> 
            let a = if actor.DesiredBounds.Intersects x.DesiredBounds 
                    then findCollision actor x
                    else actor
            sortCollisions a xs

    let rec FixCollisions (toFix:WorldActor list) (alreadyFixed:WorldActor list) =
        match toFix with
        | [] -> alreadyFixed
        | x :: xs -> let a = sortCollisions x alreadyFixed
                     FixCollisions xs (a::alreadyFixed)

    FixCollisions dyn stc

let resolveVelocities actor =
    match actor.BodyType with
    | Dynamic (s) -> { actor with Position = actor.Position + s }
    | _ -> actor

