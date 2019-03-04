module Physics

open Actors
open Microsoft.Xna.Framework

let IsActorStatic actor =
    match actor.BodyType with
    | Static -> true
    | _ -> false

let PartitionWorldObjects worldObjects =
    worldObjects
    |> List.partition IsActorStatic

let HandleCollisions worldObjects =
    let stc, dyn = PartitionWorldObjects worldObjects
    
    let FindNewVelocity rect1 rect2 velocity actor1 actor2 =
        let inter = Rectangle.Intersect(rect1,rect2)
        let mutable (newVel:Vector2) = velocity
        if inter.Height > inter.Width then
            do match actor1.ActorType, actor2.ActorType with 
               | Ball, Obstacle -> newVel.X <- -1.f * newVel.X
               | Ball, Paddle(_) -> newVel.X <- -1.1f * newVel.X
               | _, _ -> newVel.X <- 0.f
        if inter.Width > inter.Height then
            do match actor1.ActorType, actor2.ActorType with 
               | Ball, Obstacle -> newVel.Y <- -1.f * newVel.Y
               | Ball, Paddle(_) -> newVel.Y <- -1.1f * newVel.Y
               | _, _ -> newVel.Y <- 0.f
        newVel

    let FindOptimumCollision a b =
        match a.ActorType,b.ActorType with
        | Paddle(_), Obstacle -> 
            match a.BodyType, b.BodyType with
            | Dynamic (s), Static -> { a with BodyType = Dynamic(Vector2(0.f, 0.f)) }
            | _ -> a
        | Ball, Obstacle ->  
            match a.BodyType, b.BodyType with
            | Dynamic (s), Static -> { a with BodyType = Dynamic((FindNewVelocity a.DesiredBounds b.CurrentBounds s a b)) }
            | _ -> a
        | Ball, Goal -> 
            match a.BodyType, b.BodyType with
            | Dynamic (s), Static -> {a with Position = Vector2(384.f,240.f); BodyType = Dynamic(Vector2(0.f,0.f))}
            | _ -> a
        | Ball, Paddle(_) -> 
            match a.BodyType, b.BodyType with
            | Dynamic (s), Dynamic(_) -> { a with BodyType = Dynamic((FindNewVelocity a.DesiredBounds b.CurrentBounds s a b)) }
            | _ -> a
        | _ -> a
    
    let rec FigureCollisions (actor:WorldActor) (sortedActors:WorldActor list) =
        match sortedActors with
        | [] -> actor
        | x :: xs -> let a = if actor.DesiredBounds.Intersects x.DesiredBounds then
                                 FindOptimumCollision actor x
                             else
                                 actor
                     FigureCollisions a xs

    let rec FixCollisions (toFix:WorldActor list) (alreadyFixed:WorldActor list) =
        match toFix with
        | [] -> alreadyFixed
        | x :: xs -> let a = FigureCollisions x alreadyFixed
                     FixCollisions xs (a::alreadyFixed)

    FixCollisions dyn stc

let ResolveVelocities actor =
    match actor.BodyType with
    | Dynamic (s) -> { actor with Position = actor.Position + s }
    | _ -> actor

