module Actors

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content

type Playable =
    | IsPlayable
    | NotPlayable

type BodyType =
    | Static
    | Dynamic of Vector2
 
type ActorType =
    | Paddle of Playable
    | Obstacle
    | Goal
    | Ball

type WorldActor =
    {
        ActorType : ActorType;
        Position : Vector2;
        Size : Vector2;
        Texture : Texture2D option;
        BodyType : BodyType
    }
    member this.CurrentBounds
        with get () = Rectangle((int this.Position.X),(int this.Position.Y),(int this.Size.X),(int this.Size.Y))
 
    member this.DesiredBounds
        with get () = 
            match this.BodyType with
            | Dynamic(s) -> Rectangle((int this.Position.X + int s.X),(int this.Position.Y + int s.Y),(int this.Size.X),(int this.Size.Y))
            | _ -> this.CurrentBounds

let createActor (content:ContentManager) (textureName, actorType, position, size, isStatic) =
    let texture = if not (System.String.IsNullOrEmpty textureName) 
                  then Some(content.Load textureName)
                  else None
    let bodyType = if isStatic 
                   then Static
                   else Dynamic(Vector2(0.f,0.f))
    { ActorType = actorType; Position = position; Size = size; Texture = texture; BodyType = bodyType; }

