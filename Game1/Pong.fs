﻿module Pong

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Content

open Actors
open Input
open Physics
open AI

type Game1 () as x =
    inherit Game()
 
    do x.Content.RootDirectory <- "Content"
    let graphics = new GraphicsDeviceManager(x)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let CreateActor' = CreateActor x.Content
 
    let mutable WorldObjects = lazy ([("paddle", Paddle(NotPlayable), Vector2(10.f,195.f), Vector2(16.f,90.f), false);
                          ("paddle", Paddle(IsPlayable), Vector2(774.f,195.f), Vector2(16.f,90.f), false);
                          ("ball", Ball, Vector2(384.f,240.f), Vector2(16.f,16.f), false);
                          ("", Obstacle, Vector2(0.f,0.f), Vector2(1024.f,0.f), true);
                          ("", Obstacle, Vector2(0.f,480.f), Vector2(1024.f,0.f), true);
                          ("", Goal, Vector2(0.f,0.f), Vector2(0.f,480.f), true);
                          ("", Goal, Vector2(800.f,0.f), Vector2(0.f,480.f), true)]
                         |> List.map CreateActor')
    
    let DrawActor (sb:SpriteBatch) actor =
        if actor.Texture.IsSome then
            do sb.Draw(actor.Texture.Value, actor.Position, Color.White)
        ()

    override x.Initialize() =
        do spriteBatch <- new SpriteBatch(x.GraphicsDevice)
        do base.Initialize()
        ()

    override x.LoadContent() =
        do WorldObjects.Force () |> ignore
        ()
 
    override x.Update (gameTime) =
        let HandleInput' = HandleInput (Keyboard.GetState ())
        let current = WorldObjects.Value
        do WorldObjects <- lazy (current 
                                 |> List.map HandleInput'
                                 |> HandleAI
                                 |> HandleCollisions
                                 |> List.map ResolveVelocities)
        do WorldObjects.Force () |> ignore
        ()
 
    override x.Draw (gameTime) =
        do x.GraphicsDevice.Clear Color.Black
        let DrawActor' = DrawActor spriteBatch
        do spriteBatch.Begin ()
        WorldObjects.Value
        |> List.iter DrawActor'
        do spriteBatch.End ()
        ()



