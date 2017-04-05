--------------------------------------------------------------
--------------------------------------------------------------
--
--  HaskellEngine
--  MyForteIsTimeTravel
--
--------------------------------------------------------------
--------------------------------------------------------------
module Collision (checkCollisions, countCollisions) where

import Graphics.Gloss
import Simulation
import Physics
import Window
import Linear
import Entity

-------------------------------------
-- Detect and Resolve
-------------------------------------
checkCollisions :: [EntityState] -> [EntityState]
checkCollisions scene = checkObjects (checkEdges scene)

------------------------------
-- | Edge Collisions
------------------------------
checkEdges :: [EntityState] -> [EntityState]
checkEdges []     = []
checkEdges [x]    = [entityToTheWall x]
checkEdges (x:xs) = [entityToTheWall x] ++ checkEdges xs

-- | detect and resolve a single Entity to wall collision
entityToTheWall :: EntityState -> EntityState
entityToTheWall entity = entity { pos = pos' }
        where pos' = 
                -- hit floor
                if (y (pos entity)) - (rad entity) <= -fromIntegral height / 2 then 
                    ((x (pos entity)), fromIntegral ( height `div` 2 ) - (rad entity)) else 
                -- hit roof
                if (y (pos entity)) + (rad entity) >=  fromIntegral height / 2 then 
                    ((x (pos entity)), -fromIntegral ( height `div` 2 ) + (rad entity)) else 
                -- hit left wall
                if (x (pos entity)) - (rad entity) <= -fromIntegral width  / 2 then 
                    ( fromIntegral ( width `div` 2 ) - (rad entity), (y (pos entity))) else
                -- hit right wall
                if (x (pos entity)) + (rad entity) >=  fromIntegral width  / 2 then 
                    ( -fromIntegral ( width `div` 2 ) + (rad entity), (y (pos entity))) else pos entity

------------------------------
-- | Object Collisions
------------------------------
checkObjects :: [EntityState] -> [EntityState]
checkObjects []     = []
checkObjects [x]    = [x]
checkObjects (x:xs) = [entityToEntities x xs] ++ checkObjects xs

entityToEntities :: EntityState -> [EntityState] -> EntityState
entityToEntities entity scene = entity { acc = foldl (add) (acc entity) (map (entityToEntity entity) (scene)) }
                            
-- | check if the Entitys are intersecting and push one away
entityToEntity :: EntityState -> EntityState -> Vector2D
entityToEntity a b = 
    if (distance (pos a) (pos b)) < ((rad a) + (rad b)) then 
        (normalize ((pos a) `sub` (pos b))) `Linear.scale` ((distance (pos a) (pos b)) - ((rad a)))
    else (0, 0)

-------------------------------------
-- Metrics
-------------------------------------
-- | tests if a single Entity is against an edge
isEntityToTheWall :: EntityState -> Bool
isEntityToTheWall entity = 
    ((y (pos entity)) - (rad entity) <= -fromIntegral height / 2) || 
    ((y (pos entity)) + (rad entity) >=  fromIntegral height / 2) ||
    ((x (pos entity)) - (rad entity) <= -fromIntegral width  / 2) || 
    ((x (pos entity)) + (rad entity) >=  fromIntegral width  / 2)
-- | count the number of collisions present
countCollisions :: [EntityState] -> Int
countCollisions state = (length (filter (==True) (collect state)))
-- | collect collisions im scene
collect :: [EntityState] -> [Bool]
collect []     = []
collect [x]    = []
collect (x:xs) = [isEntityToTheWall x] ++ collect xs