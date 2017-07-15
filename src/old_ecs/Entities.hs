module Entities where
import Components

-- Characters
-- ============================
tony :: [Component]
tony = [
        Position 0 0,
        Velocity 0 0,
        Acceleration 0 0,
        BoundingBox (Rect 50 100),
        Player "Tony",
        Health 10,
        Texture "tony.png"
    ]

cop :: [Component]
cop = [
        Position 0 0,
        Velocity 0 0,
        Acceleration 0 0,
        BoundingBox (Rect 50 100),
        Enemy "Cop",
        Health 3,
        Texture "cop.png"
    ]

-- Weapons
-- ===========================
miniOozie :: [Component]
miniOozie = [
        Position 0 0,
        BoundingBox (Rect 10 10),
        Weapon "Mini Oozie",
        Supply 340,
        Capacity 40,
        Texture "minioozie.png"
    ]

revolver :: [Component]
revolver = [
        Position 0 0,
        BoundingBox (Rect 10 10),
        Weapon "Revolver",
        Supply 100,
        Capacity 8,
        Texture "minioozie.png"
    ]

-- Props
-- ==========================
hammock :: [Component]
hammock = [
        Position 0 0,
        BoundingBox (Rect 100 50),
        Prop "Hammock",
        Texture "hammock.png"
    ]

baggedHammock :: [Component]
baggedHammock = [
        Position 0 0,
        BoundingBox (Rect 50 50),
        Prop "Bagged Hammock",
        Texture "baggedhammock.png"
    ]

-- Background
-- =========================
beach :: [Component]
beach = [
        Position 0 0,
        BoundingBox AllAvailableSpace,
        Background "Beach",
        Texture "beach.png"
    ]
