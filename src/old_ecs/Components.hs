
-- -- Components file
module Components where

data Bounds = Rect Double Double 
            | Rect4 Double Double Double Double
            | AllAvailableSpace

    deriving (Show)

data Component =  
                -- Physics Components (x, y) 
                -- or (x1, y1, x2, y2)
                  Position      Double Double
                | Velocity      Double Double
                | Acceleration  Double Double
                | BoundingBox   Bounds

                -- Object Tags
                | Player        String
                | Enemy         String
                | Ally          String
                | Vehicle       String
                | Weapon        String
                | Prop          String
                | Background    String

                -- Amounts of things
                | Health        Integer
                | Capacity      Integer
                | Supply        Integer -- for things like bullets / items

                -- Misc. Properties
                | Texture       String     -- for drawing

    deriving (Show)
