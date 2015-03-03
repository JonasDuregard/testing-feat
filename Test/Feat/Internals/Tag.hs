module Test.Feat.Internals.Tag where

data Tag  =  Class                       
          |  Source String String Int Int
   deriving (Show,Eq,Ord)  
