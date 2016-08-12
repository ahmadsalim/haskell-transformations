module BlogPost where

import Nats
import Tip
import Test.LazySmallCheck as LSC

type Title = Nat

type Timestamp = Nat

data Post = SinglePost Title Timestamp
          | AggregatePost [Post]
      deriving (Show, Eq)

instance Serial Post where
  series = cons2 SinglePost \/ cons1 AggregatePost

isAggregate :: Post -> Bool
isAggregate (AggregatePost _) = True
isAggregate _ = False

timestamps :: Post -> [Timestamp]
timestamps (SinglePost _ tsp) = [tsp]
timestamps (AggregatePost posts) = timestamps' posts

combineStamps :: [Timestamp] -> [Timestamp] -> [Timestamp]
combineStamps [] tsps' = tsps'
combineStamps (tsp:tsps) tsps' = tsp : combineStamps tsps tsps'

timestamps' :: [Post] -> [Timestamp]
timestamps' [] = []
timestamps' (post:posts) = combineStamps (timestamps post) (timestamps' posts)

hasNStamps :: Nat -> [Timestamp] -> Bool
hasNStamps Z _     = True
hasNStamps _ []    = False
hasNStamps (S n) (tsp:tsps) = hasNStamps n tsps

capitaliseTitles :: Post -> Post
capitaliseTitles (SinglePost tit tsp) = SinglePost (S tit) tsp
capitaliseTitles (AggregatePost posts) = AggregatePost (capitaliseTitles' posts)

capitaliseTitles' :: [Post] -> [Post]
capitaliseTitles' [] = []
capitaliseTitles' (post:posts) = capitaliseTitles post : capitaliseTitles' posts

prop_Stamps p = Tip.neg (hasNStamps (S $ S Z) (timestamps p))

prop_Titles p = isAggregate p Tip.==> capitaliseTitles p === p

lsc_Stamps = smallCheck 5 (\p -> not (hasNStamps (S $ S Z) (timestamps p)))

lsc_Titles = smallCheck 5 (\p -> isAggregate p LSC.==> capitaliseTitles p == p)
