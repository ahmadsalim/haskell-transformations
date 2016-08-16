{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes, DeriveDataTypeable, DataKinds, KindSignatures, GADTs, ImpredicativeTypes, StandaloneDeriving #-}
module PathExp2PetriNet where

import Nats
import Tip
import Test.LazySmallCheck as LSC
import Data.Data
import Data.Typeable
import Data.Maybe

import Data.Map as Map
import Data.Set as Set

import Control.Monad.State.Lazy

type Name = Nat
type Identifier = Nat

data PathExp = PathExp { pe_transitions :: Map Name PETransition, pe_states :: Map Identifier PEState }
  deriving (Show, Eq, Ord, Data, Typeable)

data PETransition = PETransition { pe_t_name :: Name, pe_source :: Name, pe_target :: Name  }
  deriving (Show, Eq, Ord, Data, Typeable)

data PEState = PEState { pe_s_id :: Identifier, pe_outgoing :: Set Name, pe_incoming :: Set Name }
  deriving (Show, Eq, Ord, Data, Typeable)

data PetriNet = PetriNet { pn_transitions :: Map Name PNTransition, pn_places :: Map Name Place, pn_arcs :: Map Name Arc }
  deriving (Show, Eq, Ord, Data, Typeable)

data Arc = PlaceToTransArc { pn_a_name :: Name, pn_source :: Identifier, pn_target :: Name, weight :: Nat }
         | TransToPlaceArc { pn_a_name :: Name, pn_source :: Name, pn_taget :: Identifier, weight :: Nat }
  deriving (Show, Eq, Ord, Data, Typeable)

data PNTransition = PNTransition { pn_t_name :: Name, pn_t_outgoing :: Set Name, pn_t_incoming :: Set Name }
  deriving (Show, Eq, Ord, Data, Typeable)

data Place = Place { pn_p_identifier :: Identifier, pn_p_name :: Name,  pn_p_outgoing :: Set Name, pn_p_incoming :: Set Name }
  deriving (Show, Eq, Ord, Data, Typeable)

data TrackingState = TrackingState { counter :: Nat, state2placeMem :: Map Identifier Identifier, transition2transitionMem :: Map Name (Name, Name, Name) }
  deriving (Show, Eq, Ord, Data, Typeable)

initializePlaceId :: MonadState TrackingState m => PEState -> m ()
initializePlaceId PEState{ pe_s_id = stateId } = do
  st <- get
  let placeId = counter st
  put st { counter = S (counter st), state2placeMem = Map.insert stateId placeId (state2placeMem st) }

transition2transition :: MonadState TrackingState m => PETransition -> m (PNTransition, Arc, Arc)
transition2transition (PETransition { pe_t_name = name, pe_source = source, pe_target = target }) = do
  st <- get
  let pniaName = S (counter st)
  let pnia = PlaceToTransArc pniaName (fromJust $ Map.lookup source (state2placeMem st)) name (S Z)
  let pnoaName = S (S (counter st))
  let pnoa = TransToPlaceArc pnoaName name (fromJust $ Map.lookup target (state2placeMem st)) (S Z)
  let pntr = PNTransition name (Set.singleton pnoaName) (Set.singleton pniaName)
  put st { counter = S (S (S (counter st))), transition2transitionMem = Map.insert name (name, pnoaName, pniaName) (transition2transitionMem st) }
  return (pntr, pnoa, pnia)

state2place :: MonadState TrackingState m => PEState -> m Place
state2place (PEState { pe_s_id = id, pe_outgoing = outgoing, pe_incoming = incoming }) = do
  st <- get
  let placeId = fromJust . Map.lookup id $ state2placeMem st
  let pnias = Set.map (\(_,_,pnia) -> pnia) . Set.map (fromJust . flip Map.lookup (transition2transitionMem st)) $ outgoing
  let pnoas = Set.map (\(_,pnoa,_) -> pnoa) . Set.map (fromJust . flip Map.lookup (transition2transitionMem st)) $ incoming
  let place = Place placeId Z pnoas pnias
  return place

pathexp2petrinet :: PathExp -> PetriNet
pathexp2petrinet pe = evalState (do mapM_ initializePlaceId $ pe_states pe
                                    (trs, pnoas, pnias) <- unzip3 <$> mapM transition2transition (Map.elems $ pe_transitions pe)
                                    plcs <- mapM state2place . Map.elems $ pe_states pe
                                    return $ PetriNet (Map.fromList . Prelude.map (\tr -> (pn_t_name tr, tr)) $ trs)
                                                      (Map.fromList . Prelude.map (\pl-> (pn_p_identifier pl, pl)) $ plcs)
                                                      (Map.fromList . Prelude.map (\arc -> (pn_a_name arc, arc)) $ pnoas ++ pnias)
                                ) TrackingState { counter = Z, state2placeMem = Map.empty, transition2transitionMem = Map.empty }
