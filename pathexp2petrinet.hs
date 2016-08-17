{-# LANGUAGE FlexibleContexts, RankNTypes, DeriveDataTypeable, DataKinds, GADTs, ImpredicativeTypes, LambdaCase #-}
module PathExp2PetriNet where

import Debug.Trace

import Nats
import Tip
import Test.LazySmallCheck as LSC
import Data.Data
import Data.Typeable
import Data.Maybe
import Data.List

import Control.Monad.State.Lazy

type Name = Nat
type Identifier = Nat

type Set a = [a]
type Map k v = [(k, v)]

isSet :: Eq a => [a] -> Bool
isSet xs = nub xs == xs

isMap :: Eq k => [(k, v)] -> Bool
isMap xs = isSet (map fst xs)

data PathExp = PathExp { pe_transitions :: Map Name PETransition, pe_states :: Map Identifier PEState }
  deriving (Show, Eq, Ord, Data, Typeable)

instance Serial PathExp where
  series = cons2 PathExp

data PETransition = PETransition { pe_t_name :: Name, pe_source :: Identifier, pe_target :: Identifier }
  deriving (Show, Eq, Ord, Data, Typeable)

instance Serial PETransition where
  series = cons3 PETransition

data PEState = PEState { pe_s_id :: Identifier, pe_outgoing :: Set Name, pe_incoming :: Set Name }
  deriving (Show, Eq, Ord, Data, Typeable)

instance Serial PEState where
  series = cons3 PEState

pathExpValid :: PathExp -> Bool
pathExpValid pe =
  isMap (pe_transitions pe) && isMap (pe_states pe) &&
  all (\(ident, st) -> ident == pe_s_id st &&
                       isSet (pe_outgoing st) && isSet (pe_incoming st) &&
                       all (any ((pe_s_id st ==) . pe_source))
                           (map (`lookup` pe_transitions pe) (pe_outgoing st)) &&
                       all (any ((pe_s_id st ==) . pe_target))
                           (map (`lookup` pe_transitions pe) (pe_incoming st)))
      (pe_states pe) &&
  all (\(name,  tr) -> name == pe_t_name tr &&
                       any (elem (pe_t_name tr) . pe_outgoing) (lookup (pe_source tr) (pe_states pe)) &&
                       any (elem (pe_t_name tr) . pe_incoming) (lookup (pe_target tr) (pe_states pe)))
      (pe_transitions pe)


data PetriNet = PetriNet { pn_transitions :: Map Name PNTransition, pn_places :: Map Identifier Place, pn_arcs :: Map Name Arc }
  deriving (Show, Eq, Ord, Data, Typeable)

data Arc = PlaceToTransArc { pn_a_name :: Name, pn_source :: Identifier, pn_target :: Name, weight :: Nat }
         | TransToPlaceArc { pn_a_name :: Name, pn_source :: Name, pn_target :: Identifier, weight :: Nat }
  deriving (Show, Eq, Ord, Data, Typeable)

data PNTransition = PNTransition { pn_t_name :: Name, pn_t_outgoing :: Set Name, pn_t_incoming :: Set Name }
  deriving (Show, Eq, Ord, Data, Typeable)

data Place = Place { pn_p_identifier :: Identifier, pn_p_name :: Name,  pn_p_outgoing :: Set Name, pn_p_incoming :: Set Name }
  deriving (Show, Eq, Ord, Data, Typeable)

petriNetValid :: PetriNet -> Bool
petriNetValid pn =
  isMap (pn_transitions pn) && isMap (pn_places pn) && isMap (pn_arcs pn) &&
  all (\(name, tr) -> name == pn_t_name tr &&
                      isSet (pn_t_outgoing tr) && isSet (pn_t_incoming tr) &&
                      all (any (\arc -> case arc of { TransToPlaceArc {} -> pn_t_name tr == pn_source arc;
                                                              PlaceToTransArc {} -> False }))
                          (map (`lookup` pn_arcs pn) (pn_t_outgoing tr)) &&
                      all (any (\arc -> case arc of { TransToPlaceArc {} -> False;
                                                              PlaceToTransArc {} -> pn_t_name tr == pn_target arc }))
                          (map (`lookup` pn_arcs pn) (pn_t_incoming tr)))
      (pn_transitions pn) &&
  all (\(ident, pl) -> ident == pn_p_identifier pl &&
                       isSet (pn_p_outgoing pl) && isSet (pn_p_incoming pl) &&
                       all (any (\arc -> case arc of { PlaceToTransArc {} -> pn_p_identifier pl == pn_source arc;
                                                               TransToPlaceArc {} -> False }))
                           (map (`lookup` pn_arcs pn) (pn_p_outgoing pl)) &&
                       all (any (\arc -> case arc of { PlaceToTransArc {} -> False;
                                                              TransToPlaceArc {} -> pn_p_identifier pl == pn_target arc }))
                           (map (`lookup` pn_arcs pn) (pn_p_incoming pl)))
      (pn_places pn) &&
  all (\(name, arc) -> name == pn_a_name arc &&
        case arc of { TransToPlaceArc {} -> any (elem (pn_a_name arc) . pn_t_outgoing) (lookup (pn_source arc) (pn_transitions pn)) &&
                                            any (elem (pn_a_name arc) . pn_p_incoming) (lookup (pn_target arc) (pn_places pn));
                      PlaceToTransArc {} -> any (elem (pn_a_name arc) . pn_p_outgoing) (lookup (pn_source arc) (pn_places pn)) &&
                                            any (elem (pn_a_name arc) . pn_t_incoming) (lookup (pn_target arc) (pn_transitions pn)) })
      (pn_arcs pn)

data TrackingState = TrackingState { counter :: Nat, state2placeMem :: Map Identifier Identifier, transition2transitionMem :: Map Name (Name, Name, Name) }
  deriving (Show, Eq, Ord, Data, Typeable)

initializePlaceId :: MonadState TrackingState m => PEState -> m ()
initializePlaceId PEState{ pe_s_id = stateId } = do
  st <- get
  let placeId = counter st
  put st { counter = S (counter st), state2placeMem = (stateId, placeId) : state2placeMem st }

transition2transition :: MonadState TrackingState m => PETransition -> m (PNTransition, Arc, Arc)
transition2transition PETransition { pe_t_name = name, pe_source = source, pe_target = target } = do
  st <- get
  let pniaName = S (counter st)
  let pnia = PlaceToTransArc pniaName (fromMaybe (error "Source State -> Place") $ lookup source (state2placeMem st)) name (S Z)
  let pnoaName = S (S (counter st))
  let pnoa = TransToPlaceArc pnoaName name (fromMaybe (error "Target State -> Place") $ lookup target (state2placeMem st)) (S Z)
  let pntr = PNTransition name [pnoaName] [pniaName]
  put st { counter = S (S (S (counter st))), transition2transitionMem = (name, (name, pnoaName, pniaName)) : transition2transitionMem st }
  return (pntr, pnoa, pnia)

state2place :: MonadState TrackingState m => PEState -> m Place
state2place PEState { pe_s_id = id, pe_outgoing = outgoing, pe_incoming = incoming } = do
  st <- get
  let placeId = fromJust . lookup id $ state2placeMem st
  let pnoas = map (\(_,_,pntia) -> pntia) . map (fromMaybe (error "Outgoing") . flip lookup (transition2transitionMem st)) $ outgoing
  let pnias = map (\(_,pntoa,_) -> pntoa) . map (fromMaybe (error "Incoming") . flip lookup (transition2transitionMem st)) $ incoming
  let place = Place placeId Z pnoas pnias
  return place

pathexp2petrinet :: PathExp -> PetriNet
pathexp2petrinet pe = evalState (do
  mapM_ initializePlaceId $ map snd (pe_states pe)
  (trs, pnoas, pnias) <- unzip3 <$> mapM (transition2transition . snd) (pe_transitions pe)
  plcs <- mapM (state2place . snd) $ pe_states pe
  return $ PetriNet (map (\tr -> (pn_t_name tr, tr)) trs)
                    (map (\pl-> (pn_p_identifier pl, pl)) plcs)
                    (map (\arc -> (pn_a_name arc, arc)) $ pnoas ++ pnias)
                                ) TrackingState { counter = Z, state2placeMem = [], transition2transitionMem = [] }

lsc_pathexp2petrinetValid :: PathExp -> LSC.Property
lsc_pathexp2petrinetValid pe = LSC.lift (pathExpValid pe) *=>* LSC.lift (petriNetValid (pathexp2petrinet pe))
