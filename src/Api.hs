module Api ( api
           ) where

import Rest.Api

import qualified Api.Item as Item

api :: Api IO
api = [(mkVersion 1 0 0, Some1 stuff)]

stuff :: Router IO IO
stuff = root -/ item
  where
    item = route Item.resource
  
