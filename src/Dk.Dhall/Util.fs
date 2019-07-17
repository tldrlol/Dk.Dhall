module Dk.Dhall.Util


let inline curry f a b =
  f (a,  b)