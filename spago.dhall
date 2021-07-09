{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-queue"
, backend = "purerl"
, dependencies =
  [ "control"
  , "either"
  , "erl-lists"
  , "filterable"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "prelude"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
