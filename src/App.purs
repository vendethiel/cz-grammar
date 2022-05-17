module App where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (ReactComponent, component, element, fragment, useState, (/\))
import React.Basic.Hooks as React

data Gender = MasculineAnimate | MasculineInanimate | Feminine | Neuter

instance showGender :: Show Gender where
  show MasculineInanimate = "Masculine Inanimate"
  show MasculineAnimate = "Masculine Animate"
  show Feminine = "Feminine"
  show Neuter = "Neuter"

data ApplyWhere = AlwaysTrue | ExceptWhen String | OnlyWhen String
data Rule = Rule String ApplyWhere -- TODO Which cases are talked about?

sgRules :: Array Rule
sgRules =
  [ Rule "Masc/neuter INST -em" $ ExceptWhen "Mascule -a, Neuter -í"
  , Rule "-a => ACC -u" $ AlwaysTrue
  ]

mkRuleTable :: React.Component { category :: Maybe Int }
mkRuleTable = do
  component "RuleTable" \{ category } -> React.do
    pure $ R.ul_ $ renderRule <$> sgRules
  where renderRule (Rule name cond) =
          R.li_
            [ R.b_ [R.text name]
            , R.br {}
            , R.i_ [showException cond]
            ]

        showException AlwaysTrue = R.text "(always applies)"
        showException (ExceptWhen except) = R.text $ "(applies except when " <> except <> ")"
        showException (OnlyWhen except) = R.text $ "(applies only when " <> except <> ")"

mkApp :: Effect (ReactComponent {})
mkApp = do
  table <- mkRuleTable
  component "App" \_  -> React.do
    selectedCategory /\ selectCategory <- useState Nothing
    pure
      $ fragment
        [ R.button
          { onClick: handler_ $ selectCategory $ const $ Just 1
          , children: [ R.text $ "Current category: " <> show selectedCategory ]
          }
        , element table { category: selectedCategory }
        ]

