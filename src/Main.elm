-----------------------
-- Pop Paul Tudor
-- 15.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Main exposing (main, calculateScore)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Random
import Debug exposing (toString)


import Card exposing (..)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )


type Msg
  = Draw
  | NewCard Card
  | ToogleDeck


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , drawCard model
      )
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ( {model | hand = newCard::model.hand, deck = List.filter (\x -> x /= newCard) model.deck }
      , Cmd.none
      )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToogleDeck ->
      ( {model | showDeck = not (model.showDeck)}
      , Cmd.none
      )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none

{-
  1. Get the value of each card (use `cardValue`) n
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hears, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hears, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}

takeIntFromList : List Int -> Int
takeIntFromList ints = case ints of
    [] -> -1
    x::xs -> x


calculateScore : List Card -> List Int
calculateScore cards =
    let
        calculateScoreHelper2 : List Int -> List Int -> List Int
        calculateScoreHelper2 list1 list2 = case list1 of
            [] -> []
            x::xs -> List.map (\a -> a + x) list2 ++ calculateScoreHelper2 xs list2

        calculateScoreHelper : List Card -> List Int
        calculateScoreHelper cardsHelper =  case cardsHelper of
            [] -> [0]
            x::xs -> calculateScoreHelper2 (cardValue x) (calculateScoreHelper xs)

    in
       if List.length (List.filter (\a -> a <= 21) (calculateScoreHelper cards)) > 0 then
            List.take 1 (List.reverse(List.sort(List.filter (\a -> a <= 21) (calculateScoreHelper cards))))
        else
            List.take 1 (List.sort (calculateScoreHelper cards))





subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"
    handMap =  viewCards model.hand
    score = takeIntFromList (calculateScore model.hand)
  in
    div []
      [ h1 [] [text appName]
      , button [ onClick Draw ] [ text "Draw" ]
      , button [ onClick ToogleDeck ] [ text "Toggle" ]
      , div [] [text ("Score: " ++ toString score)]
      , if score == 21 then div [] [text ("YOU WON")]
            else if (score > 21) then div [] [text ("YOU SHOULD TRY AGAIN")]
            else if (score < 15 ) then div [] [text ("YOU CAN'T STOP YET")]
            else div [] [text ("THAT'S GOOD ENOUGH")]
      , div [] (viewCards model.hand)
      , if model.showDeck == True then div [] (viewCards model.deck)
      else div [] []
      ]