-----------------------
-- Pop Paul Tudor
-- 15.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Card exposing (Card(..), Face(..), Suit(..), cardValue, viewCard, cardToString, deck, viewCards)

import Html exposing (..)
import Html.Attributes exposing (style)

{-
  Replace with your definitions from assignment 1
-}
type Suit = Spades | Clubs | Hearts | Diamonds
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type  Card = Card Face Suit

faceToString : Face -> String
faceToString face = case face of
    Ace -> "Ace"
    Two -> "Two"
    Three -> "Three"
    Four -> "Four"
    Five -> "Five"
    Six -> "Six"
    Seven -> "Seven"
    Eight -> "Eight"
    Nine -> "Nine"
    Ten -> "Ten"
    Jack -> "Jack"
    Queen -> "Queen"
    King -> "King"

suitToString : Suit -> String
suitToString suit = case suit of
    Spades -> "Spades"
    Clubs -> "Clubs"
    Hearts -> "Hearts"
    Diamonds -> "Diamonds"

cardToString : Card -> String
cardToString card =
    let
        (Card face suit) = card
    in
        faceToString(face) ++ " of " ++ suitToString(suit)


cardValue : Card -> List Int
cardValue card =
    let
        (Card face _) = card
    in
        case face of
                 Ace -> [1, 11]
                 Two -> [2]
                 Three -> [3]
                 Four -> [4]
                 Five -> [5]
                 Six -> [6]
                 Seven -> [7]
                 Eight -> [8]
                 Nine -> [9]
                 _ -> [10]

deck : List Card
deck =
    let
        faces = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
        deckHelper f =
            case f of
                [] -> []
                x::xs -> (Card x Diamonds)::(Card x Hearts)::(Card x Clubs)::(Card x Spades)::deckHelper xs
    in
        deckHelper faces

{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode c =
    let
        (Card face suit) = c
    in
       case face of
         Ace -> case suit of
           Spades ->"🂡"
           Hearts -> "🂱"
           Clubs ->  "🃑"
           Diamonds -> "🃁"
         Two -> case suit of
           Spades ->"🂢"
           Hearts -> "🂲"
           Clubs ->  "🃒"
           Diamonds -> "🃂"
         Three -> case suit of
           Spades ->"🂣"
           Hearts -> "🂳"
           Clubs ->  "🃓"
           Diamonds ->"🃃"
         Four -> case suit of
           Spades ->"🂤"
           Hearts -> "🂴"
           Clubs ->  "🃔"
           Diamonds -> "🃄"
         Five -> case suit of
           Spades ->"🂥"
           Hearts -> "🂵"
           Clubs ->  "🃕"
           Diamonds -> "🃅"
         Six -> case suit of
           Spades ->"🂦"
           Hearts -> "🂶"
           Clubs ->  "🃖"
           _ -> "🃆"
         Seven -> case suit of
           Spades ->"🂧"
           Hearts -> "🂷"
           Clubs ->  "🃗"
           Diamonds -> "🃇"
         Eight -> case suit of
           Spades -> "🂨"
           Hearts ->  "🂸"
           Clubs ->   "🃘"
           Diamonds ->  "🃈"
         Nine -> case suit of
           Spades -> "🂩"
           Hearts ->  "🂹"
           Clubs ->   "🃙"
           Diamonds ->  "🃉"
         Ten -> case suit of
           Spades ->"🂪"
           Hearts -> "🂺"
           Clubs ->  "🃚"
           Diamonds -> "🃊"
         Jack -> case suit of
           Spades ->"🂫"
           Hearts -> "🂻"
           Clubs ->  "🃛"
           Diamonds -> "🃋"
         Queen -> case suit of
           Spades ->"🂭"
           Hearts -> "🂽"
           Clubs ->  "🃝"
           Diamonds -> "🃍"
         King -> case suit of
           Spades -> "🂮"
           Hearts -> "🂾"
           Clubs ->  "🃞"
           Diamonds -> "🃎"


{-
  Modify this function (if needed) to work with your `Card` definition
-}



viewCard : Card -> Html msg
viewCard card =
   let
     (Card face suit) = card
     faceName = faceToString face
     suitName = suitToString suit
     suitColor s =
       case s of
         Diamonds -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode card
   in
     div [style "display" "inline-block"] [
       div [style "font-size" "12em", style "color" (suitColor suit)] [text unicode],
       div [style "font-size" "0.8em"]  [text (cardToString card)]
     ]


viewCards : List Card -> List (Html msg)
viewCards cards = case cards of
    [] -> []
    x::xs -> viewCard x :: viewCards xs
