namespace LearnYouSomePoker
open System
open NUnit.Framework
open Poker


[<TestFixture>]
type Test() = 

  let isThreeOfAKind h = match HandRanking.For h with | ``Three of a Kind`` _ -> true | _ -> false
  let isFourOfAKind h = match HandRanking.For h with | ``Four of a Kind`` _ -> true | _ -> false
  let isStraight h = match HandRanking.For h with | Straight _ -> true | _ -> false
  let isFlush h =    match HandRanking.For h with | Flush _ -> true | _ -> false
  let isStraightFlush h = match HandRanking.For h with | ``Straight Flush`` _ -> true | _ -> false
  let h = Hand.Parse
  let beats h1 h2 = Assert.True(h1 > h2)

  [<Test>]
  member x.``Three 4s is a three of a kind``() =
    h"♡4 ♠4 ♣4 ♣K ♣5" |> isThreeOfAKind |> Assert.True
  
  [<Test>]
  member x.``Four 4s is a four of a kind``() =
    h"♡4 ♠4 ♣4 ♢4 ♣5" |> isFourOfAKind |> Assert.True

  [<Test>]
  member x.``Five clubs is a Flush``() =
    let hand = Hand.Parse 
    h"♣10 ♣9 ♣A ♣K ♣5" |> isFlush |> Assert.True

  [<Test>]
  member x.``Four clubs and a Spade is NOT a Flush``() =
    h"♣10 ♣9 ♣A ♣K ♠5" |> isFlush |> Assert.False
  
  [<Test>]
  member x.``Five sequential cards in mixed suits is a Straight``() =
    h"♣10 ♣9 ♣8 ♣6 ♠7" |> isStraight |> Assert.True

  [<Test>]
  member x.``Five non-sequential cards is NOT a Straight``() =
    h"♣10 ♣9 ♣8 ♣3 ♠7" |> isStraight |> Assert.False

  [<Test>]
  member x.``Five sequential cards of a suit is a Straight Flush``() =
    h"♣10 ♣9 ♣8 ♣6 ♣7" |> isStraightFlush |> Assert.True

  [<Test>]
  member x.``Five non-sequential cards of a suit is a NOT Straight Flush``() =
    h"♣10 ♣9 ♣8 ♣6 ♣5" |> isStraightFlush |> Assert.False

  [<Test>]
  member x.``Two hands are equal if all cards' ranks are equivalent``() =
    Assert.AreEqual(h"♣10 ♣9 ♣8 ♣6 ♣5" , h"♡9 ♡10 ♡5 ♡6 ♡8")

  [<Test>]
  member x.``Straight Flush beats a Flush``() =
    h"♣10 ♣9 ♣8 ♣6 ♣7" |> beats <| h"♣10 ♣9 ♣8 ♣6 ♣4"
  
  [<Test>]
  member x.``Straight Flush beats 4 of a Kind``() =
    h"♣10 ♣9 ♣8 ♣6 ♣7" |> beats <| h"♡4 ♠4 ♣4 ♢4 ♣5" 
  
  [<Test>]
  member x.``Highest four of a kind wins``() =
    h"♡7 ♠7 ♣7 ♢7 ♣5" |> beats <| h"♡4 ♠4 ♣4 ♢4 ♣5"

  [<Test>]
  member x.``Flush beats a Straight``() =
    h"♣10 ♣9 ♣8 ♣6 ♣4" |> beats <| h"♣10 ♣9 ♣8 ♣6 ♡7"
  
  [<Test>]
  member x.``Full House beat a Flush``() =
    h"♡4 ♠4 ♣4 ♢5 ♣5" |> beats <| h"♣10 ♣9 ♣8 ♣6 ♣4"
  
  [<Test>]
  member x.``Three of a kind beats two pair``() =
    h"♡4 ♠4 ♣4 ♢2 ♣5" |> beats <| h"♡4 ♠4 ♣2 ♢2 ♣5" 

  [<Test>]
  member x.``Two pair beats a pair``() =
    h"♡4 ♠4 ♣2 ♢2 ♣5" |> beats <| h"♡4 ♠4 ♣3 ♢2 ♣5" 
  
  [<Test>]
  member x.``High card wins``() =
    h"♡K ♠4 ♣2 ♢2 ♣5" |> beats <| h"♡Q ♠4 ♣3 ♢2 ♣5" 
  
  [<Test>]
  member x.``Pair beats high card``() =
    h"♡K ♠K ♣3 ♢2 ♣5" |> beats <| h"♡A ♠4 ♣3 ♢2 ♣5" 
  
  [<Test>]
  member x.``High pair wins``() =
    h"♡A ♠A ♣3 ♢2 ♣5" |> beats <| h"♡K ♠K ♣3 ♢2 ♣5"