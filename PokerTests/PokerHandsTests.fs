namespace PokerTests
open System
open NUnit.Framework
open LearnYouSomePoker.Poker


[<TestFixture>]
type Test() = 

  let h = Hand.Parse
  let isThreeOfAKind h = match HandRanking.For h with | ``Three of a Kind`` _ -> true | _ -> false
  let isFourOfAKind h = match HandRanking.For h with | ``Four of a Kind`` _ -> true | _ -> false
  let isStraight h = match HandRanking.For h with | Straight _ -> true | _ -> false
  let isFlush h =    match HandRanking.For h with | Flush _ -> true | _ -> false
  let isFullHouse h =    match HandRanking.For h with | ``Full House`` _ -> true | _ -> false
  let isStraightFlush h = match HandRanking.For h with | ``Straight Flush`` _ -> true | _ -> false
  let beats h1 h2 = Assert.IsTrue(h h1 > h h2)
  let (<<<) h1 h2 = h1 |> beats <| h2

  [<Test>]
  member x.``Three 4s is a three of a kind``() =
    h"♡4 ♠4 ♣4 ♣K ♣5" |> isThreeOfAKind |> Assert.IsTrue
  
  [<Test>]
  member x.``Four 4s is a four of a kind``() =
    h"♡4 ♠4 ♣4 ♢4 ♣5" |> isFourOfAKind |> Assert.IsTrue

  [<Test>]
  member x.``Five clubs is a Flush``() =
    let hand = Hand.Parse 
    h"♣10 ♣9 ♣A ♣K ♣5" |> isFlush |> Assert.IsTrue

  [<Test>]
  member x.``Four clubs and a Spade is NOT a Flush``() =
    h"♣10 ♣9 ♣A ♣K ♠5" |> isFlush |> Assert.IsFalse
  
  [<Test>]
  member x.``Five sequential cards in mixed suits is a Straight``() =
    h"♣10 ♣9 ♣8 ♣6 ♠7" |> isStraight |> Assert.IsTrue

  [<Test>]
  member x.``Five non-sequential cards is NOT a Straight``() =
    h"♣10 ♣9 ♣8 ♣3 ♠7" |> isStraight |> Assert.IsFalse

  [<Test>]
  member x.``Five sequential cards of a suit is a Straight Flush``() =
    h"♣10 ♣9 ♣8 ♣6 ♣7" |> isStraightFlush |> Assert.IsTrue

  [<Test>]
  member x.``Five non-sequential cards of a suit is a NOT Straight Flush``() =
    h"♣10 ♣9 ♣8 ♣6 ♣5" |> isStraightFlush |> Assert.IsFalse

  [<Test>]
  member x.``Two hands are equal if all cards' ranks are equivalent``() =
    Assert.AreEqual(h"♣10 ♣9 ♣8 ♣6 ♣5" , h"♡9 ♡10 ♡5 ♡6 ♡8")

  [<Test>]
  member x.``Straight Flush beats a Flush``() =
    "♣10 ♣9 ♣8 ♣6 ♣7" <<< "♣10 ♣9 ♣8 ♣6 ♣4"
  
  [<Test>]
  member x.``Straight Flush beats 4 of a Kind``() =
    "♣10 ♣9 ♣8 ♣6 ♣7" <<< "♡4 ♠4 ♣4 ♢4 ♣5" 
  
  [<Test>]
  member x.``Highest four of a kind wins``() =
    "♡7 ♠7 ♣7 ♢7 ♣5" <<< "♡4 ♠4 ♣4 ♢4 ♣5"

  [<Test>]
  member x.``Flush beats a Straight``() =
    "♣10 ♣9 ♣8 ♣6 ♣4" <<< "♣10 ♣9 ♣8 ♣6 ♡7"
  
  [<Test>]
  member x.``Full House beat a Flush``() =
    "♡4 ♠4 ♣4 ♢5 ♣5" |> beats <| "♣10 ♣9 ♣8 ♣6 ♣4"
  
  [<Test>]
  member x.``Three of a kind beats two pair``() =
    "♡4 ♠4 ♣4 ♢2 ♣5" |> beats <| "♡4 ♠4 ♣2 ♢2 ♣5" 

  [<Test>]
  member x.``Two pair beats a pair``() =
    "♡4 ♠4 ♣2 ♢2 ♣5" |> beats <| "♡4 ♠4 ♣3 ♢2 ♣5" 
  
  [<Test>]
  member x.``High card wins``() =
    "♡K ♠4 ♣2 ♢2 ♣5" |> beats <| "♡Q ♠4 ♣3 ♢2 ♣5" 
  
  [<Test>]
  member x.``Pair beats high card``() =
    "♡K ♠K ♣3 ♢2 ♣5" |> beats <| "♡A ♠4 ♣3 ♢2 ♣5" 
  
  [<Test>]
  member x.``High pair wins``() =
    "♡A ♠A ♣3 ♢2 ♣5" |> beats <| "♡K ♠K ♣3 ♢2 ♣5"
  
  [<Test>]
  member x.``Given a eight cards return the Full House``() =
    h"♡2 ♡4 ♠A ♣4 ♢5 ♣3 ♣5 ♠4" |> isFullHouse |> Assert.IsTrue

  [<Test>]
  member x.``Can compare to drawn hands``() =
    use deck = new Deck()
    let hand1 = [for x in [1..5] do yield deck.NextCard()]
    let hand2 = [for x in [1..5] do yield deck.NextCard()]
    printfn "Hand1: %A \nHand2: %A" hand1 hand2
    Assert.That ((hand1 > hand2) || (hand2 >= hand1))