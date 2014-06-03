namespace Poker
open System
open NUnit.Framework
open Game


[<TestFixture>]
type Test() = 

  let isThreeOfAKind h = match HandRanking.For h with | ``Three of a Kind`` _ -> true | _ -> false
  let isStraight h = match HandRanking.For h with | Straight _ -> true | _ -> false
  let isFlush h =    match HandRanking.For h with | Flush _ -> true | _ -> false
  let isStraightFlush h = match HandRanking.For h with | ``Straight Flush`` _ -> true | _ -> false
  let h = Hand.Parse
  let beats h1 h2 = Assert.True(h1 > h2)

  [<Test>]
  member x.``Three 4s is a three of a kind``() =
    h"♡4 ♠4 ♣4 ♣K ♣5" |> isThreeOfAKind |> Assert.True

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
  member x.``Flush beats a Straight``() =
    h"♣10 ♣9 ♣8 ♣6 ♣4" |> beats <|  h"♣10 ♣9 ♣8 ♣6 ♡7"
  
  [<Test>]
  member x.``Straight Flush beats a Flush``() =
    let flush =         h"♣10 ♣9 ♣8 ♣6 ♣4"
    let straightFlush = h"♣10 ♣9 ♣8 ♣6 ♣7"
    Assert.True(straightFlush > flush)
