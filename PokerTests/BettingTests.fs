namespace PokerTests
open System
open NUnit.Framework
open LearnYouSomePoker.Poker

[<TestFixture>]
type BettingTests() = 
  let firstPlayer  = Player("P1", 100)
  let secondPlayer = Player("P2", 100)
  let thirdPlayer  = Player("P3", 100)
  let fourthPlayer = Player("P4", 100)
  let fourPlayers = [|firstPlayer; secondPlayer; thirdPlayer; fourthPlayer|]

  [<Test>]
  member x.``Four players: small blind 5 makes big blind 10``() =
    let game = Round(players = fourPlayers, small = 5)
    Assert.AreEqual(10, game.State.BigBlind |> snd)

  [<Test>]
  member x.``Four players: small blind 5, first to act raises 10, making her bet 20``() =
    let game = Round(players = fourPlayers, small = 5)
    let newGame = { game.State with History = seq { yield! game.State.History; 
                                                    yield Raise(game.State.Current, 10) }}
    Assert.AreEqual(20, newGame.CurrentBet)
