namespace LearnYouSomePoker
open System
open MathNet.Numerics.Probability

module Poker =
  type Rank = 
  | R of int * string
    override r.ToString() =
      match r with 
      | R (_,f) -> f
    member r.Value = let (R(v,_)) = r in v
    static member Ranks = seq { 
      for i in [1..10] do yield R(i, i.ToString())
      yield! [|  R(11, "J") ; R(12, "Q") ; R(13, "K") ; R(14, "A") |]
    }
    static member Parse(s) = Seq.tryFind (fun r -> r.ToString() = s) Rank.Ranks
    static member Create(n) = Seq.find ((=) n) Rank.Ranks

  type Suit = 
    | Clubs 
    | Spades
    | Hearts 
    | Diamonds
    override s.ToString() = 
      match s with
      | Clubs -> "♣"
      | Spades -> "♠"
      | Hearts -> "♡"
      | Diamonds -> "♢"
    static member Suits = [| Clubs; Spades; Hearts; Diamonds |]
    static member Parse(s) = Seq.tryFind (fun c -> c.ToString() = s) Suit.Suits

  [<CustomEquality;CustomComparison>]
  type Card = 
  | C of Rank*Suit
    member c.Suit = let (C (_, suit)) = c in suit
    member c.Rank = let (C (rank, _)) = c in rank
    override c.ToString() =
      let (C (rank, suit)) = c
      match rank with R (_,f) -> sprintf "%O%s" suit f
    
    static member Create(s: String) =
      let (|IsSuit|_|) (c: char) = Suit.Parse (c.ToString())
      let (|IsRank|_|) (c: char) = Rank.Parse (c.ToString())
      match s.ToCharArray() with
      | [| IsSuit a; IsRank b |] -> (C (b, a))
      | [| IsSuit a; '1'; '0' |] -> (C (R(10,"10"), a))
      | _ -> failwithf "Cannot parse card from %s" s

    override x.Equals(yobj) =
      match yobj with
      | :? Card as y -> x.Rank = y.Rank
      | _ -> false
 
    override x.GetHashCode() = hash (x.ToString())
    interface System.IComparable with
      member x.CompareTo yobj =
          match yobj with
          | :? Card as y -> compare x.Rank y.Rank
          | _ -> invalidArg "yobj" "cannot compare values of different types"
    
  type Hand(h: seq<Card>) = 
    let cards = h |> Seq.sort |> Seq.toList |> List.rev
    member __.Cards = cards 

    static member Parse(s: String) : Hand =
      let cards = s.Split(' ') |> Seq.map Card.Create in Hand(cards)

    override x.Equals(yobj) =
      match yobj with
      | :? Hand as y -> x.Cards = y.Cards
      | _ -> false
    override x.GetHashCode() = hash (x.ToString())

    interface System.IComparable with
      member x.CompareTo yobj =
        let compareHands x y = 
          let highcard = compare
          let xRank = HandRanking.For x
          let yRank = HandRanking.For y
          match xRank, yRank with
          | ``Straight Flush``(xf), ``Straight Flush``(yf)   -> highcard xf yf
          | ``Four of a Kind``(xf), ``Four of a Kind``(yf)   -> highcard xf yf
          | ``Full House``(x3,_),   ``Full House``(y3,_)     -> highcard x3 y3
          | Flush(xf),              Flush(yf)                -> highcard xf yf
          | Straight(xf),           Straight(yf)             -> highcard xf yf
          | ``Three of a Kind``(xf), ``Three of a Kind``(yf) -> highcard xf yf
          | ``Two Pair``(xfh,xfl),   ``Two Pair``(yfh,yfl)   -> let c = highcard xfh yfh in if c = 0 then highcard xfl yfl else c
          | Pair(xf),                Pair(yf)                -> highcard xf yf
          | ``High Card``(xf),      ``High Card``(yf)        -> highcard xf yf 
          | _, _ -> (HandRanking.Order xRank) - (HandRanking.Order yRank)
        match yobj with
        | :? Hand as y -> compareHands x y
        | _ -> invalidArg "yobj" "cannot compare values of different types"

  and HandRanking = 
  | ``Straight Flush``  of List<Card>
  | ``Four of a Kind``  of Rank
  | ``Full House``      of Rank*Rank
  | Flush               of List<Card>
  | Straight            of Rank
  | ``Three of a Kind`` of Rank
  | ``Two Pair``        of Rank*Rank
  | Pair                of Rank
  | ``High Card``       of Card
    static member Order = function
      | ``Straight Flush`` _ -> 10
      | ``Four of a Kind`` _ -> 9
      | ``Full House``     _ -> 8
      | Flush              _ -> 7
      | Straight           _ -> 6
      | ``Three of a Kind``_ -> 5
      | ``Two Pair``       _ -> 4
      | Pair               _ -> 3
      | ``High Card``      _ -> 2
    static member For(h: Hand) =
      let rec comb n l = 
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs
      let makeHands (cards: seq<Card>) i =
        (Seq.toList cards) |> comb i 

      let (|IsFlush|_|) (h: Hand) =
          let flush suit = h.Cards |> List.forall (fun c -> c.Suit = suit)
          if Suit.Suits |> Seq.exists flush then Some (Flush h.Cards)
          else None

      let (|IsStraight|_|) (h: Hand) =
        let highCard = h.Cards |> List.head
        match h.Cards |> List.map (fun c -> c.Rank.Value) with
        | [ high; c1; c2; c3; low ] when    high - c1 = 1 
                                         && high - c2 = 2 
                                         && high - c3 = 3 
                                         && high - low = 4 -> Some (Straight highCard.Rank)
        | _ -> None

      let (|StraightFlush|_|) (h: Hand) =
        match h with
        | IsFlush _ & IsStraight s -> Some (``Straight Flush`` h.Cards)
        | _ -> None

      let (|Of_a_Kind|_|) i (h: seq<Card>) =
        let hands = makeHands h i
        let kinds = seq {
          for hand in hands do
            let (C (r, _)) as c = List.head hand 
            if Seq.forall ((=) c) hand then
              yield c
        }
        if Seq.isEmpty kinds then None
        else
          let max = kinds |> Seq.max
          Some max.Rank
      let (|Three_of_a_Kind|_|) (h: Hand) = 
        match h.Cards with
        | Of_a_Kind 3 r -> Some r
        | _ -> None
      let (|Four_of_a_Kind|_|) (h: Hand) = 
        match h.Cards with
        | Of_a_Kind 4 r -> Some r
        | _ -> None 
      
      let (|FullHouse|_|) = function
        | Three_of_a_Kind r -> 
          let rest = h.Cards |> List.filter (fun c ->  c.Rank <> r)
          match rest with
          | Of_a_Kind 2 f -> Some (``Full House`` (r, f))
          | _ -> None
        | _ -> None

      let (|HighPair|_|) = function
        | Of_a_Kind 2 r -> Some r
        | _ -> None
      let (|IsPair|_|) (h: Hand) =
        match h.Cards with
        | HighPair r -> Some (Pair r)
        | _ -> None
      let (|TwoPair|_|) (h: Hand) = 
        match h.Cards with
        | HighPair r ->
          let rest = h.Cards |> List.filter (fun c -> c.Rank <> r)
          match rest with
          | HighPair r2 -> Some (``Two Pair`` (r, r2))
          | _ -> None
        | _ -> None
      

      let (|HighCard|) (h: Hand) = ``High Card`` h.Cards.[0]

      match h with
      | StraightFlush f -> f
      | Four_of_a_Kind f -> (``Four of a Kind`` f)
      | FullHouse f -> f
      | IsFlush f -> f
      | IsStraight f -> f
      | Three_of_a_Kind f -> (``Three of a Kind`` f)
      | TwoPair f -> f
      | IsPair f -> f
      | HighCard c -> c

  type Deck() = 
    let rng = new MathNet.Numerics.Random.MersenneTwister()
    let mutable deck = [| for r in Rank.Ranks do
                          for s in Suit.Suits -> C (r, s) |]
    member __.NextCard() = 
      let slot = rng.Next(0, deck.Length)
      let card = deck.[slot]
      deck <- Array.filter (fun c -> not (Object.ReferenceEquals(c, card))) deck
      card

    interface IDisposable with
      override __.Dispose() = rng.Dispose()
