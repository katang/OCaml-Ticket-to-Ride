type action = 
| DrawFaceUp of int
| DrawDeck
| ClaimRoute of (Board.route * Color.color)
| RequestTickets