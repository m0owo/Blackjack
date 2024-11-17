# % Define card values
# card_value('2', 2).
# card_value('3', 3).
# card_value('4', 4).
# card_value('5', 5).
# card_value('6', 6).
# card_value('7', 7).
# card_value('8', 8).
# card_value('9', 9).
# card_value('10', 10).
# card_value('J', 10).
# card_value('Q', 10).
# card_value('K', 10).
# card_value('A', 11).

# % Calculate hand score, adjusting Aces to avoid going over 21
# calculate_score([], 0).
# calculate_score([Card | Rest], Score) :-
#     card_value(Card, Value),
#     calculate_score(Rest, RestScore),
#     TempScore is RestScore + Value,
#     (TempScore =< 21 -> Score = TempScore ; adjust_for_aces([Card | Rest], TempScore, Score)).

# % Adjust score if Aces are present and score > 21
# adjust_for_aces(Hand, Score, AdjustedScore) :-
#     include(==( 'A' ), Hand, Aces),      % Filter out Aces from the hand
#     length(Aces, NumAces),
#     adjust_ace_value(NumAces, Score, AdjustedScore).

# adjust_ace_value(0, Score, Score).
# adjust_ace_value(NumAces, Score, AdjustedScore) :-
#     Score > 21,
#     NewScore is Score - 10,
#     NewNumAces is NumAces - 1,
#     adjust_ace_value(NewNumAces, NewScore, AdjustedScore).
# adjust_acevalue(, Score, Score) :-  % If no further adjustment needed
#     Score =< 21.

# % Dealer decision tree
# decisiontree(, DealerScore, hit) :-
#     DealerScore < 17.
# decisiontree(, DealerScore, stand) :-
#     DealerScore >= 17.

# % Simple heuristic for player action
# heuristic(, PlayerScore, hit) :-
#     PlayerScore < 17.
# heuristic(, PlayerScore, stand) :-
#     PlayerScore >= 17.

# % Draw a random card
# draw_card(Card) :-
#     % Define available cards
#     Cards = ['2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A'],
#     random_member(Card, Cards).

# % Player's turn based on heuristic
# player_turn(PlayerHand, PlayerFinalHand) :-
#     calculate_score(PlayerHand, PlayerScore),
#     ( heuristic(PlayerHand, PlayerScore, hit) ->
#         % Player decides to hit
#         draw_card(NewCard),
#         append(PlayerHand, [NewCard], NewHand),
#         player_turn(NewHand, PlayerFinalHand)
#     ; 
#         % Player stands
#         PlayerFinalHand = PlayerHand
#     ).

# % Dealer's turn based on decision tree
# dealer_turn(DealerHand, DealerFinalHand) :-
#     calculate_score(DealerHand, DealerScore),
#     ( decision_tree(DealerHand, DealerScore, hit) ->
#         % Dealer decides to hit
#         draw_card(NewCard),
#         append(DealerHand, [NewCard], NewHand),
#         dealer_turn(NewHand, DealerFinalHand)
#     ; 
#         % Dealer stands
#         DealerFinalHand = DealerHand
#     ).

# % Determine the winner
# determine_winner(PlayerHand, DealerHand, Result) :-
#     calculate_score(PlayerHand, PlayerScore),
#     calculate_score(DealerHand, DealerScore),
#     (PlayerScore > 21 -> Result = dealer_wins ;
#      DealerScore > 21 -> Result = player_wins ;
#      PlayerScore > DealerScore -> Result = player_wins ;
#      DealerScore > PlayerScore -> Result = dealer_wins ;
#      Result = draw).

# % Main game loop
# play_game(Result) :-
#     % Initial hands for player and dealer
#     draw_card(PlayerCard1), draw_card(PlayerCard2),
#     draw_card(DealerCard1), draw_card(DealerCard2),
#     PlayerInitialHand = [PlayerCard1, PlayerCard2],
#     DealerInitialHand = [DealerCard1, DealerCard2],
#     writeln('Player initial hand: '), writeln(PlayerInitialHand),
#     writeln('Dealer initial hand: '), writeln(DealerInitialHand),

#     % Player's turn
#     player_turn(PlayerInitialHand, PlayerFinalHand),
#     writeln('Player final hand: '), writeln(PlayerFinalHand),

#     % Dealer's turn
#     dealer_turn(DealerInitialHand, DealerFinalHand),
#     writeln('Dealer final hand: '), writeln(DealerFinalHand),

#     % Determine winner
#     determine_winner(PlayerFinalHand, DealerFinalHand, Result),
#     writeln('Game result: '), writeln(Result).

% Define card values
card_value('2', 2).
card_value('3', 3).
card_value('4', 4).
card_value('5', 5).
card_value('6', 6).
card_value('7', 7).
card_value('8', 8).
card_value('9', 9).
card_value('10', 10).
card_value('J', 10).
card_value('Q', 10).
card_value('K', 10).
card_value('A', 11).

% Calculate hand score, adjusting Aces to avoid going over 21
calculate_score([], 0).
calculate_score([Card | Rest], Score) :-
    card_value(Card, Value),
    calculate_score(Rest, RestScore),
    TempScore is RestScore + Value,
    (TempScore =< 21 -> Score = TempScore ; adjust_for_aces([Card | Rest], TempScore, Score)).

% Adjust score if Aces are present and score > 21
adjust_for_aces(Hand, Score, AdjustedScore) :-
    include(==('A'), Hand, Aces),
    length(Aces, NumAces),
    adjust_ace_value(NumAces, Score, AdjustedScore).

adjust_ace_value(0, Score, Score).
adjust_ace_value(NumAces, Score, AdjustedScore) :-
    Score > 21,
    NewScore is Score - 10,
    NewNumAces is NumAces - 1,
    adjust_ace_value(NewNumAces, NewScore, AdjustedScore).
adjust_ace_value(_, Score, Score) :-
    Score =< 21.

:- dynamic current_deck/1.

% initialize the deck with specified number of decks
initialize_deck(NumDecks) :-
    retractall(current_deck(_)),
    generate_decks(NumDecks, FullDeck),
    assertz(current_deck(FullDeck)).

% Generate the total cards in the game
generate_decks(NumDecks, FullDeck) :-
    SingleDeck = ['2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A',
                  '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A',
                  '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A',
                  '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A'],
    replicate_deck(SingleDeck, NumDecks, FullDeck).

% Replicate a single deck the specified number of times
replicate_deck(_, 0, []).
replicate_deck(SingleDeck, NumDecks, FullDeck) :-
    NumDecks > 0,
    NewNumDecks is NumDecks - 1,
    replicate_deck(SingleDeck, NewNumDecks, Rest),
    append(SingleDeck, Rest, FullDeck).


# % Draw a random card
# draw_card(Card) :-
#     Cards = ['2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A'],
#     random_member(Card, Cards).

% Draw a random card and remove it from current deck
draw_card(Card) :-
    current_deck(Cards),
    random_member(Card, Cards),
    delete(Cards, Card, NewDeck),
    retractall(current_deck(_)),
    assertz(current_deck(NewDeck)).

% Dealers turn based on fixed rules
dealer_play(DealerHand, DealerScore, FinalHand, FinalScore) :-
    (DealerScore < 17 ->
        draw_card(NewCard),
        append(DealerHand, [NewCard], NewHand),
        calculate_score(NewHand, NewScore),
        dealer_play(NewHand, NewScore, FinalHand, FinalScore)
    ;
        FinalHand = DealerHand,
        FinalScore = DealerScore
    ).

% Determine the winner
determine_winner(PlayerScore, DealerScore, Result) :-
    (PlayerScore > 21 -> Result = dealer_wins;
     DealerScore > 21 -> Result = player_wins;
     PlayerScore > DealerScore -> Result = player_wins;
     DealerScore > PlayerScore -> Result = dealer_wins;
     Result = draw).

% Initial deal
initial_deal(PlayerHand, DealerHand) :-
    draw_card(PlayerCard1), 
    draw_card(PlayerCard2),
    draw_card(DealerCard1), 
    draw_card(DealerCard2),
    PlayerHand = [PlayerCard1, PlayerCard2],
    DealerHand = [DealerCard1, DealerCard2].
