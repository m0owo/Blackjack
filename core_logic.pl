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

# calculate_score([], 0).
# calculate_score([Card | Rest], Score) :-
#     card_value(Card, Value),
#     calculate_score(Rest, RestScore),
#     TempScore is RestScore + Value,
#     (TempScore =< 21 -> Score = TempScore; adjust_for_aces([Card | Rest], TempScore, Score)).

% Base case for calculating score
calculate_score([], 0).
calculate_score([Card | Rest], Score) :-
    card_value(Card, Value),
    calculate_score(Rest, RestScore),
    Score is RestScore + Value.

% Adjust score if Aces are present and score > 21
adjust_for_aces(Hand, Score, AdjustedScore) :-
    include(==( 'A'), Hand, Aces),
    length(Aces, NumAces),
    adjust_ace_value(NumAces, Score, AdjustedScore), !. % Prevent backtracking after adjustment

% Adjust Ace value if necessary
adjust_ace_value(0, Score, Score) :- !.
adjust_ace_value(NumAces, Score, AdjustedScore) :-
    Score > 21,
    NumAces > 0,
    NewScore is Score - 10,  % Reduce the score by 10 for each Ace as needed
    NewNumAces is NumAces - 1,
    adjust_ace_value(NewNumAces, NewScore, AdjustedScore).
adjust_ace_value(_, Score, Score).

% Main predicate for calculating the score of a hand
calculate_hand_score(Hand, FinalScore) :-
    calculate_score(Hand, InitialScore),
    adjust_for_aces(Hand, InitialScore, FinalScore).

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

determine_winner(PlayerScore, DealerScore, Result) :-
    (PlayerScore > 21, DealerScore > 21 -> Result = draw;           % Both bust
     PlayerScore > 21 -> Result = dealer_wins;                      % Player busts
     DealerScore > 21 -> Result = player_wins;                      % Dealer busts
     21 - PlayerScore < 21 - DealerScore -> Result = player_wins;   % Closest to 21
     21 - DealerScore < 21 - PlayerScore -> Result = dealer_wins;   % Closest to 21
     Result = draw).                                                % Scores are equal

% Initial deal
initial_deal(PlayerHand, DealerHand) :-
    draw_card(PlayerCard1), 
    draw_card(PlayerCard2),
    draw_card(DealerCard1), 
    draw_card(DealerCard2),
    PlayerHand = [PlayerCard1, PlayerCard2],
    DealerHand = [DealerCard1, DealerCard2].

% Dealer decision-making based on difficulty
dealer_decision(Easy, DealerHand, DealerScore, PlayerScore, Deck, Action) :-
    Easy = easy, % Easy level: Stand at 17 or higher
    (DealerScore < 17 -> Action = hit ; Action = stand).

dealer_decision(Medium, DealerHand, DealerScore, PlayerScore, Deck, Action) :-
    Medium = medium, % Medium level: Evaluate bust probability
    (DealerScore < 17 -> Action = hit ; 
     bust_probability(DealerScore, Deck, Probability),
     (Probability < 0.5 -> Action = stand ; Action = hit)). % Stand if bust probability < 50%

dealer_decision(Hard, DealerHand, DealerScore, PlayerScore, Deck, Action) :-
    Hard = hard, % Hard level: Use an advanced evaluation function
    evaluate_move_advanced(DealerHand, DealerScore, PlayerScore, Deck, Action).

% Calculate bust probability based on DealerScore and current deck composition
bust_probability(DealerScore, Deck, Probability) :-
    findall(Card, member(Card, Deck), Cards),
    include(card_value_exceeds(DealerScore), Cards, BustCards),
    length(BustCards, BustCount),
    length(Cards, TotalCount),
    (TotalCount > 0 -> Probability is BustCount / TotalCount ; Probability = 0).

% Helper to filter cards that would make the dealer bust
card_value_exceeds(DealerScore, Card) :-
    card_value(Card, Value),
    NewScore is DealerScore + Value,
    NewScore > 21.

% Advanced evaluation for Hard difficulty
evaluate_move_advanced(DealerHand, DealerScore, PlayerScore, Deck, Action) :-
    simulate_hit(DealerHand, DealerScore, Deck, ScoreHit, ProbHit),
    simulate_stand(DealerScore, PlayerScore, Deck, ProbStand),
    (ProbHit >= ProbStand -> Action = hit ; Action = stand).

% Simulate the effect of hitting
simulate_hit(DealerHand, DealerScore, Deck, NewScore, ProbHit) :-
    draw_card(NextCard),
    append(DealerHand, [NextCard], NewHand),
    calculate_score(NewHand, NewScore),
    win_probability(NewScore, DealerScore, Deck, ProbHit).

% Simulate the effect of standing
simulate_stand(DealerScore, PlayerScore, Deck, ProbStand) :-
    win_probability(DealerScore, PlayerScore, Deck, ProbStand).

% Win probability calculation based on remaining cards and scores
win_probability(DealerScore, PlayerScore, Deck, Probability) :-
    (DealerScore > 21 -> Probability = -1; % Dealer busts
     PlayerScore > 21 -> Probability = 1; % Player busts
     DealerScore > PlayerScore -> Probability = 0.7; % Likely win
     DealerScore =< PlayerScore -> Probability = 0.3). % Likely loss
