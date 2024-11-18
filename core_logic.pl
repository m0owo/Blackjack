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

% calculate the score including the ace adjustment
calculate_hand_score(Hand, FinalScore) :-
    calculate_score(Hand, InitialScore),
    adjust_for_aces(Hand, InitialScore, FinalScore).

% calculating the sum of all cards
calculate_score([], 0).
calculate_score([Card | Rest], Score) :-
    card_value(Card, Value),
    calculate_score(Rest, RestScore),
    Score is RestScore + Value.

% adjust the score with aces when it exceeds 21
adjust_for_aces(Hand, Score, AdjustedScore) :-
    include(==( 'A'), Hand, Aces),
    length(Aces, NumAces),
    adjust_ace_value(NumAces, Score, AdjustedScore), !.

% try minus 10 for every ace in the hand
adjust_ace_value(0, Score, Score) :- !.
adjust_ace_value(NumAces, Score, AdjustedScore) :-
    Score > 21,
    NumAces > 0,
    NewScore is Score - 10,
    NewNumAces is NumAces - 1,
    adjust_ace_value(NewNumAces, NewScore, AdjustedScore).
adjust_ace_value(_, Score, Score).

% current deck might change during runtime
:- dynamic current_deck/1.

% initialize the deck with the generated deck
initialize_deck(NumDecks) :-
    retractall(current_deck(_)),
    generate_decks(NumDecks, FullDeck),
    assertz(current_deck(FullDeck)).

% generate the replicated decks
generate_decks(NumDecks, FullDeck) :-
    SingleDeck = ['2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A',
                  '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A',
                  '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A',
                  '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A'],
    replicate_deck(SingleDeck, NumDecks, FullDeck).

% replicate a single deck the specified number of times
replicate_deck(_, 0, []).
replicate_deck(SingleDeck, NumDecks, FullDeck) :-
    NumDecks > 0,
    NewNumDecks is NumDecks - 1,
    replicate_deck(SingleDeck, NewNumDecks, Rest),
    append(SingleDeck, Rest, FullDeck).

% draw a random card and remove it from current deck
draw_card(Card) :-
    current_deck(Cards),
    random_member(Card, Cards),
    delete(Cards, Card, NewDeck),
    retractall(current_deck(_)),
    assertz(current_deck(NewDeck)).

% dealer turn based on fixed rules
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

% determine who is winner based on score comparison
determine_winner(PlayerScore, DealerScore, Result) :-
    (PlayerScore > 21 -> Result = dealer_wins;
     DealerScore > 21 -> Result = player_wins;
     PlayerScore > DealerScore -> Result = player_wins;
     DealerScore > PlayerScore -> Result = dealer_wins;
     Result = draw).

% dealing the initial cards
initial_deal(PlayerHand, DealerHand) :-
    draw_card(PlayerCard1), 
    draw_card(PlayerCard2),
    draw_card(DealerCard1), 
    draw_card(DealerCard2),
    PlayerHand = [PlayerCard1, PlayerCard2],
    DealerHand = [DealerCard1, DealerCard2].

% dealer_decision making process based on difficulty
% EASY: stand at 17 or higher
dealer_decision(Easy, DealerHand, DealerScore, PlayerScore, Deck, Action) :-
    Easy = easy,
    (DealerScore < 17 -> Action = hit ; Action = stand).

% MEDIUM: evaluate bust probability (stand if bust probability < 50%)
dealer_decision(Medium, DealerHand, DealerScore, PlayerScore, Deck, Action) :-
    Medium = medium,
    (DealerScore < 17 -> Action = hit ; 
     bust_probability(DealerScore, Deck, Probability),
     (Probability < 0.5 -> Action = stand ; Action = hit)).

% HARD: use an evaluation function
dealer_decision(Hard, DealerHand, DealerScore, PlayerScore, Deck, Action) :-
    Hard = hard,
    evaluate_move_advanced(DealerHand, DealerScore, PlayerScore, Deck, Action).

% calculate bust probability based on DealerScore and current deck composition
% the probability is the ratio of cards left in the deck which will cost bust
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

% more advanced evaluation function
% simulates the different moves (hitting, standing)
evaluate_move_advanced(DealerHand, DealerScore, PlayerScore, Deck, Action) :-
    simulate_hit(DealerHand, DealerScore, Deck, ScoreHit, ProbHit),
    simulate_stand(DealerScore, PlayerScore, Deck, ProbStand),
    (ProbHit >= ProbStand -> Action = hit ; Action = stand).

% simulate hitting: draw a card and calculate the probability based on the card drawn
simulate_hit(DealerHand, DealerScore, Deck, NewScore, ProbHit) :-
    draw_card(NextCard),
    append(DealerHand, [NextCard], NewHand),
    calculate_score(NewHand, NewScore),
    win_probability(NewScore, DealerScore, Deck, ProbHit).

% simulate standing: calculate the probability based on the current hand
simulate_stand(DealerScore, PlayerScore, Deck, ProbStand) :-
    win_probability(DealerScore, PlayerScore, Deck, ProbStand).

% win probability: 
win_probability(DealerScore, PlayerVisibleScore, Deck, Probability) :-
    findall(HiddenCard, member(HiddenCard, Deck), HiddenCards),
    calculate_possible_player_scores(PlayerVisibleScore, HiddenCards, PlayerPossibleScores),
    evaluate_outcomes(DealerScore, PlayerPossibleScores, HiddenCards, Probability).

% find all possible score on the player hand based on the visible cards and the cards remaining in the deck
calculate_possible_player_scores(PlayerVisibleScore, HiddenCards, PlayerScores) :-
    findall(FinalScore, (
        member(Card, HiddenCards),
        card_value(Card, Value),
        FinalScore is PlayerVisibleScore + Value
    ), PlayerScores).

% evaluate dealer possibility using all possible player scores
evaluate_outcomes(DealerScore, PlayerScores, Deck, Probability) :-
    % get the ratio of possible player scores resulting in player bust
    include(>(21), PlayerScores, BustScores),
    length(BustScores, PlayerBusts),
    length(PlayerScores, TotalScores),
    WinProbability is PlayerBusts / TotalScores,

    % get the ratio of possible dealer score resulting in 21
    TargetScore is 21 - DealerScore,
    include(=(TargetScore), Deck, WinningCards),
    length(WinningCards, NumWinningCards),
    length(Deck, TotalCards),
    (TotalCards > 0 -> Draw21Probability is NumWinningCards / TotalCards ; Draw21Probability = 0),

    % consider all the possible dealer outcomes
    (DealerScore > 21 -> Probability = -1 ; % dealer busts
     Draw21Probability > 0 -> Probability = 0.9 ; % dealer can draw to 21
     DealerScore >= 17, 
     WinProbability > 0.5 -> Probability = 0.7 ;
     Probability = 0.3).

% basic strategy recommendation for player based on visible cards
suggest_move(PlayerHand, DealerUpCard, Suggestion) :-
    calculate_hand_score(PlayerHand, PlayerScore),
    card_value(DealerUpCard, DealerValue),
    (
        % If player has Ace and value is <= 21, treat as soft hand
        member('A', PlayerHand),
        PlayerScore =< 21 ->
        suggest_move_soft(PlayerScore, DealerValue, Suggestion)
    ;
        % Otherwise treat as hard hand
        suggest_move_hard(PlayerScore, DealerValue, Suggestion)
    ).

% Suggestions for hard hands (no usable Ace)
suggest_move_hard(PlayerScore, DealerValue, Suggestion) :-
    (
        PlayerScore >= 17 -> Suggestion = stand
    ;   PlayerScore =< 11 -> Suggestion = hit
    ;   PlayerScore >= 12, PlayerScore =< 16 ->
        (DealerValue >= 7 -> Suggestion = hit ; Suggestion = stand)
    ).

% Suggestions for soft hands (with usable Ace)
suggest_move_soft(PlayerScore, DealerValue, Suggestion) :-
    (
        PlayerScore >= 19 -> Suggestion = stand
    ;   PlayerScore =< 17 -> Suggestion = hit
    ;   PlayerScore = 18 ->
        (DealerValue >= 9 -> Suggestion = hit ; Suggestion = stand)
    ).

% Calculate win probability for current hand
calculate_win_probability(PlayerHand, DealerUpCard, Probability) :-
    calculate_hand_score(PlayerHand, PlayerScore),
    card_value(DealerUpCard, DealerValue),
    (
        PlayerScore > 21 -> Probability = 0.0  % Bust
    ;   PlayerScore = 21 -> Probability = 0.9  % Blackjack
    ;   calculate_probability(PlayerScore, DealerValue, Probability)
    ).

% Helper predicate to calculate win probability based on scores
calculate_probability(PlayerScore, DealerValue, Probability) :-
    (
        PlayerScore >= 17 -> 
            (DealerValue >= 7 -> Probability = 0.4 ; Probability = 0.7)
    ;   PlayerScore =< 11 -> Probability = 0.6
    ;   PlayerScore >= 12, PlayerScore =< 16 ->
            (DealerValue >= 7 -> Probability = 0.3 ; Probability = 0.5)
    ).
