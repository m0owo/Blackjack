import pygame
from pyswip import Prolog

PLAYER_BUST = 1
PLAYER_WIN = 2
DEALER_WIN = 3
TIE = 4

PLAYER_W_RECORD = 0
DEALER_W_RECORD = 1
TIE_RECORD = 2

class Game:
    def __init__(self, screen, rounds, decks, difficulty):
        # Load the Prolog file
        self.prolog = Prolog()
        self.prolog.consult("core_logic.pl")
        
        # Draw the screen
        self.screen = screen
        self.font = pygame.font.Font('freesansbold.ttf', 44)
        self.smaller_font = pygame.font.Font('freesansbold.ttf', 36)
        self.deal = pygame.draw.rect(self.screen, 'white', [150, 220, 300, 100], 0, 5)

        # Init the game
        self.add_score = False 
        self.game_result = 0
        self.rounds = rounds # number of rounds in the game
        self.decks = decks # number of decks used in the game
        self.difficulty = difficulty
        self.reset_game()

    def get_rounds(self):
        return self.rounds
    
    def get_decks(self):
        return self.decks

    def get_difficulty(self):
        return self.difficulty
    
    def set_rounds(self, rounds):
        self.rounds = rounds

    def set_decks(self, decks):
        self.decks = decks
        
    def set_difficulty(self, difficulty):
        self.difficulty = difficulty
        
    def reset_game(self):
        self.my_hand = [] # current cards in player hand
        self.dealer_hand = [] # current cards in dealer hand
        self.player_score = 0 # current player hand score
        self.dealer_score = 0 # current dealer hand score
        self.outcome = 0 # outcome of the current round
        self.active = True
        self.reveal_dealer = False
        self.hand_active = True
        self.initial_deal = True
        self.records = [0] * 3 # number of player wins, dealer wins, and ties
        self.player_hands = [] # total in player's hands from all rounds
        self.dealer_hands = [] # total in dealer's hands from all rounds
        self.turn = "player"

        # Initialize the deck with 2 decks
        list(self.prolog.query(f"initialize_deck({self.decks})"))

        # Now query the current deck
        for result in self.prolog.query("current_deck(Cards)"):
            print("Current Deck:", result["Cards"])
            
        # Re-initialize the deck in Prolog
        list(self.prolog.query(f"initialize_deck({self.decks})"))
        print("Game state reset")
    
    def reset_round(self):
        self.my_hand = []
        self.dealer_hand = []
        self.player_score = 0
        self.dealer_score = 0
        self.outcome = 0
        self.active =  True
        self.reveal_dealer = False
        self.hand_active = True
        self.initial_deal = True

        # Initialize the deck with 2 decks
        list(self.prolog.query(f"initialize_deck({self.decks})"))

        # Now query the current deck
        for result in self.prolog.query("current_deck(Cards)"):
            print("Current Deck:", result["Cards"])
            
         # Re-initialize the deck in Prolog
        list(self.prolog.query(f"initialize_deck({self.decks})"))
        print("Game state reset")

    # game running
    def handle_event(self, event):
        if self.rounds > 0:
            if self.initial_deal:
                self.initial_deal_func()

            if self.outcome == 0 and event.type == pygame.MOUSEBUTTONUP: # the round has no outcome yet and clicked button
                if self.hit_button.collidepoint(event.pos): # player hits
                    if self.turn == "player" and self.hand_active: 
                        # Draw one card using Prolog
                        for card in self.prolog.query("draw_card(Card)"):
                            new_card = card["Card"]
                            self.my_hand.append(new_card)
                            
                        # Calculate new score
                        self.player_score = self.calculate_hand_score(self.my_hand)
                        
                        if self.player_score > 21:
                            self.hand_active = False
                            self.outcome = PLAYER_BUST  # Player busts
                        else:
                            self.turn = "dealer"  # Switch turn to dealer
                                
                elif self.stand_button.collidepoint(event.pos):
                    if self.turn == "player" and self.hand_active:
                        self.hand_active = False
                        self.turn = "dealer"  # Switch turn to dealer
                        self.dealer_turn()

            elif self.turn == "dealer":
                self.dealer_turn()

            self.check_winner()

        self.game_result = self.calculate_game_result()

        return self.outcome if self.outcome != 0 and self.rounds <= 0 else None
    
    def calculate_game_result(self):
        if self.records[DEALER_W_RECORD] > self.records[PLAYER_W_RECORD]:
            print("game result is 1")
            return 1
        elif self.records[PLAYER_W_RECORD] > self.records[DEALER_W_RECORD]:
            print("game result is 2")
            return 2
        return 3

    def initial_deal_func(self):
        # get initial hands from prolog
        for result in self.prolog.query("initial_deal(PlayerHand, DealerHand)"):
            self.my_hand = list(result["PlayerHand"])
            self.dealer_hand = list(result["DealerHand"])
        
        # calculate the initial scores
        self.player_score = self.calculate_hand_score(self.my_hand)
        self.dealer_score = self.calculate_hand_score(self.dealer_hand)

        # no long initial deal
        self.initial_deal = False

    def calculate_hand_score(self, hand):
        # convert list to prolog format
        prolog_hand = str(hand).replace('[', '[').replace(']', ']')
        query = f"calculate_score({prolog_hand}, Score)"
        print(f"Query: {query}")
        for result in self.prolog.query(query):
            print(f"Prolog Result: {result}")
            return result["Score"]
        print("Failed to calculate score")
        return 0

    # def dealer_turn(self):
    #     self.reveal_dealer = True
        
    #     # Convert Python list to Prolog list format
    #     dealer_hand = str(self.dealer_hand).replace('[', '[').replace(']', ']')
        
    #     # Use Prolog's dealer_play predicate
    #     query = f"dealer_play({dealer_hand}, {self.dealer_score}, FinalHand, FinalScore)"
    #     for result in self.prolog.query(query):
    #         self.dealer_hand = list(result["FinalHand"])
    #         self.dealer_score = result["FinalScore"]
    
    def dealer_turn(self):
        dealer_hand = str(self.dealer_hand).replace('[', '[').replace(']', ']')
        dealer_score = self.dealer_score
        player_score = self.player_score

        # Query the current deck from Prolog
        deck_query = list(self.prolog.query("current_deck(Cards)"))
        if not deck_query:
            print("Error: Unable to retrieve the current deck from Prolog.")
            return
        current_deck = deck_query[0]["Cards"]
        print(f"Current Deck: {current_deck}")

        # Convert the deck into Prolog-compatible format
        prolog_deck = str(current_deck).replace('[', '[').replace(']', ']')

        while self.turn == "dealer":
            # Query Prolog for the dealer's action
            query = f"dealer_decision({self.difficulty}, {dealer_hand}, {dealer_score}, {player_score}, {prolog_deck}, Action)"
            print(f"Querying dealer_decision with: {query}")
            result = list(self.prolog.query(query))
            
            if not result:
                print("Error: Prolog query did not return a result.")
                break
            
            action = result[0]["Action"]
            print(f"Dealer Decision: {action}")

            if action == "hit":
                # Query Prolog to draw a card
                card_result = list(self.prolog.query("draw_card(Card)"))
                if card_result:
                    new_card = card_result[0]["Card"]
                    self.dealer_hand.append(new_card)

                    # Recalculate the dealer's score
                    self.dealer_score = self.calculate_hand_score(self.dealer_hand)
                    print(f"Dealer hits and gets: {new_card}, new score: {self.dealer_score}")
                    self.turn = "player"
                    
                    # Check if the dealer busts
                    if self.dealer_score > 21:
                        print("Dealer busts!")
                        self.turn = "player"  # Switch turn to player
                        break
                else:
                    print("Error: No cards left in the deck.")
                    break

            elif action == "stand":
                print("Dealer stands.")
                self.turn = "player"  # Switch turn to player
                break
            else:
                print("Unexpected action:", action)
                break

    def check_winner(self):
        if not self.hand_active:
            # Determine the winner based on current hand scores in Prolog
            query = f"determine_winner({self.player_score}, {self.dealer_score}, Result)"
            for result in self.prolog.query(query):
                winner = result["Result"]
                if winner == "dealer_wins":
                    self.outcome = PLAYER_BUST if self.player_score > 21 else DEALER_WIN
                elif winner == "player_wins":
                    self.outcome = PLAYER_WIN
                else:  # draw
                    self.outcome = TIE

            # Append the hand scores to the hands records
            self.player_hands.append(self.player_score)
            self.dealer_hands.append(self.dealer_score)

            # Append the round outcome to the records
            if self.outcome == PLAYER_BUST or self.outcome == DEALER_WIN:
                self.records[DEALER_W_RECORD] += 1  # Dealer win
            elif self.outcome == PLAYER_WIN:
                self.records[PLAYER_W_RECORD] += 1  # Player win
            else:
                self.records[TIE_RECORD] += 1  # Draw
            
            # Decrement rounds count
            self.rounds -= 1
            print("rounds left", self.rounds)

            # If no more rounds are left, end the game
            if self.rounds <= 0:
                self.active = False  # Set game to inactive (no more rounds)
                self.outcome = self.calculate_game_result()  # Calculate final result


    def draw(self):
        self.draw_cards(self.my_hand, self.dealer_hand, self.reveal_dealer)
        self.draw_scores(self.player_score, self.dealer_score)
        self.draw_game(self.active, self.records, self.outcome)
        pygame.display.update()

    # [The rest of the drawing methods remain unchanged as they handle UI only]
    def draw_cards(self, player, dealer, reveal):
        for i in range(len(player)):
            pygame.draw.rect(self.screen, 'white', [70 + (70 * i), 460 + (5 * i), 120, 220], 0, 5)
            self.screen.blit(self.font.render(player[i], True, 'black'), (75 + 70 * i, 465 + 5 * i))
        
        for i in range(len(dealer)):
            pygame.draw.rect(self.screen, 'white', [70 + (70 * i), 160 + (5 * i), 120, 220], 0, 5)
            if i != 0 or reveal:
                self.screen.blit(self.font.render(dealer[i], True, 'black'), (75 + 70 * i, 165 + 5 * i))
            else:
                self.screen.blit(self.font.render('???', True, 'black'), (75 + 70 * i, 165 + 5 * i))

    def draw_scores(self, player, dealer):
        self.screen.blit(self.font.render(f'Score[{player}]', True, 'white'), (350, 400))
        if self.reveal_dealer:
            self.screen.blit(self.font.render(f'Score[{dealer}]', True, 'white'), (350, 100))
        else:
            self.screen.blit(self.font.render(f'Score[{self.calculate_hand_score(self.dealer_hand[1:])}] + ???', True, 'white'), (350, 100))

    def draw_game(self, act, record, result):
        button_list = []

        if not act:
            deal = pygame.draw.rect(self.screen, 'white', [150, 20, 300, 100], 0, 5)
            pygame.draw.rect(self.screen, 'green', [150, 20, 300, 100], 3, 5)
            deal_text = self.font.render('DEAL HAND', True, 'black')
            self.screen.blit(deal_text, (165, 50))
            button_list.append(deal)
        else:
            self.hit_button = pygame.draw.rect(self.screen, 'white', [0, 700, 300, 100], 0, 5)
            pygame.draw.rect(self.screen, 'green', [0, 700, 300, 100], 3, 5)
            hit_text = self.font.render('HIT ME', True, 'black')
            self.screen.blit(hit_text, (55, 735))
            button_list.append(self.hit_button)

            self.stand_button = pygame.draw.rect(self.screen, 'white', [300, 700, 300, 100], 0, 5)
            pygame.draw.rect(self.screen, 'green', [300, 700, 300, 100], 3, 5)
            stand_text = self.font.render('STAND', True, 'black')
            self.screen.blit(stand_text, (355, 735))
            button_list.append(self.stand_button)

            score_text = self.smaller_font.render(f'Wins: {record[0]}   Losses: {record[1]}   Draws: {record[2]}', True, 'white')
            self.screen.blit(score_text, (15, 840))

        if result != 0:
            print("here", result)
            if result == PLAYER_BUST:
                print("here1", result)
                outcome_text = self.font.render("You Lost!", True, 'red')
            elif result == PLAYER_WIN:
                print("here2", result)
                outcome_text = self.font.render("You Win!", True, 'green')
            elif result == DEALER_WIN:
                print("here3", result)
                outcome_text = self.font.render("Dealer Wins!", True, 'red')
            elif result == TIE:
                print("here4", result)
                outcome_text = self.font.render("It's a Draw!", True, 'yellow')
            self.screen.blit(outcome_text, (200, 150))

            self.reset_round()

            # self.screen.blit(self.font.render(str(self.records[result]), True, 'white'), (15, 25))
            # self.deal = pygame.draw.rect(self.screen, 'white', [150, 220, 300, 100], 0, 5)
            # pygame.draw.rect(self.screen, 'green', [150, 220, 300, 100], 3, 5)
            # deal_text = self.font.render('NEW HAND', True, 'black')
            # self.screen.blit(deal_text, (165, 250))
            # button_list.append(self.deal)

        return button_list
    