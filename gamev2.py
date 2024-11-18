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
        # load prolog file
        self.prolog = Prolog()
        self.prolog.consult("core_logic.pl")
        
        # init the screen
        self.screen = screen
        self.screen_width, self.screen_height = self.screen.get_size()

        # set font sizes
        font_size = 44
        smaller_font_size = 36
        self.font = pygame.font.Font('freesansbold.ttf', font_size)
        self.smaller_font = pygame.font.Font('freesansbold.ttf', smaller_font_size)

        self.next_round_button = pygame.draw.rect(self.screen, 'white', [0, 0, 0, 0], 0, 5)


        # init game settings
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

        # init the set number of decks to reset the game
        list(self.prolog.query(f"initialize_deck({self.decks})"))
        for result in self.prolog.query("current_deck(Cards)"):
            print("Current Deck:", result["Cards"])
        print("game reset")
    
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

        # init the set number of decks to reset the round
        list(self.prolog.query(f"initialize_deck({self.decks})"))
        for result in self.prolog.query("current_deck(Cards)"):
            print("Current Deck:", result["Cards"])
        print("round reset")

    # game running
    def handle_event(self, event):
        if self.rounds > 0:
            if self.initial_deal:
                self.initial_deal_func()
            if self.outcome == 0 and event.type == pygame.MOUSEBUTTONUP: # the round has no outcome yet and clicked button
                # player hits
                if self.hit_button.collidepoint(event.pos):
                    if self.turn == "player" and self.hand_active: 
                        # draw a card using Prolog
                        for card in self.prolog.query("draw_card(Card)"):
                            new_card = card["Card"]
                            self.my_hand.append(new_card)
                        # calculate the score in player hand
                        self.player_score = self.calculate_hand_score(self.my_hand)
                        # busts
                        if self.player_score > 21:
                            self.hand_active = False
                            self.outcome = PLAYER_BUST
                        else:
                            self.turn = "dealer"
                # player stands
                elif self.stand_button.collidepoint(event.pos):
                    if self.turn == "player" and self.hand_active:
                        self.hand_active = False
                        self.turn = "dealer"
                        self.dealer_turn()
            elif self.outcome != 0:
                # show result until button pressed
                while event.type != pygame.MOUSEBUTTONUP:
                    for event in pygame.event.get():
                        if event.type == pygame.QUIT:
                            pygame.quit()
                            exit()
                    pygame.time.Clock().tick(60)
                if self.next_round_button.collidepoint(event.pos):
                    self.reset_round()

            elif self.turn == "dealer":
                self.dealer_turn()
                self.screen.fill('black')
                self.draw()
            self.check_winner()
        self.game_result = self.calculate_game_result()
        # show result until button pressed
        return self.game_result if self.game_result != 0 and self.rounds <= 0 else None
    
    # overall game result
    def calculate_game_result(self):
        if self.records[DEALER_W_RECORD] > self.records[PLAYER_W_RECORD]:
            return 1
        elif self.records[PLAYER_W_RECORD] > self.records[DEALER_W_RECORD]:
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

    # sum of cards in hand
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
    
    # dealer's turn to play
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
                dealer_turn_text = self.smaller_font.render(f"Dealer Hit", True, 'Red')
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
                dealer_turn_text = self.smaller_font.render(f"Dealer Stands", True, 'Red')
                print("Dealer stands.")
                self.turn = "player"  # Switch turn to player
                break
            else:
                print("Unexpected action:", action)
                break

            self.screen.fill('black')
            self.screen.blit(dealer_turn_text, (275, 100))
            self.draw()
            start_ticks = pygame.time.get_ticks()
            while pygame.time.get_ticks() - start_ticks < 1000:
                pygame.display.update()
                for event in pygame.event.get():
                    if event.type == pygame.QUIT:
                        pygame.quit()
                        exit()
                pygame.time.Clock().tick(60) 

    # winner of the round
    def check_winner(self):
        if not self.hand_active:
            # Determine the winner based on current hand scores in Prolog
            query = f"determine_winner({self.player_score}, {self.dealer_score}, Result)"
            for result in self.prolog.query(query):
                winner = result["Result"]
                if winner == "dealer_wins":
                    self.outcome = DEALER_WIN
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
                # self.active = False  # Set game to inactive (no more rounds)
                self.outcome = self.calculate_game_result()  # Calculate final result

    # draw game screen
    def draw(self):
        self.draw_cards(self.my_hand, self.dealer_hand, self.reveal_dealer)
        self.draw_scores(self.player_score, self.dealer_score)
        self.draw_game(self.active, self.records, self.outcome)
        pygame.display.update()

    def draw_cards(self, player, dealer, reveal):
        # player cards
        p_top_left_x = 70
        p_top_left_y = 475

        # dealer cards
        d_top_left_x = 70
        d_top_left_y = 175

        # card dimensions
        gap = 5
        card_width = 120
        card_height = 220

        for i in range(len(player)):
            pygame.draw.rect(self.screen, 'white', [p_top_left_x + (p_top_left_x * i), p_top_left_y + (gap * i), card_width, card_height], 0, gap)
            self.screen.blit(self.font.render(player[i], True, 'black'), ((p_top_left_x + gap) + p_top_left_x * i, p_top_left_y + gap + gap * i))
        
        for i in range(len(dealer)):
            pygame.draw.rect(self.screen, 'white', [d_top_left_x + (d_top_left_x * i), d_top_left_y + (gap * i), card_width, card_height], 0, gap)
            if i != 0 or self.outcome != 0:
                self.screen.blit(self.font.render(dealer[i], True, 'black'), ((d_top_left_x + gap) + d_top_left_x * i, d_top_left_y + gap * i))
            else:
                self.screen.blit(self.font.render('???', True, 'black'), ((d_top_left_x + gap) + d_top_left_x * i, d_top_left_y + gap * i))

    def draw_scores(self, player, dealer):
        # player score
        p_x = 400
        p_y = 450

        #dealer score
        d_x = 400
        d_y = 150

        self.screen.blit(self.font.render(f'Score[{player}]', True, 'white'), (p_x, p_y))
        if self.outcome != 0:
            self.screen.blit(self.font.render(f'Score[{dealer}]', True, 'white'), (d_x, d_y))
        else:
            self.screen.blit(self.font.render(f'Score[{self.calculate_hand_score(self.dealer_hand[1:])}] + ???', True, 'white'), (d_x, d_y))

    def draw_game(self, act, record, result):
        # hit button
        h_top_left_x = 100
        h_top_left_y = 725

        # stand button
        s_top_left_x = 400
        s_top_left_y = 725

        # next round button
        n_top_left_x = 250
        n_top_left_y = 725

        # score text
        all_scores_x = 150
        all_scores_y = 25

        # button dimensions
        button_width = 300
        button_height = 100

        # outcome text
        outcome_x = 275
        outcome_y = 100

        button_list = []

        score_text = self.smaller_font.render(f'Wins: {record[0]}   Losses: {record[1]}   Draws: {record[2]}', True, 'white')
        self.screen.blit(score_text, (all_scores_x, all_scores_y))
    
        if self.turn == "player" and result == 0:
            self.hit_button = pygame.draw.rect(self.screen, 'white', [h_top_left_x, h_top_left_y, button_width, button_height], 0, 5)
            pygame.draw.rect(self.screen, 'green', [h_top_left_x, h_top_left_y, button_width, button_height], 3, 5)
            hit_text = self.font.render('HIT ME', True, 'black')
            self.screen.blit(hit_text, (h_top_left_x + 55, h_top_left_y + 25))
            button_list.append(self.hit_button)

            self.stand_button = pygame.draw.rect(self.screen, 'white', [s_top_left_x, s_top_left_y, button_width, button_height], 0, 5)
            pygame.draw.rect(self.screen, 'green', [s_top_left_x, s_top_left_y, button_width, button_height], 3, 5)
            stand_text = self.font.render('STAND', True, 'black')
            self.screen.blit(stand_text, (s_top_left_x + 55, s_top_left_y + 25))
            button_list.append(self.stand_button)
        elif self.turn =='dealer':
            dealer_turn_text = self.smaller_font.render(f"Dealer's Turn", True, 'Red')
            self.screen.blit(dealer_turn_text, (outcome_x, s_top_left_y))
            start_ticks = pygame.time.get_ticks()
            while pygame.time.get_ticks() - start_ticks < 1000:
                pygame.display.update()
                for event in pygame.event.get():
                    if event.type == pygame.QUIT:
                        pygame.quit()
                        exit()
                pygame.time.Clock().tick(60) 

        if result != 0:
            # display round outcome
            if result == PLAYER_BUST:
                outcome_text = self.font.render("You Lost!", True, 'red')
            elif result == PLAYER_WIN:
                outcome_text = self.font.render("You Win!", True, 'green')
            elif result == DEALER_WIN:
                outcome_text = self.font.render("Dealer Wins!", True, 'red')
            elif result == TIE:
                outcome_text = self.font.render("It's a Draw!", True, 'yellow')
            self.screen.blit(outcome_text, (outcome_x, outcome_y))

            # add a button to move on
            self.next_round_button = pygame.draw.rect(self.screen, 'white', [n_top_left_x, n_top_left_y, button_width, button_height], 0, 5)
            if self.rounds != 0:
                next_round_text = self.font.render('Next Round', True, 'black')
            else:
                next_round_text = self.font.render('End Game', True, 'black')
                
            self.screen.blit(next_round_text, (n_top_left_x + 30, n_top_left_y + 25))
            button_list.append(self.next_round_button)
            pygame.display.update()
        
            # self.reset_round()

        return button_list
    