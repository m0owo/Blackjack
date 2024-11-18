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

        # the end game button
        self.next_round_button = pygame.draw.rect(self.screen, 'white', [0, 0, 0, 0], 0, 5)

        # init game settings
        self.game_result = 0 # the overall final result of the game
        self.rounds = rounds # number of rounds in the game
        self.decks = decks # number of decks in the game
        self.difficulty = difficulty # level of difficulty
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
        self.my_hand = [] # cards in player hand
        self.dealer_hand = [] # cards in dealer hand
        self.player_score = 0 # sum of player cards
        self.dealer_score = 0 # sum of dealer cards
        self.outcome = 0 # outcome of the round
        self.game_result = 0 # result of game

        self.initial_deal = True # initial card dealing
        self.player_stood = False
        self.dealer_stood = False

        self.records = [0] * 3 # records of round results (wins, losses, ties)
        self.player_hands = [] # records of player hands in all rounds
        self.dealer_hands = [] # records of dealer hands in all rounds

        self.turn = "player" # starting turn

        self.initialize_deck() #initialize deck with the set number of decks

    def reset_round(self):
        self.my_hand = []
        self.dealer_hand = []
        self.player_score = 0
        self.dealer_score = 0
        self.outcome = 0

        self.initial_deal = True
        self.player_stood = False
        self.dealer_stood = False

        self.initialize_deck()
    
    def initialize_deck(self):
        list(self.prolog.query(f"initialize_deck({self.decks})"))
        for result in self.prolog.query("current_deck(Cards)"):
            print("Current Deck:", result["Cards"])
        print("deck initialized")

    def draw(self):
        self.draw_cards()
        self.draw_scores()
        self.draw_game()
        pygame.display.update()
    
    def draw_cards(self):
        player = self.my_hand
        dealer = self.dealer_hand

        # player cards coordinates
        p_top_left_x = 70
        p_top_left_y = 475

        # dealer cards coordinates
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
    
    def draw_scores(self):
        player = self.player_score
        dealer = self.dealer_score

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

    def draw_game(self):
        record = self.records
        result = self.outcome

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
        outcome_y = 725

        # display the current wins, losses, and ties
        score_text = self.smaller_font.render(f'Wins: {record[0]}   Losses: {record[1]}   Draws: {record[2]}', True, 'white')
        self.screen.blit(score_text, (all_scores_x, all_scores_y))

        if result == 0:
            # if it is the player's turn and round is ongoing
            if self.turn == "player":
                # draw the hit button
                self.hit_button = pygame.draw.rect(self.screen, 'white', [h_top_left_x, h_top_left_y, button_width, button_height], 0, 5)
                pygame.draw.rect(self.screen, 'green', [h_top_left_x, h_top_left_y, button_width, button_height], 3, 5)
                hit_text = self.font.render('HIT ME', True, 'black')
                self.screen.blit(hit_text, (h_top_left_x + 55, h_top_left_y + 25))

                # draw the stand button
                self.stand_button = pygame.draw.rect(self.screen, 'white', [s_top_left_x, s_top_left_y, button_width, button_height], 0, 5)
                pygame.draw.rect(self.screen, 'green', [s_top_left_x, s_top_left_y, button_width, button_height], 3, 5)
                stand_text = self.font.render('STAND', True, 'black')
                self.screen.blit(stand_text, (s_top_left_x + 55, s_top_left_y + 25))

            # if it is the dealer's turn and the round is ongoing
            elif self.turn =='dealer':
                # dont draw the buttons and instead show that its dealer's turn
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
        else:
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
            pygame.display.update()

    def handle_event(self, event):
        # the game hasnt ended
        if self.game_result == 0:
            #initial card dealing
            if self.initial_deal:
                self.initial_deal_func()
            # the round hasnt ended
            if self.outcome == 0:
                if self.player_stood and self.dealer_stood:
                    self.check_winner()
                # click happens on player's turn
                if self.turn == "player" and event.type == pygame.MOUSEBUTTONUP:
                    # player hits
                    if self.hit_button.collidepoint(event.pos):
                        # draw a card using prolog
                        for card in self.prolog.query("draw_card(Card)"):
                            new_card = card["Card"]
                            self.my_hand.append(new_card)
                        # calculate the score in the player's hand
                        self.player_score = self.calculate_hand_score(self.my_hand)
                        # end the turn
                        self.turn = "dealer"
                    # player stands
                    elif self.stand_button.collidepoint(event.pos):
                        self.player_stood = True
                        self.turn = "dealer"
                # dealer's turn 
                if self.turn == "dealer":
                    self.dealer_turn()
            # round has ended
            elif self.outcome != 0:
                # show the next round button
                while event.type != pygame.MOUSEBUTTONUP:
                    for event in pygame.event.get():
                        if event.type == pygame.QUIT:
                            pygame.quit()
                            exit()
                    pygame.time.Clock().tick(60)
                if self.next_round_button.collidepoint(event.pos):
                    if self.rounds > 0:
                        self.reset_round()
                    else:
                        self.game_result = self.calculate_game_result()
        return self.game_result if self.game_result != 0 and self.rounds <= 0 else None

    def dealer_turn(self):
        dealer_hand = str(self.dealer_hand).replace('[', '[').replace(']', ']')
        dealer_score = self.dealer_score
        player_score = self.player_score

        # query the current deck
        deck_query = list(self.prolog.query("current_deck(Cards)"))
        current_deck = deck_query[0]["Cards"]
        prolog_deck = str(current_deck).replace('[', '[').replace(']', ']')

        while self.turn == "dealer":
            # query for dealer's decision
            query = f"dealer_decision({self.difficulty}, {dealer_hand}, {dealer_score}, {player_score}, {prolog_deck}, Action)"
            result = list(self.prolog.query(query))
            action = result[0]["Action"]

            if action == "hit":
                # show the dealer decision
                dealer_decision = self.smaller_font.render("Dealer Hit", True, 'Red')
                self.player_stood = False

                # draw a card
                card_result = list(self.prolog.query("draw_card(Card)"))
                if card_result:
                    new_card = card_result[0]["Card"]
                    self.dealer_hand.append(new_card)

                    self.dealer_score = self.calculate_hand_score(self.dealer_hand)
                    self.turn = "player"
            elif action == "stand":
                # show dealer decision
                dealer_decision = self.smaller_font.render("Dealer Stand", True, 'Red')
                self.dealer_stood = True

                self.turn = "player"
        
        # clear the screen and redraw the screen
        self.screen.fill('black')
        self.draw()
        self.screen.blit(dealer_decision, (275, 100))

        # wait a bit
        start_ticks = pygame.time.get_ticks()
        while pygame.time.get_ticks() - start_ticks < 1000:
            pygame.display.update()
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
                    exit()
            pygame.time.Clock().tick(60) 
    
    def check_winner(self):
        self.player_stood = False
        self.dealer_stood = False

        # determine the winner using prolog
        query = f"determine_winner({self.player_score}, {self.dealer_score}, Result)"
        for result in self.prolog.query(query):
            winner = result["Result"]
            if winner == "dealer_wins":
                self.outcome = PLAYER_BUST if self.player_score > 21 else DEALER_WIN
            elif winner == "player_wins":
                self.outcome = PLAYER_WIN
            else:  # draw
                self.outcome = TIE
        
        # append the scores to the hand records
        self.player_hands.append(self.player_score)
        self.dealer_hands.append(self.dealer_score)

        # append round outcome to records
        if self.outcome == PLAYER_BUST or self.outcome == DEALER_WIN:
            self.records[DEALER_W_RECORD] += 1  # Dealer win
        elif self.outcome == PLAYER_WIN:
            self.records[PLAYER_W_RECORD] += 1  # Player win
        else:
            self.records[TIE_RECORD] += 1  # Draw
            
        # decrement rounds count
        self.rounds -= 1

    def initial_deal_func(self):
        print("initial dealing")
        # get initial hands from prolog
        for result in self.prolog.query("initial_deal(PlayerHand, DealerHand)"):
            self.my_hand = list(result["PlayerHand"])
            self.dealer_hand = list(result["DealerHand"])
        print(self.my_hand)
        print(self.dealer_hand)
        
        # calculate the initial scores
        self.player_score = self.calculate_hand_score(self.my_hand)
        self.dealer_score = self.calculate_hand_score(self.dealer_hand)

        # no longer initial deal
        self.initial_deal = False

    # sum of cards in hand
    def calculate_hand_score(self, hand):
        # convert list to prolog format
        prolog_hand = str(hand).replace('[', '[').replace(']', ']')
        query = f"calculate_score({prolog_hand}, Score)"
        # print(f"Query: {query}")
        for result in self.prolog.query(query):
            # print(f"Prolog Result: {result}")
            return result["Score"]
        # print("Failed to calculate score")
        return 0
    
    def calculate_game_result(self):
        if self.records[DEALER_W_RECORD] > self.records[PLAYER_W_RECORD]:
            return 1
        elif self.records[PLAYER_W_RECORD] > self.records[DEALER_W_RECORD]:
            return 2
        return 3
                        
