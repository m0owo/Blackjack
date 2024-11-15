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
    def __init__(self, screen):
        self.screen = screen
        self.prolog = Prolog()
        # Load the Prolog file
        self.prolog.consult("core_logic.pl")
        
        self.font = pygame.font.Font('freesansbold.ttf', 44)
        self.smaller_font = pygame.font.Font('freesansbold.ttf', 36)
        self.records = [0] * 8
        self.add_score = False 
        self.reset_game()
        self.game_result = None

    def reset_game(self):
        self.my_hand = []
        self.dealer_hand = []
        self.player_score = 0
        self.dealer_score = 0
        self.outcome = 0
        self.active = True
        self.reveal_dealer = False
        self.hand_active = True
        self.initial_deal = True
        self.records = [0] * 8

    def calculate_hand_score(self, hand):
        # Convert Python list to Prolog list format
        prolog_hand = str(hand).replace('[', '[').replace(']', ']')
        query = f"calculate_score({prolog_hand}, Score)"
        for result in self.prolog.query(query):
            return result["Score"]
        return 0

    def handle_event(self, event):
        if self.initial_deal:
            self.initial_deal_func()

        if self.outcome == 0 and event.type == pygame.MOUSEBUTTONUP:
            if self.hit_button.collidepoint(event.pos):
                if self.hand_active:
                    # Draw one card using Prolog
                    for card in self.prolog.query("draw_card(Card)"):
                        new_card = card["Card"]
                        self.my_hand.append(new_card)
                    
                    # Calculate new score
                    self.player_score = self.calculate_hand_score(self.my_hand)
                    
                    if self.player_score > 21:
                        self.hand_active = False
                        self.outcome = PLAYER_BUST  # Player busts
                        
            elif self.stand_button.collidepoint(event.pos):
                if self.hand_active:
                    self.hand_active = False
                    self.dealer_turn()

        self.check_winner()
        return self.outcome if self.outcome != 0 else None

    def initial_deal_func(self):
        # Get initial hands from Prolog
        for result in self.prolog.query("initial_deal(PlayerHand, DealerHand)"):
            self.my_hand = list(result["PlayerHand"])
            self.dealer_hand = list(result["DealerHand"])
        
        # Calculate initial scores
        self.player_score = self.calculate_hand_score(self.my_hand)
        self.dealer_score = self.calculate_hand_score(self.dealer_hand)
        self.initial_deal = False

    def dealer_turn(self):
        self.reveal_dealer = True
        
        # Convert Python list to Prolog list format
        dealer_hand = str(self.dealer_hand).replace('[', '[').replace(']', ']')
        
        # Use Prolog's dealer_play predicate
        query = f"dealer_play({dealer_hand}, {self.dealer_score}, FinalHand, FinalScore)"
        for result in self.prolog.query(query):
            self.dealer_hand = list(result["FinalHand"])
            self.dealer_score = result["FinalScore"]

    def check_winner(self):
        if not self.hand_active:
            # Use Prolog to determine winner based on scores
            query = f"determine_winner({self.player_score}, {self.dealer_score}, Result)"
            for result in self.prolog.query(query):
                winner = result["Result"]
                
                if winner == "dealer_wins":
                    self.outcome = PLAYER_BUST if self.player_score > 21 else DEALER_WIN
                elif winner == "player_wins":
                    self.outcome = PLAYER_WIN
                else:  # draw
                    self.outcome = TIE
                    
            self.game_result = self.outcome
            
            if self.outcome == PLAYER_BUST or self.outcome == DEALER_WIN:
                self.records[DEALER_W_RECORD] += 1  # Dealer win
            elif self.outcome == PLAYER_WIN:
                self.records[PLAYER_W_RECORD] += 1  # Player win
            else:
                self.records[TIE_RECORD] += 1  # Draw
            
            self.add_score = False

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

    def draw_game(self, act, record, result):
        button_list = []
        
        if result != 0:
            if result == 1:
                outcome_text = self.font.render("You Lost!", True, 'red')
            elif result == 2:
                outcome_text = self.font.render("You Win!", True, 'green')
            elif result == 3:
                outcome_text = self.font.render("Dealer Wins!", True, 'red')
            elif result == 4:
                outcome_text = self.font.render("It's a Draw!", True, 'yellow')

            self.screen.blit(outcome_text, (200, 150))

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
            self.screen.blit(self.font.render(str(self.records[result]), True, 'white'), (15, 25))
            deal = pygame.draw.rect(self.screen, 'white', [150, 220, 300, 100], 0, 5)
            pygame.draw.rect(self.screen, 'green', [150, 220, 300, 100], 3, 5)
            deal_text = self.font.render('NEW HAND', True, 'black')
            self.screen.blit(deal_text, (165, 250))
            button_list.append(deal)

        return button_list
    