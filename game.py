import pygame
import random
import copy

class Game:
    def __init__(self, screen):
        self.screen = screen
        self.cards = ['2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A']
        self.one_deck = 4 * self.cards
        self.decks = 4
        self.font = pygame.font.Font('freesansbold.ttf', 44)
        self.smaller_font = pygame.font.Font('freesansbold.ttf', 36)
        self.records = [0] * 8
        self.add_score = False 
        self.reset_game()
        self.game_result = None

    def reset_game(self):
        self.game_deck = copy.deepcopy(self.decks * self.one_deck)
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

    def handle_event(self, event):
        if self.initial_deal:
            self.initial_deal_func()

        if self.outcome == 0 and event.type == pygame.MOUSEBUTTONUP:
            if self.hit_button.collidepoint(event.pos):
                if self.hand_active:
                    self.deal_cards(self.my_hand)
                    self.player_score = self.calculate_score(self.my_hand)
                    if self.player_score > 21:  # Check if player busts
                        self.hand_active = False
                        self.outcome = 1  # Player busts
            elif self.stand_button.collidepoint(event.pos):
                if self.hand_active:
                    self.hand_active = False
                    self.dealer_turn()

        self.outcome, self.records, _ = self.check_endgame()
        return self.outcome if self.outcome != 0 else None

    def initial_deal_func(self):
        for i in range(2):
            self.deal_cards(self.my_hand)
            self.deal_cards(self.dealer_hand)
        self.player_score = self.calculate_score(self.my_hand)
        self.dealer_score = self.calculate_score(self.dealer_hand)
        self.initial_deal = False

    def deal_cards(self, hand):
        if len(self.game_deck) == 0:  # Check if the deck is exhausted
            self.game_deck = copy.deepcopy(self.decks * self.one_deck)  # Reset the deck
        card = random.choice(self.game_deck)
        hand.append(card)
        self.game_deck.remove(card)

    def calculate_score(self, hand):
        hand_score = 0
        aces_count = hand.count('A')
        
        for card in hand:
            if card in self.cards[:8]:  # Cards '2' to '9'
                hand_score += int(card)
            elif card in ['10', 'J', 'Q', 'K']:  # Face cards and '10'
                hand_score += 10
            elif card == 'A':  # Ace
                hand_score += 11
        
        # Adjust Aces from 11 to 1 as needed to stay <= 21
        while hand_score > 21 and aces_count > 0:
            hand_score -= 10
            aces_count -= 1
        
        return hand_score

    def dealer_turn(self):
        while self.dealer_score < 17:
            self.deal_cards(self.dealer_hand)
            self.dealer_score = self.calculate_score(self.dealer_hand)
        self.check_endgame()

    def check_endgame(self):
        if not self.hand_active and self.dealer_score >= 17:
            if self.player_score > 21:
                self.outcome = 1  # Player busts
            elif self.dealer_score < self.player_score <= 21 or self.dealer_score > 21:
                self.outcome = 2  # Player wins
            elif self.player_score < self.dealer_score <= 21:
                self.outcome = 3  # Dealer wins
            else:
                self.outcome = 4  # Tie game
                
            # Store the result to be used on the restart screen
            self.game_result = self.outcome
            
            if self.outcome == 1 or self.outcome == 3:
                self.records[1] += 1  # Dealer win
            elif self.outcome == 2:
                self.records[0] += 1  # Player win
            else:
                self.records[2] += 1  # Draw
            
            self.add_score = False  # Prevents updating records again
        return self.outcome, self.records, self.add_score

    def draw(self):
        # Drawing the game content
        self.draw_cards(self.my_hand, self.dealer_hand, self.reveal_dealer)
        self.draw_scores(self.player_score, self.dealer_score)
        self.draw_game(self.active, self.records, self.outcome)

        # Update the screen with the new content
        pygame.display.update() 


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
        
        # Display win/loss/draw message when game ends
        if result != 0:
            if result == 1:
                outcome_text = self.font.render("You Lost!", True, 'red')  # Player loses
            elif result == 2:
                outcome_text = self.font.render("You Win!", True, 'green')  # Player wins
            elif result == 3:
                outcome_text = self.font.render("Dealer Wins!", True, 'red')  # Dealer wins
            elif result == 4:
                outcome_text = self.font.render("It's a Draw!", True, 'yellow')  # Draw

            self.screen.blit(outcome_text, (200, 150))  # Display message at the center

        # If the game is not active, show the 'Deal' button for a new hand
        if not act:
            deal = pygame.draw.rect(self.screen, 'white', [150, 20, 300, 100], 0, 5)
            pygame.draw.rect(self.screen, 'green', [150, 20, 300, 100], 3, 5)
            deal_text = self.font.render('DEAL HAND', True, 'black')
            self.screen.blit(deal_text, (165, 50))
            button_list.append(deal)
        else:
            # Drawing the 'Hit' and 'Stand' buttons during gameplay
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

            # Display the win/loss/draw record
            score_text = self.smaller_font.render(f'Wins: {record[0]}   Losses: {record[1]}   Draws: {record[2]}', True, 'white')
            self.screen.blit(score_text, (15, 840))

        # Show the 'New Hand' button after a game ends
        if result != 0:
            self.screen.blit(self.font.render(str(self.records[result]), True, 'white'), (15, 25))
            deal = pygame.draw.rect(self.screen, 'white', [150, 220, 300, 100], 0, 5)
            pygame.draw.rect(self.screen, 'green', [150, 220, 300, 100], 3, 5)
            deal_text = self.font.render('NEW HAND', True, 'black')
            self.screen.blit(deal_text, (165, 250))
            button_list.append(deal)

        return button_list

