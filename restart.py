import pygame

PLAYER_BUST = 1
PLAYER_WIN = 2
DEALER_WIN = 3
TIE = 4

class Restart:
    def __init__(self, screen):
        self.screen = screen
        self.font = pygame.font.Font('freesansbold.ttf', 44)
        self.retry_button = pygame.Rect(150, 300, 300, 100)
        self.menu_button = pygame.Rect(150, 440, 300, 100)
        self.game_result = None  # Store game result
        self.round = 0
        self.player_hands = [0] * self.round
        self.dealer_hands = [0] * self.round
        self.prev_round_button = pygame.Rect(100, 650, 50, 50)
        self.next_round_button = pygame.Rect(400, 650, 50, 50)
    
    def set_result(self, result):
        print("setting game result", result)
        self.game_result = result
    
    def set_player_hands(self, hands):
        self.player_hands = hands
    
    def set_dealer_hands(self, hands):
        self.dealer_hands = hands
    
    def next_round(self):
        if self.round < len(self.player_hands) - 1:
            self.round += 1
     
    def prev_round(self):
        if self.round > 0:
            self.round -= 1

    def draw(self):
        # Restart Screen
        self.screen.fill('black')
        self.screen.blit(self.font.render("Results", True, 'white'), (200, 600))
        pygame.draw.rect(self.screen, 'white', self.prev_round_button)
        self.screen.blit(self.font.render("<", True, 'black'), (100, 650))
        self.screen.blit(self.font.render("Round", True, 'white'), (200, 650))
        self.screen.blit(self.font.render(str(self.round + 1), True, 'white'), (350, 650))
        pygame.draw.rect(self.screen, 'white', self.next_round_button)
        self.screen.blit(self.font.render(">", True, 'black'), (410, 650))

        print(self.player_hands)
        print(self.dealer_hands)
        print(self.round)

        if len(self.player_hands) > self.round:
            self.screen.blit(self.font.render("Player", True, 'white'), (100, 700))
            self.screen.blit(self.font.render(str(self.player_hands[self.round]), True, 'white'), (150, 750))
            self.screen.blit(self.font.render("Dealer", True, 'white'), (300, 700))
            self.screen.blit(self.font.render(str(self.dealer_hands[self.round]), True, 'white'), (300, 750))
 
        pygame.draw.rect(self.screen, 'white', self.retry_button)
        self.screen.blit(self.font.render("Try Again", True, 'black'), (210, 320))
        pygame.draw.rect(self.screen, 'white', self.menu_button)
        self.screen.blit(self.font.render("Back to Menu", True, 'black'), (160, 460))

        # Display the result of the game
        if self.game_result != 0:
            print("game result in restart is", self.game_result)
            if self.game_result == 1:
                outcome_text = self.font.render("You Lost!", True, 'red')  # Player loses
            elif self.game_result == 2:
                outcome_text = self.font.render("You Win!", True, 'green')  # Player wins
            elif self.game_result == 3:
                outcome_text = self.font.render("It's a Draw!", True, 'yellow')  # Draw
            self.screen.blit(outcome_text, (200, 150))  # Display message at the center

        pygame.display.update()
