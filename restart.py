import pygame

PLAYER_BUST = 1
PLAYER_WIN = 2
DEALER_WIN = 3
TIE = 4

class Restart:
    def __init__(self, screen):
        self.screen = screen
        screen.fill('white')
        self.WIDTH = self.screen.get_width()
        self.HEIGHT = self.screen.get_height()
        self.CENTER_X = self.WIDTH // 2
        self.CENTER_Y = self.HEIGHT // 2
        self.rubik_font = pygame.font.SysFont("rubikbubblesregular", 65)
        self.font = pygame.font.SysFont("questrialregular", 45)
        self.small_font = pygame.font.SysFont("questrialregular", 35)
        self.game_result = None  # Store game result
        self.round = 0
        self.player_hands = [0] * self.round
        self.dealer_hands = [0] * self.round
        self.retry_button = pygame.Rect(130, 650, 250, 80)
        self.menu_button = pygame.Rect(430, 650, 250, 80)
        self.prev_round_button = pygame.Rect(150, 425, 50, 50)
        self.next_round_button = pygame.Rect(600, 425, 50, 50)
    
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
            
    def draw_gradient_background(self):
        # Draw a radial gradient background
        center_x = self.WIDTH // 2
        center_y = self.HEIGHT // 2
        max_radius = max(self.WIDTH, self.HEIGHT) // 2

        for radius in range(max_radius, 0, -1):
            # Calculate the color for the current circle
            t = radius / max_radius
            r = int((1 - t) * 128 + t * 255)  # Blend purple (128) and white (255)
            g = int((1 - t) * 0 + t * 255)    # Blend purple (0) and white (255)
            b = int((1 - t) * 128 + t * 255)  # Blend purple (128) and white (255)

            color = (r, g, b)
            pygame.draw.circle(self.screen, color, (center_x, center_y), radius)
            
    def draw(self):
        self.draw_gradient_background()
        RED = (236, 30, 30)
        GREEN = (67, 223, 103)
        YELLOW = (240, 231, 104)
        
        # Display the result of the game
        if self.game_result != 0:
            if self.game_result == 1:
                text = "You Lost!"
                text_color = RED
            elif self.game_result == 2:
                text = "You Win!"
                text_color = GREEN
            elif self.game_result == 3:
                text = "It's a Draw!"
                text_color = YELLOW

            # Create shadow
            shadow_color = (50, 50, 50)  # Dark gray shadow
            shadow_offset = (4, 4)       # Offset for shadow
            shadow_text = self.rubik_font.render(text, True, shadow_color)
            shadow_rect = shadow_text.get_rect(center=(self.CENTER_X + shadow_offset[0], 150 + shadow_offset[1]))
            self.screen.blit(shadow_text, shadow_rect)
            
            # Create main text
            outcome_text = self.rubik_font.render(text, True, text_color)
            outcom_rect = outcome_text.get_rect(center=(self.CENTER_X, 150))
            self.screen.blit(outcome_text, outcom_rect)
            
        #Background
        pygame.draw.rect(self.screen, 'black', pygame.Rect(130, 225, 550, 350), border_radius=20)
        
        # Record of the result
        result_text = self.rubik_font.render("Results", True, 'white')
        result_rect = result_text.get_rect(center=(self.CENTER_X, 275))
        round_text = self.font.render("Round " + str(self.round + 1), True, 'white')
        round_rect = round_text.get_rect(center=(self.CENTER_X, 350))

        # Player/Dealer texts
        player_text = self.font.render("Player", True, 'white')
        player_rect = player_text.get_rect(center=(self.CENTER_X - 100, 420))
        dealer_text = self.font.render("Dealer", True, 'white')
        dealer_rect = dealer_text.get_rect(center=(self.CENTER_X + 100, 420))
        self.screen.blit(result_text, result_rect)
        self.screen.blit(round_text, round_rect)
        self.screen.blit(player_text, player_rect)
        self.screen.blit(dealer_text, dealer_rect)

        # Left and right arrow button
        pygame.draw.rect(self.screen, 'white', self.prev_round_button, border_radius=15)
        pygame.draw.rect(self.screen, 'white', self.next_round_button, border_radius=15)
        left_arrow_text = self.font.render("<", True, 'black')
        right_arrow_text = self.font.render(">", True, 'black')
        self.screen.blit(left_arrow_text, left_arrow_text.get_rect(center=self.prev_round_button.center))
        self.screen.blit(right_arrow_text, right_arrow_text.get_rect(center=self.next_round_button.center))


        # Player and dealer values
        player_value_width = self.font.size(str(self.player_hands[self.round]))[0]
        dealer_value_width = self.font.size(str(self.dealer_hands[self.round]))[0]
        total_value_width = player_value_width + dealer_value_width + 40  
        player_value_x = self.CENTER_X - (total_value_width // 2)  
        dealer_value_x = player_value_x + player_value_width + 40  
        if len(self.player_hands) > self.round:
            self.screen.blit(self.font.render(str(self.player_hands[self.round]), True, 'white'), (player_value_x - 50, 450))
            self.screen.blit(self.font.render(str(self.dealer_hands[self.round]), True, 'white'), (dealer_value_x + 50, 450))
        
        # Draw the Try again and Back to menu buttons
        pygame.draw.rect(self.screen, 'white', self.retry_button, border_radius=15)
        pygame.draw.rect(self.screen, 'white', self.menu_button, border_radius=15)
        retry_text = self.small_font.render("Try Again", True, 'black')
        menu_text = self.small_font.render("Back to Menu", True, 'black')
        self.screen.blit(retry_text, retry_text.get_rect(center=self.retry_button.center))
        self.screen.blit(menu_text, menu_text.get_rect(center=self.menu_button.center))

        pygame.display.update()