import pygame
class Restart:
    def __init__(self, screen):
        self.screen = screen
        self.font = pygame.font.Font('freesansbold.ttf', 44)
        self.retry_button = pygame.Rect(150, 300, 300, 100)
        self.menu_button = pygame.Rect(150, 440, 300, 100)
        self.game_result = None  # Store game result
    
    def set_result(self, result):
        """Set the result to display on the restart screen."""
        self.game_result = result

    def draw(self):
        # Restart Screen
        self.screen.fill('black')
        pygame.draw.rect(self.screen, 'white', self.retry_button)
        self.screen.blit(self.font.render("Try Again", True, 'black'), (210, 320))
        pygame.draw.rect(self.screen, 'white', self.menu_button)
        self.screen.blit(self.font.render("Back to Menu", True, 'black'), (160, 460))

        # Display the result of the game
        if self.game_result is not None:
            if self.game_result == 1:
                outcome_text = self.font.render("You Lost!", True, 'red')  # Player loses
            elif self.game_result == 2:
                outcome_text = self.font.render("You Win!", True, 'green')  # Player wins
            elif self.game_result == 3:
                outcome_text = self.font.render("Dealer Wins!", True, 'red')  # Dealer wins
            elif self.game_result == 4:
                outcome_text = self.font.render("It's a Draw!", True, 'yellow')  # Draw
            self.screen.blit(outcome_text, (200, 150))  # Display message at the center

        pygame.display.update()
