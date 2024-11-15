import pygame

class Menu:
    def __init__(self, screen):
        self.screen = screen
        self.font = pygame.font.Font('freesansbold.ttf', 44)
        self.start_button = pygame.Rect(150, 250, 300, 100)
        self.settings_button = pygame.Rect(150, 400, 300, 100)  

    def draw(self):
        # Draw start and settings buttons
        self.screen.fill('black')
        title_text = self.font.render("Main Menu", True, 'white')
        self.screen.blit(title_text, (175, 150))

        pygame.draw.rect(self.screen, 'white', self.start_button)
        self.screen.blit(self.font.render("Start Game", True, 'black'), (175, 275))

        pygame.draw.rect(self.screen, 'white', self.settings_button)
        self.screen.blit(self.font.render("Settings", True, 'black'), (200, 425))
