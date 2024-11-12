import pygame

import pygame

class Setting:
    def __init__(self, screen):
        self.screen = screen
        self.font = pygame.font.Font('freesansbold.ttf', 44)
        self.back_button = pygame.Rect(150, 500, 300, 100) 

    def draw(self):
        # Clear the screen and draw settings items
        self.screen.fill('black')
        title_text = self.font.render("Settings", True, 'white')
        self.screen.blit(title_text, (150, 150))

        # Draw the "Back to Menu" button
        pygame.draw.rect(self.screen, 'white', self.back_button)
        back_text = self.font.render("Back to Menu", True, 'black')
        self.screen.blit(back_text, (160, 525))

        pygame.display.update()
