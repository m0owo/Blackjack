import pygame

import pygame

class Setting:
    def __init__(self, screen):
        self.screen = screen
        self.font = pygame.font.Font('freesansbold.ttf', 44)
        self.rounds = 1
        self.decks = 1
        self.minus_round_button = pygame.Rect(250, 225, 50, 50)
        self.add_round_button = pygame.Rect(450, 225, 50, 50)
        self.minus_deck_button = pygame.Rect(250, 400, 50, 50)
        self.add_deck_button = pygame.Rect(450, 400, 50, 50)
        self.back_button = pygame.Rect(150, 500, 300, 100) 
    
    def get_rounds(self):
        print('getting set rounds', self.rounds)
        return self.rounds
    
    def add_round(self):
        self.rounds += 1
    
    def minus_round(self):
        if self.rounds > 1:
            self.rounds -= 1

    def get_decks(self):
        print('getting set decks', self.decks)
        return self.decks

    def add_deck(self):
        self.decks += 1
    
    def minus_deck(self):
        if self.decks > 1:
            self.decks -= 1

    def draw(self):
        # Clear the screen and draw settings items
        self.screen.fill('black')
        title_text = self.font.render("Settings", True, 'white')
        self.screen.blit(title_text, (200, 150))

        # Number of rounds
        self.screen.blit(self.font.render("rounds:", True, 'white'), (50, 225))
        pygame.draw.rect(self.screen, 'white', self.minus_round_button)
        self.screen.blit(self.font.render("-", True, 'black'), (275, 225))
        pygame.draw.rect(self.screen, 'white', self.add_round_button)
        self.screen.blit(self.font.render("+", True, 'black'), (450, 225))
        self.screen.blit(self.font.render(str(self.rounds), True, 'white'), (350, 225))

        # Number of decks
        self.screen.blit(self.font.render("decks:", True, 'white'), (50, 400))
        pygame.draw.rect(self.screen, 'white', self.minus_deck_button)
        self.screen.blit(self.font.render("-", True, 'black'), (275, 400))
        pygame.draw.rect(self.screen, 'white', self.add_deck_button)
        self.screen.blit(self.font.render("+", True, 'black'), (450, 400))
        self.screen.blit(self.font.render(str(self.decks), True, 'white'), (350, 400))

        # Draw the "Back to Menu" button
        pygame.draw.rect(self.screen, 'white', self.back_button)
        back_text = self.font.render("Back to Menu", True, 'black')
        self.screen.blit(back_text, (160, 525))

        pygame.display.update()

