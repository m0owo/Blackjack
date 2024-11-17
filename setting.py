import pygame

class Setting:
    def __init__(self, screen):
        self.screen = screen
        self.font = pygame.font.Font('freesansbold.ttf', 44)
        self.rounds = 1
        self.decks = 1
        self.difficulty_levels = ["Easy", "Medium", "Hard"]
        self.current_difficulty_index = 0  
        
        self.minus_round_button = pygame.Rect(250, 225, 50, 50)
        self.add_round_button = pygame.Rect(450, 225, 50, 50)
        self.minus_deck_button = pygame.Rect(250, 400, 50, 50)
        self.add_deck_button = pygame.Rect(450, 400, 50, 50)
        self.back_button = pygame.Rect(150, 500, 300, 100)
        
        self.left_difficulty_button = pygame.Rect(250, 300, 50, 50)
        self.right_difficulty_button = pygame.Rect(450, 300, 50, 50)
    
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
    
    def get_difficulty(self):
        print('getting difficulty', self.difficulty_levels[self.current_difficulty_index])
        return self.difficulty_levels[self.current_difficulty_index]

    def change_difficulty_left(self):
        if self.current_difficulty_index > 0:
            self.current_difficulty_index -= 1

    def change_difficulty_right(self):
        if self.current_difficulty_index < len(self.difficulty_levels) - 1:
            self.current_difficulty_index += 1

    def draw(self):
        # Clear the screen and draw settings items
        self.screen.fill('black')
        title_text = self.font.render("Settings", True, 'white')
        self.screen.blit(title_text, (200, 100))

        # Number of rounds
        self.screen.blit(self.font.render("Rounds:", True, 'white'), (50, 225))
        pygame.draw.rect(self.screen, 'white', self.minus_round_button)
        self.screen.blit(self.font.render("-", True, 'black'), (275, 225))
        pygame.draw.rect(self.screen, 'white', self.add_round_button)
        self.screen.blit(self.font.render("+", True, 'black'), (450, 225))
        self.screen.blit(self.font.render(str(self.rounds), True, 'white'), (350, 225))

        # Difficulty
        self.screen.blit(self.font.render("Difficulty:", True, 'white'), (50, 300))
        pygame.draw.rect(self.screen, 'white', self.left_difficulty_button)
        self.screen.blit(self.font.render("<", True, 'black'), (275, 300))
        pygame.draw.rect(self.screen, 'white', self.right_difficulty_button)
        self.screen.blit(self.font.render(">", True, 'black'), (450, 300))
        current_difficulty = self.difficulty_levels[self.current_difficulty_index]
        self.screen.blit(self.font.render(current_difficulty, True, 'white'), (330, 300))

        # Number of decks
        self.screen.blit(self.font.render("Decks:", True, 'white'), (50, 400))
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
