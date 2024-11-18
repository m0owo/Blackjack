import pygame

class Setting:
    def __init__(self, screen):
        # Initialize screen
        self.screen = screen
        screen.fill('white')
        self.WIDTH = self.screen.get_width()
        self.HEIGHT = self.screen.get_height()
        self.CENTER_X = self.WIDTH // 2
        self.CENTER_Y = self.HEIGHT // 2

        # Fonts
        self.font = pygame.font.Font('freesansbold.ttf', 44)
        self.rubik_font = pygame.font.Font("fonts/RubikBubbles-Regular.ttf", 65)
        self.font = pygame.font.Font("fonts/Questrial-Regular.ttf", 45)
        self.small_font = pygame.font.Font("fonts/Questrial-Regular.ttf", 35)

        # Round settings
        self.rounds = 1
        self.minus_round_button = pygame.Rect(200, 250, 50, 50)
        self.add_round_button = pygame.Rect(550, 250, 50, 50)

        # Deck settings
        self.decks = 1
        self.minus_deck_button = pygame.Rect(200, 400, 50, 50)
        self.add_deck_button = pygame.Rect(550, 400, 50, 50)

        # Difficulty settings
        self.difficulty_levels = ["Easy", "Medium", "Hard"]
        self.current_difficulty_index = 0  
        self.left_difficulty_button = pygame.Rect(150, 325, 50, 50)
        self.right_difficulty_button = pygame.Rect(600, 325, 50, 50)
        
        # Back button
        self.back_button = pygame.Rect(self.CENTER_X - 150, 475, 300, 75)

    def get_rounds(self):
        return self.rounds
    
    def add_round(self):
        self.rounds += 1
    
    def minus_round(self):
        if self.rounds > 1:
            self.rounds -= 1

    def get_decks(self):
        return self.decks

    def add_deck(self):
        self.decks += 1
    
    def minus_deck(self):
        if self.decks > 1:
            self.decks -= 1
    
    def get_difficulty(self):
        return self.difficulty_levels[self.current_difficulty_index]

    def change_difficulty_left(self):
        if self.current_difficulty_index > 0:
            self.current_difficulty_index -= 1

    def change_difficulty_right(self):
        if self.current_difficulty_index < len(self.difficulty_levels) - 1:
            self.current_difficulty_index += 1

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
        # draw the gradient background
        self.draw_gradient_background()

        # create shadow for title
        shadow_color = (50, 50, 50)
        shadow_offset = (4, 4)
        shadow_text = self.rubik_font.render("Settings", True, shadow_color)
        shadow_rect = shadow_text.get_rect(center=(self.CENTER_X + shadow_offset[0], 150 + shadow_offset[1]))
        self.screen.blit(shadow_text, shadow_rect)

        # background box for settings
        pygame.draw.rect(self.screen, 'black', pygame.Rect(130, 225, 550, 350), border_radius=20)

        content_y = 250

        # rounds settings
        rounds_text = self.font.render("Rounds:", True, 'white')
        rounds_rect = rounds_text.get_rect(topleft=(300, content_y))
        self.screen.blit(rounds_text, rounds_rect)
        rounds_num_text = self.font.render(str(self.rounds), True, 'white')
        rounds_num_rect = rounds_num_text.get_rect(topleft=(500, content_y))
        self.screen.blit(rounds_num_text, rounds_num_rect)
        pygame.draw.rect(self.screen, 'white', self.minus_round_button, border_radius=15)
        pygame.draw.rect(self.screen, 'white', self.add_round_button, border_radius=15)
        minus_arrow_text = self.font.render("-", True, 'black')
        add_arrow_text = self.font.render("+", True, 'black')
        self.screen.blit(minus_arrow_text, minus_arrow_text.get_rect(topleft=(220, content_y)))
        self.screen.blit(add_arrow_text, add_arrow_text.get_rect(topleft=(560, content_y)))

        content_y += 75

        # difficulty settings
        difficulty_text = self.font.render("Difficulty:", True, 'white')
        difficulty_rect = difficulty_text.get_rect(topleft=(250, content_y))
        self.screen.blit(difficulty_text, difficulty_rect)
        current_difficulty = self.difficulty_levels[self.current_difficulty_index]
        difficulty_level_text = self.font.render(current_difficulty, True, 'white')
        difficulty_level_text_rect = difficulty_level_text.get_rect(topleft=(450, content_y))
        self.screen.blit(difficulty_level_text, difficulty_level_text_rect)
        pygame.draw.rect(self.screen, 'white', self.left_difficulty_button, border_radius=15)
        pygame.draw.rect(self.screen, 'white', self.right_difficulty_button, border_radius=15)
        left_diff_text = self.font.render("<", True, 'black')
        right_diff_text = self.font.render(">", True, 'black')
        self.screen.blit(left_diff_text, left_diff_text.get_rect(topleft=(150, content_y)))
        self.screen.blit(right_diff_text, right_diff_text.get_rect(topleft=(600, content_y)))

        content_y += 75

        # decks settings
        decks_text = self.font.render("Decks:", True, 'white')
        decks_rect = rounds_text.get_rect(topleft=(300, content_y))
        self.screen.blit(decks_text, decks_rect)
        decks_num_text = self.font.render(str(self.decks), True, 'white')
        decks_num_rect = decks_num_text.get_rect(topleft=(500, content_y))
        self.screen.blit(decks_num_text, decks_num_rect)
        pygame.draw.rect(self.screen, 'white', self.minus_deck_button, border_radius=15)
        pygame.draw.rect(self.screen, 'white', self.add_deck_button, border_radius=15)
        minus_arrow_text = self.font.render("-", True, 'black')
        add_arrow_text = self.font.render("+", True, 'black')
        self.screen.blit(minus_arrow_text, minus_arrow_text.get_rect(topleft=(220, content_y)))
        self.screen.blit(add_arrow_text, add_arrow_text.get_rect(topleft=(560, content_y)))

        content_y += 75

        # back to menu
        pygame.draw.rect(self.screen, 'white', self.back_button.inflate(5, 5), border_radius=15)
        pygame.draw.rect(self.screen, 'black', self.back_button, border_radius=15)
        back_text = self.small_font.render("Back to Menu", True, 'white')
        self.screen.blit(back_text, (300, content_y + 20))

        pygame.display.update()
