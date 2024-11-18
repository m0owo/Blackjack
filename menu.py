import pygame  

class Menu:     
    def __init__(self, screen):         
        self.screen = screen         
        self.WIDTH = self.screen.get_width()
        self.HEIGHT = self.screen.get_height()
        self.CENTER_X = self.WIDTH // 2
        self.font = pygame.font.SysFont("questrialregular", 45)
        self.rubik_font = pygame.font.SysFont("rubikbubblesregular", 65)
        self.start_button = pygame.Rect(150, 250, 300, 100)         
        self.settings_button = pygame.Rect(150, 400, 300, 100)        

    def draw_gradient_background(self):
        center_x = self.WIDTH // 2
        center_y = self.HEIGHT // 2
        max_radius = max(self.WIDTH, self.HEIGHT) // 2

        for radius in range(max_radius, 0, -1):
            t = radius / max_radius
            r = int((1 - t) * 128 + t * 255)
            g = int((1 - t) * 0 + t * 255)    
            b = int((1 - t) * 128 + t * 255)  

            color = (r, g, b)
            pygame.draw.circle(self.screen, color, (center_x, center_y), radius)

    def draw(self):         
        self.draw_gradient_background()

        logo = pygame.image.load('images/blackjack_logo.png') 
        logo = pygame.transform.scale(logo, (80, 80))  
        
        rect_width = 400  
        rect_height = 400  
        rect_x = (self.WIDTH - rect_width) // 2
        rect_y = (self.HEIGHT - rect_height) // 2

        pygame.draw.rect(self.screen, 'black', pygame.Rect(rect_x, rect_y, rect_width, rect_height), border_radius=20)
        center_x = rect_x + rect_width // 2
        
        title_text = self.rubik_font.render("Blackjack 21", True, 'black')
 
        # Position logo first
        logo_x = self.CENTER_X - (title_text.get_width() + logo.get_width() + 10) // 2
        logo_y = rect_y - 75 - logo.get_height() // 2
        
    
        title_x = logo_x + logo.get_width() + 10
        title_y = rect_y - 75
        title_rect = title_text.get_rect(topleft=(title_x, title_y))
        
        # Blit logo and title
        self.screen.blit(logo, (logo_x, logo_y))
        self.screen.blit(title_text, title_rect)


        # Subtitle inside the black rectangle
        subtitle_font = pygame.font.SysFont("questrialregular", 50)
        subtitle_text = subtitle_font.render("Main Menu", True, 'white')
        subtitle_rect = subtitle_text.get_rect(center=(center_x, rect_y + 50))
        self.screen.blit(subtitle_text, subtitle_rect)

        # Adjust button positions with smaller size
        self.start_button = pygame.Rect(center_x - 125, rect_y + 125, 250, 80) 
        self.settings_button = pygame.Rect(center_x - 125, rect_y + 250, 250, 80)  

        # Start button
        pygame.draw.rect(self.screen, 'white', self.start_button, border_radius=15)
        start_text = self.font.render("Start Game", True, 'black')
        start_rect = start_text.get_rect(center=self.start_button.center)
        self.screen.blit(start_text, start_rect)

        # Settings button
        pygame.draw.rect(self.screen, 'white', self.settings_button, border_radius=15)
        settings_text = self.font.render("Settings", True, 'black')
        settings_rect = settings_text.get_rect(center=self.settings_button.center)
        self.screen.blit(settings_text, settings_rect)

        pygame.display.update()