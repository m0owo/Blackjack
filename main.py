import pygame
from gamev2 import Game
from menu import Menu
from restart import Restart
from setting import Setting

pygame.init()
screen = pygame.display.set_mode((600, 900))
pygame.display.set_caption("Blackjack Game")

menu = Menu(screen)
restart = Restart(screen)
game = Game(screen)
setting = Setting(screen)

clock = pygame.time.Clock()

running = True
setting_ = False
in_game = False
game_over = False

while running:
    screen.fill('black')  # Clear the screen

    # Determine which page to display based on flags
    if not in_game and not setting_:
        menu.draw()
    elif in_game and not game_over:
        game.draw()
    elif game_over:
        restart.set_result(game.game_result)
        restart.draw()
    elif setting_:
        setting.draw()

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

        # Handle menu navigation and mouse clicks
        if not in_game and not setting_:
            if event.type == pygame.MOUSEBUTTONUP:
                if menu.start_button.collidepoint(event.pos):
                    in_game = True
                    game.reset_game()
                elif menu.settings_button.collidepoint(event.pos): 
                    setting_ = True  

        elif in_game and not game_over:
            if game.handle_event(event):  # Handle game events
                game_over = True  # Game over state
        
        elif game_over:
            if event.type == pygame.MOUSEBUTTONUP:
                if restart.retry_button.collidepoint(event.pos):
                    game.reset_game()
                    game_over = False
                elif restart.menu_button.collidepoint(event.pos):
                    in_game = False
                    game_over = False

        elif setting_:
            if event.type == pygame.MOUSEBUTTONUP:
                if setting.back_button.collidepoint(event.pos):
                    setting_ = False  # Go back to menu

    pygame.display.update()
    clock.tick(60)



