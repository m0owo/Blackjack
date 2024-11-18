import pygame
from gamev2 import Game
from menu import Menu
from restart import Restart
from setting import Setting

pygame.init()
screen = pygame.display.set_mode((800, 900))
pygame.display.set_caption("Blackjack Game")

menu = Menu(screen)
restart = Restart(screen)
setting = Setting(screen)
game = Game(screen, setting.get_rounds(), setting.get_decks(), setting.get_difficulty())

clock = pygame.time.Clock()

running = True
setting_ = False
in_game = False
game_over = False

while running:
    # clear the screen 
    screen.fill('black')

    # determine which page
    if not in_game and not setting_:
        # main menu
        menu.draw()
    elif in_game and not game_over:
        # gameplay
        game.draw()
    elif game_over:
        # results to display on restart screen 
        restart.set_result(game.game_result)
        restart.set_player_hands(game.player_hands)
        restart.set_dealer_hands(game.dealer_hands)
        restart.draw()
    elif setting_:
        # settings page
        setting.draw()

    # mouse clicks
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

        # main menu mouse clicks
        if not in_game and not setting_:
            if event.type == pygame.MOUSEBUTTONUP:
                # click start game
                if menu.start_button.collidepoint(event.pos):
                    game.set_rounds(setting.get_rounds())
                    game.set_decks(setting.get_decks())
                    game.set_difficulty(setting.get_difficulty())
                    in_game = True
                    game.reset_game()
                # click settings
                elif menu.settings_button.collidepoint(event.pos): 
                    setting_ = True  
        # game play mouse clicks
        elif in_game and not game_over:
            # game has run its course
            if game.handle_event(event):
                restart.set_result(game.game_result)
                game_over = True 
        elif game_over:
            if event.type == pygame.MOUSEBUTTONUP:
                # click retry
                if restart.retry_button.collidepoint(event.pos):
                    print("Retry button clicked")
                    game_over = False
                    in_game = True
                    print("Setting rounds and decks")
                    game.set_rounds(setting.get_rounds())
                    game.set_decks(setting.get_decks())
                    game.set_difficulty(setting.get_difficulty())
                    print("Resetting game")
                    game.reset_game()
                    print("Game reset complete")

                # click menu
                elif restart.menu_button.collidepoint(event.pos):
                    game_over = False
                    in_game = False
                    
                # click add round
                elif restart.next_round_button.collidepoint(event.pos):
                    restart.next_round()
                elif restart.prev_round_button.collidepoint(event.pos):
                    restart.prev_round()
        # settings clicks
        elif setting_:
            if event.type == pygame.MOUSEBUTTONUP:
                # go back to menu and implement settings
                if setting.back_button.collidepoint(event.pos):
                    setting_ = False 
                    game.set_rounds(setting.get_rounds())
                    game.set_decks(setting.get_decks())
                    game.set_difficulty(setting.get_difficulty())
                # increase/decrease decks/rounds
                if setting.add_deck_button.collidepoint(event.pos):
                    setting.add_deck()
                if setting.minus_deck_button.collidepoint(event.pos):
                    setting.minus_deck()
                if setting.add_round_button.collidepoint(event.pos):
                    setting.add_round()
                if setting.minus_round_button.collidepoint(event.pos):
                    setting.minus_round()
                if setting.left_difficulty_button.collidepoint(event.pos):
                    setting.change_difficulty_left()
                if setting.right_difficulty_button.collidepoint(event.pos):
                    setting.change_difficulty_right()

    pygame.display.update()
    clock.tick(60)



