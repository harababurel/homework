from repos.route import RouteRepository
from controllers.route import RouteController
from ui.console import Console
from models.route import *

def main():
    repo = RouteRepository()
    controller = RouteController(repo)
    app = Console(controller)

    app.run()

if __name__ == '__main__':
    main()
