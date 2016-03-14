from repos.route import RouteRepository
from controllers.route import RouteController
from ui.console import Console
from models.route import *
from tests.test import Test

def main():
    try:
        Test().testEverything()
        print("All tests passed :).")
    except Exception as e:
        print("Some tests failed :(.")
        print(e)

    repo = RouteRepository()
    controller = RouteController(repo)
    app = Console(controller)

    app.run()

if __name__ == '__main__':
    main()
