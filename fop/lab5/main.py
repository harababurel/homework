"""
    The brains of the application.
"""

from ui.FacultyApplication import FacultyApplication
from models import *

def main():
    app = FacultyApplication()
    app.run()

if __name__ == '__main__':
    main()
