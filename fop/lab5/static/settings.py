"""
    The configuration of the application lies here.
    (list of known commands, number of arguments
    required for each on, and so on)
"""
SETTINGS = {
        'validCommands': [
            'help',
            'clear',
            'exit'
            ],
        'neededArgs': {
            'help': [0],
            'clear': [0],
            'exit': [0]
            }
        }
