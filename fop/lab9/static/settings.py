"""
    The configuration of the application lies here.
    (list of known commands, number of arguments
    required for each on, and so on)
"""
SETTINGS = {
        'validCommands': [
            'help',
            'add',
            'remove',
            'list',
            'undo',
            'redo',
            'stats',
            'clear',
            'exit'
            ],
        'neededArgs': {
            'help':   [0],
            'add':    [0],
            'remove': [0],
            'list':   [0],
            'undo':   [0],
            'redo':   [0],
            'stats':  [0],
            'clear':  [0],
            'exit':   [0]
            }
        }
