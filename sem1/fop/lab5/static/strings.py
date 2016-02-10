"""
    Most long messages displayed by the UI will be found here.
"""
from util.Color import bold

STRINGS = {
        'helpPrompt':
            'Commands:\n' +
            '\t%s  - displays this prompt.\n' % bold('help') +
            '\t%s   - adds a new student or assignment.\n' % bold('add') +
            '\t%s  - displays all students or assignments.\n' % bold('list') +
            '\t%s  - goes to previous state.\n' % bold('undo') +
            '\t%s  - goes to next state.\n' % bold('redo') +
            '\t%s - clears the screen.\n' % bold('clear') +
            '\t%s  - saves the work session and exits the application.' % bold('exit')
        }
