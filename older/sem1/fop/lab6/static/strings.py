"""
    Most long messages displayed by the UI will be found here.
"""
from util.Color import *

STRINGS = {
        'helpPrompt':
            'Commands:\n' +
            '\t%s   - displays this prompt.\n' % Color.bold('help') +
            '\t%s    - adds a new student or assignment.\n' % Color.bold('add') +
            '\t%s - removes an existing student or assignment.\n' % Color.bold('remove') +
            '\t%s   - displays all students or assignments.\n' % Color.bold('list') +
            '\t%s   - goes to previous state.\n' % Color.bold('undo') +
            '\t%s   - goes to next state.\n' % Color.bold('redo') +
            '\t%s  - clears the screen.\n' % Color.bold('clear') +
            '\t%s   - saves the work session and exits the application.' % Color.bold('exit')
        }
