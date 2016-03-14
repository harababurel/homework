"""
    Most long messages displayed by the UI will be found here.
"""
from util.Color import *

STRINGS = {
        'helpPrompt':
            'Commands:\n' +
            '\t%s   - displays this prompt.\n' % Color.strong('help') +
            '\t%s    - adds a new student or assignment.\n' % Color.strong('add') +
            '\t%s - removes an existing student or assignment.\n' % Color.strong('remove') +
            '\t%s   - displays all students or assignments.\n' % Color.strong('list') +
            '\t%s   - goes to previous state.\n' % Color.strong('undo') +
            '\t%s   - goes to next state.\n' % Color.strong('redo') +
            '\t%s  - displays statistics.\n' % Color.strong('stats') +
            '\t%s  - clears the screen.\n' % Color.strong('clear') +
            '\t%s   - saves the work session and exits the application.' % Color.strong('exit')
        }
