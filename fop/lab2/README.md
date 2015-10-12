# Contest
<hr>

## Problem statement
At a ***programming contest***, after evaluating the existing solutions, the evaluation committee has
recorded into a list the scores obtained by the participants (at position *i* into the list is the score of the
*i*-th participant). Knowing that the participants had to solve 10 problems, each problem evaluated
with maximum 10 points, write a program in order to help the committee to repeatedly execute the
following functionalities (each functionality is exemplified):

## Feature list

Feature no.   | Description                                                  | Commands
:------------:|:-------------------------------------------------------------|:----------------------------------------------------------------------
1             | Add into the list the result of a new participant.           | <ul><li>insert X</li><li>insert X Y</li></ul>
2             | Modify the scores from the list.                             | <ul><li>remove X</li><li>remove X Y</li><li>replace X Y</li></ul>
3             | Write the participants whose score has different properties. | <ul><li>less X</li><li>greater X</li><li>sorted</li><li>list</li></ul>
4             | Obtain different characteristics of participants.            | <ul><li>avg X Y</li><li>min X Y</li><li>mul X Y Z</li></ul>
5             | Filter scores.                                               | <ul><li>filter mul X</li><li>filter greater X</li></ul>
6             | Undo the last operation.                                     | <ul><li>undo</li></ul>

## Iteration plan

Iteration no. | Planned features
:------------:|:----------------
1             | 1, 2
2             | TBD
3             | TBD

## Running scenarios

### Iteration 1

<table>
    <thead>
        <tr>
            <th>Step</th>
            <th>User</th>
            <th>Program</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>1</td>
            <td></td>
            <td>Restoring previous session.<br>
                Could not restore session. Starting from scratch.<br>
                Saving new session to disk.<br>
                Session saved.<br>
                Please enter a command. Try "help".<br>
            </td>
            <td>Program tries to restore previously saved data.<br>
                No such data exists, so a new session is created.
            </td>
        </tr>
        <tr>
            <td>2</td>
            <td>`salut`</td>
            <td></td>
            <td>User doesn't know what they're doing.</td>
        </tr>
        <tr>
            <td>3</td>
            <td></td>
            <td>Command not recognized. Try "help".</td>
            <td>Program guides user in the right direction.</td>
        </tr>
        <tr>
            <td>4</td>
            <td>`help`</td>
            <td></td>
            <td></td>
        </tr>
        <tr>
            <td>5</td>
            <td></td>
            <td>These are the possible commands:<br>
                <ul>
                    <li>`help` - displays this prompt</li>
                    <li>`insert X` - adds a new participant with score X</li>
                    <li>`insert X Y` - adds a new participant with score X at position Y</li>
                    <li>`remove X` - removes participant at position X</li>
                    <li>`remove X Y` - removes participants with positions between X and Y</li>
                    <li>`replace X Y` - replaces the score of the participant at position X with the score Y</li>
                    <li>`list` - shows all participants</li>
                    <li>`exit` - saves the current state and closes the program</li>
                </ul>
            </td>
            <td>Program provides list of commands.</td>
        </tr>
        <tr>
            <td>6</td>
            <td>`insert 100`</td>
            <td></td>
            <td>User inserts participant with score 100.</td>
        </tr>
        <tr>
            <td>7</td>
            <td>`insert 50`</td>
            <td></td>
            <td>User inserts participant with score 50.</td>
        </tr>
        <tr>
            <td>8</td>
            <td>`insert 42 1`</td>
            <td></td>
            <td>User inserts participant with score 42 at the beginning.</td>
        </tr>
        <tr>
            <td>9</td>
            <td>`list`</td>
            <td></td>
            <td>User requests the list of participants.</td>
        </tr>
        <tr>
            <td>10</td>
            <td></td>
            <td>Participants:<br>
                <ul>
                    <li>1: 42</li>
                    <li>2: 100</li>
                    <li>3: 50</li>
                </ul>
            </td>
            <td>Program provides list of participants.</td>
        </tr>
        <tr>
            <td>11</td>
            <td>`exit`</td>
            <td></td>
            <td></td>
        </tr>
        <tr>
            <td>12</td>
            <td></td>
            <td>Saving new session to disk.<br>
                Session saved.<br>
                Exiting...
            </td>
            <td></td>
        </tr>
    </tbody>
</table>
