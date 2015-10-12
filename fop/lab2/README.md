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
1             | Add into the list the result of a new participant.           | <ul><li><code>insert X</code></li><li><code>insert X Y</code></li></ul>
2             | Modify the scores from the list.                             | <ul><li><code>remove X</code></li><li><code>remove X Y</code></li><li><code>replace X Y</code></li></ul>
3             | Write the participants whose score has different properties. | <ul><li><code>less X</code></li><li><code>greater X</code></li><li><code>sorted</code></li><li><code>list</code></li></ul>
4             | Obtain different characteristics of participants.            | <ul><li><code>avg X Y</code></li><li><code>min X Y</code></li><li><code>mul X Y Z</code></li></ul>
5             | Filter scores.                                               | <ul><li><code>filter mul X</code></li><li><code>filter greater X</code></li></ul>
6             | Undo the last operation.                                     | <ul><li><code>undo</code></li></ul>
7             | Save state to disk and close program.                        | <ul><li><code>exit</code></li></ul>

## Iteration plan

Iteration no. | Planned features
:------------:|:----------------
1             | 1, 2, 7
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
                Please enter a command. Try <code>help</code>.<br>
            </td>
            <td>Program tries to restore previously saved data.<br>
                No such data exists, so a new session is created.
            </td>
        </tr>
        <tr>
            <td>2</td>
            <td><code>salut</code></td>
            <td></td>
            <td>User doesn't know what they're doing.</td>
        </tr>
        <tr>
            <td>3</td>
            <td></td>
            <td>Command not recognized. Try <code>help</code>.</td>
            <td>Program guides user in the right direction.</td>
        </tr>
        <tr>
            <td>4</td>
            <td><code>help</code></td>
            <td></td>
            <td></td>
        </tr>
        <tr>
            <td>5</td>
            <td></td>
            <td>These are the possible commands:<br>
                <ul>
                    <li><code>help</code> - displays this prompt</li>
                    <li><code>insert X</code> - adds a new participant with score X</li>
                    <li><code>insert X Y</code> - adds a new participant with score X at position Y</li>
                    <li><code>remove X</code> - removes participant at position X</li>
                    <li><code>remove X Y</code> - removes participants with positions between X and Y</li>
                    <li><code>replace X Y</code> - replaces the score of the participant at position X with the score Y</li>
                    <li><code>list</code> - shows all participants</li>
                    <li><code>exit</code> - saves the current state and closes the program</li>
                </ul>
            </td>
            <td>Program provides list of commands.</td>
        </tr>
        <tr>
            <td>6</td>
            <td><code>insert 100</code></td>
            <td></td>
            <td>User inserts participant with score 100.</td>
        </tr>
        <tr>
            <td>7</td>
            <td><code>insert 50</code></td>
            <td></td>
            <td>User inserts participant with score 50.</td>
        </tr>
        <tr>
            <td>8</td>
            <td><code>insert 42 1</code></td>
            <td></td>
            <td>User inserts participant with score 42 at the beginning.</td>
        </tr>
        <tr>
            <td>9</td>
            <td><code>list</code></td>
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
            <td><code>exit</code></td>
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
