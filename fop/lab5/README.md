# Student lab assignments
<hr>

## Problem statement
Write an application that manages lab assignments for students at a given discipline. The application
will store:

* Students:
  * <*studentID*>
  * <*name*>
  * <*group*>

* Assignments:
  * <*studentID*>
  * <*description*>
  * <*deadline*>
  * <*grade*>

Create an application that allows to:
* Manage a list of students and a list of assignments.
* Add, remove, update, list students and assignments
* Search for a student based on his/her ID.
* Create statistics: list of students and grades at a single assignment ordered: alphabetically, by their grades as well as all students with the average grade lower than 5.
* Unlimited undo/redo functionality. Each step will undo/redo the previous operation that modified the data structure.

## Feature list

Feature no.   | Description                                                  | Commands
:------------:|:-------------------------------------------------------------|:--------
1             | Add a student or an assignment.                              | <code>add</code>
2             | List all students and assignments.                           | <code>list</code>
3             | Go back to the previous application state.                   | <code>undo</code>
4             | Go forward to the next application state.                    | <code>redo</code>
5             | Clear the application screen.                                | <code>clear</code>
6             | Save state to disk and close the application.                | <code>exit</code>

## Iteration plan

Iteration no. | Planned features
:------------:|:----------------
1             | 1-6

## Running scenario

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
            <td>All tests passed :).<br>
                Restoring previous session.<br>
                No saved session exists.<br>
                Starting a new session.<br>
                New session saved :).
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
            <td>salut: command not found. Try '<b>help</b>'.</td>
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
            <td>Commands:<br>
                <ul>
                    <li><b>help</b>  - displays this prompt.</li>
                    <li><b>add</b>   - adds a new student or assignment.</li>
                    <li><b>list</b>  - displays all students or assignments.</li>
                    <li><b>undo</b>  - goes to previous state.</li>
                    <li><b>redo</b>  - goes to next state.</li>
                    <li><b>clear</b> - clears the screen.</li>
                    <li><b>exit</b>  - saves the work session and exits the application.</li>
                </ul>
            </td>
            <td>Program provides list of commands.</td>
        </tr>
        <tr>
            <td>6</td>
            <td><code>exit</code></td>
            <td></td>
            <td></td>
        </tr>
        <tr>
            <td>7</td>
            <td></td>
            <td>Session saved :).<br>
                Exiting.
            </td>
            <td></td>
        </tr>
    </tbody>
</table>
