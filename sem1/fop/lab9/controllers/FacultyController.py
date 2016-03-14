"""
    The FacultyController class implements methods for
    managing the students and assignments of a faculty.
"""
from repo.FacultyRepository import FacultyRepository
from util.Color import *


class FacultyController:
    def __init__(self, repository):
        """
            The controller initializes with a brand new repository.
            If, however, it is able to restore an older repository
            that was saved to disk, it does so.
        """
        self.repository = repository

        if self.repository.persistent:
            # print("Restoring previous session.")
            try:
                self.repository.restoreSession()
                #print("Previous session restored :).")
            except IOError:
                #print("No saved session exists.")
                #print("Starting a new session.")
                try:
                    self.repository.saveSession()
                    #print("New session saved :).")
                except IOError:
                    #print("Could not save new session.", end='')
                    #print("Your work will be lost once you close the application :(.")

    def getCurrentFaculty(self):
        """
            Method returns the faculty corresponding to the
            present time.
        """
        return self.repository.states[self.repository.now]

    def getCurrentStudents(self):
        """
            Method returns the current faculty's students.
        """
        return self.getCurrentFaculty().students

    def getCurrentAssignments(self):
        """
            Method returns the current faculty's assignments.
        """
        return self.getCurrentFaculty().assignments

    def getCurrentStudentCount(self):
        """
            Method returns the number of students in the current
            faculty.
        """
        return len(self.getCurrentStudents())

    def getCurrentAssignmentCount(self):
        """
            Method returns the number of assignments in the current
            faculty.
        """
        return len(self.getCurrentAssignments())

    def studentIDExists(self, studentID):
        """
            Method checks whether a student with a specific studentID
            exists or not.
        """
        return studentID in [x.studentID for x in self.getCurrentStudents()]

    def addStudent(self, who):
        """
            Method adds a new student to the faculty.
            If the student already exists (someone with the same studentID),
            then nothing happens.
        """
        if self.studentIDExists(who.studentID):
            return "Student #%i already exists." % who.studentID
        else:
            self.repository.prepare()
            self.repository.states[self.repository.now].addStudent(who)
            return "Added student."

    def addAssignment(self, what):
        """
            Method adds a new assignment to the faculty.
        """
        self.repository.prepare()
        self.getCurrentFaculty().addAssignment(what)

    def removeStudent(self, studentID):
        """
            Method removes an existing student (identified by
            their studentID) from the faculty.
        """
        self.repository.prepare()
        self.getCurrentFaculty().removeStudent(studentID)

    def removeAssignment(self, assignmentID):
        """
            Method removes an existing assignment (identified
            by its position in the list of assignments) from
            the faculty.
        """
        self.repository.prepare()
        self.getCurrentFaculty().removeAssignment(assignmentID)

    def undo(self):
        """
            Method goes back in time one step.
        """
        if self.repository.now == 0:
            print("Already at oldest state.")
        else:
            self.repository.now -= 1

    def redo(self):
        """
            Method goes forward in time one step.
        """
        if self.repository.now + 1 == len(self.repository.states):
            print("Already at most recent state.")
        else:
            self.repository.now += 1

    def exitApplication(self):
        """
            Method saves current session and then exits
            with an exit-code of 0.
        """

        if self.repository.persistent:
            try:
                self.repository.saveSession()
                print("Session saved :).")
            except IOError:
                print("Could not save session. Your work will be lost :(.")

        print("Exiting.")
        exit(0)

    def getStudentNameFromID(self, studentID):
        students = self.getCurrentStudents()

        targetStudent = [x for x in students if x.getStudentID() == studentID][0]
        return targetStudent.getStudentName()

    def getStatistics(self, assignmentID):
        """
            Method returns a human-readable string that contains
            some nice faculty statistics for a certain assignment.
        """
        assignments = self.getCurrentAssignments()
        targetAssignment = assignments[assignmentID]

        assignments = [x for x in assignments if x.getDescription() == targetAssignment.getDescription()]

        alphabetically = '\n\t'.join([self.getStudentNameFromID(x.getStudentID()) for x in sorted(assignments, key=lambda x: self.getStudentNameFromID(x.getStudentID()))])
        byGrade = '\n\t'.join(["%s - %.2f" % (self.getStudentNameFromID(x.getStudentID()), x.getGrade()) for x in sorted(assignments, key=lambda x: x.getGrade(), reverse=True)])
        corigenti = '\n\t'.join(["%s - %.2f" % (self.getStudentNameFromID(x.getStudentID()), x.getGrade()) for x in sorted(assignments, key=lambda x: x.getStudentID()) if x.getGrade() < 5.0])

        out = 'Students with this assignment:\n\n'
        out += '%s:\n\t%s\n\n' % (Color.strong('Alphabetically'), alphabetically)
        out += '%s:\n\t%s\n\n' % (Color.strong('by Grade (desc.)'), byGrade)
        out += '%s:\n\t%s\n\n' % (Color.strong('Corigenti'), corigenti)

        return out
