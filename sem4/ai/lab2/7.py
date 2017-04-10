#!/usr/bin/python3

# 7. The missionaries and cannibals problem – solving techniques: DFS, GBFS
# Three missionaries and three cannibals are on the left side of a river. They want to cross
# over the right side with the help of a boat that can only transport two persons per trip. The
# number of the cannibals can not be higher than the number of missionaries on one side
# because the missionaries will be eaten by the cannibals. Find a solution to cross all of them on
# the other side.
from collections import defaultdict, deque
from random import shuffle

class State:
    def __init__(self, missionariesL=3, missionariesR=0, cannibalsL=3, cannibalsR=0, boat='L'):
        self.missionariesL = missionariesL
        self.missionariesR = missionariesR
        self.cannibalsL = cannibalsL
        self.cannibalsR = cannibalsR
        self.boat = boat

    def __repr__(self):
        boatRepr = ('⛵%s' if self.boat == 'L' else '%s⛵') % (11 * ' ')
        ret = "%s%s%s%s%s" % (self.missionariesL * 'M', \
                self.cannibalsL * 'C', \
                boatRepr, \
                self.cannibalsR * 'C', \
                self.missionariesR * 'M')
        return ret

    def __str__(self):
        return self.__repr__()

    def __eq__(self, other):
        return self.missionariesL == other.missionariesL and \
                self.missionariesR == other.missionariesR and \
                self.cannibalsL == other.cannibalsL and \
                self.cannibalsR == other.cannibalsR and \
                self.boat == other.boat

    def __lt__(self, other):
        selfScore = self.missionariesR + self.cannibalsR
        otherScore = other.missionariesR + other.cannibalsR
        return selfScore <= otherScore



    def isValid(self):
        valid = True

        valid &= self.missionariesL + self.missionariesR > 0

        valid &= self.missionariesL >= 0
        valid &= self.missionariesR >= 0
        valid &= self.cannibalsL >= 0
        valid &= self.cannibalsR >= 0
        if self.missionariesL > 0:
            valid &= self.missionariesL >= self.cannibalsL
        if self.missionariesR > 0:
            valid &= self.missionariesR >= self.cannibalsR

        return valid

    def expand(self):
        newStates = []

        for cannibalsTaken in [0, 1, 2]:
            for missionariesTaken in [0, 1, 2]:
                if cannibalsTaken + missionariesTaken > 2:
                    continue

                if self.boat == 'L':
                    newState = State(self.missionariesL-missionariesTaken, \
                                     self.missionariesR+missionariesTaken, \
                                     self.cannibalsL-cannibalsTaken, \
                                     self.cannibalsR+cannibalsTaken, \
                                     'R')
                else:
                    newState = State(self.missionariesL+missionariesTaken, \
                                     self.missionariesR-missionariesTaken, \
                                     self.cannibalsL+cannibalsTaken, \
                                     self.cannibalsR-cannibalsTaken, \
                                     'L')

                if newState.isValid():
                    newStates.append(newState)
        return newStates

class Problem:
    def __init__(self, n=3):
        self.initialState = State(n, 0, n, 0, 'L')
        self.finalState = State(0, n, 0, n, 'R')

    def dfs(self):
        stack = [self.initialState]
        processedStates = {}
        prev = {str(self.initialState): None}

        while stack is not []:
            currentState = stack.pop()

            if str(currentState) in processedStates:
                continue
            processedStates[str(currentState)] = True

            if currentState == self.finalState:
                return '\n'.join(self.reconstructPath(prev))

            newStates = currentState.expand()
            # newStates = sorted(newStates)
            # shuffle(newStates)

            for newState in newStates:
                if newState not in stack and str(newState) not in processedStates:
                    stack.append(newState)
                    prev[str(newState)] = str(currentState)

    def gbfs(self):
        queue = deque([self.initialState])
        processedStates = {}
        prev = {str(self.initialState): None}

        while len(queue) > 0:
            currentState = queue.popleft()

            if str(currentState) in processedStates:
                continue
            processedStates[str(currentState)] = True

            if currentState == self.finalState:
                return '\n'.join(self.reconstructPath(prev))

            newStates = currentState.expand()
            newStates = sorted(newStates)
            # shuffle(newStates)

            for newState in newStates:
                if newState not in queue and str(newState) not in processedStates:
                    queue.append(newState)
                    prev[str(newState)] = str(currentState)


    def reconstructPath(self, prev):
        currentState = str(self.finalState)
        path = [currentState]

        while currentState != str(self.initialState):
            currentState = prev[currentState]
            path.append(currentState)

        path = path[::-1]
        return path

class UI:
    def __init__(self):
        self.problem = None

    def run(self):
        print("RUNNING")

        while True:
            try:
                n = int(input("N = "))
                assert(n > 0)
                self.problem = Problem(n)
                break
            except:
                print("N must be a positive integer")


        while True:
            print("1. DFS")
            print("2. GBFS")

            command = input()

            if command == '1':
                print(self.problem.dfs())
                break
            elif command == '2':
                print(self.problem.gbfs())
                break

def main():
    app = UI()
    app.run()

if __name__ == '__main__':
    main()
