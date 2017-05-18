#!/bin/python3
from random import random, uniform
from math import sin, sqrt, exp, pi
from collections import deque
from random import choice

import matplotlib.pyplot as plt
import sys


"""
https://en.wikipedia.org/wiki/Particle_swarm_optimization

Let S be the number of particles in the swarm, each having a position xi ∈ ℝn in the search-space and a velocity vi ∈ ℝn. Let pi be the best known position of particle i and let g be the best known position of the entire swarm. A basic PSO algorithm is then:

for each particle i = 1, ..., S do
   Initialize the particle's position with a uniformly distributed random vector: xi ~ U(blo, bup)
   Initialize the particle's best known position to its initial position: pi ← xi
   if f(pi) < f(g) then
       update the swarm's best known  position: g ← pi
   Initialize the particle's velocity: vi ~ U(-|bup-blo|, |bup-blo|)
while a termination criterion is not met do:
   for each particle i = 1, ..., S do
      for each dimension d = 1, ..., n do
         Pick random numbers: rp, rg ~ U(0,1)
         Update the particle's velocity: vi,d ← ω vi,d + φp rp (pi,d-xi,d) + φg rg (gd-xi,d)
      Update the particle's position: xi ← xi + vi
      if f(xi) < f(pi) then
         Update the particle's best known position: pi ← xi
         if f(pi) < f(g) then
            Update the swarm's best known position: g ← pi
"""

class Problem:
    functions = {
            'McCormick': {
                'xMin': -1.5,
                'xMax': 4,
                'yMin': -3,
                'yMax': 4,
                'population': 200,
                'maxAttempts': 500,
                'omega': 0.01,
                'phiP': 0.06,
                'phiG': 0.03,
                'particleSize': 4,
                'f': lambda p: sin(p[0] + p[1]) + (p[0] - p[1])**2.0 - 1.5 * p[0] + 2.5 * p[1] + 1.0
                },

            'Cross-in-tray': {
                'xMin': -10,
                'xMax': 10,
                'yMin': -10,
                'yMax': 10,
                'population': 100,
                'maxAttempts': 10000,
                'omega': 0.01 * 0.5,
                'phiP': 0.05 * 0.5,
                'phiG': 0.05 * 0.5,
                'particleSize': 1,
                'f': lambda p: -0.0001 * (abs(sin(p[0]) * sin(p[1]) * exp(100 - sqrt(p[0]**2 + p[1]**2) / pi)) + 1) ** 0.1
                },

            }

    config = functions['McCormick']
    # config = functions['Cross-in-tray']

    xBound = abs(config['xMax'] - config['xMin'])
    yBound = abs(config['yMax'] - config['yMin'])

    clearBetweenIterations = False
    iterationsShown = 10


class Particle:
    def __init__(self, swarm=None):
        self.setRandomPosition()
        self.setRandomVelocity()

        self.bestPosition = self.position
        self.previousPositions = deque([self.position])
        self.swarm = swarm

    def __repr__(self):
        return "Particle(%.2f, %.2f)" % self.position

    def setRandomPosition(self):
        x = uniform(Problem.config['xMin'], Problem.config['xMax'])
        y = uniform(Problem.config['yMin'], Problem.config['yMax'])

        self.position = (x, y)

    def setRandomVelocity(self):
        dx = uniform(-Problem.xBound, Problem.xBound)
        dy = uniform(-Problem.yBound, Problem.yBound)

        self.velocity = (dx, dy)

    def updateVelocity(self, dimension, p, g):
        newComponent = Problem.config['omega'] * self.velocity[dimension] \
                     + Problem.config['phiP']* p * (self.bestPosition[dimension] - self.position[dimension]) \
                     + Problem.config['phiG']* g * (self.swarm.bestPosition[dimension] - self.position[dimension])

        if dimension == 0:
            self.velocity = (newComponent, self.velocity[1])
        else:
            self.velocity = (self.velocity[0], newComponent)

    def updatePosition(self):
        newX = max(min(self.position[0] + self.velocity[0], Problem.config['xMax']), Problem.config['xMin'])
        newY = max(min(self.position[1] + self.velocity[1], Problem.config['yMax']), Problem.config['yMin'])

        self.position = (newX, newY)

    def updateBestPosition(self):
        self.bestPosition = self.position

class Swarm:
    def __init__(self, population):
        self.particles = [Particle(swarm=self) for _ in range(population)]
        self.bestPosition = min([x.position for x in self.particles], key=lambda x: Problem.config['f'](x))

        self.currentlyPlotted = {
                'iterations': deque([]),
                'text': None
                }

    def clearOldPlottedIterations(self):
        iterations = self.currentlyPlotted['iterations']
        if len(iterations) > Problem.iterationsShown:
            iterations[0].remove()
            iterations.popleft()

    def plotCurrentIteration(self):
        # plt.axis([Problem.config['xMin'], Problem.config['xMax'], Problem.config['yMin'], Problem.config['yMax']])

        xs = list(map(lambda particle: particle[0], self.particles))
        ys = list(map(lambda particle: particle[1], self.particles))

        newIteration = plt.scatter(xs, ys, s=Problem.config['particleSize'])
        self.currentlyPlotted['iterations'].append(newIteration)


    def plotText(self):
        figureText = 'Best particle so far: (%.5f, %.5f)\n' % self.bestPosition + \
                     'Fitness: %.5f' % Problem.config['f'](self.bestPosition)

        self.currentlyPlotted['text'] = plt.text(
                Problem.config['xMin'] + 0.2,
                Problem.config['yMax'] + 0.2,
                figureText,
                withdash=True,
                bbox=dict(facecolor='blue', alpha=0.5),
                fontsize=10,
                family="Ubuntu Mono")

    def clearOldText(self):
        if self.currentlyPlotted['text'] is not None:
            self.currentlyPlotted['text'].remove()

    def plotEverything(self):
        if Problem.clearBetweenIterations:
            plt.clf()

        self.plotCurrentIteration()
        # self.clearOldIterations()
        # self.clearOldText()
        # self.plotText()
        plt.pause(0.0001)

    def simulate(self):
        for attempt in range(Problem.config['maxAttempts']):

            self.plotEverything()
            # plt.savefig("asdf", dpi=300)

            print("ATTEMPT #%i" % attempt, file=sys.stderr)

            for particle in self.particles:
                for dimension in range(2):
                    p, g = random(), random()
                    particle.updateVelocity(dimension, p, g)
                particle.updatePosition()

                if Problem.config['f'](particle.position) < Problem.config['f'](particle.bestPosition):
                    particle.updateBestPosition()

                    if Problem.config['f'](particle.bestPosition) < Problem.config['f'](self.bestPosition):
                        self.bestPosition = particle.bestPosition # (particle.bestPosition[0], particle.bestPosition[1])

            print("BEST POSITION SO FAR: (%.3f, %.3f)" % self.bestPosition)

def main():
    # plt.ion() # interactive plotting

    swarm = Swarm(Problem.config['population'])
    swarm.simulate()

    print("Found best position = (%.2f, %.2f) after %i iterations" % (swarm.bestPosition[0], swarm.bestPosition[1], Problem.config['maxAttempts']))


if __name__ == '__main__':
    main()
