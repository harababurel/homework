#!/bin/python3
from math import inf
from pprint import pprint
from collections import defaultdict

import matplotlib.pyplot as plt
import numpy as np

# Design and implement a control module to adjust the washing cycle for a washing machine. The
# wash cycle (delicate, easy, normal, intense) depends on the texture of clothes (very soft, soft, normal,
# resistant) and the amount of clothes loaded in the car (small, medium, high) - see Figure 7, Figure 8,
# Figure 9 and Table 3.

texture_table = {
    'very_soft': [(-inf, 1), (0.2, 1), (0.4, 0), (inf, 0)],
    'soft': [(-inf, 0), (0.2, 0), (0.4, 1), (0.8, 0), (inf, 0)],
    'normal': [(-inf, 0), (0.3, 0), (0.7, 1), (0.9, 0), (inf, 0)],
    'resistant': [(-inf, 0), (0.7, 0), (0.9, 1), (inf, 1)]
}

capacity_table = {
    'small': [(-inf, 1), (1, 1), (2, 0), (inf, 0)],
    'medium': [(-inf, 0), (1, 0), (2.5, 1), (4, 0), (inf, 0)],
    'high': [(-inf, 0), (3, 0), (4, 1), (inf, 1)]
}

decision_table = {
    'very_soft': {
        'small': 'delicate',
        'medium': 'easy',
        'high': 'normal'
    },
    'soft': {
        'small': 'easy',
        'medium': 'normal',
        'high': 'normal'
    },
    'normal': {
        'small': 'easy',
        'medium': 'normal',
        'high': 'intense'
    },
    'resistant': {
        'small': 'easy',
        'medium': 'normal',
        'high': 'intense'
    }
}

decision_colors = {
    'delicate': 'blue',
    'easy': 'purple',
    'normal': 'green',
    'intense': 'red'
}


def get_membership(x, table):
    membership = {}
    for cluster, points in table.items():
        try:
            membership[cluster] = [point[1]
                                   for point in points if point[0] == x][0]
        except:
            for i, point in enumerate(points):
                next_point = points[i + 1]

                if point[0] <= x and x <= next_point[0]:
                    if next_point[1] == point[1]:
                        delta, ratio = 0, 0
                    else:
                        delta = next_point[1] - point[1]
                        ratio = (x - point[0]) / (next_point[0] - point[0])

                    membership[cluster] = point[1] + delta * ratio
                    break

    return membership


def solve(texture, capacity):
    texture_membership = get_membership(texture, texture_table)
    capacity_membership = get_membership(capacity, capacity_table)

    decision_weight = defaultdict(int)

    for texture_type in texture_table.keys():
        for capacity_type in capacity_table.keys():
            decision = decision_table[texture_type][capacity_type]
            decision_weight[decision] += texture_membership[texture_type] * \
                capacity_membership[capacity_type]

    return {
        'weights': decision_weight,
        'decision': sorted(list(decision_weight.items()), key=lambda x: x[1])[-1][0]
    }


def main():
    pprint(solve(0.4, 4))
    pprint(solve(0.25, 2.5))

    points = defaultdict(list)

    for x in np.arange(0.0, 1.0, 0.003):
        for y in np.arange(0.0, 6.0, 0.003):
            decision = solve(x, y)['decision']
            points[decision].append((x, y))

    print(points.keys())

    for decision in points.keys():
        print(decision)

        xs = [point[0] for point in points[decision]]
        ys = [point[1] for point in points[decision]]

        plt.scatter(xs, ys, s=5, color=decision_colors[decision], alpha=0.5)

    plt.savefig('decision-high-res.png', dpi=500)
    # plt.show()


if __name__ == '__main__':
    main()
