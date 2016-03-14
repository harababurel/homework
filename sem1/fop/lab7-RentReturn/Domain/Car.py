'''
Created on Nov 13, 2015

@author: Vlad
'''

class Car:

        def __init__(self, id_car, name):

                self.__id_car = id_car
                self.__name = name

        @property
        def id_car(self):
                return self.__id_car

        @property
        def name(self):
                return self.__name

        def __str__(self):
                return "{0}. {1}".format(self.id_car, self.name)

        def __repr__(self):
                return str(self)

        def __eq__(self, other):
                return type(self) == type(other) and self.id_car == other.id_car

        def __hash__(self):

                # valoare unica, folosit pentru adaugare in dictionarele din repository

                return hash(self.id_car)
