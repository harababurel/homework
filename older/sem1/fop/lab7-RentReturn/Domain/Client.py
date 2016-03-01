'''
Created on Nov 13, 2015

@author: Vlad
'''

class Client:

        def __init__(self, id_client, name):

                self.__id_client = id_client
                self.__name = name

        @property
        def id_client(self):
                return self.__id_client

        @property
        def name(self):
                return self.__name

        def __str__(self):
                return "{0}. {1}".format(self.id_client, self.name)

        def __repr__(self):
                return str(self)

        def __eq__(self, other):
                return type(self) == type(other) and self.id_client == other.id_client

        def __hash__(self):

                # valoare unica, folosit pentru adaugare in dictionarele din repository

                return hash(self.id_client)
