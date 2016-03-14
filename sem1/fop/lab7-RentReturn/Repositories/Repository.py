'''
Created on Nov 13, 2015

@author: Vlad
'''

class GenericRepository:
	
	def __init__(self):
		
		self._storage = []
		
	def add(self, obj):
		
		if obj in self._storage:
			raise KeyError("Duplicate object!")
		
		self._storage.append(obj)
		
	def update(self, obj):
		
		if obj not in self._storage:
			raise KeyError("Object does not exist!")
		
		for i in range(len(self._storage)):
			if self._storage[i] == obj:
				self._storage[i] = obj
		
	def get_all(self):
		
		return self._storage