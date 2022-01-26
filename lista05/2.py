class Node:
    def __init__(self):
        self.n = 0
        self.e = ''

class Stack:
    def __init__(self):
        self.first = 0
    
    def add(self, newElement):
        newNode = Node()
        newNode.e = newElement
        if self.first != 0:
            newNode.n = self.first
        self.first = newNode
    
    def remove(self):
        nodeToBeRemoved = self.first
        self.first = nodeToBeRemoved.n
        return nodeToBeRemoved.e
    
    def isNotEmpty(self):
        return self.first != 0

s = Stack()
s.add(" Baltimore ")
s.add(" Lord ")
s.add(" Sir")
s.isNotEmpty()

while(s.isNotEmpty()):
  print(s.remove())