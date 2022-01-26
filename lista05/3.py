class Node:
    def __init__(self):
        self.n = 0
        self.e = ''

class Queue:
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

    def getSmaller(self):
        if self.first.n == 0:
            actualNode = 0
        else:
           actualNode = self.first.n

        smallerElement = self.first.e

        while actualNode != 0:
            if smallerElement > actualNode.e:
                smallerElement = actualNode.e
            actualNode = actualNode.n
        return smallerElement

q = Queue()
q.add("C")
q.add("A")
q.add("B")
print(q.isNotEmpty())
print(q.getSmaller())