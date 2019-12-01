class room:
    def __init__(self,row,col):
        self.coordinates = (row,col)
        self.doors = {'N':False, 'S':False, 'E': False, 'W':False}
        self.dist = 0
 
if __name__ == '__main__':
    import numpy as np
    import cv2
    import random
    import matplotlib.pyplot as plt
    import re
    opposed = {'N':'S', 'S':'N', 'E':'W', 'W':'E'}
    coords = {'N':(1,0), 'S':(-1,0), 'E':(0,1), 'W':(0,-1)}
    file = open("input.txt", 'r')
    stringfile = file.read().rstrip()
    start = room(0,0)
    curr = start
    branch = []
    maxdist = 0
    count = 0
    rooms = []
    for i in stringfile:
        if i == '^': continue
        elif i == '(':
            branch.append(curr)
        elif i == '|':
            curr = branch.pop()
            branch.append(curr)
        elif i == ')':
            curr = branch.pop()
        elif i == '$':
            print("done")
        else:
            if not curr.doors[i]:
                curr.doors[i] = room(coords[i][0]+curr.coordinates[0], coords[i][1]+curr.coordinates[1])
                newdist = curr.dist+1
                newroom = curr.doors[i]
                if not newroom.dist: newroom.dist = newdist
                else: newroom.dist = min(newdist, newroom.dist)
                if newroom.dist > maxdist: maxdist = newdist
                newroom.doors[opposed[i]] = curr
                rooms.append(newroom)
                curr = newroom
            else:
                curr.doors[i].dist = min(curr.doors[i].dist, curr.dist+1)
                curr = curr.doors[i]
    for r in rooms:
        if r.dist >= 1000: count += 1
    print(maxdist, count)