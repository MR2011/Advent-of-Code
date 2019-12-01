#!/usr/bin/env python3
from PIL import Image
with open('input.txt', 'r') as f:
    data = f.read().splitlines()

class Point(object):
    def __init__(self, pos, vel=None):
        self.pos = pos
        if not vel:
            self.vel = [0, 0]
        else:
            self.vel = vel

    @property
    def x(self):
        return self.pos[0]

    @property
    def y(self):
        return self.pos[1]

    def __repr__(self):
        return str(self.pos)

    def tick(self):
        self.pos[0] += self.vel[0]
        self.pos[1] += self.vel[1]

    def untick(self):
        self.pos[0] -= self.vel[0]
        self.pos[1] -= self.vel[1]

points = []

def makePoints():
    for p in data:
        x = p.replace("position=<", "").replace("velocity=<", "")
        x = x.replace(">", "").replace("<", "").replace(",", "")
        y = [v for v in x.split(" ") if v]
        pos = [int(y[0]), int(y[1])]
        vel = [int(y[2]), int(y[3])]
        points.append(Point(pos, vel))

def tlbr(pts):
    topleft = Point([0, 0])
    botright = Point([0, 0])
    for p in pts:
        if p.x + p.y < topleft.x + topleft.y:
            topleft = p
        if p.x + p.y > botright.x + botright.y:
            botright = p
    return [topleft, botright]

def gridSize(topleft, botright):
    size = (botright.x-topleft.x)*(botright.y-topleft.y)
    return size

def displayGrid():
    img = Image.new('RGB', (400, 400), "black")
    pixels = img.load()
    for p in points:
        pixels[p.x, p.y+10] = (255, 255, 255)
    img.show()

def tickAll():
    for p in points:
        p.tick()

def untickAll():
    for p in points:
        p.untick()

makePoints()
minsize = None
counter = 0
while True:
    coords = tlbr(points)
    gsize = gridSize(*coords)
    if not minsize:
        minsize = gsize
    elif gsize < minsize:
        minsize = gsize
    elif gsize > minsize:
        untickAll()
        counter -= 1
        break
    tickAll()
    counter += 1

print("Smallest point cloud size:", minsize, "@ (P2)", counter)
displayGrid()