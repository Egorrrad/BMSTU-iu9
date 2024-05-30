import random

from OpenGL.GL import *
import glfw
import math

import numpy as np

delta = 0.1
angle1, angle2, angle3 = 0, 0, 0
window = None
display = None
msh = 0.7
m = []

scale = 1
subdiv = 5
name = 'Icosomething'

a = np.pi / 2
l = .25
T = (1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1, 0,
     0, 0, 0, 1)

faces = (
    (0, 1, 2, 3),
    (3, 2, 7, 6),
    (6, 7, 5, 4),
    (4, 5, 1, 0),
    (1, 5, 7, 2),
    (4, 0, 3, 6)
)

surfaces = (
    # 5 полигонов вокруг нулевой точки
    [1, 0, 11, 5],
    [1, 0, 5, 1],
    [1, 0, 1, 7],
    [1, 0, 7, 10],
    [1, 0, 10, 11],

    # Полигоны вокруг
    [1, 1, 5, 9],
    [1, 5, 11, 4],
    [1, 11, 10, 2],
    [1, 10, 7, 6],
    [1, 7, 1, 8],

    # 5 полигонов вокруг третьей точки
    [1, 3, 9, 4],
    [1, 3, 4, 2],
    [1, 3, 2, 6],
    [1, 3, 6, 8],
    [1, 3, 8, 9],

    # Полигоны вокруг
    [1, 4, 9, 5],
    [1, 2, 4, 11],
    [1, 6, 2, 10],
    [1, 8, 6, 7],
    [1, 9, 8, 1],
)

colors = []
for i in range(len(surfaces)):
    color1 = (random.random(), random.random(), random.random())
    colors.append(color1)

PHI = (1 + math.sqrt(5)) / 2


def vertex(x, y, z):
    length = math.sqrt(x ** 2 + y ** 2 + z ** 2)

    return [(i * scale) / length for i in (x, y, z)]


verticies = (
    vertex(-1, PHI, 0),
    vertex(1, PHI, 0),
    vertex(-1, -PHI, 0),
    vertex(1, -PHI, 0),

    vertex(0, -1, PHI),
    vertex(0, 1, PHI),
    vertex(0, -1, -PHI),
    vertex(0, 1, -PHI),

    vertex(PHI, 0, -1),
    vertex(PHI, 0, 1),
    vertex(-PHI, 0, -1),
    vertex(-PHI, 0, 1),
)


def main():
    global window

    if not glfw.init():
        return

    window = glfw.create_window(1200, 900, "Lab3 " + name, None, None)
    if not window:
        glfw.terminate()
        return

    glfw.make_context_current(window)
    glfw.set_key_callback(window, key_callback)
    while not glfw.window_should_close(window):
        display()

    glfw.destroy_window(window)
    glfw.terminate()


def key_callback(window, key, scancode, action, mods):
    global angle1, angle2, angle3, delta
    k = 10

    if key == glfw.KEY_A:
        angle1 -= k
    if key == glfw.KEY_D:
        angle1 += k
    if key == glfw.KEY_W:
        angle2 -= k
    if key == glfw.KEY_S:
        angle2 += k
    if key == glfw.KEY_Q:
        angle3 -= k
    if key == glfw.KEY_E:
        angle3 += k
    if key == glfw.KEY_Z:
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    if key == glfw.KEY_X:
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)

    global a
    global l
    global T
    if key == glfw.KEY_V:
        a += np.pi / 9
        T = (1, 0, -l * np.cos(a), 0,
             0, 1, -l * np.sin(a), 0,
             0, 0, 1, 0,
             0, 0, 0, 1)


(dx, dy, dz) = (1, 1, 1)


def ikosaidr():
    global angle1, angle2, angle3, m
    global dx, dy, dz

    glPushMatrix()

    glScalef(msh, msh, msh)

    glRotate(angle1, 0, 0, 1)
    glRotate(angle2, 0, 1, 0)
    glRotate(angle3, 1, 0, 0)

    if angle1 % 2 == 0:
        (dx, dy, dz) = (dx * (-1), dy * (-1), dz * (-1))

    # glTranslate(dx,dy,dz)

    glClear(GL_COLOR_BUFFER_BIT)
    glMultMatrixd(T)

    glBegin(GL_QUADS)
    for x, surface in enumerate(surfaces):
        glColor3fv(colors[x])
        for i, vertex in enumerate(surface):
            glVertex3fv(verticies[vertex])

    glEnd()
    glPopMatrix()


def display():
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glLoadIdentity()
    glClearColor(0.0, 0.0, 0.0, 0.0)

    glEnable(GL_DEPTH_TEST)

    glDepthFunc(GL_LESS)

    ikosaidr()

    glfw.swap_buffers(window)
    glfw.poll_events()


main()
