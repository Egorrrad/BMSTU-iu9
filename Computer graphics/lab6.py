import math
import time

import glfw
import numpy as np
import pygame
from OpenGL.GL import *
from OpenGL.GLUT import *

window_width = 1600
window_height = 600
scale = 0.525

k_v = 1.5

animation_mode = False
texture_sides = None

fi = 0
tetha = 0

flying_speed = 0
V = 0.0009 * 10
acl = 0.00006 * 5

light_mode = False
texture_mode = 1
filling_mode = True


def main():
    if not glfw.init():
        return
    window = glfw.create_window(window_width, window_height, "Lab6", None, None)
    if not window:
        glfw.terminate()
        return
    glfw.make_context_current(window)

    glfw.set_key_callback(window, key_callback)
    glfw.set_mouse_button_callback(window, mouse_callback)
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    generate_texture()
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    light()

    while not glfw.window_should_close(window):
        display(window)

    glfw.destroy_window(window)
    glfw.terminate()


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

delta = 2.1
angle1, angle2, angle3 = 0, 0, 0
window = None
display = None
msh = 1.7
m = []

subdiv = 5

(dx, dy, dz) = (1, 1, 1)

texture_coords = (
    (0, 0),
    (1, 0),
    (1, 1),
    (0, 1)
)


def draw_icosahedron():
    glBegin(GL_QUADS)
    for surface in surfaces:
        x = 0
        for vertex in surface:
            glTexCoord2fv(texture_coords[x])
            x += 1
            glVertex3fv(verticies[vertex])
    glEnd()


def display(window):
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LESS)
    glClearColor(0.0, 0.0, 0.0, 0.0)
    glLoadIdentity()
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    if animation_mode:
        move_object()
    glScale(scale, scale, scale)
    glTranslatef(0, flying_speed, 0)
    glRotatef(fi, 1, 0, 0)
    glRotatef(tetha, 0, 1, 0)
    draw_icosahedron()
    glfw.swap_buffers(window)
    glfw.poll_events()


def key_callback(window, key, scancode, action, mods):
    global x_angle, y_angle, scale, animation_mode, fi, tetha, k_v
    if action == glfw.PRESS and key == glfw.KEY_ENTER:
        mode = glGetIntegerv(GL_POLYGON_MODE)
        if mode[1] == GL_LINE:
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        else:
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
    if action == glfw.PRESS or action == glfw.REPEAT:
        if key == glfw.KEY_A:
            fi -= 4
        if key == glfw.KEY_D:
            fi += 4
        if key == glfw.KEY_W:
            tetha -= 4
        if key == glfw.KEY_S:
            tetha += 4
        if key == glfw.KEY_UP:
            scale += 0.05
            k_v -= 0.15
        if key == glfw.KEY_DOWN:
            scale -= 0.05
            k_v += 0.10
        global light_mode
        if key == glfw.KEY_L:
            if glIsEnabled(GL_LIGHTING):
                glDisable(GL_LIGHTING)
            else:
                glEnable(GL_LIGHTING)
            return
        if key == glfw.KEY_M:
            animation_mode = not animation_mode
            return

    global a, l, T
    if key == glfw.KEY_V:
        a += np.pi / 9
        T = (1, 0, -l * np.cos(a), 0,
             0, 1, -l * np.sin(a), 0,
             0, 0, 1, 0,
             0, 0, 0, 1)


def mouse_callback(window, button, action, mods):
    global filling_mode, texture_mode
    if action == glfw.PRESS:
        if button == glfw.MOUSE_BUTTON_LEFT:
            filling_mode = not filling_mode
            if filling_mode:
                glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
            else:
                glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
        elif button == glfw.MOUSE_BUTTON_RIGHT:
            texture_mode = not texture_mode
            if texture_mode:
                glBindTexture(GL_TEXTURE_2D, texture_sides)
            else:
                glBindTexture(GL_TEXTURE_2D, 0)


def move_object():
    global V, flying_speed, acl
    flying_speed -= V
    V += acl
    middle = window_height//2
    end_down = window_height - 5
    limit = k_v
    if flying_speed < -limit or flying_speed > limit:
        V = -V


def generate_texture():
    textureSurface = pygame.image.load('456.jpg')
    textureData = pygame.image.tostring(textureSurface, "RGBA", 1)
    width = textureSurface.get_width()
    height = textureSurface.get_height()

    glEnable(GL_TEXTURE_2D)
    texid = glGenTextures(1)

    glBindTexture(GL_TEXTURE_2D, texid)

    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height,
                 0, GL_RGBA, GL_UNSIGNED_BYTE, textureData)


def light():
    glEnable(GL_LIGHTING)
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, [[0.2, 0.2, 0.2, 1]])
    glLightfv(GL_LIGHT0, GL_AMBIENT, [0, 0, 0, 1])
    glLightfv(GL_LIGHT0, GL_DIFFUSE, [0.4, 0.4, 0.4, 1])
    glLightfv(GL_LIGHT0, GL_SPECULAR, [1, 1, 1, 1])
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, [1, 1, 1, 1])
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, 50.0)
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, [0, 0, 0, 1])
    glLightfv(GL_LIGHT0, GL_POSITION, [[1, 2, -2, 0]])
    glEnable(GL_LIGHT1)
    glEnable(GL_LIGHT2)
    glEnable(GL_COLOR_MATERIAL)
    glLightf(GL_LIGHT2, GL_CONSTANT_ATTENUATION, 0.0)
    glLightf(GL_LIGHT2, GL_LINEAR_ATTENUATION, 0.2)
    glLightf(GL_LIGHT2, GL_QUADRATIC_ATTENUATION, 0.4)


start = time.monotonic()
main()
stop = time.monotonic()
res = round(stop - start, 3)
print('slow:', res)
