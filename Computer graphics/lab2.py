from OpenGL.GL import *
import glfw
import math

delta = 0.1
angle1, angle2, angle3 = 0, 0, 0
window = None
display = None
msh = 0.2
m = []

d_x, d_z = 10, 0

colors = (
    (0, 1.3, 0.1),
    (0, 1, 0),
    (0.4, 0, 1),
    (0.1, 0.1, 0.9),
    (0, 2, 0.7),
    (0.1, 0.5, 0.3)
)

surfaces = (
    (0, 1, 2, 3),
    (3, 2, 7, 6),
    (6, 7, 5, 4),
    (4, 5, 1, 0),
    (1, 5, 7, 2),
    (4, 0, 3, 6)
)

verticies = (
    (1, -1, -1),
    (1, 1, -1),
    (-1, 1, -1),
    (-1, -1, -1),
    (1, -1, 1),
    (1, 1, 1),
    (-1, -1, 1),
    (-1, 1, 1)
)

edges = (
    (0, 1),
    (0, 3),
    (0, 4),
    (2, 1),
    (2, 3),
    (2, 7),
    (6, 3),
    (6, 4),
    (6, 7),
    (5, 1),
    (5, 4),
    (5, 7)
)


def my_glRotate(angle, x, y, z):
    if x == 1:
        matrixX = [
            1, 0, 0, 0,
            0, math.cos(angle), -math.sin(angle), 0,
            0, math.sin(angle), math.cos(angle), 0,
            0, 0, 0, 1
        ]
        glMultMatrixd(matrixX)
    if y == 1:
        matrixY = [
            math.cos(angle), 0, math.sin(angle), 0,
            0, 1, 0, 0,
            -math.sin(angle), 0, math.cos(angle), 0,
            0, 0, 0, 1
        ]
        glMultMatrixd(matrixY)
    if z == 1:
        matrixZ = [
            math.cos(angle), -math.sin(angle), 0, 0,
            math.sin(angle), math.cos(angle), 0, 0,
            0, 0, 1, 0,
            0, 0, 0, 1
        ]
        glMultMatrixd(matrixZ)


def my_glScalef(x, y, z):
    matrix = [
        x, 0, 0, 0,
        0, y, 0, 0,
        0, 0, z, 0,
        0, 0, 0, 1
    ]

    glMultMatrixd(matrix)


def main():
    global window

    if not glfw.init():
        return

    window = glfw.create_window(1200, 900, "Lab2", None, None)
    if not window:
        glfw.terminate()
        return

    glfw.make_context_current(window)
    glfw.set_key_callback(window, key_callback)
    while not glfw.window_should_close(window):
        display()

    glfw.destroy_window(window)
    glfw.terminate()


def genmatrix():
    global m

    fi, tao = 35.26, 45
    sf, cf, st, ct = math.sin(fi), math.cos(fi), math.sin(tao), math.cos(tao)
    m = [
        cf, 0, sf, 0,
         sf * st, ct, -cf * st, 0,
         sf * ct, -sf, -cf * ct, 0,
         0, 0, 0, 1
         ]


def key_callback(window, key, scancode, action, mods):
    global angle1, angle2, angle3, delta
    k = 1

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


def cube_default(x_shift=4):
    glPushMatrix()

    my_glRotate(50, 1, 0, 0)

    my_glScalef(msh, msh, msh)
    my_glRotate(15, 0, 1, 0)
    my_glRotate(10, 0, 0, 1)

    glBegin(GL_QUADS)

    for x, surface in enumerate(surfaces):
        glColor3fv(colors[x])
        for i, vertex in enumerate(surface):
            v = list(verticies[vertex])
            v[0] += x_shift
            glVertex3fv(v)

    glEnd()

    glBegin(GL_LINES)
    for edge in edges:
        for vertex in edge:
            glColor3fv((1.0, 0.0, 0.0))
            v = list(verticies[vertex])
            v[0] += x_shift
            glVertex3fv(v)
    glEnd()

    glPopMatrix()


def cube():
    global angle1, angle2, angle3, m

    genmatrix()

    glPushMatrix()

    my_glScalef(msh, msh, msh)

    my_glRotate(angle1, 0, 0, 1)
    my_glRotate(angle2, 0, 1, 0)
    my_glRotate(angle3, 1, 0, 0)

    glMultMatrixd(m)

    glBegin(GL_QUADS)
    for x, surface in enumerate(surfaces):
        glColor3fv(colors[x])
        for i, vertex in enumerate(surface):
            glVertex3fv(verticies[vertex])

    glEnd()

    glBegin(GL_LINES)
    for edge in edges:
        for vertex in edge:
            glColor3fv((1.0, 0.0, 0.0))
            glVertex3fv(verticies[vertex])

    glEnd()
    glPopMatrix()


def display():
    #glClear(GL_COLOR_BUFFER_BIT)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glLoadIdentity()
    glClearColor(0.0, 0.0, 0.0, 0.0)

    glEnable(GL_DEPTH_TEST)

    glDepthFunc(GL_LESS)

    cube()
    cube_default()

    glfw.swap_buffers(window)
    glfw.poll_events()


genmatrix()
main()

