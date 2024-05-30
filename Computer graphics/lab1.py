import glfw
from OpenGL.GL import *
from OpenGL.GLUT import *

vertices = [
    [0, 0.1],
    [0, 0.3],
    [0, 0.4],
    [0.2, 0.1],
    [0.2, 0.3],
    [0.2, 0.7],
    [0.6, 0.3],
    [0.6, 0.4],
    [6, 7],
]

colors = [
    [1.0, 0.0, 0.0],
    [0.0, 1.0, 0.0],
    [0.0, 0.0, 1.1],
    [1.0, 1.0, 0.0]
]

angle = 0.0
color_index = 0


def draw_polygon():
    glClear(GL_COLOR_BUFFER_BIT)
    glLoadIdentity()

    glPushMatrix()
    glRotatef(angle, 0.0, 0.0, 1.0)

    glBegin(GL_POLYGON)
    for i in range(len(vertices)):
        glColor3f(*colors[(color_index + i) % len(colors)])
        glVertex2f(*vertices[i])
    glEnd()

    glPopMatrix()
    glutSwapBuffers()


def key_pressed(key):
    global color_index
    global angle
    if key == b' ':
        color_index = (color_index + 1) % len(colors)
    elif key == glfw.KEY_S:
        angle += 10.0
    elif key == glfw.KEY_L:
        angle -= 10.0


def update_scene(value):
    global angle
    angle += 210.0
    if angle > 360.0:
        angle -= 360.0

    glutPostRedisplay()
    glutTimerFunc(30, update_scene, 0)


glutInit()
glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB)
glutInitWindowSize(1500, 900)
glutCreateWindow(b"Moving")

glutDisplayFunc(draw_polygon)
glutKeyboardFunc(key_pressed)
glClearColor(0.0, 0.0, 0.0, 1.0)

glMatrixMode(GL_PROJECTION)
glLoadIdentity()
glOrtho(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0)

glutTimerFunc(30, update_scene, 0)
glutMainLoop()
